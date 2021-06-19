%%%-------------------------------------------------------------------
%%% @author zkieda
%%% @copyright (C) 2021, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 11. May 2021 1:11 PM
%%%-------------------------------------------------------------------
-module(worker_scripted_event).
-author("zkieda").

%%
-export([load/1]).
-export([new_state/1, auto_capture/0, update_state/2]).

-record(scripted_network_params, {
  graph,
  events,
  tests,
  plugins,
  args
}).

% representation of the module we can load from
-type scripted_network_params() :: #scripted_network_params{
  graph   :: caffe_graph:graph(),
  % vertex -> list of events to occur
  events  :: [{caffe_graph:vertex(), [Event::any()]}],
  % plugin id -> vertex -> list of expected plugin states
  tests   :: [{caffe:plugin_id(),
    [{caffe_graph:vertex(), [caffe:plugin_state()]}]
  }],
  % plugins we deploy for each vertex
  plugins :: [caffe:plugin_id()],
  % common plugins for vertex
  args    :: #{
    atom() => any()
  }
}.

-spec verify(scripted_network_params()) -> ok | no_return().
verify(#scripted_network_params{
    graph = {V, _},
    events = Events,
    tests = Tests,
    plugins = Plugins }) ->
  % Vertex -> length of events we are playing
  EventLengths = maps:from_list(lists:map(fun({Vertex, E}) -> {Vertex, length(E)} end, Events )),
  ok = case lists:usort(maps:keys(EventLengths)) -- lists:usort(V) of
    % ensure that each vertex in Events corresponds to a vertex deployed
    [] -> ok;
    UnknownVerts -> throw({unknown_vertices, events, UnknownVerts})
  end,
  ok = case Tests of
    test_mode -> ok;
    _ -> case lists:usort(lists:map(fun({Plugin, _}) -> Plugin end, Tests)) -- lists:usort(Plugins) of
           % ensure that each plugin in Tests corresponds to a plugin deployed
           [] -> ok;
           UnknownPlugins -> throw({unknown_plugins, UnknownPlugins})
         end
  end,
  true = case Tests of
     test_mode -> true;
     _ -> lists:all(
       fun({_, test_mode}) -> true;
       ({Module, PluginTests}) ->
         % represents the length of tests that we will apply on each vertex
         TestLengths = maps:from_list(lists:map(fun({Vertex, T}) -> {Vertex, length(T)} end, PluginTests)),

         % we want each list of tests on a vertex to correspond to a vertex deployed
         case lists:usort(maps:keys(TestLengths)) -- lists:usort(V) of
           [] -> true;
           Unknown -> throw({unknown_vertices, tests, Unknown})
         end,

         % check that events specified on each vertex match the number of tests we perform on each vertex
         if TestLengths /= EventLengths -> throw({mismatch, Module, TestLengths, EventLengths});
           true -> true
         end
       end, Tests)
   end,
  ok.

-spec load(module()) -> caffe_graph:network().
load(Module) ->
  P = #scripted_network_params{
    graph = G = {V, _} = caffe_graph:load(Module),
    events = apply(Module, events, []),
    tests = apply(Module, tests, []),
    plugins = Plugins = caffe_util:get_exported_default(Module, plugins, []),
    args = BaseArgs = caffe_util:get_exported_default(Module, args, maps:new())
  },
  % verify consistency of loaded module
  ok = verify(P),
  EventMap = maps:from_list(P#scripted_network_params.events),
  TestMap = case P#scripted_network_params.tests of
              test_mode -> test_mode;
              T -> maps:from_list(lists:map(
                fun({Plugin, test_mode}) -> {Plugin, test_mode};
                   ({Plugin, Tests}) -> {Plugin, maps:from_list(Tests)}
                end, T))
            end,
  % generate args
  Specs = maps:from_list(lists:map(
    fun(Vertex) ->
      Tests = case TestMap of
                test_mode -> test_mode;
                _ -> maps:map(fun(_, test_mode) -> test_mode;
                                 (_, Tests) -> maps:get(Vertex, Tests, [])
                              end, TestMap)
              end,
      WorkerArgs = #{
        % [ event ]
        worker_scripted_events => maps:get(Vertex, EventMap, []),
        % plugin -> [ test ] | test_mode
        worker_scripted_event_tests => Tests,
        % plugins
        worker_scripted_event_plugins => Plugins
      },
      VertexArgs = maps:merge(WorkerArgs, BaseArgs),
      CaffeSpec = {
        Plugins,
        worker_scripted_event,
        VertexArgs
      },
      {Vertex, CaffeSpec}
    end, V)),
  caffe:build(G, Specs).

% as we progress through events queued and move them into completed
-record(worker_state, {
  events_scripted,
  tests_scripted,
  plugins
}).

new_state(#{worker_scripted_events := Events,
  worker_scripted_event_tests := Tests,
  worker_scripted_event_plugins := Plugins}) ->
  #worker_state{
    events_scripted = Events,
    tests_scripted = Tests,
    plugins = Plugins
  }.

% we manually capture items
auto_capture() -> false.

check_result(Plugin, State, ExpectedState) ->
  case caffe_util:is_exported(Plugin, format, 1) of
    true -> ActualState = apply(Plugin, format, [caffe:get_plugin_state(Plugin, State)]),
      if ExpectedState == ActualState ->
        caffe:log(State, "test passed ~w: ~p\n", [Plugin, ExpectedState]),
        ok;
        true -> throw({invariant, Plugin, ExpectedState, ActualState, graph_state:get_vertex(State)})
      end;
    false -> throw({undef, Plugin, compare})
  end.

update_state(State, W = #worker_state{
      events_scripted = [Event|Events],
      tests_scripted = Tests }) ->
  State2 = case Event of
    {'receive', Messages} -> messenger:wait_messages(State, Messages);
    _ -> caffe:process_event(Event, State)
  end,
  Rest = case Tests of
           test_mode -> lists:foreach(
             fun(Module) ->
               caffe:log(State, "test_mode ~s: ~p", [Module, apply(Module, format, [caffe:get_plugin_state(Module, State2)])])
             end, W#worker_state.plugins),
             test_mode;
           _ -> maps:map(
             fun(Module, test_mode) ->
                   caffe:log(State, "test_mode ~s: ~p", [Module, apply(Module, format, [caffe:get_plugin_state(Module, State2)])]),
                   test_mode;
                (Module, [Test|Tail]) ->
                   ok = check_result(Module, State2, Test),
                   Tail
             end, Tests)
         end,
  {State2, W#worker_state{events_scripted = Events, tests_scripted = Rest}};
%%  case Event of
%%    {'internal', Msg} -> ;
%%    {'send', Vertex, Msg} -> ;
%%    {'send_control', Vertex, Msg} -> ;
%%    {'receive', Msg} -> ;
%%    {'receive_control', Msg} -> ;
%%    Other -> caffe:process_order(Event, State)
%%  end,
%%
%%  maps:map(
%%    fun(Module, [Test|Rest]) -> {Test, Rest},
%%    Tests
%%  )
%%  update_state(State, UserState#worker_state{events_scripted = Events, tests_scripted = Tests});
update_state(State, S = #worker_state{ events_scripted = [] }) ->
  {terminator:terminate(State), S}.

% we can enforce ordering on individual vertices, but is it possible to
% receive events out of order?
%% Ordering:
%{vertex_a, 'internal', internal_a1},
%{vertex_b, 'internal', internal_b1},
%{vertex_a, 'send', vertex_c, message_a1},
%{vertex_c, 'receive', message_a1},
%{vertex_a, 'receive', message_b1}

%{vertex_b, 'receive', message_c1},
%{vertex_b, 'send', vertex_a, message_b1}


%{vertex_c, 'internal', internal_c1},
%{vertex_c, 'send', vertex_b, message_c1},
%{vertex_c, 'internal', internal_c2}