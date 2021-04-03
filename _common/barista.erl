%%%-------------------------------------------------------------------
%%% @author zkieda
%%% @doc
%%%   barista is you caffe's go-to process to run in your caffe. It's basically a plugin-manager.
%%%
%%%   A Bad Analogy:
%%%
%%%   baristas have a set of plugins in the caffe (latte, customer, beans, tables)
%%%   that can change in response to some other signal (customer orders latte, customer at counter, take beans, douche can't clean up after themselves)
%%%   multiple plugins may listen on the same signal: deliver_latte and espresso_machine might listen to a signal like latte_made
%%%
%%%   a plugin can have dependencies:
%%%     latte might depend on espresso_machine.
%%%     latte can send froth_milk signal and wipe_espresso_machine to espresso_machine
%%%     signals are fulfilled immediately, and a barista can only do one thing at a time.
%%%
%%%     dependencies should form a DAG, and are auto-loaded
%%%     Please, I don't need to have turing complete behavior for plugins talking to each other.
%%%
%%%   each plugin keeps track of its state:
%%%     for example beans might keep track of roast beans and non-roasted beans available
%%%
%%%   a plugins can view the state of other plugins in the caffe:
%%%     view the number of people currently in the caffe when deciding to let another customer in due to COVID/or maybe fire safety regulations if this library outlasts it
%%%     accessed via get_plugin_state
%%%
%%%   you can specify invariants:
%%%       no more than X people in your caffe due to covid (or fire) protocols
%%%       if invariant is broken, the caffe crashes and burns in a smoldering fire (the process fails)
%%%
%%%       another example - invariant on latte is that the espresso_machine is clean - we make latte and clean it up.
%%%       if the espresso machine is not clean after making a latte, the caffe crashes and burns in hellfire
%%% @end
%%% Created : 01. Apr 2021 10:11 PM
%%%-------------------------------------------------------------------
-module(barista).
-author("zkieda").

%% API
-export([get_plugin_state/2, process_order/2]).     % accessor functions
-export([new_state/1, update_state/1,               % caffe implementation
        get_state_info/1, add_signal_to_state/2]).

-export_type([barista_state/0]).

-type plugin_id() :: module().

-record(barista_state, {
  plugin_spec_map,
  plugin_state_map,
  plugin_list,
  user_func = none,
  incoming = []
}).

%% plugin manager state
-opaque barista_state() :: caffe:vertex_state(#barista_state{
  %% Plugin Specs %%
  plugin_spec_map :: #{ plugin_id() => plugin_spec() },

  %% Plugin States %%
  plugin_state_map :: #{ plugin_id() => plugin_state() },

  %% Plugins %%
  plugin_list :: [plugin_id()],

  %% User-defined function %%
  user_func :: user_func(),

  %% queued incoming signals %%
  incoming :: [any()]
}).

-type plugin_spec() :: #{
  dependencies => [plugin_id()],      % default []
  unmodifiable => [atom()|plugin_id], % default [plugin_id]

  %% these fields are auto-populated when we add a new plugin %%
  new_plugin => mfa(),               % default new_plugin/2
  update_plugin => mfa(),            % default update_plugin/2|update_state/2
  invariant => mfa()                 % default invariant/2|invariant/3. optional.
}.

%%% plugin state is user defined, plugin_id is always specified and cannot be overwritten
-type plugin_state() :: #{
  plugin_id := atom(),
  any() => any()
}.

%%% user-defined function to run on vertex
-type user_func() :: { anonymous, fun((barista_state()) -> barista_state())}
                   | { named, module(), atom() }
                   | none.

%% we create a digraph with properties private and acyclic
-type plugin_dag() :: digraph:graph().

get_plugin_impl(Plugin, Module, dependencies) ->
  Dependencies = sets:from_list(maps:get(dependencies, Plugin)),
  case sets:is_element(0, Dependencies) of
    true  -> apply(Module, dependencies, []);
    false -> []
  end;
get_plugin_impl(Plugin, Module, unmodifiable) ->
  Unmodifiable = sets:from_list(maps:get(unmodifiable, Plugin)),
  case sets:is_element(0, Unmodifiable) of
    true  -> [plugin_id|apply(Module, unmodifiable, [])];
    false -> [plugin_id]
  end;
get_plugin_impl(Plugin, Module, new_plugin) ->
  % new_plugin( VertexArgs, CurrentState ) -> PluginState
  NewPlugin = sets:from_list(maps:get(new_plugin, Plugin)),
  case sets:is_element(2, NewPlugin) of
    true  -> {Module, new_plugin, 2};
    false -> {bad_plugin, {unimplemented, [{new_plugin, 2}]}}
  end;
get_plugin_impl(Plugin, Module, FunctionName) when
    FunctionName =:= update_plugin;
    FunctionName =:= invariant ->
  % chooses one of two impls specified in the module for the following:
  %
  %   update_plugin( Message, PluginState ) -> PluginState
  %   or
  %   update_state( Message, { PluginState, State } ) -> { PluginState, State }
  %
  %   invariant( Message, StateOld, StateNew ) -> ok | { broken, ... }
  %   or
  %   invariant( Message, StateDelta ) -> ok | { broken, ... }
  %   or none
  {{F1, A1}, {F2, A2}, Required} = case FunctionName of
                           update_plugin    -> {{update_plugin, 2}, {update_state, 2}, true};
                           invariant        -> {{invariant, 2}, {invariant, 3}, false}
                         end,

  Impl1 = sets:from_list(maps:get(F1, Plugin)),
  Impl2  = sets:from_list(maps:get(F2, Plugin)),
  case { sets:is_element(A1, Impl1), sets:is_element(A2, Impl2) } of
    { true, false }  -> {Module, F1, A1};
    { false, true }  -> {Module, F2, A2};
    { true, true }   -> {bad_plugin, {choose_one, [{F1, A1}, {F2, A2}]}};
    { false, false } -> if Required -> {bad_plugin, {unimplemented, [{F1, A1}, {F2, A2}]}};
                          true -> none
                        end
  end.

-spec load_plugin_spec(PluginID::plugin_id()) -> PluginSpec::plugin_spec().
load_plugin_spec(PluginID) ->
  %% get exported functions in implementation %%
  PluginImpl = caffe_util:get_exported(PluginID, sets:from_list([
    dependencies, unmodifiable,
    new_plugin, update_plugin,
    invariant
  ])),

  %% Construct plugin specification %%
  #{
    dependencies => get_plugin_impl(PluginImpl, PluginID, dependencies),
    unmodifiable => get_plugin_impl(PluginImpl, PluginID, unmodifiable),
    new_plugin   => get_plugin_impl(PluginImpl, PluginID, new_plugin),
    invariant    => get_plugin_impl(PluginImpl, PluginID, invariant)
  }.

-spec get_plugin_state(PluginID::plugin_id(), State::barista_state()) -> PluginState::plugin_state().
get_plugin_state(PluginID, #barista_state{plugin_state_map = PluginStates}) -> maps:get(PluginID, PluginStates).

-spec process_order(Message::any(), State::barista_state()) -> barista_state().
process_order(Message, State = #barista_state{plugin_list = Plugins}) -> process_order_helper(Message, State, Plugins).

process_order_helper(Message, State0 = #barista_state{plugin_spec_map = PluginSpecs, plugin_state_map = PluginStates}, [Plugin|Plugins]) ->
  Spec = maps:get(Plugin, PluginSpecs),
  PluginState0 = maps:get(Plugin, PluginStates),
  % extract Spec
  % todo - should we add enforcement onto the dependencies?
  % todo   Possibly have a current_plugin in the State, and ensure one plugin can only send an order to a dependent
  #{ dependencies := Dependencies, unmodifiable := Unmodifiable,
      update_plugin := UpdatePlugin, invariant := Invariant } = Spec,

  {PluginStateNew, StateNew, Ignored} = case UpdatePlugin of
    {PluginMod, update_state, 2} ->
      % update state
      case apply(PluginMod, update_state, [Message, {PluginState0, State0}]) of
        % plugin ignores this message
        ignore -> {PluginState0, State0, true};
        % merge plugin state, use updated state
        {PluginState1, State1} -> {PluginState1, State0#barista_state{plugin_state_map = maps:update(PluginMod, PluginState1, State1)}, false}
      end;
    {PluginMod, update_plugin, 2} ->
      case apply(PluginMod, update_plugin, [Message, PluginState0]) of
        % plugin ignores this message
        ignore -> {PluginState0, State0, true};
        % merge plugin state
        PluginState1 -> {PluginState1, State0#barista_state{plugin_state_map = maps:update(PluginMod, PluginState1, PluginStates)}, false}
      end
  end,

  ok = if Ignored -> ok;
          % diff plugin state
          true -> PluginStateDelta = caffe_util:diff_deep(PluginState0, PluginStateNew),

          % check that fields designated as unmodifiable are indeed not modified
          ok = case maps:with(Unmodifiable, PluginStateDelta) of
            [] -> ok;
            Modifications -> {error, {modified, Modifications}}
          end,

          % run plugin assertion
          ok = case Invariant of
                 none -> ok;
                 {Mod, invariant, 3} -> apply(Mod, invariant, [Message, PluginState0, PluginStateNew]);
                 {Mod, invariant, 2} -> apply(Mod, invariant, [Message, PluginStateDelta])
               end
       end,

  process_order_helper(Message, StateNew, Plugins);
process_order_helper(_, State, []) -> State.

%%% Implementation %%%
-spec new_state(caffe_graph:vertex_args()) -> barista_state().
new_state(Args = #{barista_user_func := UserFunc,
  barista_plugin_list := Plugins}) ->

  % we auto-load all dependent plugins, order them such that dependent plugins are before plugins that depend on it
  {Dag, LoadedPlugins} = create_plugin_dag(Plugins),
  PluginList = flatten_plugin_dag(Dag),
  InitialState = #barista_state{
    plugin_spec_map = LoadedPlugins,
    plugin_state_map = #{},
    plugin_list = PluginList,
    user_func = UserFunc},

  % creates state according to dag order.
  lists:foldl(fun(PluginID, State = #barista_state{plugin_state_map = PluginStates, plugin_spec_map = Loaded}) ->
       % create plugins from args via new_plugin
       {Module, Function, _} = maps:get(new_plugin, maps:get(Loaded, PluginID)),
       PluginState = apply(Module, Function, [Args, State]),
       State#barista_state{plugin_state_map = maps:put(PluginID, PluginState, PluginStates)}
     end, InitialState, PluginList);
new_state(Args = #{barista_plugin_list := _}) -> new_state(maps:put(barista_user_func, none, Args)).

-spec update_state(barista_state()) -> barista_state().
update_state(State0 = #barista_state{incoming = [Order|Rest]}) ->
  State1 = process_order({'receive', Order}, State0),
  update_state(State1#barista_state{incoming = Rest});
update_state(State0 = #barista_state{incoming = [], user_func = none}) -> State0;
update_state(State0 = #barista_state{incoming = [], user_func = UserFunc}) ->
  caffe_util:apply_function_spec(UserFunc, [State0]).

-spec add_signal_to_state(barista_state(), Signal::any()) -> barista_state().
add_signal_to_state(State0 = #barista_state{incoming = Incoming}, Signal) ->
  State0#barista_state{incoming = [Signal|Incoming]}.

-spec get_state_info(barista_state()) -> caffe:state_info().
get_state_info(State) ->
  Terminate = terminator:should_exit(get_plugin_state(terminator, State)),
  caffe:make_state_info(Terminate).

% creates a directed acyclic graph of the plugins
% exception if user
-spec create_plugin_dag([plugin_id()]) -> {plugin_dag(), #{ plugin_id() => plugin_spec() }} | {error, _}.
create_plugin_dag(Plugins) ->
  Dag = digraph:new([private, acyclic]),
  LoadedPlugins = load_plugin_vertices(Dag, Plugins, maps:new()),
  {Dag, maps:map(fun(Module, PluginSpec) ->
      Dependencies = maps:get(dependencies, PluginSpec),
      _ = lists:map(fun(Dep) -> digraph:add_edge(Module, Dep) end, Dependencies),
      PluginSpec
    end, LoadedPlugins)}.

load_plugin_vertices(Dag, [Module|Plugins], LoadedPluginMap) ->
  case maps:is_key(Module, LoadedPluginMap) of
    true -> load_plugin_vertices(Dag, Plugins, LoadedPluginMap);
    false ->
      {PluginSpec, _} = {load_plugin_spec(Module), digraph:add_vertex(Dag, Module)},
      Dependencies = maps:get(dependencies, PluginSpec),
      load_plugin_vertices(Dag, Dependencies ++ Plugins, maps:put(Module, PluginSpec, LoadedPluginMap))
  end;
load_plugin_vertices(_, [], LoadedPluginMap) -> LoadedPluginMap.

-spec flatten_plugin_dag(plugin_dag()) -> [plugin_id()].
flatten_plugin_dag(Dag) -> digraph_utils:preorder(Dag).

% PluginID : module

% PluginState : { plugin_id => PluginID }
% PluginSpec : { dependencies : [PluginID],
%                unmodifiable : [atom] }   -- fields in this list are checked for modification
%
% Accessible functions:
%  API? Pass around general state and send events to everyone (better imo, we still want deps tho to ez load correct plugins)
%   caffe:process_event( Message, State ) -> State
%   caffe:get_plugin_state( PluginID, State ) -> PluginState
%
% Plugin functions needing implementation:
%   plugin_spec() -> PluginSpec

% Plugin specification:
%   Plugins = [ lamport_clock, chandylamport ]

%   suppose I have a plugin - qwerty - that wants to change the graph plugin using the publicly
%   accessible functions in the graph module.
%
% graph plugin
% state:
%   graph         := graph(),                     % the network graph
%   vertex        := vertex(),                    % this vertex
%   outgoing      := #{vertex() => identifier()}, % known outgoing edges with respective identifier/PID
%   outgoing_vertices := vertex_list(),           % list of outgoing vertices on network
%   spawn_vertex_function_spec := function_spec(),     % function def used spawn this node
% spec:
%   dependencies : []
%   unmodifiable : [ spawn_vertex_function_spec, vertex ]
% update_state( { add_vertex, .. }, .. )
% update_state( { add_edge, .. }, .. )
% add_vertex( Vertex, PluginState )
% add_edge( Edge, PluginState )

% my_plugin
% spec:
%   dependencies : [graph]
% update_graph(GraphState) ->
%     graph:add_edge( {GraphState.vertex, my_vertex}, graph:add_vertex( my_vertex, GraphState ) );
% update_state( ... ) ->
%     Graph = caffe:get_plugin_state( graph, State );
%     caffe:process_event( { add_edge, Graph.vertex, my_vertex}, caffe:process_event( { add_vertex, my_vertex }, State ) )
