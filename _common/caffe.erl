%%%-------------------------------------------------------------------
%%% @author zkieda
%%% @doc
%%%   caffe is a plugin manager that runs on the caffe_graph. Each plugin is essentially
%%%   a σ -> σ' transition, and a set of public accessors to aide in calling the transition.
%%%
%%%   You specify a set of plugins you want to load, and we auto-load based on their specified dependencies.
%%%   No, you are not allowed to specify cyclic dependencies. I don't want shenanigans of turing
%%%   complete dependency traversals please please please it's a headache to debug.
%%%
%%%   todo: we should actually verify that plugins are only calling their dependencies.
%%%         So technically you can still write cyclic dependency traversals so the only thing
%%%         stopping you is your own shame
%%% @end
%%% Created : 18. Apr 2021 2:50 PM
%%%-------------------------------------------------------------------
-module(caffe).

%%% API to modify state in your caffe %%%
-export([process_event/2, user_func_none/0, get_plugin_state/2]). % accessor functions
-export([log/2, log/3]).                                          % logging
-export([args_from_list/1]).                                      % used to construct args to build the network
-export([build/2, start/1]).                                      % used to build and start the network
-export([open_caffe/1]).                                          % implementation to run on a vertex. Cheeky name ikr???

-export([wrap_outgoing/3, unwrap_incoming/3]).                    % wraps/unwraps a message based on available plugins. Used by messenger only!

-export_type([caffe_state/1]).
-export_type([plugin_id/0, user_func/1]).
-export_type([caffe_spec/1, caffe_spec_map/1, caffe_spec_list/1]).

%%%
%%% Caffe Args
%%%
%%% Specify the following in VertexArgs:
%%%   caffe_user_func         - Required. Defines underlying/user function to run each loop
%%%   caffe_plugin_list       - Required. Defines plugins to run
%%%   caffe_wave_wait_time    - time we sleep in each wave
%%%   caffe_logging           - logging verbosity on a per-plugin basis

-type caffe_args(UserState) :: #{
  % user function to run
  caffe_user_func       := user_func(UserState),
  % plugins
  caffe_plugin_list     := [plugin_id()],
  % how long should we wait in between loops
  caffe_loop_wait_time  := non_neg_integer(),
  % logging parameters. Use 'caffe' atom to specify verbosity for internal logging
  caffe_logging         := [{plugin_id(), log_level()}]
}.

% see : caffe:log
-type log_level() :: quiet % silence output from this plugin
  | plugin_only            % only have output for the plugin within the plugin itself
  | debug.                 % more verbose logging for the plugin. Default setting

%%%
%%% State management
%%%
%%% The main loop is really just some Σ -> Σ transition, where Σ represents a collection of σ "states":
%%%  1. The plugins and their current σ_plugin_i
%%%  2. The user function and its current σ_user
%%%  3. Loaded accessors for each plugin

% State Management - internal state representation
-record(caffe_state, {
  plugin_spec_map,
  plugin_state_map,
  plugin_list,
  plugin_callstack = [],
  user_func,
  user_state,
  logging,
  settings
}).

% settings provided by the user
-type caffe_settings() :: #{
  caffe_loop_wait_time => non_neg_integer()
}.

-spec get_setting(caffe_state(any()), atom(), any()) -> any().
get_setting(#caffe_state{settings = Settings}, Setting, Default) -> maps:get(Setting, Settings, Default).

-opaque caffe_state(UserState) :: #caffe_state{
  %% Basic algo %%
  user_func :: user_func(UserState),                        % user function to run
  user_state :: UserState,                                  % current user state

  %% Plugins %%
  plugin_spec_map :: #{ plugin_id() => plugin_spec() },     % specification for plugins
  plugin_state_map :: #{ plugin_id() => plugin_state() },   % current states of plugins
  plugin_list :: [plugin_id()],                             % list of plugins in-order of evaluation
  plugin_callstack :: [plugin_id()],                        % callstack of plugins

  %% Logging/Settings %%
  logging :: #{ plugin_id() => log_level() },
  settings :: caffe_settings()
}.

% State Management - User Function

% user-defined function to run on vertex. Choose between...
-type user_func(UserState) ::
  % 1. specify two separate functions
  {
    % new_state - create a new state
    new_user_state(UserState),
    % update_state - updates state
    update_user_state(UserState),
    % auto_capture - do we automatically capture signals and pass them into the framework,
    %                or is that the responsibility of user_func? Default true
    boolean()
  }
  |
  {
    new_user_state(UserState),
    update_user_state(UserState)
  }

  % 2. define a module implementation
  %    new_state(VertexArgs) -> {State, UserState}
  %    update_state(State, UserState) -> {State, UserState}
  %    auto_capture() -> boolean()
  | module().

% new user state
-type new_user_state(UserState)
  :: { anonymous, fun((caffe_state(UserState), caffe_graph:vertex_args(caffe_state(UserState))) -> {caffe_state(UserState), UserState})}
   | { named, module(), atom() }.

% update user state
-type update_user_state(UserState)
  :: { anonymous, fun((caffe_state(UserState), UserState) -> {caffe_state(UserState), UserState})}
   | { named, module(), atom() }.

%%use this to specify no-op for user_func
-spec user_func_none() -> user_func(any()).
user_func_none() -> {
  fun(X, _) -> {X, none} end,
  fun(X, Y) -> {X, Y} end,
  true
}.

% State Management - Plugins
-type plugin_id() :: module().
-type plugin_state() :: any().

-record(plugin_spec, {
  dependencies,
  new_plugin,
  update_plugin,
  invariant,
  format
}).

-type plugin_spec() :: #plugin_spec{
  dependencies :: [plugin_id()],

  %% these fields are auto-populated when we add a new plugin %%
  % internally, these are just represented as MFAs but we list out the type below for additional clarity
  new_plugin :: new_plugin(plugin_state()),
  update_plugin :: update_plugin(plugin_state()),
  invariant :: invariant(plugin_state()),
  format :: format(plugin_state())
}.

% types for functions defined in a plugin:
% fun( vertex_args, caffe_state ) -> plugin_state
-type new_plugin(PluginState)
  :: fun((caffe_graph:vertex_args(caffe_state(UserState::any()))) -> PluginState)
   | mfa().
% fun( Msg, caffe_state, plugin_state ) -> { caffe_state, plugin_state }
%  | fun( Msg, plugin_state ) -> plugin_state
-type update_plugin(PluginState)
  :: fun((Message::any(), PluginState) -> PluginState)
   | fun((Message::any(), caffe_state(UserState), PluginState) -> {caffe_state(UserState), PluginState})
   | mfa().
% fun( StateOld, StateNew ) -> ok | { broken, _ }
%  | fun( StateDelta ) -> ok | { broken, _ }
-type invariant(PluginState)
  :: fun((PluginState, PluginState) -> ok | {broken, _ })
   | fun((PluginState::any()) -> ok | {broken, _})
   | mfa().
% fun( PluginState ) -> PrintableFormat
-type format(PluginState)
  :: fun((PluginState) -> any())
   | mfa()
   | none.

% function to translate args to an initial state, then run a loop on the state & args.
-spec open_caffe(caffe_graph:vertex_args(caffe_state(UserState)) | caffe_args(UserState)) -> caffe_state(UserState).
open_caffe(Args) ->
  State = new_state(Args),
  open_caffe_loop(State).

-spec open_caffe_loop(caffe_state(UserState)) -> caffe_state(UserState) | no_return().
open_caffe_loop(State1) ->

  % get info on current state
  ShouldExit = terminator:should_exit(State1),
  if
    % terminate - return current state
    ShouldExit -> State1;
    % otherwise update state & loop
    true ->
      % update the state
      State2 = update_state(State1),

      % sleep after updating
      WaitTime = get_setting(State2, caffe_loop_wait_time, 0),
      ok = if WaitTime > 0 -> timer:sleep(WaitTime);
             true -> ok
           end,
      open_caffe_loop(State2)
  end.

%% we create a digraph with properties private and acyclic
-type plugin_dag() :: digraph:graph().

% helper to load functions for a plugin. Throws an exception if module does not
% properly implement the plugin spec.
get_plugin_impl(Plugin, Module, dependencies) ->
  Dependencies = sets:from_list(maps:get(dependencies, Plugin, [])),
  case sets:is_element(0, Dependencies) of
    true  -> apply(Module, dependencies, []);
    false -> []
  end;
get_plugin_impl(Plugin, Module, format) ->
  Format = sets:from_list(maps:get(format, Plugin, [])),
  case sets:is_element(1, Format) of
    true -> {Module, format, 1};
    false -> none
  end;
get_plugin_impl(Plugin, Module, new_plugin) ->
  % new_plugin( VertexArgs, CurrentState ) -> PluginState
  NewPlugin = sets:from_list(maps:get(new_plugin, Plugin, [])),
  case sets:is_element(1, NewPlugin) of
    true  -> {Module, new_plugin, 1};
    false -> throw({bad_plugin, unimplemented, [{new_plugin, 1}]})
  end;
get_plugin_impl(Plugin, Module, FunctionName) when
  FunctionName =:= update_plugin;
  FunctionName =:= invariant ->
  % chooses one of two impls specified in the module for the following:
  %
  %   update_plugin( Message, PluginState ) -> PluginState
  %   or
  %   update_plugin( Message, PluginState, State ) -> { PluginState, State }
  %
  %   invariant( StateOld, StateNew ) -> ok | { broken, ... }
  %   or
  %   invariant( StateDelta ) -> ok | { broken, ... }
  %   or none
  {{F1, A1}, {F2, A2}, Required} = case FunctionName of
                                     update_plugin    -> {{update_plugin, 2}, {update_plugin, 3}, true};
                                     invariant        -> {{invariant, 1}, {invariant, 2}, false}
                                   end,

  Impl1 = sets:from_list(maps:get(F1, Plugin, [])),
  Impl2 = sets:from_list(maps:get(F2, Plugin, [])),
  case { sets:is_element(A1, Impl1), sets:is_element(A2, Impl2) } of
    { true, false }  -> {Module, F1, A1};
    { false, true }  -> {Module, F2, A2};
    { true, true }   -> throw({bad_plugin, choose_one, [{F1, A1}, {F2, A2}]});
    { false, false } -> if Required -> throw({bad_plugin, unimplemented, [{F1, A1}, {F2, A2}]});
                          true -> none
                        end
  end.

% loads functions for a plugin
-spec load_plugin_spec(PluginID::plugin_id()) -> PluginSpec::plugin_spec().
load_plugin_spec(PluginID) ->
  %% get exported functions in implementation %%
  PluginImpl = caffe_util:get_exported(PluginID, sets:from_list([
    dependencies,
    new_plugin, update_plugin,
    invariant, format
  ])),

  %% Construct plugin specification %%
  #plugin_spec{
    dependencies  = get_plugin_impl(PluginImpl, PluginID, dependencies),
    new_plugin    = get_plugin_impl(PluginImpl, PluginID, new_plugin),
    update_plugin = get_plugin_impl(PluginImpl, PluginID, update_plugin),
    invariant     = get_plugin_impl(PluginImpl, PluginID, invariant),
    format        = get_plugin_impl(PluginImpl, PluginID, format)
  }.

% gets plugin state given a plugin_id
-spec get_plugin_state(plugin_id(), caffe_state(_)) -> plugin_state().
get_plugin_state(PluginID, #caffe_state{plugin_state_map = PluginStates}) -> maps:get(PluginID, PluginStates).

wrap_outgoing(Msg, Vertex, State = #caffe_state{plugin_list = L}) ->
  % traverse from left to right, dependencies wrapped first
  lists:foldl(
    fun(Module, {Msg0, State0}) ->
      wrap_unwrap_single(Module, wrap_msg, Msg0, Vertex, State0)
    end, {Msg, State}, L).

unwrap_incoming(Msg, Vertex, State = #caffe_state{plugin_list = L}) ->
  % traverse from right to left, dependencies unwrapped last
  lists:foldr(
    fun(Module, {Msg0, State0}) ->
      wrap_unwrap_single(Module, unwrap_msg, Msg0, Vertex, State0)
    end, {Msg, State}, L).

wrap_unwrap_single(Module, Function, Msg, Vertex, State0 = #caffe_state{plugin_callstack = CallStack}) ->
  case caffe_util:is_exported(Module, Function, 4) of
    true -> % set plugin callstack so we're in the Module's environment
            State1 = State0#caffe_state{plugin_callstack = [Module|CallStack]},
            % retrieve current plugin state
            P = get_plugin_state(Module, State1),
            % call wrap_msg, merge in plugin state
            {Msg1, State2, P1} = apply(Module, Function, [Msg, Vertex, State1, P]),
            State3 = State2#caffe_state{plugin_state_map = maps:update(Module, P1, State2#caffe_state.plugin_state_map)},
            log(State3, false, "~s old => new : ~p => ~p", [Function, Msg, Msg1]),
            log(State3, false, "~s plugin old => new : ~p => ~p", [Function, format_plugin(Module, State1), format_plugin(Module, State3)]),
            % pop plugin callstack, return modified message and state
            {Msg1, State3#caffe_state{plugin_callstack = CallStack}};
    false -> {Msg, State0}
  end.

% processes an arbitrary event, updating the state along the way
-spec process_event(Message::any(), caffe_state(UserState)) -> caffe_state(UserState).
process_event(Message, State = #caffe_state{plugin_list = Plugins}) ->
  ok = caffe:log(State, "processing ~p", [Message]),
  process_event(Message, State, Plugins).
process_event(Message, State0 = #caffe_state{plugin_spec_map = PluginSpecs, plugin_state_map = PluginStates, plugin_callstack = CallStack}, [Plugin|Plugins]) ->
  State1 = State0#caffe_state{plugin_callstack = [Plugin|CallStack]}, % Append the current plugin we're evaluating to the callstack
  Spec = maps:get(Plugin, PluginSpecs),
  PluginState0 = maps:get(Plugin, PluginStates),
  FormatOld = format_plugin(Plugin, State1),
  % todo - should we add enforcement onto the dependencies?
  % todo   Possibly have a current_plugin in the State, and ensure one plugin can only send an order to a dependent
  #plugin_spec{ update_plugin = UpdatePlugin, invariant = Invariant } = Spec,

  {PluginStateNew, StateNew, WasIgnored} = case UpdatePlugin of
                                           {PluginMod, update_plugin, 2} ->
                                             case apply(PluginMod, update_plugin, [Message, PluginState0]) of
                                               % plugin ignores this message
                                               ignore -> {PluginState0, State1, true};
                                               % merge plugin state
                                               PluginState1 -> {PluginState1, State1#caffe_state{plugin_state_map = maps:update(PluginMod, PluginState1, PluginStates)}, false}
                                             end;
                                          {PluginMod, update_plugin, 3} ->
                                            % update state
                                            case apply(PluginMod, update_plugin, [Message, State1, PluginState0]) of
                                              % plugin ignores this message
                                              ignore -> {PluginState0, State1, true};
                                              % merge plugin state, use updated state
                                              {State2, PluginState1} -> {PluginState1, State2#caffe_state{plugin_state_map = maps:update(PluginMod, PluginState1, State2#caffe_state.plugin_state_map)}, false}
                                            end
                                        end,

  ok = if WasIgnored -> ok;
         % diff plugin state
         true ->
           % run plugin assertion
           ok = case Invariant of
                  none -> ok;
                  {Mod, invariant, 3} -> apply(Mod, invariant, [Message, PluginState0, PluginStateNew]);
                  {Mod, invariant, 2} -> apply(Mod, invariant, [Message, caffe_util:diff_deep(PluginState0, PluginStateNew)])
                end
       end,

  FormatNew = format_plugin(Plugin, StateNew),
  ok = if WasIgnored -> log(StateNew, false, "ignored", []);
          true -> log(StateNew, false, "old => new : ~p => ~p", [FormatOld, FormatNew])
       end,
  % go through the rest of the plugins % reset the callstack
  process_event(Message, StateNew#caffe_state{plugin_callstack = CallStack}, Plugins);
process_event(_, State, []) -> State.

-spec current_plugin(caffe_state(UserState::any())) -> plugin_id().
current_plugin(#caffe_state{plugin_callstack = [Plugin|_]}) -> Plugin;
current_plugin(#caffe_state{plugin_callstack = []}) -> [].

format_plugin(PluginID, #caffe_state{ plugin_state_map = M, plugin_spec_map = S}) ->
  PluginState = maps:get(PluginID, M),
  #plugin_spec{ format = Format } = maps:get(PluginID, S),
  case Format of
    none -> PluginState;
    {Module, Function, _} -> apply(Module, Function, [PluginState])
  end.

-spec log(caffe_state(any()), string()) -> ok.
-spec log(caffe_state(any()), string(), [string()]) -> ok.
log(State, Format) -> log(State, Format, []).
log(State, Format, Args) -> log(State, true, Format, Args).

% internal-only accessor for logging.
log(State, FromPlugin, Format, Args) ->
  case current_plugin(State) of
    [] -> case {FromPlugin, maps:get(caffe, State#caffe_state.logging, debug)} of
            {_, quiet} -> ok;
            {false, plugin_only} -> ok;
            _ -> io:fwrite(ezpr:bullet("~w", Format, [{separator, " - "}]), [graph_state:get_vertex(State)|Args])
          end;
    Plugin -> case {FromPlugin, maps:get(Plugin, State#caffe_state.logging, debug)} of
                {_, quiet} -> ok;
                {false, plugin_only} -> ok;
                _ -> io:fwrite(ezpr:bullet("~w:~w", Format, [{separator, " - "}]), [graph_state:get_vertex(State), Plugin|Args])
              end
  end.

% translate vertex_args to the initial state, Σ
-spec new_state(caffe_graph:vertex_args(caffe_state(UserState))) -> caffe_state(UserState).
new_state(Args) ->
  UserFunc = maps:get(caffe_user_func, Args, user_func_none()),
  Plugins = maps:get(caffe_plugin_list, Args, []),
  Logging = maps:get(caffe_logging, Args, []),
  Settings = maps:with([caffe_loop_wait_time], Args),

  % we auto-load all dependent plugins, order them such that dependent plugins are before plugins that depend on it
  {Dag, LoadedPlugins} = create_plugin_dag(Plugins),
  PluginList = flatten_plugin_dag(Dag),
  InitialState = #caffe_state{
    plugin_spec_map = LoadedPlugins,
    plugin_state_map = #{},
    plugin_callstack = [],
    plugin_list = PluginList,
    user_state = none,
    user_func = UserFunc,
    logging = maps:from_list(Logging),
    settings = Settings},
  % Load each plugin, accumulate into State1. Iterates according to calculated dag order for the dependencies..
  State1 = lists:foldl(
    fun(PluginID, State0 = #caffe_state{plugin_state_map = PluginStates, plugin_spec_map = Loaded, plugin_callstack = C}) ->
      State1 = State0#caffe_state{plugin_callstack = [PluginID|C]},
      % create plugins from args via new_plugin
      #plugin_spec{new_plugin = {Module, Function, 1}} = maps:get(PluginID, Loaded),
      PluginState0 = apply(Module, Function, [Args]),
      State2 = State1#caffe_state{plugin_state_map = maps:put(PluginID, PluginState0, PluginStates)},
      ok = log(State2, false, "init = ~p", [ format_plugin(PluginID, State2) ]),
      State2#caffe_state{plugin_callstack = C}
    end, InitialState, PluginList),

  % create the user_state for the user_func after the plugins have been created and initialized.
  State1#caffe_state{user_state = case UserFunc of
                                    {NewUserState, _} -> caffe_util:apply_function_spec(NewUserState, [Args]);
                                    Module -> HasImplementation = caffe_util:is_exported(Module, new_state, 1),
                                      if HasImplementation ->
                                        caffe_util:apply_function_spec({named, Module, new_state}, [Args])
                                      end
                                  end}.

% transition function Σ -> Σ to update user state
-spec update_state(caffe_state(UserState)) -> caffe_state(UserState).
update_state(State = #caffe_state{user_state = UserState, user_func = { _, UpdateUserState, AutoCapture }}) -> update_state_internal(State, UserState, AutoCapture, UpdateUserState);
update_state(State = #caffe_state{user_state = UserState, user_func = { _, UpdateUserState }}) -> update_state_internal(State, UserState, true, UpdateUserState);
update_state(State = #caffe_state{user_state = UserState, user_func = Module}) -> module_update_state(State, UserState, Module).

% update state using a specified module
module_update_state(State, UserState, Module) ->
  case caffe_util:is_exported(Module, update_state, 2) of
    true -> AutoCapture = caffe_util:get_exported_default(Module, auto_capture, [], true),
            update_state_internal(State, UserState, AutoCapture, {named, Module, update_state});
    false -> throw({function_clause, Module, update_state, 2})
  end.

% internal function used for updating state - any additional transformations/logic should be added here.
update_state_internal(State0, UserState0, AutoCapture, UpdateUserState) ->
  % Captures signals, produces an event for each signal, and processes each event
  State1 = if AutoCapture -> messenger:wait_message(State0);
              true -> State0
           end,

  {State2, UserState1} = caffe_util:apply_function_spec(UpdateUserState, [State1, UserState0]),
  State2#caffe_state{user_state = UserState1}.

% creates a directed acyclic graph of the plugins
% exception if user specifies cyclic dependencies
-spec create_plugin_dag([plugin_id()]) -> {plugin_dag(), #{ plugin_id() => plugin_spec() }} | {error, _}.
create_plugin_dag(Plugins) ->
  Dag = digraph:new([private, acyclic]),
  LoadedPlugins = load_plugin_vertices(Dag, Plugins, maps:new()),
  PluginSpecMap = maps:map(
    fun(Module, PluginSpec) ->
      _ = lists:map(fun(Dep) -> digraph:add_edge(Dag, Module, Dep) end, PluginSpec#plugin_spec.dependencies),
      PluginSpec
    end, LoadedPlugins),
  {Dag, PluginSpecMap}.

load_plugin_vertices(Dag, [Module|Plugins], LoadedPluginMap) ->
  case maps:is_key(Module, LoadedPluginMap) of
    true -> load_plugin_vertices(Dag, Plugins, LoadedPluginMap);
    false ->
      {PluginSpec, _} = {load_plugin_spec(Module), digraph:add_vertex(Dag, Module)},
      load_plugin_vertices(Dag, PluginSpec#plugin_spec.dependencies ++ Plugins, maps:put(Module, PluginSpec, LoadedPluginMap))
  end;
load_plugin_vertices(_, [], LoadedPluginMap) -> LoadedPluginMap.

-spec flatten_plugin_dag(plugin_dag()) -> [plugin_id()].
flatten_plugin_dag(Dag) -> digraph_utils:postorder(Dag).

%%
%% Network Deployment!
%% Example usage:
%%
%%  G = {V, E} = caffe_graph:load(graph1),
%%  Network = caffe:build( G,
%%    caffe:args_from_list( [ { V, { [ chandylamport ], worker_random_messenger } } ] )
%%  ),
%%  caffe:start(Network)
%%

%%
%% G = {V, E} = caffe_graph:load(graph1).
%% caffe:start( caffe:build( G, caffe:args_from_list( [ { V, { [ lamport_clock ], worker_random_messenger } } ] ) ) ).

system_plugins() -> [
  graph_state,      % keeps current known state of the graph/network
%  plugin_manager,   % manages plugins - adds/removes them - todo
  terminator,       % allows for client termination of caffe process
  messenger         % hooks for sending and receiving messages
].

% condensed specifications we use to define a vertex on our graph
-type caffe_spec(UserState) :: {
  [plugin_id()],          % user-defined plugins/algos
  user_func(UserState),   % user-defined function
  #{ atom() => any() }    % user-specified arguments
} | {
  [plugin_id()],          % user-defined plugins/algos
  user_func(UserState)    % user-defined function
  % default #{}
}.

% map from the vertex to the spec
-type caffe_spec_map(UserState) :: #{ caffe_graph:vertex() => caffe_spec(UserState) }.
-type caffe_spec_list(UserState) :: [ { caffe_graph:vertex_list(), caffe_spec(UserState) }].

-spec args_from_list(caffe_spec_list(UserState)) -> caffe_spec_map(UserState).
args_from_list(SpecList) ->
  maps:from_list(
    lists:flatmap(
      fun({VertexList, Spec})
        -> lists:map(fun(Vertex) -> {Vertex, Spec} end, VertexList)
      end, SpecList)
  ).

% internal function to create default args
new_args(Plugins, Func, Args) ->
  maps:merge(Args,
    #{
      caffe_user_func => Func,
      caffe_plugin_list => Plugins ++ system_plugins()
    }).

% constructs the network using a graph and a map of items to deploy
% start the network using caffe:start(Network)
-spec build(caffe_graph:graph(), caffe_spec_map(UserState::any())) -> caffe_graph:network().
build(G = {V, _}, SpecMap) ->
  % all vertices run the caffe:open_caffe routine
  SpawnVertexMap = caffe_graph:new_vertex_map([ { V, { named, caffe, open_caffe } } ]),
  VertexArgs = maps:map(
    fun(_, {Plugins, Func, Args}) -> new_args(Plugins, Func, Args);
      (_, {Plugins, Func}) -> new_args(Plugins, Func, maps:new())
    end, SpecMap),
  caffe_graph:build_network(G, SpawnVertexMap, VertexArgs).

-spec start(caffe_graph:network(Val)) -> caffe_graph:network(Val).
start(N) -> caffe_graph:start_network(N).