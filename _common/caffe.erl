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
-module(caffe_temp).

%%% API to modify state in your caffe %%%
-export([process_order/2, user_func_none/0]).       % accessor functions
-export([args_from_list/1]).                        % used to construct args to build the network
-export([build/2, start/1]).                        % used to build and start the network
-export([open_caffe/1]).                            % implementation to run on a vertex. Cheeky name ikr???

-export_type([caffe_state/1]).
-export_type([plugin_id/0, user_func/1]).
-export_type([caffe_spec/1, caffe_spec_map/1, caffe_spec_list/1]).

%%%
%%% Caffe Parameters
%%%
%%% Specify the following in VertexArgs:
%%%   caffe_user_func         - Required. Defines underlying/user function to run each loop
%%%   caffe_plugin_list       - Required. Defines plugins to run
%%%   caffe_wave_wait_time    - time we sleep in each wave
%%%   caffe_receive_time      - time we wait to receive a signal


% Caffe Parameters - data representation (internal only)
-record(caffe_params, {
  loop_wait_time  = 0,
  receive_time    = 0
}).
-type caffe_params() :: #caffe_params{
  % how long should we wait in between loops
  loop_wait_time  :: non_neg_integer(),
  % if so, how long should we wait?
  receive_time    :: non_neg_integer()|infinity
}.

% Caffe Parameters - Args -> Params
-spec get_caffe_params(caffe_graph:vertex_args(any())) -> caffe_params().
get_caffe_params(VertexArgs) ->
  Default = #caffe_params{},
  #caffe_params{loop_wait_time = maps:get(caffe_loop_wait_time, VertexArgs, Default#caffe_params.loop_wait_time),
    receive_time = maps:get(caffe_receive_time, VertexArgs, Default#caffe_params.receive_time)}.

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
  user_func,
  user_state
}).
-opaque caffe_state(UserState) :: #caffe_state{
  %% Plugin Specs %%
  plugin_spec_map :: #{ plugin_id() => plugin_spec() },

  %% Plugin States %%
  plugin_state_map :: #{ plugin_id() => plugin_state() },

  %% Plugins %%
  plugin_list :: [plugin_id()],

  %% User-defined function & state %%
  user_func :: user_func(UserState),
  user_state :: UserState
}.

% State Management - User Function

% user-defined function to run on vertex. Choose between...
-type user_func(UserState) ::
  % 1. specify two separate functions
  {
    % new_state - create a new state
    new_caffe_state(UserState),
    % update_state - updates
    update_caffe_state(UserState),
    % capture_signals - true if we should capture any incoming signals here.
    % If false, that logic should be in the user_func
    boolean()
  }
  |
  {
    new_caffe_state(UserState),
    update_caffe_state(UserState)
    % third value defaults to true
  }

  % 2. define a module implementation
  %    new_state(VertexArgs) -> {State, UserState}
  %    update_state(State, UserState) -> {State, UserState}
  %    capture_signals() -> boolean() (optional)
  | module().

% new user state
-type new_caffe_state(UserState)
  :: { anonymous, fun((caffe_state(UserState), caffe_graph:vertex_args(caffe_state(UserState))) -> {caffe_state(UserState), UserState})}
   | { named, module(), atom() }.

% update user state
-type update_caffe_state(UserState)
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

-record(plugin_spec, {
  dependencies,
  new_plugin,
  update_plugin,
  invariant
}).
-type plugin_spec(PluginState) :: #plugin_spec{
  dependencies :: [plugin_id()],

  %% these fields are auto-populated when we add a new plugin %%
  % internally, these are just represented as MFAs but we list out the type below for additional clarity
  new_plugin :: new_plugin(PluginState),
  update_plugin :: update_plugin(PluginState),
  invariant :: invariant(PluginState)
}.

% types for functions defined in a plugin:
% fun( vertex_args, caffe_state ) -> plugin_state
-type new_plugin(PluginState)
  :: fun((caffe_graph:vertex_args(caffe_state(UserState)), caffe_state(UserState)) -> PluginState)
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

% retrieves whether or not we should be capturing signals in each loop
-spec do_capture_signals(caffe_state(UserState)) -> boolean().
do_capture_signals(#caffe_state{user_func = {_, _, CaptureSignals}}) -> CaptureSignals;
do_capture_signals(#caffe_state{user_func = {_, _}}) -> true;
do_capture_signals(#caffe_state{user_func = Module}) ->
  CaptureSignalImplemented = caffe_util:is_exported(Module, capture_signals, 0),
  if CaptureSignalImplemented -> apply(Module, capture_signals, []);
    true -> true
  end.

% receives all signals over a period "ReceiveTime", returning a list of messages received
capture_signals(CaptureTime) -> capture_signals(CaptureTime, 0).
capture_signals(CaptureTime, TimePassed) when TimePassed > CaptureTime -> [];
capture_signals(CaptureTime, TimePassed) ->
TStart = erlang:system_time(),
  receive
    Message ->
      TEnd = erlang:system_time(),
      TAdded = TEnd - TStart,
      [Message| capture_signals(CaptureTime, TimePassed + TAdded)]
  after CaptureTime - TimePassed -> []
  end.

% function to translate args to an initial state, then run a loop on the state & args.
-spec open_caffe(caffe_graph:vertex_args(State)) -> State.
open_caffe(Args) ->
  CaffeParams = get_caffe_params(Args),
  State = new_state(Args),
  open_caffe_helper(State, CaffeParams).

-spec open_caffe_helper(caffe_state(UserState), caffe_params()) -> caffe_state(UserState) | no_return().
open_caffe_helper(State1,
    CaffeParams = #caffe_params{ loop_wait_time = WaitTime, receive_time = ReceiveTime}) ->

  % get info on current state
  ShouldExit = terminator:should_exit(State1),
  if
    % terminate - return current state
    ShouldExit -> State1;
    % otherwise update state & loop
    true ->
      % boolean, do we capture the signals?
      CaptureSignals = do_capture_signals(State1),
      % conditional to new State1 incorporating the captured signals (or not)
      State2 =
        if CaptureSignals ->
          % receive signals and process them
          lists:foldl(fun(Message, State) -> messenger:receive_message(Message, State) end, State1, capture_signals(ReceiveTime));
          true -> State1
        end,

      % update the state
      State3 = update_state(State2),

      % sleep after updating
      ok = if WaitTime > 0 -> timer:sleep(WaitTime);
             true -> ok
           end,
      open_caffe_helper(State3, CaffeParams)
  end.

%% we create a digraph with properties private and acyclic
-type plugin_dag() :: digraph:graph().

% helper to load functions for a plugin. Throws an exception if module does not
% properly implement the plugin spec.
get_plugin_impl(Plugin, Module, dependencies) ->
  Dependencies = sets:from_list(maps:get(dependencies, Plugin)),
  case sets:is_element(0, Dependencies) of
    true  -> apply(Module, dependencies, []);
    false -> []
  end;
get_plugin_impl(Plugin, Module, new_plugin) ->
  % new_plugin( VertexArgs, CurrentState ) -> PluginState
  NewPlugin = sets:from_list(maps:get(new_plugin, Plugin)),
  case sets:is_element(2, NewPlugin) of
    true  -> {Module, new_plugin, 2};
    false -> throw({bad_plugin, unimplemented, [{new_plugin, 2}]})
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

  Impl1 = sets:from_list(maps:get(F1, Plugin)),
  Impl2  = sets:from_list(maps:get(F2, Plugin)),
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
    invariant
  ])),

  %% Construct plugin specification %%
  #plugin_spec{
    dependencies  = get_plugin_impl(PluginImpl, PluginID, dependencies),
    new_plugin    = get_plugin_impl(PluginImpl, PluginID, new_plugin),
    update_plugin = get_plugin_impl(PluginImpl, PluginID, update_plugin),
    invariant     = get_plugin_impl(PluginImpl, PluginID, invariant)
  }.

% processes some signal or message
-spec process_order(Message::any(), caffe_state(UserState)) -> caffe_state(UserState).
process_order(Message, State = #caffe_state{plugin_list = Plugins}) -> process_order(Message, State, Plugins).
process_order(Message, State0 = #caffe_state{plugin_spec_map = PluginSpecs, plugin_state_map = PluginStates}, [Plugin|Plugins]) ->
  Spec = maps:get(Plugin, PluginSpecs),
  PluginState0 = maps:get(Plugin, PluginStates),
  % extract Spec
  % todo - should we add enforcement onto the dependencies?
  % todo   Possibly have a current_plugin in the State, and ensure one plugin can only send an order to a dependent
  #{ update_plugin := UpdatePlugin, invariant := Invariant } = Spec,

  {PluginStateNew, StateNew, WasIgnored} = case UpdatePlugin of
                                           {PluginMod, update_plugin, 2} ->
                                             case apply(PluginMod, update_plugin, [Message, PluginState0]) of
                                               % plugin ignores this message
                                               ignore -> {PluginState0, State0, true};
                                               % merge plugin state
                                               PluginState1 -> {PluginState1, State0#caffe_state{plugin_state_map = maps:update(PluginMod, PluginState1, PluginStates)}, false}
                                             end;
                                          {PluginMod, update_plugin, 3} ->
                                            % update state
                                            case apply(PluginMod, update_plugin, [Message, PluginState0, State0]) of
                                              % plugin ignores this message
                                              ignore -> {PluginState0, State0, true};
                                              % merge plugin state, use updated state
                                              {PluginState1, State1} -> {PluginState1, State0#caffe_state{plugin_state_map = maps:update(PluginMod, PluginState1, State1)}, false}
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

  process_order(Message, StateNew, Plugins);
process_order(_, State, []) -> State.

% translate vertex_args to the initial state, Σ
-spec new_state(caffe_graph:vertex_args(caffe_state(UserState))) -> caffe_state(UserState).
new_state(Args = #{caffe_user_func := UserFunc, caffe_plugin_list := Plugins}) ->

  % we auto-load all dependent plugins, order them such that dependent plugins are before plugins that depend on it
  {Dag, LoadedPlugins} = create_plugin_dag(Plugins),
  PluginList = flatten_plugin_dag(Dag),
  InitialState = #caffe_state{
    plugin_spec_map = LoadedPlugins,
    plugin_state_map = #{},
    plugin_list = PluginList,
    user_state = none,
    user_func = UserFunc},

  % Load each plugin, accumulate into State1. Iterates according to calculated dag order for the dependencies..
  State1 = lists:foldl(
    fun(PluginID, State = #caffe_state{plugin_state_map = PluginStates, plugin_spec_map = Loaded}) ->
      % create plugins from args via new_plugin
      {Module, Function, 2} = maps:get(new_plugin, maps:get(Loaded, PluginID)),
      PluginState0 = apply(Module, Function, [Args, State]),
      State#caffe_state{plugin_state_map = maps:put(PluginID, PluginState0, PluginStates)}
    end, InitialState, PluginList),
  % create the user_state for the user_func after the plugins have been created and initialized.
  State1#caffe_state{user_state = case UserFunc of
                                    {UserState0, _} -> caffe_util:apply_function_spec(UserState0, [State1, Args]);
                                    Module -> HasImplementation = caffe_util:is_exported(Module, new_state, 2),
                                      if HasImplementation -> new_user_state({{named, Module, new_state}, none}, State1, Args) end
                                  end};
new_state(Args = #{caffe_plugin_list := _}) -> new_state(maps:put(caffe_user_func, user_func_none(), Args)).

% transition function Σ -> Σ to update user state
-spec update_state(caffe_state(UserState)) -> caffe_state(UserState).
update_state(State = #caffe_state{user_state = UserState, user_func = { _, UpdateUserState, _ }}) -> function_update_state(State, UserState, UpdateUserState);
update_state(State = #caffe_state{user_state = UserState, user_func = { _, UpdateUserState }}) -> function_update_state(State, UserState, UpdateUserState);
update_state(State = #caffe_state{user_state = UserState, user_func = Module}) -> module_update_state(State, UserState, Module).

% update state using an specified function or a module
function_update_state(State0, UserState0, UpdateUserState) ->
  {State1, UserState1} = caffe_util:apply_function_spec(UpdateUserState, [State0, UserState0]),
  State1#caffe_state{user_state = UserState1}.
module_update_state(State, UserState, Module) ->
  HasImplementation = caffe_util:is_exported(Module, update_state, 2),
  if HasImplementation ->
    function_update_state(State, UserState, {named, Module, update_state})
  end.

% creates a directed acyclic graph of the plugins
% exception if user specifies cyclic dependencies
-spec create_plugin_dag([plugin_id()]) -> {plugin_dag(), #{ plugin_id() => plugin_spec() }} | {error, _}.
create_plugin_dag(Plugins) ->
  Dag = digraph:new([private, acyclic]),
  LoadedPlugins = load_plugin_vertices(Dag, Plugins, maps:new()),
  {Dag, maps:map(
    fun(Module, PluginSpec) ->
      Dependencies = maps:get(dependencies, PluginSpec),
      _ = lists:map(fun(Dep) -> digraph:add_edge(Module, Dep) end, Dependencies),
      PluginSpec
    end, LoadedPlugins)}.

load_plugin_vertices(Dag, [Module|Plugins], LoadedPluginMap) ->
  case maps:is_key(Module, LoadedPluginMap) of
    true -> load_plugin_vertices(Dag, Plugins, LoadedPluginMap);
    false ->
      {PluginSpec, _} = {load_plugin_spec(Module), digraph:add_vertex(Dag, Module)},
      load_plugin_vertices(Dag, PluginSpec#plugin_spec.dependencies ++ Plugins, maps:put(Module, PluginSpec, LoadedPluginMap))
  end;
load_plugin_vertices(_, [], LoadedPluginMap) -> LoadedPluginMap.

-spec flatten_plugin_dag(plugin_dag()) -> [plugin_id()].
flatten_plugin_dag(Dag) -> digraph_utils:preorder(Dag).


%%
%% Network Deployment!
%% Example usage:
%%
%%  G = {V, E} = caffe_graph:load(graph1),
%%  Network = caffe:build( G,
%%    caffe:new( [ { V, { [ chandylamport ], worker_random_messenger } } ] )
%%  caffe:start(Network)
%%

system_plugins() -> [
  graph_state,      % keeps current known state of the graph/network
  plugin_manager,   % manages plugins - adds/removes them
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
      fun(VertexList, Spec)
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