%%%-------------------------------------------------------------------
%%% @author zkieda
%%% @doc
%%%
%%%   Common algo framework for Erlang (caffe) separates the distributed algorithm
%%%   logic from the underlying messages being passed in the graph.
%%%   We allow the user to capture signals,
%%%   which is passed and processed in their implementation's state.
%%%
%%%   Our caffe is pluggable, so you can add multiple distributed algos to your
%%%   network graph.
%%%
%%%   State-based vertex_fun implementation that will run on a vertex.
%%%   Just updates its state in a loop
%%%
%%%   You can specify a module (or a set of anonymous functions)
%%%   as an API to interact & change the state.
%%%   Default module is barista
%%% @end
%%% Created : 30. Mar 2021 11:17 AM
%%%-------------------------------------------------------------------
-module(caffe_old).
-author("zkieda").

%%% API to modify state in your caffe %%%
%% To run: open_caffe/1  (cheeky ikr??)
%% Your functions to implement:
%%  (required) new_state :: Args -> State
%%  (required) update_state :: State -> State
%%
%%  (optional) add_order :: State, Any -> State
%%  (optional) get_state_info :: State -> StateInfo
-export([open_caffe/1]).                % state implementation to run on a vertex
-export([make_state_info/1]).           % utility function

-export_type([new_state/1, update_state/1]).             % required functions to be defined
-export_type([add_order/1, get_state_info/1]). % optional functions to be defined
-export_type([state_info/0]).                            % result of get_state_info

% create a new state from args
-type new_state(State) :: caffe_graph:vertex_fun(State).

% update state
-type update_state(State)
  :: { anonymous, fun((State) -> State) }
   | { named, module(), atom()}.

% adds a signal to the state
-type add_order(State)
  :: { anonymous, fun((State, Signal::any()) -> State) }
   | { named, module(), atom() }
   | none. % when none is specified we do not capture signals

% gets information from the state
-type get_state_info(State)
  :: { anonymous, fun((State) -> state_info())}
   | { named, module(), atom() }.

-record(caffe_params, {
  wave_wait_time  = 0,
  receive_time    = 0
}).

-type caffe_params() :: #caffe_params{
  % how long should we wait in between waves?
  wave_wait_time  :: non_neg_integer(),
  % if so, how long should we wait?
  receive_time    :: non_neg_integer()|infinity
}.

% passed in as vertex_args as the following keys:
%     caffe_new_state
%     caffe_update_state
%     caffe_add_order       (optional)
%     caffe_get_state_info  (optional)
%
% or, you may define the following key in vertex_args
%     caffe_module_impl
% to specify a module with the following functions implemented:
%     new_state
%     update_state
%     add_order           (optional)
%     get_state_info      (optional)
-record(caffe_functions, { new_state, update_state, get_state_info, add_order = none }).
-type caffe_functions(State) :: #caffe_functions{
  new_state           :: new_state(State),      % creates a new state from args
  update_state        :: update_state(State),   % updates existing state
  add_order           :: add_order(State),      % (optional) Adds the order to the state, where order represents an external signal.
  get_state_info      :: get_state_info(State)  % (optional) transforms state into information used in loop
}.

-record(state_info, {
  terminate = false
}).
-opaque state_info() :: #state_info{
  % say hi to Arnold because ur caffe has just been terminated.
  % caffe doesn't reopen
  terminate :: boolean()
  % If I were Schwarzenegger and I met a barista named Connor I would totally say "I'm looking for John, Connor!"
  % If the toilet clogs I would quietly inform it "You have been terminated."
}.

% function to translate args to an initial state, then run a loop on the state & args.
-spec open_caffe(caffe_graph:vertex_args(State)) -> State.
open_caffe(Args) ->
    CaffeParams = get_caffe_params(Args),
    CaffeFuns = #caffe_functions{new_state = NewState} = get_caffe_functions(Args),
    State = caffe_util:apply_function_spec(NewState, [Args]),
    open_caffe_helper(State, CaffeParams, CaffeFuns).

-spec open_caffe_helper(State, caffe_params(), caffe_functions(State)) -> State | no_return().
open_caffe_helper(State1,
    CaffeParams = #caffe_params{ wave_wait_time = WaitTime, receive_time = ReceiveTime},
    CaffeFuns = #caffe_functions{ update_state = UpdateState, add_order = AddOrder, get_state_info = GetStateInfo }) ->

  % get info on current state
  StateInfo = caffe_util:apply_function_spec(GetStateInfo, [State1] ),
  if
    % terminate - return current state
    StateInfo#state_info.terminate -> State1;
    % otherwise update state & loop
    true ->
      % if we should capture signals, do so and add to the state
      State2 = if AddOrder =/= none ->
          % todo - batch received signals?
          receive
            Any -> caffe_util:apply_function_spec(AddOrder, [State1, Any])
          after ReceiveTime -> State1
          end;
        true -> State1
      end,

      % update the state
      StateNew = caffe_util:apply_function_spec(UpdateState, [State2]),

      % sleep after updating
      ok = if WaitTime > 0 -> timer:sleep(WaitTime);
             true -> ok
           end,
      open_caffe_helper(StateNew, CaffeParams, CaffeFuns)
  end.

% Specify the following in VertexArgs:
%   caffe_wave_wait_time    - time we sleep in each wave
%   caffe_capture_time      - time we wait to receive a signal
-spec get_caffe_params(caffe_graph:vertex_args(any())) -> #caffe_params{}.
get_caffe_params(VertexArgs) ->
  Default = #caffe_params{},
  #caffe_params{wave_wait_time = wait_time = maps:get(caffe_wave_wait_time, VertexArgs, Default#caffe_params.wave_wait_time),
    receive_time = maps:get(caffe_capture_time, VertexArgs, Default#caffe_params.receive_time)}.

-spec get_state_info_default() -> get_state_info(any()).
get_state_info_default() -> {anonymous, fun(_) -> #state_info{} end}.

% Specify the following in VertexArgs:
%   Either:
%     caffe_new_state        - creates a new state from args
%     caffe_update_state     - updates existing state
%     caffe_add_order        - (optional) Adds the order to the state, where order represents an external signal.
%     caffe_get_state_info   - (optional) transforms state into information used in loop
%   Or:
%     caffe_module_impl      - module that contains function implementations: new_state, update_state, and (optionally) add_order
-spec get_caffe_functions(caffe_graph:vertex_args(any())) -> #caffe_functions{}.
get_caffe_functions(#{caffe_new_state := New, caffe_update_state := Update,
    caffe_add_order := AddOrder, caffe_get_state_info := GetStateInfo}) ->
  #caffe_functions{new_state = New,
    update_state = Update,
    get_state_info = GetStateInfo,
    add_order = AddOrder};
get_caffe_functions(R = #{caffe_new_state := _, caffe_update_state := _,
    caffe_add_order := _}) ->
  get_caffe_functions(maps:put(caffe_get_state_info, get_state_info_default(), R));
get_caffe_functions(R = #{caffe_new_state := _, caffe_update_state := _,
  caffe_get_state_info := _}) ->
  get_caffe_functions(maps:put(caffe_get_state_info, none, R));
get_caffe_functions(#{caffe_module_impl := Module}) ->
  #caffe_functions{new_state = {named, Module, new_state},
    update_state = {named, Module, update_state},
    get_state_info = case caffe_util:is_exported(Module, get_state_info, 1) of
                       true -> {named, Module, get_state_info};
                       false -> get_state_info_default()
                     end,
    add_order = case caffe_util:is_exported(Module, add_order, 2) of
                            true ->  {named, Module, add_order};
                            false -> none
                          end}.

% helper function to construct the state_info data record so it can be used as a parameter
-spec make_state_info(Terminate::boolean()) -> state_info().
make_state_info(Terminate) ->
  #state_info{terminate = Terminate}.