%%%-------------------------------------------------------------------
%%% @author zkieda
%%% @copyright (C) 2021, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 15. Aug 2021 12:20 AM
%%%-------------------------------------------------------------------
-module(wave_algo).
-author("zkieda").

%% API
-export([new_plugin/1, update_plugin/3]).
-export([register/3, register/4]).

% map from the wave_name to the implementation
-type wave_plugin() :: #{
  atom() => wave_algo()
}.

% used to construct various wave algos
-record(wave_algo, {
  system_wave,  % used by the algo (tarry's, cidon's, etc)
  user_wave,    % used by the user to make a decision (e.g. termination)
  token_info    % used to keep track of unique waves
}).

% token_info is used to keep track of unique waves - we may have multiple waves
%            with multiple implementations all running on the same system
%            Users specify a unique wave_name that defines a wave implementation
-record(token_info, {
  wave_name,    % used to distinguish what wave we're using - maps to the system and user functions
  initiator,    % the initiator of the wave
  count         % the count of the wave run on this initiator
}).

-record(wave, {
  % transition function: {State, TokenState} -> {State, TokenState}
  transition,
  % TokenState
  token_state,
  % decide function = {State, TokenState} -> {State, any()}
  decide
}).

-type system_token() :: any().
-type user_token() :: any().
-type token() :: {token_info(), system_token(), user_token()}.

% implementations of a wave algo will have both system wave data and user wave data
% system wave function is used by the algo (tarry's, cidon's, echo, etc).
% user wave function is specified by the user, and can be used to create decisions easily off the back of a wave algo (e.g. termination)

default_user_wave() -> {
  % transition function
  fun(S, wave_algo) -> {S, wave_algo} end,
  % initial token
  wave_algo,
  % decide event
  fun(S, wave_algo) -> {S, true} end,
  % participation - does this vertex participate in the wave?
  % optional.
  fun(_, wave_algo) -> true end
}.

-spec new_plugin(caffe_graph:vertex_args()) -> wave_plugin().
new_plugin(_) -> #{}.

% creates a new wave algo, with default user behavior
% wave algos are stored in a key/value map so we may have multiple waves for a system
%   - key is an atom name we use to reference
%   - value stores the wave algo definition, and the user behavior for the wave
register(State, WaveName, SystemWave) -> register(State, WaveName, SystemWave, default_user_wave()).
% creates a new wave algo, with specified user behavior - all vertices participate
register(State, WaveName, SystemWave, {T, I, D}) -> register(State, WaveName, SystemWave, {T, I, D, fun(_, _) -> true end});
% creates a new wave algo, with specified user behavior - vertices might not participate
register(State, WaveName, SystemWave, {T, I, D, P}) ->
  PluginState = caffe:get_plugin_state(?MODULE, State),
  ok = case maps:is_key(WaveName, PluginState) of
    true ->
      caffe:log(State, "register: error! registering multiple waves under ~s", [WaveName]),
      {error, WaveName};
    false -> ok
  end,
  WaveAlgo = #wave_algo{
    system_wave = SystemWave,
    user_wave = {T, I, D, P},
    token_info = #token_info{
      wave_name = WaveName,
      initiator = graph_state:get_vertex(State),
      count = 0
    }
  },
  caffe:process_event({wave_algo, register, WaveAlgo}).

% registers the wave algo as a usable algorithm
update_plugin({wave_algo, register, WaveAlgo}, State, PluginState) ->
  TokenInfo = WaveAlgo#wave_algo.token_info,
  {State, maps:put(TokenInfo#token_info.wave_name, WaveAlgo, PluginState)};

% this should be invoked when we officially pass the token to this vertex, and its counted in the traversal.
% the System Token and User Token are both updated

% we receive a message on this vertex - we get the wave token
update_plugin({receive_control, wave_token, VertexIn, Token = {#token_info{wave_name = WaveName, initiator = Initiator, count = Count}, SystemToken, UserToken}}, State, PluginState) ->
  case maps:is_key(PluginState, WaveName) of
    true ->
      % look up the System and User handles
      #wave_algo{system_wave = SystemWave,
        user_wave = {T, _, _, P},
        token_info = _} = maps:get(WaveName, PluginState),
      % does this vertex participate in this wave?
      case P(State, UserToken) of
        true ->
          %
          State1 = caffe:process_event({SystemWave, receive_wave_token, Token}, State),
          {caffe:get_plugin_state(?MODULE, State1), State1};
        false -> ignore
      end;
    false ->
      caffe:log(State, "visit: unregistered wave ~s", [WaveName]),
      ignore
  end;
update_plugin({wave_algo, visit, WaveName, Token = {{WaveName, Initiator, Count}, SystemToken, UserToken}}, State, PluginState) ->
  % this is called when
  {};
update_plugin({wave_algo, decide, WaveName, Token = {{WaveName, Initiator, Count}, SystemToken, UserToken}}, State, PluginState) ->
  {}.


% call this to forward token along the outgoing vertex
send_token(State, WaveName, VertexOut) ->
  messenger:send_message(Msg, VertexOut, State).
% call this to "visit" the vertex, updating the state.
% each vertex should be visited once
visit(State0, WaveName) ->
  PluginState = caffe:get_plugin_state(?MODULE, State0),
  case maps:get(PluginState, WaveName, none) of
    #wave_algo{user_wave = {T, _, _, _}} ->
      % where do we get the current user token state from?
      {State1, Token1} = T(State0, Token0),
      caffe:process_event({wave_algo, decide, WaveName, DecideResult}, State);
    none ->
      caffe:log(State0, "decide: unregistered wave ~s", [WaveName]),
      {error, WaveName}
  end.
decide(State, WaveName) ->
  PluginState = caffe:get_plugin_state(?MODULE, State),
  case maps:get(PluginState, WaveName, none) of
    #wave_algo{user_wave = {_, _, D, _}} ->
      % where do we get the current user token state from?
      DecideResult = D(State, UserTokenState),
      % broadcast the decision result
      caffe:process_event({wave_algo, decide, WaveName, DecideResult}, State);
    none ->
      caffe:log(State, "decide: unregistered wave ~s", [WaveName]),
      {error, WaveName}
  end.