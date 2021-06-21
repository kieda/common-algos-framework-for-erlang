%%%-------------------------------------------------------------------
%%% @author zkieda
%%% @copyright (C) 2021, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 19. Jun 2021 3:54 PM
%%%-------------------------------------------------------------------
-module(lai_yang).
-author("zkieda").

%% public accessors
-export([take_snapshot/1]).
%% implementation
-export([dependencies/0, format/1, new_plugin/1, update_plugin/3]).
-export([wrap_msg/4, unwrap_msg/4]).

-record(lai_yang, {
  % set to true when we take a local snapshot, reset to false when snapshot completed
  snapshot_taken = false,
  % number of presnapshot messages expected on each incoming channel
  counter_incoming = #{},
  % number of presnapshot messages sent to each outgoing channel
  counter_outgoing = #{},
  % calculated channel state
  channel_state = #{},
  % used to store snapshot captured
  snapshot = none
}).

-type lai_yang() :: #lai_yang{
  snapshot_taken :: boolean(),
  counter_incoming :: #{ caffe_graph:vertex() => non_neg_integer() },
  counter_outgoing :: #{ caffe_graph:vertex() => non_neg_integer() },
  channel_state :: #{ caffe_graph:vertex() => [any()]},
  snapshot :: caffe:caffe_state(any())
}.

dependencies() -> [
  graph_state,
  messenger
].

format(#lai_yang{snapshot_taken = T,
  counter_incoming = I,
  counter_outgoing = O,
  channel_state = C}) -> {T, I, O, C}.

-spec new_plugin(caffe_graph:vertex_args(caffe:caffe_state(any()))) -> lai_yang().
new_plugin(_) ->
  #lai_yang{}.

% decorates all outgoing basic messages with lai_yang markers
% plugins with wrap_msg are called right before message has actually been sent, after send event has been processed
wrap_msg(Msg, Vertex, State, P = #lai_yang{snapshot_taken = T, counter_outgoing = O}) ->
  % update counter if we are presnapshot
  caffe:log(State, "decorate basic message with ~s:~s", [Vertex, T]),
  O2 = case T of
         false ->
           NewCounter = maps:get(Vertex, O, 0) + 1,
           caffe:log(State, "counter on ~s <- ~s", [Vertex, NewCounter]),
           maps:put(Vertex, NewCounter, O);
         true  -> O
       end,
  {{lai_yang, Msg, T}, State, P#lai_yang{counter_outgoing = O2}}.
% undecorates all incoming messages for lai_yang algo
% plugins with unwrap_msg are called immediately after reception, before receive event occurs
unwrap_msg({lai_yang, Msg, true}, Vertex, State, _) ->
  caffe:log(State, "undecorate basic message from ~s:true", [Vertex]),
  State1 = take_snapshot(State),
  {Msg, State1, caffe:get_plugin_state(?MODULE, State1)};
unwrap_msg({lai_yang, Msg, false}, Vertex, State,
    P = #lai_yang{snapshot_taken = T, channel_state = C}) ->
  caffe:log(State, "undecorate basic message from ~s:false", [Vertex]),
  P2 = case T of
    true -> % 1. update channel state with new message
            caffe:log(State, "snapshot state receive <- ~s:~p", [Vertex, Msg]),
            C2 = maps:update_with(Vertex, fun(ChannelState) -> [Msg|ChannelState] end, [Msg], C),
            % 2. terminate if we received all expected messages on incoming channels
            check_terminate(State, P#lai_yang{channel_state = C2});
    false -> P
  end,
  {Msg, State, P2}.

% checks state for termination. If the lai_yang should terminate we reset the plugin state
check_terminate(State, P = #lai_yang{counter_incoming = I, channel_state = C}) ->
  IncomingV = graph_state:get_incoming(State),
  IncomingCounters = lists:map(fun(VertexIn) -> maps:get(VertexIn, I, 0) end, IncomingV),
  IncomingReceived = lists:map(fun(VertexIn) -> length(maps:get(VertexIn, C, [])) + 1 end, IncomingV),
  caffe:log(State, "incoming counters = ~p", [lists:zip(IncomingV, IncomingCounters)]),
  caffe:log(State, "incoming received = ~p", [lists:zip(IncomingV, IncomingReceived)]),
  case IncomingCounters == IncomingReceived of
    true ->
      % terminate algo - reset incoming and outgoing counters
      caffe:log(State, "snapshot end ~p", [{P#lai_yang.snapshot, P#lai_yang.channel_state}]),
      P#lai_yang{snapshot_taken = false, counter_incoming = #{}, counter_outgoing = #{}};
    false -> P
  end.

update_plugin({internal, lai_yang_snapshot}, State0, #lai_yang{snapshot_taken = true}) ->
  % lai_yang already in progress
  caffe:log(State0, "snapshot already in progress, ignoring"),
  ignore;
update_plugin({internal, lai_yang_snapshot}, State0, P = #lai_yang{snapshot_taken = false, counter_outgoing = O}) ->
  % 1. specify a new snapshot is in progress
  P1 = P#lai_yang{snapshot_taken = true},
  caffe:log(State0, "snapshot start"),
  % 2. send a <presnap, Counter> control message through all outgoing vertices
  VOut = graph_state:get_outgoing(State0),
  caffe:log(State0, "sending marker to outgoing ~p", [VOut]),
  State1 = lists:foldr(
    fun(V, StateA) ->
      messenger:send_control_message({presnap, maps:get(V, O, 0) + 1}, lai_yang, V, StateA)
    end, State0, VOut),
  % 3. take a local snapshot of the internal data
  P2 = P1#lai_yang{snapshot = State1},
  caffe:log(State1, "internal snapshot: ~p", [State1]),
  {State1, P2};
% receives <presnap, Counter> from VertexIn
update_plugin({receive_control, lai_yang, VertexIn, {presnap, Counter}}, State0, _) ->
  caffe:log(State0, "receive_control <- ~s:{presnap, ~s}", [VertexIn, Counter]),
  % 1. update incoming counter
  State1 = increment_counter(State0, VertexIn, Counter),
  % 2. take a snapshot
  State2 = take_snapshot(State1),
  P1 = caffe:get_plugin_state(?MODULE, State2),
  % 3. check for termination
  {State2, check_terminate(State2, P1)};
update_plugin({internal, lai_yang_snapshot, increment_counter, VertexIn, Increment}, State, P0 = #lai_yang{counter_incoming = I}) ->
  % only used internally to increment the internal counter of a vertex
  P1 = P0#lai_yang{counter_incoming = maps:update_with(VertexIn, fun(VCount) -> VCount + Increment end, 0, I)},
  {State, P1};
update_plugin(_, _, _) -> ignore.

% designates this vertex as an initiator of a snapshot & takes a snapshot using the chandy lamport method
take_snapshot(State) ->
  caffe:process_event({internal, lai_yang_snapshot}, State).

increment_counter(State, Vertex, Increment) ->
  caffe:process_event({internal, lai_yang_snapshot, increment_counter, Vertex, Increment}, State).