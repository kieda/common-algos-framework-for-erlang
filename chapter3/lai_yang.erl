%%%-------------------------------------------------------------------
%%% @author zkieda
%%% @copyright (C) 2021, <COMPANY>
%%% @doc
%%%   modified lai_yang algorithm to handle multiple iterations of the snapshot algo
%%%   a classic implementation of lai_yang 'colors' the graph from white to red, but this has hiccups
%%%   if we want to run the algo multiple times and messages are received out of order, e.g. if we reset our state to 'white'
%%%   after algo termination but we receive a 'red' message from an earlier run we will take a snapshot again.
%%%
%%%   this can be avoided by having an ordered coloring scheme, where we color the graph from white to red to blue to yellow etc.
%%%   we use integers as colors here
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
  snapshot_in_progress = false,
  % increment when we take a local snapshot
  % if we receive a message from a previous iteration we know to disregard the message
  % if we receive a message from a future iteration we update to that count, and start a snapshot if not already in progress
  color = 0,
  % number of presnapshot messages expected on each incoming channel
  incoming_counter = #{},
  % number of presnapshot messages sent to each outgoing channel
  outgoing_counter = #{},
  % current calculated channel state
  channel_state = #{},
  % final channel snapshot
  channel_snapshot = #{},
  % used to store snapshot captured
  snapshot = none
}).

-type lai_yang() :: #lai_yang{
  snapshot_in_progress :: boolean(),
  color :: non_neg_integer(),
  incoming_counter :: #{ caffe_graph:vertex() => non_neg_integer() },
  outgoing_counter :: #{ caffe_graph:vertex() => non_neg_integer() },
  channel_state :: #{ caffe_graph:vertex() => [any()]},
  snapshot :: caffe:caffe_state(any())
}.

dependencies() -> [
  graph_state,
  messenger
].

format(#lai_yang{snapshot_in_progress = S,
  color = T,
  outgoing_counter = O,
  incoming_counter = I,
  channel_state = C}) -> #{
    snapshot_in_progress => S,
    color => T,
    outgoing => O,
    incoming => I,
    channel_state => C}.

-spec new_plugin(caffe_graph:vertex_args(caffe:caffe_state(any()))) -> lai_yang().
new_plugin(_) ->
  #lai_yang{}.

% decorates all outgoing basic messages with lai_yang markers
% plugins with wrap_msg are called right before message has actually been sent, after send event has been processed
wrap_msg(Msg, Vertex, State, P = #lai_yang{snapshot_in_progress = S, color = T, outgoing_counter = O}) ->
  % update counter if we are presnapshot
  caffe:log(State, "decorate basic message with ~s:~b", [Vertex, T]),
  O2 = case S of
         false ->
           NewCounter = maps:get(Vertex, O, 0) + 1,
           caffe:log(State, "counter on ~s <- ~b", [Vertex, NewCounter]),
           maps:put(Vertex, NewCounter, O);
         true -> O
       end,
  {{lai_yang, Msg, T}, State, P#lai_yang{outgoing_counter = O2}}.
% undecorates all incoming messages for lai_yang algo
% plugins with unwrap_msg are called immediately after reception, before receive event occurs
unwrap_msg({lai_yang, Msg, TReceive}, Vertex, State, #lai_yang{color = TInternal})
    when TReceive > TInternal -> % we receive a higher coloring, take a snapshot
  caffe:log(State, "undecorate basic message from ~s:true", [Vertex]),
  % update the color and take a snapshot
  State1 = take_snapshot_internal(update_color(State, TReceive)),
  {Msg, State1, caffe:get_plugin_state(?MODULE, State1)};
unwrap_msg({lai_yang, Msg, TReceive}, Vertex, State,
    P = #lai_yang{snapshot_in_progress = S, color = TInternal, channel_state = C}) ->
  caffe:log(State, "undecorate basic message from ~s:false", [Vertex]),
  % if we are currently taking a snapshot, and we received a message from a previous color
  % then we incorporate it into the channel's state
  P2 = case S andalso TReceive < TInternal of
    true -> % 1. update channel state with new message
            caffe:log(State, "snapshot state receive <- ~s:~p", [Vertex, Msg]),
            C2 = maps:update_with(Vertex, fun(ChannelState) -> [Msg|ChannelState] end, [Msg], C),
            % 2. terminate if we received all expected messages on incoming channels
            check_terminate(State, P#lai_yang{channel_state = C2});
    false -> P
  end,
  {Msg, State, P2}.

% checks state for termination. If the lai_yang should terminate we reset the plugin state
check_terminate(State, P = #lai_yang{incoming_counter = I, channel_state = C}) ->
  IncomingV = graph_state:get_incoming(State),
  IncomingCounters = lists:map(fun(VertexIn) -> maps:get(VertexIn, I, 0) end, IncomingV),
  IncomingReceived = lists:map(fun(VertexIn) -> length(maps:get(VertexIn, C, [])) + 1 end, IncomingV),
  caffe:log(State, "incoming counters = ~p", [lists:zip(IncomingV, IncomingCounters)]),
  caffe:log(State, "incoming received = ~p", [lists:zip(IncomingV, IncomingReceived)]),
  case IncomingCounters == IncomingReceived of
    true ->
      % terminate algo - reset incoming and outgoing counters
      caffe:log(State, "snapshot end ~p", [{P#lai_yang.snapshot, C}]),
      % reset incoming_counter, channel_state, and snapshot_in_progress now that we have received all expected messages
      % outgoing_counter is reset after we send out our control message
      P#lai_yang{snapshot_in_progress = false, incoming_counter = #{}, channel_state = #{}, channel_snapshot = C};
    false -> P
  end.

% note: there will only be at most a difference of 1 among colors in strongly connected component (without failure/recovery)
% proof: the algorithm requires a control message to be received on all incoming vertices as we send Incoming + 1 on the presnap message
%        thus, algorithm will not set snapshot_in_progress = false until all presnap control messages are received
%        we will not increment the color unless snapshot_in_progress = false, wherein all vertices in the graph will have the same color because algo terminated
% note: with failure/recovery we can recover through multiple missed snapshot checkpoints, so long as messages aren't lost
% proof: todo - though we do accumulate presnap incoming counters. However, logic is a bit finicky wrt termination checking. Will need more concrete proof

update_plugin({internal, new_lai_yang_snapshot}, State0, #lai_yang{snapshot_in_progress = true}) ->
  % lai_yang already in progress
  caffe:log(State0, "snapshot already in progress, ignoring"),
  ignore;
update_plugin({internal, new_lai_yang_snapshot}, State0, #lai_yang{snapshot_in_progress = false, color = T}) ->
  % new snapshot - increment current color and take the snapshot
  State1 = take_snapshot_internal(update_color(State0, T + 1)),
  {State1, caffe:get_plugin_state(?MODULE, State1)};
update_plugin({internal, lai_yang_snapshot}, State0, #lai_yang{snapshot_in_progress = true}) ->
  % lai_yang already in progress
  caffe:log(State0, "snapshot already in progress, ignoring"),
  ignore;
update_plugin({internal, lai_yang_snapshot}, State0, P = #lai_yang{snapshot_in_progress = false, color = T, outgoing_counter = O}) ->
  % 1. specify a new snapshot is in progress
  P1 = P#lai_yang{snapshot_in_progress = true},
  caffe:log(State0, "snapshot start @ color ~b", [T]),
  % 2. send a <presnap, Counter, Color> control message through all outgoing vertices
  VOut = graph_state:get_outgoing(State0),
  caffe:log(State0, "sending marker to outgoing ~p", [VOut]),
  State1 = lists:foldr(
    fun(V, StateA) ->
      messenger:send_control_message({presnap, maps:get(V, O, 0) + 1, T}, lai_yang, V, StateA)
    end, State0, VOut),
  % 3. take a local snapshot of the internal data, reset outgoing counters as we just sent our presnap message
  P2 = P1#lai_yang{snapshot = State1, outgoing_counter = #{}},
  caffe:log(State1, "internal snapshot: ~p", [State1]),
  {State1, P2};
% receives <presnap, Counter, Color> from VertexIn
update_plugin({receive_control, lai_yang, VertexIn, {presnap, Counter, T}}, State0, _) ->
  caffe:log(State0, "receive_control <- ~s:{presnap, ~b, ~b}", [VertexIn, Counter, T]),
  % 1. update incoming counter and color
  State1 = update_color(update_incoming(State0, VertexIn, Counter), T),
  % 2. take a snapshot
  State2 = take_snapshot_internal(State1),
  P1 = caffe:get_plugin_state(?MODULE, State2),
  % 3. check for termination
  {State2, check_terminate(State2, P1)};
update_plugin({internal, lai_yang_snapshot, update_incoming, VertexIn, Increment}, State,
    P0 = #lai_yang{incoming_counter = I}) ->
  % only used internally to increment the internal counter of a vertex
  P1 = P0#lai_yang{
    % increment the expected incoming messages
    incoming_counter = maps:update_with(VertexIn, fun(VCount) -> VCount + Increment end, Increment, I)
  },
  {State, P1};
update_plugin({internal, lai_yang_snapshot, update_color, ColorNew}, State,
    P0 = #lai_yang{color = ColorInternal}) ->
  % only used internally to increase the color - either on message reception or when starting a new lai_yang snapshot
  P1 = P0#lai_yang{
    color = max(ColorInternal, ColorNew)
  },
  {State, P1};
update_plugin(_, _, _) -> ignore.

% designates this vertex as an initiator of a snapshot & takes a snapshot using the chandy lamport method
take_snapshot(State) ->
  caffe:process_event({internal, new_lai_yang_snapshot}, State).
take_snapshot_internal(State) ->
  caffe:process_event({internal, lai_yang_snapshot}, State).


update_incoming(State, Vertex, Increment) ->
  caffe:process_event({internal, lai_yang_snapshot, update_incoming, Vertex, Increment}, State).
update_color(State, ColorNew) ->
  caffe:process_event({internal, lai_yang_snapshot, update_color, ColorNew}, State).