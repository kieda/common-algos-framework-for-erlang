%%%-------------------------------------------------------------------
%%% @author zkieda
%%% @copyright (C) 2021, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 03. Jun 2021 1:40 AM
%%%-------------------------------------------------------------------
-module(chandy_lamport).
-author("zkieda").

%% public accessors
-export([take_snapshot/1]).
%% implementation
-export([dependencies/0, format/1, new_plugin/1, update_plugin/3]).

-record(chandy_lamport, {
  % set to true when we take a local snapshot, reset to false when snapshot completed
  snapshot_in_progress = false,
  % records if we received a marker from a given vertex yet
  marker_received = #{},
  % records channel state of each outgoing vertex V
  % after snapshot_in_progress and before marker_received for V
  channel_state = #{},
  % used to store snapshot captured
  snapshot = none
}).

-type chandy_lamport() :: #chandy_lamport{
  snapshot_in_progress :: boolean(),
  marker_received :: #{ caffe_graph:vertex() => boolean() },
  channel_state :: #{ caffe_graph:vertex() => [any()]}
}.

dependencies() -> [
  graph_state,
  messenger
].

format(#chandy_lamport{snapshot_in_progress = S, marker_received = M, channel_state = C}) -> {S, M, C}.

-spec new_plugin(caffe_graph:vertex_args(caffe:caffe_state(any()))) -> chandy_lamport().
new_plugin(_) ->
  #chandy_lamport{}.

% internal event: take a snapshot
% {internal, snapshot} -> method to override snapshot taking for a given Plugin
update_plugin({internal, chandy_lamport_snapshot}, State0, P) ->
  case P#chandy_lamport.snapshot_in_progress of
    % snapshot already in progress, ignore
    true ->
      caffe:log(State0, "snapshot already in progress, ignoring"),
      ignore;
    false ->
      % 1. reset state, with specifying a new snapshot is in progress
      P1 = #chandy_lamport{snapshot_in_progress = true},
      caffe:log(State0, "snapshot start"),
      % 2. send a <marker> control message through all outgoing vertices
      VOut = graph_state:get_outgoing(State0),
      caffe:log(State0, "sending marker to outgoing ~p", [VOut]),
      State1 = lists:foldr(
        fun(V, StateA) ->
          messenger:send_control_message(marker, chandy_lamport, V, StateA)
        end, State0, VOut),
      % 3. take a local snapshot of the internal data
      P2 = P1#chandy_lamport{snapshot = State1},
      caffe:log(State0, "internal snapshot: ~p", [State1]),
      {State1, P2}
  end;
% receives a <marker> from VertexIn
update_plugin({receive_control, chandy_lamport, VertexIn, marker}, State0, _) ->
  caffe:log(State0, "receive_control <- marker:~s", [VertexIn]),
  % 1. take a snapshot
  State1 = take_snapshot(State0),
  P1 = caffe:get_plugin_state(?MODULE, State1),
  % 2. set marker_received on VertexIn
  P2 = P1#chandy_lamport{
    marker_received = maps:put(VertexIn, true, P1#chandy_lamport.marker_received)
  },
  % 3. snapshot is done iff we received a marker on all incoming vertices
  IncomingV = graph_state:get_incoming(State1),
  MarkersReceived = lists:map(fun(V) -> maps:get(V, P2#chandy_lamport.marker_received, false) end, IncomingV),
  caffe:log(State1, "markers received = ~p", [lists:zip(IncomingV, MarkersReceived)]),
  SnapshotDone = lists:all(fun(B) -> B end, MarkersReceived),
  % todo - what do we do with our final snapshot data P2, including the calculated incoming channels?
  % todo - possibly add serialization/deserialization so we can recover from a snapshot
  if SnapshotDone ->
      caffe:log(State1, "snapshot end ~p", [{P2#chandy_lamport.snapshot, P2#chandy_lamport.channel_state}]),
      % keep our recorded state & messages, but set snapshot_in_progress to signal that we can begin this process anew
      {State1, P2#chandy_lamport{snapshot_in_progress = false}};
    true -> {State1, P2}
  end;
% receive a basic message from VertexIn
update_plugin({'receive', VertexIn, Msg}, State, P = #chandy_lamport{channel_state = C}) ->
  % record basic messages received on a given channel when both a snapshot is in progress
  % and also a marker has not yet been received on the incoming channel yet
  case {P#chandy_lamport.snapshot_in_progress, maps:get(VertexIn, P#chandy_lamport.marker_received, false)} of
    {true, false} ->
      caffe:log(State, "snapshot state receive <- ~s:~p", [VertexIn, Msg]),
      % update the channel state with the new Msg
      P1 = P#chandy_lamport{channel_state =
        maps:update_with(VertexIn, fun(L) -> [Msg|L] end, [Msg], C)},
      {State, P1};
    {false, _} ->
      caffe:log(State, "ignore basic message for snapshot, not in progress"),
      ignore;
    {true, true} ->
      caffe:log(State, "ignore basic message for snapshot, marker received from ~s", [VertexIn]),
      ignore
  end;
update_plugin(_, _, _) -> ignore.

% designates this vertex as an initiator of a snapshot & takes a snapshot using the chandy lamport method
take_snapshot(State) ->
  caffe:process_event({internal, chandy_lamport_snapshot}, State).
