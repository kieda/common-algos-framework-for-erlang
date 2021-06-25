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

% represents the snapshot for an individual color
-record(lai_yang, {
  % has the snapshot been taken for this color yet?
  snapshot_taken = false,
  % has the snapshot terminated for this color yet (received all messages we expected?)
  terminal = false,
  % number of presnapshot messages expected on each incoming channel
  incoming_counter = #{},
  % number of presnapshot messages sent to each outgoing channel
  outgoing_counter = #{},
  % current calculated channel state
  channel_state = #{},
  % used to store snapshot captured
  snapshot = none
}).

-type color_snapshot() :: #lai_yang{
  snapshot_taken :: boolean(),
  terminal :: boolean(),
  % incoming may be negative if we receive presnapshot messages but are not taking a snapshot
  % if both send and receive events are presnapshot then the message should not be included in the channel state
  incoming_counter :: #{ caffe_graph:vertex() => integer() },
  outgoing_counter :: #{ caffe_graph:vertex() => non_neg_integer() },
  channel_state :: #{ caffe_graph:vertex() => [any()]},
  snapshot :: caffe:caffe_state(any())
}.

% represents the current snapshot state for each color
% we apply classic lai_yang but track each color independently
-type lai_yang() :: #{
  non_neg_integer() => color_snapshot()
}.

dependencies() -> [
  graph_state,
  messenger
].

-spec current_color(lai_yang()) -> non_neg_integer().
current_color(P) ->
  SnapshotTaken = maps:filter(fun(_, #lai_yang{snapshot_taken = B}) -> B end, P),
  lists:max([-1|maps:keys(SnapshotTaken)]) + 1.

% this is for scrubbing purposes - we want to get rid of all earlier terminal entries, but
% keep the newest of the earlier terminal entries
smallest_relevant(P) when map_size(P) =:= 0 -> 0;
smallest_relevant(P) ->
  Colors = maps:keys(P),
  NonTerminal = maps:filter(fun(_, #lai_yang{terminal = T}) -> not T end, P),
  % smallest non terminal entry
  SmallestNonTerminal = lists:min([lists:max(Colors)|maps:keys(NonTerminal)]),
  % terminal entries before the smallest non terminal
  EarlierTerminalEntries = lists:filter(fun(Color) -> Color < SmallestNonTerminal end, Colors),
  case EarlierTerminalEntries of
    [] -> SmallestNonTerminal;
    _ -> lists:max(EarlierTerminalEntries)
  end.

scrub(P) when map_size(P) =:= 0 -> P;
scrub(P) ->
  % get the minimum color of a snapshot currently in progress, or just use the current max if none are in progress
  MinColor = smallest_relevant(P),
  % keep all snapshots greater than or equal to the minimum in progress
  maps:filter(fun(Color, _) -> Color >= MinColor end, P).

format(P) ->
  % grab the snapshot(s) in progress along with along with snapshot(s) of a higher color
  % todo we can probably combine these into a single snapshot by aggregating incoming/outgoing counters and channel states
  % any( snapshot_taken ), sum( incoming_counter ) by vertex, sum( outgoing_counter ) by vertex, union( channel_state ) by vertex
  maps:map(fun(_, #lai_yang{
                    terminal = T,
                    snapshot_taken = B,
                    incoming_counter = I,
                    outgoing_counter = O,
                    channel_state = C}) ->
             #{terminal => T,
               snapshot_taken => B,
               incoming => I,
               outgoing => O,
               channel_state => C}
           end, P).

-spec new_plugin(caffe_graph:vertex_args(caffe:caffe_state(any()))) -> lai_yang().
new_plugin(_) -> #{}.

take_snapshots(State0, ColorMax) ->
  P = caffe:get_plugin_state(?MODULE, State0),
  % largest terminal that is not preceded by non-terminal color
  ColorStart = smallest_relevant(P),
  Snapshots2Take = lists:filter(
    fun(Color) ->
      #lai_yang{ snapshot_taken = B } = maps:get(Color, P, #lai_yang{}),
      not B
    end,
    lists:seq(ColorStart, ColorMax)),
  caffe:log(State0, "taking snapshots for colors ~s", [Snapshots2Take]),
  lists:foldl(
    fun(Color, StateA) -> take_snapshot(StateA, Color)
    end, State0, Snapshots2Take).

% decorates all outgoing basic messages with lai_yang markers
% plugins with wrap_msg are called right before message has actually been sent, after send event has been processed
wrap_msg(Msg, Vertex, State, P) ->
  Color = current_color(P),
  LaiYang = #lai_yang{ snapshot_taken = B, outgoing_counter = O } = maps:get(Color, P, #lai_yang{}),

  % update counter if we are presnapshot
  caffe:log(State, "decorate basic message to ~s with ~b", [Vertex, Color]),
  % B should be false, current color will always be presnapshot
  ok = case B of
         true -> {invariant, Vertex, Color, LaiYang};
         false -> ok
       end,
  NewCounter = maps:get(Vertex, O, 0) + 1,
  caffe:log(State, "counter on ~s <- ~b", [Vertex, NewCounter]),
  LaiYang2 = LaiYang#lai_yang{ outgoing_counter = maps:put(Vertex, NewCounter, O) },
  P2 = maps:put(Color, LaiYang2, P),
  % convention: add name as atom as prefix, additional arguments as suffix
  {{lai_yang, Msg, Color}, State, P2}.

unwrap_msg({lai_yang, Msg, ColorRcv}, Vertex, State0, _) ->
  caffe:log(State0, "undecorate basic message from ~s@~b", [Vertex, ColorRcv]),

  % take a snapshot for all colors less than this one (when relevant)
  State1 = take_snapshots(State0, ColorRcv - 1),
  P1 = caffe:get_plugin_state(?MODULE, State1),

  % incorporate message into snapshot for the color
  LaiYang = maps:get(ColorRcv, P1, #lai_yang{}),
  P = case LaiYang#lai_yang.snapshot_taken of
    % snapshot has been taken for this color
    % we include the message in the channel state to mark it as received
    true ->
      caffe:log(State0, "channel_state ~b include ~s:~p", [ColorRcv, Vertex, Msg]),
      C = maps:update_with(Vertex, fun(ChannelState) -> [Msg|ChannelState] end, [Msg], LaiYang#lai_yang.channel_state),
      P2 = maps:put(ColorRcv, LaiYang#lai_yang{channel_state = C}, P1),
      check_terminate(State0, ColorRcv, P2);
    % snapshot has not been taken for this color
    % we do not include the message in the channel state
    % but mark the message as received by decrementing the incoming counter
    false ->
      caffe:log(State0, "channel_state ~b discard ~s:~p", [ColorRcv, Vertex, Msg]),
      I = maps:update_with(Vertex, fun(Count) -> Count - 1 end, -1, LaiYang#lai_yang.incoming_counter),
      maps:put(ColorRcv, LaiYang#lai_yang{incoming_counter = I}, P1)
  end,
  {Msg, State0, P}.

% checks state for termination. If the lai_yang should terminate we reset the plugin state
check_terminate(State, Color, P) ->
  LaiYang = #lai_yang{incoming_counter = I, channel_state = C} = maps:get(Color, P),
  IncomingV = graph_state:get_incoming(State),
  IncomingCounters = lists:map(fun(VertexIn) -> maps:get(VertexIn, I, 0) end, IncomingV),
  IncomingReceived = lists:map(fun(VertexIn) -> length(maps:get(VertexIn, C, [])) + 1 end, IncomingV),
  caffe:log(State, "incoming counters@~b = ~p", [Color, lists:zip(IncomingV, IncomingCounters)]),
  caffe:log(State, "incoming received@~b = ~p", [Color, lists:zip(IncomingV, IncomingReceived)]),
  case IncomingCounters == IncomingReceived of
    true ->
      % terminate algo for the given color, scrub old snapshots
      caffe:log(State, "snapshot@~b end ~p", [Color, {LaiYang#lai_yang.snapshot, C}]),
      scrub(maps:put(Color, LaiYang#lai_yang{terminal = true}, P));
    false -> P
  end.

update_plugin({internal, lai_yang_snapshot}, State0, P) ->
  % perform a snapshot on the current color
  % will always take a snapshot by definition of current_color
  State1 = take_snapshot(State0, current_color(P)),
  {State1, caffe:get_plugin_state(?MODULE, State1)};
update_plugin({internal, lai_yang_snapshot, Color}, State0, P) ->
  case P of
    #{ Color := #lai_yang{ snapshot_taken = true } } ->
      % snapshot already taken for the given color
      caffe:log(State0, "snapshot already in progress, ignoring"),
      ignore;
    _ ->
      caffe:log(State0, "snapshot start @ color ~b", [Color]),
      % 1. send a <presnap, Counter, Color> control message through all outgoing vertices
      #lai_yang{outgoing_counter = Outgoing} = maps:get(Color, P, #lai_yang{}),

      VOut = graph_state:get_outgoing(State0),
      caffe:log(State0, "sending marker to outgoing ~p", [VOut]),
      State1 = lists:foldr(
        fun(V, StateA) ->
          messenger:send_control_message({presnap, Color, maps:get(V, Outgoing, 0) + 1}, lai_yang, V, StateA)
        end, State0, VOut),
      % 2. take local snapshot and specify a new snapshot is in progress
      P0 = caffe:get_plugin_state(?MODULE, State1),
      P1 = maps:update_with(Color,
        fun(LaiYang) -> LaiYang#lai_yang{snapshot_taken = true, snapshot = State1} end,
        #lai_yang{snapshot_taken = true, snapshot = State1},
        P0
      ),
      {State1, P1}
  end;
% receives <presnap, Counter, Color> from VertexIn
update_plugin({receive_control, lai_yang, VertexIn, {presnap, Color, Count}}, State0, _) ->
  caffe:log(State0, "receive_control <- ~s:{presnap, color:~b, count:~b}", [VertexIn, Color, Count]),
  % 1. update incoming counter
  State1 = update_incoming(State0, Color, VertexIn, Count),
  % 2. take a snapshot at Color (and below)
  State2 = take_snapshots(State1, Color),
  P1 = caffe:get_plugin_state(?MODULE, State2),
  % 3. check for termination
  {State2, check_terminate(State2, Color, P1)};
update_plugin({internal, lai_yang_snapshot, update_incoming, Color, VertexIn, Increment}, State, P) ->
  % only used internally to increment the internal counter of a vertex
  LaiYang0 = #lai_yang{incoming_counter = I} = maps:get(Color, P, #lai_yang{}),
  LaiYang1 = LaiYang0#lai_yang{
    % increment the expected incoming messages
    incoming_counter = maps:update_with(VertexIn, fun(VCount) -> VCount + Increment end, Increment, I)
  },
  {State, maps:put(Color, LaiYang1, P)};
update_plugin(_, _, _) -> ignore.

% designates this vertex as an initiator of a snapshot, takes a snapshot on the next color
take_snapshot(State) ->
  caffe:process_event({internal, lai_yang_snapshot}, State).
% takes a snapshot for a specific color. internal only
take_snapshot(State, Color) ->
  caffe:process_event({internal, lai_yang_snapshot, Color}, State).
% updates incoming counter for a given color. internal only
update_incoming(State, Color, Vertex, Increment) ->
  caffe:process_event({internal, lai_yang_snapshot, update_incoming, Color, Vertex, Increment}, State).