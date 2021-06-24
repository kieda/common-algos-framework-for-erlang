%%%-------------------------------------------------------------------
%%% @author zkieda
%%% @copyright (C) 2021, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 21. Jun 2021 12:56 PM
%%%-------------------------------------------------------------------
-module(test_lai_yang).
-author("zkieda").

%% API
-export([graph/0, events/0, tests/0, plugins/0, args/0]).

graph() -> {
  [vertex_a, vertex_b],
  [{vertex_a, vertex_b}, {vertex_b, vertex_a}]
}.

events() ->
  [
    { vertex_a, [
      {'send', vertex_b, message1}, % <message1, color:0>
      {'send', vertex_b, message2}, % <message2, color:0>
      {'internal', new_lai_yang_snapshot}, % kick off algo on vertex_a
      % send_control <presnap, 3, 1> to vertex_b
      {'send', vertex_b, message3}, % <message3, color:1>
      {'send', vertex_b, message4}, % <message4, color:1>
      {'receive', [{control, vertex_b, lai_yang}]},
      % snapshot end
      {'send', vertex_b, message5}, % <message5, color:1>
      {'receive', [{basic, {lai_yang, message7, 1}}]},
      {'receive', [{control, vertex_b, lai_yang}]}, % receive <presnap, 3, 2>
      % send_control <presnap, 2, 2>
      {'receive', [{basic, {lai_yang, message9, 2}}]},
      {'send', vertex_b, message6}, % <message6, color:2>
      {'receive', [{basic, {lai_yang, message8, 1}}]}
    ]},
    { vertex_b, [
      % we receive message[1-4] out of order
      {'receive', [{basic, {lai_yang, message3, 1}}]},
      % snapshot occurs: received a higher coloring
      % send_control <presnap, 1, 1> to vertex_a
      {'receive', [{basic, {lai_yang, message1, 0}}]},
      {'receive', [{control, vertex_a, lai_yang}]},
      {'receive', [{basic, {lai_yang, message2, 0}}]},
      % snapshot end
      {'receive', [{basic, {lai_yang, message4, 1}}]}, % not included in snapshot, and does not restart snapshot
      {'send', vertex_a, message7}, % <message1, color:1>
      {'receive', [{basic, {lai_yang, message5, 1}}]},
      {'send', vertex_a, message8}, % <message2, color:1>
      {'internal', new_lai_yang_snapshot}, % kick off algo on vertex_b
      % send_control <presnap, 2, 2>
      {'send', vertex_a, message9}, % <message2, color:2>
      {'receive', [{basic, {lai_yang, message6, 2}}]},
      {'receive', [{control, vertex_a, lai_yang}]}
    ]}
  ].

% specify values we expect the plugins to be after each event
tests() -> test_mode.

bad() -> [
  { lai_yang, [
    {vertex_a, [
      % message1 sent presnapshot
      #{snapshot_taken => false,
        incoming => #{},
        outgoing => #{vertex_b => 1},
        channel_state => #{}},
      % message2 sent presnapshot
      #{snapshot_taken => false,
        incoming => #{},
        outgoing => #{vertex_b => 2},
        channel_state => #{}},
      % vertex_a initiates snapshot
      #{snapshot_taken => true,
        incoming => #{},
        outgoing => #{vertex_b => 2},
        channel_state => #{}},
      % message3 is postsnapshot and thus does not modify state
      #{snapshot_taken => true,
        incoming => #{},
        outgoing => #{vertex_b => 2},
        channel_state => #{}},
      % message4 is postsnapshot and thus does not modify state
      #{snapshot_taken => true,
        incoming => #{},
        outgoing => #{vertex_b => 2},
        channel_state => #{}},
      % we receive <presnap, 1> from vertex_a. We reset state/terminate
      #{snapshot_taken => false,
        incoming => #{},
        outgoing => #{},
        channel_state => #{}}
    ]},
    {vertex_b, [
      % message3 received. We take snapshot as message3 postsnapshot, but do not include it
      % in our channel state
      #{snapshot_taken => true,
        incoming => #{},
        outgoing => #{},
        channel_state => #{}},
      % message1 received, which is included in our channel state
      #{snapshot_taken => true,
        incoming => #{},
        outgoing => #{},
        channel_state => #{vertex_a => [message1]}},
      % control message received
      #{snapshot_taken => true,
        incoming => #{vertex_a => 3},
        outgoing => #{},
        channel_state => #{vertex_a => [message1]}},
      % message2 received, all messages received so we reset state/terminate
      #{snapshot_taken => false,
        incoming => #{},
        outgoing => #{},
        channel_state => #{vertex_a => [message2,message1]}},
      % message4 received which is presnapshot. We do not start a snapshot again since it's not part of the same snapshot counter
      #{snapshot_taken => true,
        incoming => #{},
        outgoing => #{},
        channel_state => #{vertex_a => [message2,message1]}}
    ]}
  ]}
].

plugins() -> [ messenger, lai_yang ].

args() -> #{
  caffe_logging => [
    {messenger, plugin_only},
    {graph_state, quiet},
    {terminator, plugin_only}
  ]
}.