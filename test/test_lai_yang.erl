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
      {'internal', lai_yang_snapshot}, % kick off algo on vertex_a
      % send_control <presnap, color:0, count:3> to vertex_b
      {'send', vertex_b, message3}, % <message3, color:1>
      {'send', vertex_b, message4}, % <message4, color:1>
      {'receive', [{control, vertex_b, lai_yang}]}, % color:0, count:0
      % snapshot end @ color:0 = []
      {'send', vertex_b, message5}, % <message5, color:1>
      {'receive', [{basic, {lai_yang, message7, 1}}]}, % <message7, color:1>
      {'receive', [{control, vertex_b, lai_yang}]}, % color:1, count:3
      % send_control <presnap, color:1, count:4>
      {'receive', [{basic, {lai_yang, message9, 2}}]}, % <message9, color:2>
      {'send', vertex_b, message6}, % <message6, color:2>
      {'receive', [{basic, {lai_yang, message8, 1}}]}  % <message8, color:1>
      % snapshot end @ color:1 = [message9, message8]
    ]},
    { vertex_b, [
      {'receive', [{basic, {lai_yang, message3, 1}}]}, % <message3, color:1>
      % snapshot occurs: received a higher coloring, take a snapshot for all colors less than received
      % send_control <presnap, color:0, count:1> to vertex_a
      {'receive', [{basic, {lai_yang, message1, 0}}]}, % <message1, color:0>
      {'receive', [{control, vertex_a, lai_yang}]}, % color:0, count:3
      {'receive', [{basic, {lai_yang, message2, 0}}]}, % <message2, color:0>
      % snapshot end @ color:0 = [message1, message2]
      {'receive', [{basic, {lai_yang, message4, 1}}]}, % <message4, color:1>
      {'send', vertex_a, message7}, % <message7, color:1>
      {'send', vertex_a, message8}, % <message8, color:1>
      {'internal', lai_yang_snapshot}, % kick off algo on vertex_b
      % send_control <presnap, color:1, count:3>
      {'receive', [{basic, {lai_yang, message5, 1}}]}, % <message5, color:1>
      {'send', vertex_a, message9}, % <message9, color:2>
      {'receive', [{basic, {lai_yang, message6, 2}}]}, % <message6, color:2>
      {'receive', [{control, vertex_a, lai_yang}]} % color:1, count:4
      % snapshot end @ color:1 = [message5]
    ]}
  ].

% specify values we expect the plugins to be after each event
tests() -> [
  { lai_yang, [
    {vertex_a, [
      % message1 sent with color 0
      #{0 =>
        #{channel_state => #{},
          incoming => #{},
          outgoing => #{vertex_b => 1},
          snapshot_taken => false,
          terminal => false}},
      % message2 sent with color 0
      #{0 =>
        #{channel_state => #{},
          incoming => #{},
          outgoing => #{vertex_b => 2},
          snapshot_taken => false,
          terminal => false}},
      % vertex_a initiates snapshot
      #{0 =>
        #{channel_state => #{},
          incoming => #{},
          outgoing => #{vertex_b => 2},
          snapshot_taken => true,
          terminal => false}},
      % message3 is sent with color 1
      #{0 =>
        #{channel_state => #{},
          incoming => #{},
          outgoing => #{vertex_b => 2},
          snapshot_taken => true,
          terminal => false},
        1 =>
        #{channel_state => #{},
          incoming => #{},
          outgoing => #{vertex_b => 1},
          snapshot_taken => false,
          terminal => false}},
      % message4 is sent with color 1
      #{0 =>
        #{channel_state => #{},
          incoming => #{},
          outgoing => #{vertex_b => 2},
          snapshot_taken => true,
          terminal => false},
        1 =>
        #{channel_state => #{},incoming => #{},
          outgoing => #{vertex_b => 2},
          snapshot_taken => false,
          terminal => false}},
      % we receive <presnap, color:0, count:1> from vertex_a.
      % snapshot for color 0 is now done
      #{0 =>
        #{channel_state => #{},
          incoming => #{vertex_b => 1},
          outgoing => #{vertex_b => 2},
          snapshot_taken => true,
          terminal => true},
        1 =>
        #{channel_state => #{},
          incoming => #{},
          outgoing => #{vertex_b => 2},
          snapshot_taken => false,
          terminal => false}},
      % message5 is sent with color 1
      #{0 =>
        #{channel_state => #{},
          incoming => #{vertex_b => 1},
          outgoing => #{vertex_b => 2},
          snapshot_taken => true,
          terminal => true},
        1 =>
        #{channel_state => #{},
          incoming => #{},
          outgoing => #{vertex_b => 3},
          snapshot_taken => false,
          terminal => false}},
      % receive basic message7 before snapshot 1 is taken, this is discounted from incoming
      #{0 =>
        #{channel_state => #{},
          incoming => #{vertex_b => 1},
          outgoing => #{vertex_b => 2},
          snapshot_taken => true,
          terminal => true},
        1 =>
        #{channel_state => #{},
          incoming => #{vertex_b => -1},
          outgoing => #{vertex_b => 3},
          snapshot_taken => false,
          terminal => false}},
      % receive <presnap, color:1, count:3>, we expect one more message from vertex_b for color 1
      #{0 =>
        #{channel_state => #{},
          incoming => #{vertex_b => 1},
          outgoing => #{vertex_b => 2},
          snapshot_taken => true,
          terminal => true},
        1 =>
        #{channel_state => #{},
          incoming => #{vertex_b => 2},
          outgoing => #{vertex_b => 3},
          snapshot_taken => true,
          terminal => false}},
      % receive basic message9 before snapshot 2 is taken, this is discounted
      #{0 =>
        #{channel_state => #{},
          incoming => #{vertex_b => 1},
          outgoing => #{vertex_b => 2},
          snapshot_taken => true,
          terminal => true},
        1 =>
        #{channel_state => #{},
          incoming => #{vertex_b => 2},
          outgoing => #{vertex_b => 3},
          snapshot_taken => true,
          terminal => false},
        2 =>
        #{channel_state => #{},
          incoming => #{vertex_b => -1},
          outgoing => #{},
          snapshot_taken => false,
          terminal => false}},
      % message6 is sent with color 2
      #{0 =>
        #{channel_state => #{},
          incoming => #{vertex_b => 1},
          outgoing => #{vertex_b => 2},
          snapshot_taken => true,
          terminal => true},
        1 =>
        #{channel_state => #{},
          incoming => #{vertex_b => 2},
          outgoing => #{vertex_b => 3},
          snapshot_taken => true,
          terminal => false},
        2 =>
        #{channel_state => #{},
          incoming => #{vertex_b => -1},
          outgoing => #{vertex_b => 1},
          snapshot_taken => false,
          terminal => false}},
      % receive basic message8 after snapshot 1 is taken, included in channel_state
      % snapshot 1 is now terminal
      % snapshot 0 is cleaned
      #{1 =>
        #{channel_state => #{vertex_b => [message8]},
          incoming => #{vertex_b => 2},
          outgoing => #{vertex_b => 3},
          snapshot_taken => true,
          terminal => true},
        2 =>
        #{channel_state => #{},
          incoming => #{vertex_b => -1},
          outgoing => #{vertex_b => 1},
          snapshot_taken => false,
          terminal => false}}

    ]},
    {vertex_b, [
      % message3 @color = 1 received
      % start snapshot 0
      % discount message3 from snapshot 1
      #{0 =>
        #{channel_state => #{},
          incoming => #{},
          outgoing => #{},
          snapshot_taken => true,
          terminal => false},
        1 =>
        #{channel_state => #{},
          incoming => #{vertex_a => -1},
          outgoing => #{},
          snapshot_taken => false,
          terminal => false}}
      ,
      % message1 received after snapshot 0 is taken, include in channel_state
      #{0 =>
        #{channel_state => #{vertex_a => [message1]},
          incoming => #{},
          outgoing => #{},
          snapshot_taken => true,
          terminal => false},
        1 =>
        #{channel_state => #{},
          incoming => #{vertex_a => -1},
          outgoing => #{},
          snapshot_taken => false,
          terminal => false}},
      % control message received, indicating we need to receive one more message on snapshot 0
      #{0 =>
        #{channel_state => #{vertex_a => [message1]},
          incoming => #{vertex_a => 3},
          outgoing => #{},
          snapshot_taken => true,
          terminal => false},
        1 =>
        #{channel_state => #{},
          incoming => #{vertex_a => -1},
          outgoing => #{},
          snapshot_taken => false,
          terminal => false}},
      % message2 received, all messages received on snapshot 0
      % snapshot 0 is terminal
      % final channel state = [message2, message1]
      #{0 =>
        #{channel_state => #{vertex_a => [message2,message1]},
          incoming => #{vertex_a => 3},
          outgoing => #{},
          snapshot_taken => true,
          terminal => true},
        1 =>
        #{channel_state => #{},
          incoming => #{vertex_a => -1},
          outgoing => #{},
          snapshot_taken => false,
          terminal => false}},
      % message4 received before snapshot 1 taken, discount
      #{0 =>
        #{channel_state =>
        #{vertex_a => [message2,message1]},
          incoming => #{vertex_a => 3},
          outgoing => #{},
          snapshot_taken => true,
          terminal => true},
        1 =>
        #{channel_state => #{},
          incoming => #{vertex_a => -2},
          outgoing => #{},
          snapshot_taken => false,
          terminal => false}},
      % message7 sent for snapshot 1, outgoing incremented
      #{0 =>
        #{channel_state =>
        #{vertex_a => [message2,message1]},
          incoming => #{vertex_a => 3},
          outgoing => #{},
          snapshot_taken => true,
          terminal => true},
        1 =>
        #{channel_state => #{},
          incoming => #{vertex_a => -2},
          outgoing => #{vertex_a => 1},
          snapshot_taken => false,
          terminal => false}},
      % message8 sent for snapshot 1, outgoing incremented
      #{0 =>
        #{channel_state => #{vertex_a => [message2,message1]},
          incoming => #{vertex_a => 3},
          outgoing => #{},
          snapshot_taken => true,
          terminal => true},
        1 =>
        #{channel_state => #{},
          incoming => #{vertex_a => -2},
          outgoing => #{vertex_a => 2},
          snapshot_taken => false,
          terminal => false}},
      % vertex_b initiates a snapshot, kicking off snapshot 1
      #{0 =>
        #{channel_state => #{vertex_a => [message2,message1]},
          incoming => #{vertex_a => 3},
          outgoing => #{},
          snapshot_taken => true,
          terminal => true},
        1 =>
        #{channel_state => #{},
          incoming => #{vertex_a => -2},
          outgoing => #{vertex_a => 2},
          snapshot_taken => true,
          terminal => false}},
      % receive message5 for snapshot1, included in channel state
      #{0 =>
        #{channel_state =>
        #{vertex_a => [message2,message1]},
          incoming => #{vertex_a => 3},
          outgoing => #{},snapshot_taken => true,
          terminal => true},
        1 =>
        #{channel_state =>
        #{vertex_a => [message5]},
          incoming => #{vertex_a => -2},
          outgoing => #{vertex_a => 2},
          snapshot_taken => true,
          terminal => false}},
      % send message9 for snapshot 2, added to outgoing
      #{0 =>
        #{channel_state => #{vertex_a => [message2,message1]},
          incoming => #{vertex_a => 3},
          outgoing => #{},
          snapshot_taken => true,
          terminal => true},
        1 =>
        #{channel_state => #{vertex_a => [message5]},
          incoming => #{vertex_a => -2},
          outgoing => #{vertex_a => 2},
          snapshot_taken => true,
          terminal => false},
        2 =>
        #{channel_state => #{},
          incoming => #{},
          outgoing => #{vertex_a => 1},
          snapshot_taken => false,
          terminal => false}},
      % message6 received presnapshot for color 2, discounted
      #{0 =>
        #{channel_state => #{vertex_a => [message2,message1]},
          incoming => #{vertex_a => 3},
          outgoing => #{},
          snapshot_taken => true,
          terminal => true},
        1 =>
        #{channel_state => #{vertex_a => [message5]},
          incoming => #{vertex_a => -2},
          outgoing => #{vertex_a => 2},
          snapshot_taken => true,
          terminal => false},
        2 =>
        #{channel_state => #{},
          incoming => #{vertex_a => -1},
          outgoing => #{vertex_a => 1},
          snapshot_taken => false,
          terminal => false}},
      % <presnap, color:1, count:4> received
      % snapshot 1 is now terminal
      % snapshot 0 is cleaned
      #{1 =>
        #{channel_state => #{vertex_a => [message5]},
          incoming => #{vertex_a => 2},
          outgoing => #{vertex_a => 2},
          snapshot_taken => true,
          terminal => true},
        2 =>
        #{channel_state => #{},
          incoming => #{vertex_a => -1},
          outgoing => #{vertex_a => 1},
          snapshot_taken => false,
          terminal => false}}
    ]}
  ]},
  { messenger, test_mode }
].

plugins() -> [ messenger, lai_yang ].

args() -> #{
  caffe_logging => [
    {messenger, plugin_only},
    {graph_state, quiet},
    {terminator, plugin_only}
  ]
}.