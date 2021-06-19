%%%-------------------------------------------------------------------
%%% @author zkieda
%%% @copyright (C) 2021, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 18. Jun 2021 8:21 PM
%%%-------------------------------------------------------------------
-module(test_chandy_lamport).
-author("zkieda").

%% API
-export([graph/0, events/0, tests/0, plugins/0, args/0]).

graph() -> {
  [vertex_a, vertex_b, vertex_c, vertex_d],
  [{vertex_a, vertex_c}, {vertex_a, vertex_d}, {vertex_b, vertex_a}, {vertex_c, vertex_b}, {vertex_d, vertex_c}]
}.

events() ->
  [
    { vertex_a, [
      {'internal', chandy_lamport_snapshot}, % kick off algo on vertex_a
      % send_control <marker> to vertex_c, vertex_d
      {'receive', [{basic, message1}]},
      {'send', vertex_c, message2},
      {'receive', [{control, vertex_b, chandy_lamport}]},
      {'receive', [{basic, message4}]}
    ]},
    { vertex_b, [
      {'send', vertex_a, message1},
      {'receive', [{control, vertex_c, chandy_lamport}]},
      % send_control <marker> to vertex_c, vertex_a
      {'send', vertex_a, message4}
    ]},
    { vertex_c, [
      {'receive', [{control, vertex_a, chandy_lamport}]},
      % send_control <marker> to vertex_b
      {'receive', [{basic, message2}]},
      {'receive', [{basic, message3}]},
      {'receive', [{control, vertex_d, chandy_lamport}]}
    ]},
    { vertex_d, [
      {'send', vertex_c, message3},
      {'receive', [{control, vertex_a, chandy_lamport}]}
      % send_control <marker> to vertex_c
    ]}
  ].

% specify values we expect the plugins to be after each event
tests() -> [
  { chandy_lamport, [
    {vertex_a, [
      {true, #{}, #{}},
      {true, #{}, #{vertex_b => [message1]}},
      {true, #{}, #{vertex_b => [message1]}},
      {false, #{vertex_b => true}, #{vertex_b => [message1]}},
      {false, #{vertex_b => true}, #{vertex_b => [message1]}}
    ]},
    {vertex_b, [
      {false, #{},#{}},
      {false, #{vertex_c => true}, #{}},
      {false, #{vertex_c => true}, #{}}
    ]},
    {vertex_c, [
      {true, #{vertex_a => true}, #{}},
      {true, #{vertex_a => true}, #{}},
      {true, #{vertex_a => true}, #{vertex_d => [message3]}},
      {false, #{vertex_a => true,vertex_d => true}, #{vertex_d => [message3]}}
    ]},
    {vertex_d, [
      {false,#{},#{}},
      {false,#{vertex_a => true},#{}}
    ]}
  ]},
  { messenger, [
    {vertex_a, [
      % 1. <marker> messages are sent out to vertex_c, vertex_d
      #{sent => [],
        received => [],
        sent_control => [
          {chandy_lamport,marker,vertex_c},
          {chandy_lamport,marker,vertex_d}],
        received_control => []},
      % 2. receive message1, included in snapshot
      #{sent => [],
        received => [{message1,vertex_b}],
        sent_control => [
          {chandy_lamport,marker,vertex_c},
          {chandy_lamport,marker,vertex_d}],
        received_control => []},
      % 3. send message2 -> vertex_c
      #{sent => [{message2,vertex_c}],
        received => [{message1,vertex_b}],
        sent_control => [
          {chandy_lamport,marker,vertex_c},
          {chandy_lamport,marker,vertex_d}],
        received_control => []},
      % 4. receive <marker> from vertex_b
      #{sent => [{message2,vertex_c}],
        received => [{message1,vertex_b}],
        sent_control => [
          {chandy_lamport,marker,vertex_c},
          {chandy_lamport,marker,vertex_d}],
        received_control => [{chandy_lamport,marker,vertex_b}]},
      % 5. receive message2 after snapshot is done - not captured
      #{sent => [{message2,vertex_c}],
        received => [{message4,vertex_b}, {message1,vertex_b}],
        sent_control => [
          {chandy_lamport,marker,vertex_c},
          {chandy_lamport,marker,vertex_d}],
        received_control => [{chandy_lamport,marker,vertex_b}]}
    ]},
    {vertex_b, [
      % 1. send message1 before snapshot starts
      #{sent => [{message1,vertex_a}],
        received => [],
        sent_control => [],
        received_control => []},
      % 2. receive <marker> from vertex_a, send <marker> to vertex_c
      #{sent => [{message1,vertex_a}],
        received => [],
        sent_control => [{chandy_lamport,marker,vertex_a}],
        received_control => [{chandy_lamport,marker,vertex_c}]},
      % 3. send message2 after snapshot ends
      #{sent => [{message4,vertex_a}, {message1,vertex_a}],
        received => [],
        sent_control => [{chandy_lamport,marker,vertex_a}],
        received_control => [{chandy_lamport,marker,vertex_c}]}
    ]},
    {vertex_c, [
      % 1. receive <marker> from vertex_a, send <marker> to vertex_b
      #{sent => [],
        received => [],
        sent_control => [{chandy_lamport,marker,vertex_b}],
        received_control => [{chandy_lamport,marker,vertex_a}]},
      % 2. receive message2 from vertex_a, ignored in snapshot since we already received <marker> from vertex_a
      #{sent => [],
        received => [{message2,vertex_a}],
        sent_control => [{chandy_lamport,marker,vertex_b}],
        received_control => [{chandy_lamport,marker,vertex_a}]},
      % 3. receive message3, included in snapshot
      #{sent => [],
        received => [{message3,vertex_d}, {message2,vertex_a}],
        sent_control => [{chandy_lamport,marker,vertex_b}],
        received_control => [{chandy_lamport,marker,vertex_a}]},
      % 4. receive <marker> from vertex_d
      #{sent => [],
        received => [{message3,vertex_d}, {message2,vertex_a}],
        received_control => [{chandy_lamport,marker,vertex_d}, {chandy_lamport,marker,vertex_a}],
        sent_control => [{chandy_lamport,marker,vertex_b}]}
    ]},
    {vertex_d, [
      % 1. send message3 to vertex_c
      #{sent => [{message3,vertex_c}],
        received => [],
        sent_control => [],
        received_control => []},
      % 2. receive <marker> from vertex_c, the only incoming edge
      #{sent => [{message3,vertex_c}],
        received => [],
        sent_control => [{chandy_lamport,marker,vertex_c}],
        received_control => [{chandy_lamport,marker,vertex_a}]}
    ]}
  ]}
].

plugins() -> [ messenger, chandy_lamport ].

args() -> #{
  caffe_logging => [
    {messenger, plugin_only},
    {graph_state, quiet},
    {terminator, plugin_only}
  ]
}.
