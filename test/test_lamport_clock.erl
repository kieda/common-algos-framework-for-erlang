%%%-------------------------------------------------------------------
%%% @author zkieda
%%% @copyright (C) 2021, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 16. Apr 2021 11:10 PM
%%%-------------------------------------------------------------------
-module(test_lamport_clock).
-author("zkieda").

%%
-export([graph/0, events/0, tests/0, plugins/0, args/0]).

graph() -> {
  [vertex_a, vertex_b, vertex_c],
  [{vertex_a, vertex_c}, {vertex_b, vertex_a}, {vertex_c, vertex_b}]
}.

% scripted events to occur. These are processed in-order
% 'receive' and 'receive_control' events are special, where we wait until
% the event is generated externally rather than produced internally.
events() -> [
  { vertex_a, [
    {'internal', internal_a1},
    {'send', vertex_c, message_a1},
    {'receive', message_b1},
    {'internal', internal_a2}
  ]},
  { vertex_b, [
    {'internal', internal_b1},
    {'receive', message_c1},
    {'send', vertex_a, message_b1}
  ]},
  { vertex_c, [
    {'receive', message_a1},
    {'internal', internal_c1},
    {'send', vertex_b, message_c1},
    {'internal', internal_c2}
  ]}
].

% specify values we expect the plugins to be after each event
tests() -> [
  { lamport_clock, [
    {vertex_a, [ 1, 2, 8, 9 ]},
    {vertex_b, [ 1, 6, 7 ]},
    {vertex_c, [ 3, 4, 5, 6 ]}
  ]},
  { vector_clock, [
    {vertex_a, caffe_util:table([
      [vertex_a, vertex_b, vertex_c],
      [1, 0, 0],
      [2, 0, 0],
      [3, 3, 3],
      [4, 3, 3]
    ])},
    {vertex_b, caffe_util:table([
      [vertex_a, vertex_b, vertex_c],
      [0, 1, 0],
      [2, 2, 3],
      [2, 3, 3]
    ])},
    {vertex_c, caffe_util:table([
      [vertex_a, vertex_b, vertex_c],
      [2, 0, 1],
      [2, 0, 2],
      [2, 0, 3],
      [2, 0, 4]
    ])}
  ]}
].

plugins() -> [ lamport_clock, vector_clock ].

args() -> #{
  caffe_logging => [
    {messenger, plugin_only},
    {graph_state, quiet},
    {terminator, plugin_only}
  ]
}.