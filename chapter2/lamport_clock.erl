%%%-------------------------------------------------------------------
%%% @author zkieda
%%% @doc
%%%   Basic lamport clock implementation
%%% @end
%%% Created : 21. Mar 2021 8:25 PM
%%%-------------------------------------------------------------------
-module(lamport_clock).
-author("zkieda").

-export([get_time/1]).                                    % returns time for given state
-export([dependencies/0, new_plugin/1, update_plugin/3]). % implementation

% State
-record(lamport_clock, {
  c_time = 0
}).


% Implementation

dependencies() -> [
  messenger
].

new_plugin(_) -> #lamport_clock{}.

% we receive a control message on lamport_clock
update_plugin({'receive_control', 'lamport_clock', TReceive}, State, C0 = #lamport_clock{c_time = TInternal}) ->
  C1 = C0#lamport_clock{c_time = max(TReceive, TInternal) + 1},
  caffe:log(State, "receive_control lamport_clock <- ~b, new = ~b", [TReceive, C1#lamport_clock.c_time]),
  {State, C1};
% an internal event has occurred
update_plugin({'internal', _}, State, C0 = #lamport_clock{c_time = T}) ->
  C1 = C0#lamport_clock{c_time = T + 1},
  caffe:log(State, "internal : new lamport_clock = ~b", [C1#lamport_clock.c_time]),
  {State, C1};
% a send event has occurred
update_plugin({'send', Vertex, _}, State0, C0 = #lamport_clock{c_time = T}) ->
  C1 = C0#lamport_clock{c_time = T + 1},
  caffe:log(State0, "send new lamport_clock = ~b -> ~s", [C1#lamport_clock.c_time, Vertex]),
  State1 = messenger:send_control_message(C1#lamport_clock.c_time, 'lamport_clock', Vertex, State0),
  {State1, C1};
update_plugin(_, _, _) -> ignore.

get_time(State) ->
  #lamport_clock{c_time = T} = caffe:get_plugin_state(?MODULE, State),
  T.