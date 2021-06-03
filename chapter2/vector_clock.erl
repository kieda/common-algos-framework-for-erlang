%%%-------------------------------------------------------------------
%%% @author zkieda
%%% @doc
%%%   Vector clock implementation
%%% @end
%%% Created : 21. Mar 2021 8:25 PM
%%%-------------------------------------------------------------------
-module(vector_clock).
-author("zkieda").

-export([dependencies/0, new_plugin/1, update_plugin/3]).
-export([get_time/1, compare/2, less_equal/2]).
-export([format/1]).

-export_type([vector_clock/0]).

%% State

% maps vertex to current (known) clock
-type vector_clock() :: #{
  caffe_graph:vertex() => non_neg_integer()
}.

%% Implementation

dependencies() -> [
  graph_state,
  messenger
].

format(V) -> V.

-spec new_plugin(caffe_graph:vertex_args(any())) -> vector_clock().
new_plugin(#{ graph := {V, _} }) ->
  maps:from_list(lists:map(fun(Vertex) -> {Vertex, 0} end, V)).

% receive event : copy over external vector clock and increment local
update_plugin({'receive_control', 'vector_clock', VReceive}, State, VNow) ->
  V = graph_state:get_vertex(State),
  VCopied = maps:map(fun(Vertex, VTime) -> max(VTime, maps:get(Vertex, VReceive)) end, VNow),
  TNew = maps:get(V, VCopied) + 1,
  VNew = maps:put(V, TNew, VCopied),
  caffe:log(State, "receive_control : vector_clock ~s <- ~b, new = ~p", [V, TNew, VNew]),
  {State, VNew};
% internal event : increment vector clock on our local vertex
update_plugin({'internal', _}, State, VNow) ->
  V = graph_state:get_vertex(State),
  TNew = maps:get(V, VNow) + 1,
  VNew = maps:put(V, TNew, VNow),
  caffe:log(State, "internal : vector_clock ~s <- ~b, new = ~p", [V, TNew, VNew]),
  {State, VNew};
update_plugin({'send', Vertex, _}, State0, VNow) ->
  V = graph_state:get_vertex(State0),
  TNew = maps:get(V, VNow) + 1,
  VNew = maps:put(V, TNew, VNow),
  caffe:log(State0, "send : vector_clock ~s <- ~b, new vector_clock ~p -> ~s", [V, TNew, VNew, Vertex]),
  State1 = messenger:send_control_message(VNew, 'vector_clock', Vertex, State0),
  {State1, VNew};
update_plugin(_, _, _) -> ignore.

%% Accessors

get_time(State) -> caffe:get_plugin_state(?MODULE, State).

compare(VClockA, VClockB) ->
  [KeysA, KeysB] = [lists:sort(maps:keys(X)) || X <- [VClockA, VClockB]],
  if KeysA /= KeysB -> throw({badcompare, KeysA, KeysB});
    true -> ok
  end.

% returns true if VClockA <= VClockB, useful for lists:sort
less_equal(VClockA, VClockB) -> compare(VClockA, VClockB) =< 0.