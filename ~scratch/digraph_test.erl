%%%-------------------------------------------------------------------
%%% @author zkieda
%%% @copyright (C) 2021, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 23. Mar 2021 8:13 AM
%%%-------------------------------------------------------------------
-module(digraph_test).
-author("zkieda").

%% API
-export([make_digraph_from_graph/1]).

make_digraph_from_graph({V, E}) ->
  DiGraph = digraph:new(),
  LabelToVertex = maps:from_list(lists:map(fun(Label) -> {Label, digraph:add_vertex(DiGraph, Label)} end, V)),
  LabelToEdge = maps:from_list(lists:map(
    fun(Edge = {LabelA, LabelB}) -> {Edge, digraph:add_edge(DiGraph, maps:get(LabelA, LabelToVertex), maps:get(LabelB, LabelToVertex), Edge)} end,
    E
  )),

 DiGraph.

%make_generating_function()
