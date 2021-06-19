%%%-------------------------------------------------------------------
%%% @author zkieda
%%% @copyright (C) 2021, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 02. Apr 2021 12:17 AM
%%%-------------------------------------------------------------------
-module(graph_state).
-author("zkieda").


%%%  Graph Update Functions
%%%   Given an existing generated and running network graph, provide functionality to do the
%%%   following:
%%%   * Add or remove edges from the network graph
%%%   * Add or remove vertices from the network graph
%%%   * Extend current network graph
%%%   All relevant data we track is updated as a result of any graph modification.
%%%
%%%   Note that all modifications might not be possible depending on the vertex we're running on. This is because a vertex
%%%   can only message its outgoing vertices.
%%%

%% API
-export([new_plugin/1, invariant/2, update_plugin/2]).
-export([get_outgoing/1, get_incoming/1, get_outgoing/2, get_vertex/1]).

-record(graph_state, {
  outgoing,
  graph,
  vertex
}).

new_plugin(#{outgoing := Outgoing, graph := Graph, vertex := Vertex}) -> #graph_state{
  vertex = Vertex,
  outgoing = Outgoing,
  graph = Graph
}.

% currently ignore. Todo : update the graph when vertices/edges are added/removed from the graph.
update_plugin(_, _) -> ignore.

invariant(#graph_state{vertex = V0}, #graph_state{vertex = V1}) ->
  if V0 == V1 -> ok;
    true -> {broken, lists:concat(["Vertex changed! Before = ", V0, ", After = ", V1])}
  end.

% gets our current vertex
get_vertex(State) ->
  #graph_state{vertex = V} = caffe:get_plugin_state(?MODULE, State),
  V.

% lists all outgoing vertices for this vertex
get_outgoing(State) ->
  #graph_state{outgoing = O} = caffe:get_plugin_state(?MODULE, State),
  maps:keys(O).

% gets outgoing PID for a specified outgoing vertex
get_outgoing(Vertex, State) ->
  #graph_state{outgoing = O} = caffe:get_plugin_state(?MODULE, State),
  maps:get(Vertex, O).

% lists all incoming vertices for this vertex
get_incoming(State) ->
  #graph_state{vertex = V, graph = {_, E}} = caffe:get_plugin_state(?MODULE, State),
  lists:filtermap(
    fun({A, B}) when V == B -> {true, A};
       (_) -> false
    end, E).
%add_vertex(Vertex, PID, State) -> State
%remove_vertex(Vertex, State) -> State