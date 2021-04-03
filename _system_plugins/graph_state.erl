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
-export([]).
