%%%-------------------------------------------------------------------
%%% @author zkieda
%%% @copyright (C) 2021, <COMPANY>
%%% @doc
%%%   basic test graph
%%% @end
%%% Created : 12. Apr 2021 10:26 PM
%%%-------------------------------------------------------------------
-module(graph1).
-author("zkieda").

%% API
-export([vertices/0, edges/0]).

vertices() -> [
  vertex_a,
  vertex_b
%  vertex_c,
%  vertex_d,
%  vertex_e,
%  vertex_f,
%  vertex_g
].

edges() -> [
  {vertex_a, vertex_b}
%  {vertex_a, vertex_d},
%  {vertex_b, vertex_d},
%  {vertex_b, vertex_c},
%  {vertex_c, vertex_a},
%  {vertex_f, vertex_c},
%  {vertex_g, vertex_d},
%  {vertex_d, vertex_a}
].
