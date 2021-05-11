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

%% API
-export([test1/0]).

test1() ->
  G = {V, _ } = caffe_graph:load(graph1),
  Spec = caffe_new:new_spec([
    { V, [lamport_clock], worker_random_messenger }
  ]),
  caffe_graph:start_network(caffe_new:build_network(G, Spec)).