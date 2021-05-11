%%%-------------------------------------------------------------------
%%% @author zkieda
%%% @copyright (C) 2021, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 08. May 2021 2:16 PM
%%%-------------------------------------------------------------------
-module('prettypr-test').
-author("zkieda").

%% API
-export([]).


G = {V, E} = caffe_graph:load(graph1), caffe:start( caffe:build( G, caffe:args_from_list( [ { V, { [ lamport_clock ], worker_random_messenger, #{ caffe_logging => [{graph_state, quiet}, {messenger, plugin_only}, {terminator, quiet}] } } } ] ) ) ).