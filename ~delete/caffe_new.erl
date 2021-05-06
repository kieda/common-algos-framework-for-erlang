%%%-------------------------------------------------------------------
%%% @author zkieda
%%% @copyright (C) 2021, <COMPANY>
%%% @doc
%%%   Used to build a graph of caffe instances, using the default barista implementation
%%%   todo : merge into barista
%%% @end
%%% Created : 12. Apr 2021 9:26 PM
%%%-------------------------------------------------------------------
-module(caffe_new).
-author("zkieda").

%% API
-export([new_spec/1, build_network/2]).
-export_type([barista_spec/1, barista_spec_map/1, barista_spec_list/1]).

get_default_plugins() -> [
  graph_state,      % keeps current known state of the graph/network
  plugin_manager,   % manages plugins - adds/removes them
  terminator,       % allows for client termination of caffe process
  messenger         % hooks for sending and receiving messages
].

%
% Deployment!
%   todo : make this the main point of entry, clean up above code, removing intermediate datatype transformations when necessary.
%   G = {V, E} = caffe_graph:load(graph1),
%   build_barista_network( G,
%     new_barista_spec_map( [ { V, { [ chandylamport ], worker_random_messenger } } ] )

% specifications we use to define a barista
-type barista_spec(UserState) :: {
  [barista:plugin_id()], % user-defined plugins/algos
  barista:user_func(UserState),   % user-defined function
  #{ atom() => any() }   % user-specified arguments
} | {
  [barista:plugin_id()], % user-defined plugins/algos
  barista:user_func(UserState)    % user-defined function
  % no arguments added
}.

% map from the vertex to the spec
-type barista_spec_map(UserState) :: #{ caffe_graph:vertex() => barista_spec(UserState) }.
-type barista_spec_list(UserState) :: [ { caffe_graph:vertex_list(), barista_spec(UserState) }].

-spec new_spec(barista_spec_list(UserState)) -> barista_spec_map(UserState).
new_spec(BaristaSpecList) ->
  maps:from_list(
    lists:flatmap(
      fun(VertexList, BaristaSpec)
        -> lists:map(fun(Vertex) -> {Vertex, BaristaSpec} end, VertexList)
      end, BaristaSpecList)
  ).

new_args(Plugins, Func, Args) ->
  maps:merge(Args,
    #{
      caffe_module_impl => barista,
      barista_user_func => Func,
      barista_plugin_list => Plugins ++ get_default_plugins()
    }).

-spec build_network(caffe_graph:graph(), barista_spec_map(UserState::any())) -> caffe_graph:network().
build_network(G = {V, _}, BaristaSpecs) ->
  % all vertices run the caffe:open_caffe routine
  SpawnVertexMap = caffe_graph:new_vertex_map([ { V, { named, caffe, open_caffe } } ]),
  VertexArgs = maps:map(
    fun(_, {Plugins, Func, Args}) -> new_args(Plugins, Func, Args);
    (_, {Plugins, Func}) -> new_args(Plugins, Func, maps:new())
    end, BaristaSpecs),
  caffe_graph:build_network(G, SpawnVertexMap, VertexArgs).