%%%-------------------------------------------------------------------
%%% @author zkieda
%%% @doc First draft of generic library to deploy distributed algorithms.
%%%  Core concept - user specifies a graph {V, E} and a function to run for each V.
%%%  Connections are made along E.
%%%
%%%  Outgoing vertices (as PIDs) are sent to each spawned process
%%%  for its outgoing edges in E when the graph is started.
%%%
%%%  User can specify additional parameters to be passed to each vertex as #{ atom() => any() }.
%%%  The following system keys are merged into this map :
%%%  * graph - the network graph
%%%  * vertex - this vertex
%%%  * outgoing - map of outgoing channels, vertex/PID pairs
%%%  * outgoing_vertices - list of outgoing vertices
%%%  * spawn_vertex_fun - function spec for process running on vertex
%%%
%%%  General use
%%%  1. Use helper functions to build graph definition
%%%  2. Build the network via build_network.
%%%     This spawns a process for each vertex, and we collect all the PIDs.
%%%  3. Start the network via start_network
%%%
%%% Limitations :
%%%   This thing builds the entire graph in one go, so we scale linearly in space
%%%   with number of vertices we have and their arguments, and all of them are spawned and
%%%   stored in one go. This might not be suitable for gigantic graphs of processes.
%%%
%%%   Perhaps do some galaxy brain shit where we generate more graph from an existing graph and
%%%   update the existing one
%%%
%%%   todo: (late-term) look at erlang Map r/w performance & space usage. What is their storage complexity?
%%%   todo:             Read and write are O(log(n)), but how does it stack up to real world access times?
%%%   todo:             Nesting maps? Medium brain idea: make library for flattened map where we access it via a list of keys
%%% @end
%%% Created : 21. Mar 2021 8:18 PM
%%%-------------------------------------------------------------------
-module(caffe_graph).
-author("zkieda").

%% Default Values %%
-define(DEFAULT_START_SIGNAL_TIMEOUT, 2500). % default value for start_signal_timeout

%% Exports %%
-export([new_vertex_map/1]).                                                        % 1. building args for 2
-export([build_network/3, start_network/1, build_and_start_network/3]).             % 2. build to spawn a process for each vertex V
%                                                                                        start to run user specified vertex_fun for each vertex V, and make connections for each edge E
-export([remove_graph_duplicates/1, make_bidirectional/1]).                         % 3. helper functions
-export([run_vertex_function_delegate/2]).                                          % 4. used internally to run user-specified function
-export([load/1]).                                                                  % 5. load a graph via an implementing module

-export_type([graph/0, network/1]).                                                 % 1. graph {V,E} specification,
%                                                                                        representation of running Graph
-export_type([spawn_vertex_list/1, spawn_vertex_map/1]).                            % 2. spawn network using a list,
%                                                                                        spawn network using a map
-export_type([vertex_args/1, vertex_args_map/1, vertex_args_list/1]).               % 3. arguments specified for each function

%% Types %%

% Types - Graphs
-type vertex() :: any().
-type vertex_list() :: [vertex()].
-type edge_list() :: [edge()].
-type edge() :: {vertex(), vertex()}.
-type graph() :: {vertex_list(), edge_list()}.

% Types - Vertex - Parameters
-type vertex_args(Val) :: #{
  %% sent to process as args, overwrites any client value %%
  graph         := graph(),                     % the network graph
  vertex        := vertex(),                    % this vertex
  outgoing      => #{vertex() => identifier()}, % known outgoing edges with respective identifier/PID
  outgoing_vertices := vertex_list(),           % list of outgoing vertices on network
  spawn_vertex_fun  := vertex_fun(Val),         % function def used spawn this node

  %% user-specified %%
  start_signal_timeout   => integer(),          % timeout to receive initial start signal for a node
  atom()                 => any()               % additional process-specific args provided by client
}.

-type vertex_args_map(Val) :: #{ vertex() => vertex_args(Val) }.
-type vertex_args_list(Val) :: [ { vertex_list(), vertex_args(Val) }].

% Types - Vertex - Function
-type vertex_fun(Val) :: { anonymous, fun((vertex_args(Val)) -> Val)}
                       | { named, module(), atom() }.

% Types - Vertex - Many
-type spawn_vertex_map(Val) :: #{ vertex() => vertex_fun(Val) }.        % specifies function that should run on each vertex
-type spawn_vertex_list(Val) :: [ { vertex_list(), vertex_fun(Val) } ]. % specifies a subset of vertices and a common function that should run on each vertex

% Types - Network
-type network(Val) :: { #{ vertex() => { identifier(), vertex_args(Val) } }, #{ vertex() => #{ vertex() => identifier() } } }.

%%
%% Functions - defining a network. Split into defining vertex spawn functions and defining vertex args %%

%% Public builder function - map specifies vertex and function that should be run on it
-spec new_vertex_map(spawn_vertex_list(Val)|vertex_args_list(Val)) -> spawn_vertex_map(Val)|vertex_args_map(Val).
new_vertex_map(ListFormat) ->
  maps:from_list(
    lists:flatmap(
      fun({VertexList, Info})
        -> lists:map(fun(Vertex) -> {Vertex, Info} end, VertexList)
      end, ListFormat)
  ).

%%
%% Functions - initializing, deploying network graph %%

%% Builds and starts network
-spec build_and_start_network(graph(), spawn_vertex_map(Val), vertex_args_map(Val)) -> network(Val).
build_and_start_network(G, SpawnVertexMap, VertexArgsMap) ->
  start_network(build_network(G, SpawnVertexMap, VertexArgsMap)).

%% Build the network to construct all of the vertices, spawning a process for each vertex in the graph.
%%
%% Here, you specify the function and the args you're going to pass for each vertex.
%%
%% This merely constructs each vertex of the network. Start the network and your specified functions/args will run on
%% each respective vertex.
-spec build_network(graph(), spawn_vertex_map(Val), vertex_args_map(Val)) -> network(Val).
build_network(G = {V, _}, SpawnVertexMap, VertexArgsMap) ->
  % todo check inputs on SpawnVertexMap, SpawnVertexArgs to make sure they have respective entries in V
  OutgoingMap = build_network_map(G),

  %% BuiltVertexMap :: #{Vertex -> {PID, SpawnVertexArgs'}}
  %% spawn a process for each network, gather the Args passed in and keep around the SpawnProcessDefinition
  SpawnedVertices = lists:map(fun(Vertex) ->
      SpawnVertexFunctionSpec = maps:get(Vertex, SpawnVertexMap),

      InitialArgs = #{
        graph             => G,
        vertex            => Vertex,
        outgoing_vertices => maps:get(Vertex, OutgoingMap),
        spawn_vertex_fun  => SpawnVertexFunctionSpec
      },
      MergedArgs = maps:merge(InitialArgs, maps:get(Vertex, VertexArgsMap)),
      {spawn_vertex(SpawnVertexFunctionSpec, MergedArgs), MergedArgs}
    end, V),
  BuiltNetworkMap = maps:from_list(lists:zip(V, SpawnedVertices)),
  %% OutgoingIdentifiers :: #{ Vertex -> #{OutgoingVertex -> OutgoingPID} }
  %% Gathers outgoing process identifiers for each vertex
  OutgoingIdentifiers = maps:map(fun(_, OutgoingVertices) ->
    % map all OutgoingVertices to their respective Identifier
    maps:from_list(lists:map(fun(OutgoingVertex) ->
      {OutgoingIdentifier, _} = maps:get(OutgoingVertex, BuiltNetworkMap),
      {OutgoingVertex, OutgoingIdentifier}
    end, OutgoingVertices))
  end, OutgoingMap),

  %% Network :: { #{Vertex -> {PID, VertexArgs}}, #{Vertex -> #{OutgoingVertex -> PID}} }
  {BuiltNetworkMap, OutgoingIdentifiers}.

%% starts this network - all of the vertices have been spawned, so we have all their PIDs. We pass the outgoing PIDs
%% to each vertex, which is merged into the vertices' args map. The vertex then boots by running its internal function,
%% with the args map as the parameter.
%%
%% following atom is merged in both on remote vertex and return value:
%% * outgoing - map from Vertex to list of PIDs that this process, each Vertex/PID pair representing an Edge on the original graph.
%%              A vertex can then use these outgoing PIDs to communicate with its neighbors in the graph.
-spec start_network(network(Val)) -> network(Val).
start_network({BuiltNetworkMap, OutgoingIdentifiers}) ->
  %% note -- outgoing :: #{ Vertex -> [Identifier] }, where identifier list is outgoing connections for the network

  {maps:map(fun(Vertex, {Identifier, Args}) ->
      StartMessageArgs = #{
        outgoing => maps:get(Vertex, OutgoingIdentifiers)
      },
      Identifier ! {self(), start, StartMessageArgs},
      {Identifier, maps:merge(StartMessageArgs, Args)}
    end, BuiltNetworkMap), OutgoingIdentifiers}.

%% Function used to run the delegate. Any additional args received from the start signal are
%% merged into the original args. This args map is passed to the delegate as its only argument
%%
%% Outgoing edges are connected and delegate is run on start signal
%% Exits when it receives a cancel signal or after a specified start_signal_timeout millis
-spec run_vertex_function_delegate(vertex_args(Val), vertex_fun(Val)) -> Val | no_return().
run_vertex_function_delegate(ArgsMap, SpawnVertexFunctionDelegate) ->
  io:fwrite("Spawning vertex ~p\n", [maps:get(vertex, ArgsMap)]),
  %% wrap network process so we can send args to it after the graph has been built completely.
  %% done so we can send PIDs of other processes to the node after all PIDs have been created.
  InitTimeout = maps:get(start_signal_timeout, ArgsMap, ?DEFAULT_START_SIGNAL_TIMEOUT),
  receive
    {From, start, SentMapArgs} ->
      ArgsNew = maps:merge(SentMapArgs, ArgsMap),
      io:fwrite("Got start signal from ~w\nArgs =\n~p\n", [From, ArgsNew]),
      caffe_util:apply_function_spec(SpawnVertexFunctionDelegate, [ArgsNew]);
    {From, cancel} ->
      io:fwrite("Vertex construction cancelled, signal from ~w\n", [From]),
      exit(cancelled)
  after InitTimeout ->
    io:fwrite("Timeout after initialization"),
    exit(timeout)
  end.

%% Spawns a vertex, returns its PID
-spec spawn_vertex(vertex_fun(Val), vertex_args(Val)) -> identifier().
spawn_vertex(SpawnVertexFunctionSpec, SpawnVertexArgs) ->
  % the function we spawn wraps the user-specified function for the vertex,
  % which is connected to its outgoing edges when it starts
  spawn(?MODULE, run_vertex_function_delegate, [SpawnVertexArgs, SpawnVertexFunctionSpec]).


%%
%% Functions - internal invariants and parameter checks

%% returns 'ok' if checks succeeded
%% throws ( [DuplicateTypes], [DuplicateItems], ErrorMessage: String )
-spec check_graph_duplicates(graph()) -> ok | no_return().
check_graph_duplicates({V, E}) ->
  USortV = lists:usort(V),
  USortE = lists:usort(E),
  [VSmall, VBig, ESmall, EBig] = [length(X) || X <- [USortV, V, USortE, E]],
  case {VSmall, VBig, ESmall, EBig} of
    {VSize, VSize, ESize, ESize} -> ok;
    {VSize, VSize, Small, Big} -> throw({[duplicate_edges], [E -- USortE], lists:concat([Big - Small, " duplicate Edges!"])});
    {Small, Big, ESmall, EBig} -> throw({[duplicate_vertices], [V -- USortV], lists:concat([Big - Small, " duplicate Vertices!"])});
    {Small1, Big1, Small2, Big2} -> throw({[duplicate_vertices, duplicate_edges], [V -- USortV, E -- USortE],
        lists:concat([Big1 - Small1, " duplicate Vertices, and ", (Big2 - Small2), " duplicate Edges!"])})
  end.

%% checks that all edges in E have a corresponding entry in V
-spec check_vertices_exist(graph()) -> ok | no_return().
check_vertices_exist({V, E}) ->
  VMap = lists:foldl(fun(X, Map) -> maps:put(X, true, Map) end, maps:new(), V),
  BadEdges = [
    {if AMissing and BMissing -> [non_existent_A, non_existent_B];
       AMissing and not BMissing -> [non_existent_A];
       BMissing and not AMissing -> [non_existent_B];
       true -> []
    end, {A, B}} ||
    {A, B} <- E, (AMissing = not maps:is_key(A, VMap)) or (BMissing = not maps:is_key(B, VMap))
  ],
  case length(BadEdges) of
    0 -> ok;
    BadEdgeSize -> throw({[non_existent_vertices], BadEdges, lists:concat([BadEdgeSize, " non-existent vertices!"])})
  end.

% Functions - public utility
-spec remove_graph_duplicates(graph()) -> graph().
remove_graph_duplicates({V, E}) ->
  {lists:usort(V), lists:usort(E)}.

-spec make_bidirectional(graph()) -> graph().
make_bidirectional(G = {V, E}) ->
  ok = check_graph_duplicates(G),
  ok = check_vertices_exist(G),

  EUnique = lists:usort(E),
  ERev = lists:map(fun({V1, V2}) -> {V2, V1} end, EUnique),
  {V, EUnique ++ ERev}.

-spec load(module()) -> graph() | {error, unimplemented, _}.
load(GraphName) ->
  Functions2Implement = [{vertices, 0}, {edges, 0}],
  Exported = lists:map(fun(I = {FName, Arity}) -> {I, caffe_util:is_exported(GraphName, FName, Arity)} end, Functions2Implement),
  Unimplemented = lists:filter(fun({_, E}) -> not E end, Exported),
  case Unimplemented of
    [] -> {apply(GraphName, vertices, []), apply(GraphName, edges, [])};
    _ -> {error, unimplemented, lists:map(fun({I, _}) -> I end, Unimplemented)}
  end.

%% Functions - internal utility

-spec build_network_map(graph()) -> #{vertex() => [vertex()]}.
build_network_map(G = {V, E}) ->
  ok = check_graph_duplicates(G),
  ok = check_vertices_exist(G),

  % Have Vertex -> [].
  BaseVerticesMap = lists:foldl(fun(X, Map) -> maps:put(X, [], Map) end, maps:new(), V),
  % Add Edges to map
  lists:foldl(fun({A, B}, Map) -> maps:update_with(A, fun(Acc) -> [B|Acc] end, Map) end, BaseVerticesMap, E).
