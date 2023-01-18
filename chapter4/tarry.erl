%%%-------------------------------------------------------------------
%%% @author zkieda
%%% @copyright (C) 2021, <COMPANY>
%%% @doc
%%%   1. never forward the token to the same channel twice
%%%   2. only forward token to parent if there is no option
%%%   3. gather parent/child relationship (tree nodes) and frond (rest)
%%% @end
%%% Created : 15. Aug 2021 12:04 AM
%%%-------------------------------------------------------------------
-module(tarry).
-author("zkieda").

%% API
%% public accessors
-export([tarry/3]).
%% implementation
-export([dependencies/0, new_plugin/1, update_plugin/3]).

-record(tarry, {
  % vertices we have sent the token to
  vertices_sent = sets:new(),
  parent = none
  % transition function: TokenState -> TokenState
%  transition,
  % Token = {?Module, TokenState}
 % token,
  % decide function for initiator
  %decide
}).

-type tarry() :: #tarry{
  vertices_sent :: sets:set(),
  parent :: {some, caffe_graph:vertex()} | none
}.

dependencies() -> [
  graph_state,
  messenger
].

-spec new_plugin(caffe_graph:vertex_args(caffe:caffe_state(any()))) -> tarry().
new_plugin(_) ->
  #tarry{}.


%% todo - token = user token and system token -> just system token
%% todo - receive_control -> receive_wave_token, tarry, ...
update_plugin({internal, tarry, WaveName}, State, P) ->
  
  ;
% tokeninfo = {WaveName, Initiator, Count}
update_plugin({receive_control, tarry, VertexIn, {TokenInfo = {_, Initiator, _}, UserToken}}, State0, P) ->
  case graph_state:get_vertex(State0) of
    Initiator ->
      % if the current vertex is the initiator and we receive a token, we decide
      caffe:process_event({decide, tarry, {TokenInfo, UserToken}}, State0);
    _ ->
      P1 = case P#tarry.parent of
             {some, _} -> P;
             none -> P#tarry{parent = VertexIn} % set parent on our received vertex
           end,
      Parent = P1#tarry.parent,
      % the remaining elements we have not sent to yet, excluding the parent
      Remaining = sets:del_element(Parent, sets:subtract(sets:from_list(graph_state:get_outgoing(State0)), P1#tarry.vertices_sent)),
      case sets:is_empty(Remaining) of
        true -> % forward to the parent if there are none remaining
          State1 = messenger:send_control_message({TokenInfo, UserToken}, tarry, Parent, State0),
          {State1, P1};
        false ->
          Forward = hd(sets:from_list(Remaining)),
          % send message to Forward
          State1 = messenger:send_control_message({TokenInfo, UserToken}, tarry, Forward, State0),
          % add Forward to the set of vertices sent
          P2 = P1#tarry{ vertices_sent = sets:sets:add_element(Forward, P1#tarry.vertices_sent) },
          {State1, P2}
      end
  end;
update_plugin({decide, tarry, {TokenInfo, UserToken}}, State0, P) ->
  % todo - do something here where we decide
  {};
update_plugin(_, _, _) -> ignore.

% designates this vertex as an initiator
tarry(State, Initial, Transition) ->
  caffe:process_event({internal, tarry}, State).