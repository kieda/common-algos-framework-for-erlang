%%%-------------------------------------------------------------------
%%% @author zkieda
%%% @copyright (C) 2021, <COMPANY>
%%% @doc
%%%   caffe system plugin for sending and receiving messages.
%%%
%%%   We have two distinct categories of messages - "basic" and "control".
%%%     - Basic messages are specified by the user, and are produced by the underlying user process.
%%%     - Control messages used to communicate information for a given distributed algorithm.
%%%   Since distributed algorithms may want to send additional information for a basic message, we don't want the same
%%%   triggers to occur when the additional information is sent.
%%%
%%%   For example, in the lamport clock algo we increment when a message is sent or received. When sending a message,
%%%   we also send the current local lamport clock time along the outgoing channel as part of the algorithm.
%%%   However, we do not want to increment and send the current lamport clock time as a result of
%%%   sending the current lamport clock time.
%%%   Thus, sending the current lamport clock time as part of the algorithm is categorized as a 'control' message, and the
%%%   send event that incremented the current time is categorized as a 'basic' message.
%%% @end
%%% Created : 12. Apr 2021 10:23 PM
%%%-------------------------------------------------------------------
-module(messenger).
-author("zkieda").

-define(MESSENGER_DEFAULT_RECEIVE_LOGSIZE, 10).
-define(MESSENGER_DEFAULT_SEND_LOGSIZE, 10).

%% API

-export([send_message/3, receive_message/2]).                 % use these to send/receive user-based messages
-export([send_control_message/4, receive_control_message/3]). % use these to send/receive control messages
-export([get_recently_received/1, get_recently_sent/1]).      % utility functions
-export([dependencies/0, new_plugin/2, update_plugin/3, format/1]).     % implementation - should not be called directly.

-export_type([accepts/0]).

%% standard for plugins - list out the messages that this plugin listens to
-type control_type() :: atom().
-type accepts()
  :: {'send', caffe_graph:vertex(), any()}
   | {'receive', any()}
   | {'send_control', control_type(), caffe_graph:vertex(), any()}
   | {'receive_control', control_type(), any()}.

%% plugin state
-record(messenger_state, {
  sent,
  received
}).

%% public accessors

% user/basic messages
send_message(Msg, Vertex, State) ->
  caffe:process_order({'send', Vertex, Msg}, State).
receive_message(Msg, State) ->
  caffe:process_order({'receive', Msg}, State).

% control messages - we associate a type with each control message
send_control_message(Msg, Type, Vertex, State) ->
  caffe:process_order({'send_control', Type, Vertex, Msg}, State).
receive_control_message(Msg, Type, State) ->
  caffe:process_order({'receive_control', Type, Msg}, State).

get_recently_sent(State) ->
  #messenger_state{sent = S} = caffe:get_plugin_state(State, ?MODULE),
  cyclic_queue:all(S).
get_recently_received(State) ->
  #messenger_state{received = R} = caffe:get_plugin_state(State, ?MODULE),
  cyclic_queue:all(R).

%% implementation

format(#messenger_state{sent = S, received = R}) ->
  {sent, cyclic_queue:all(S), received, cyclic_queue:all(R)}.

dependencies() -> [
  graph_state
].

new_plugin(Args, _) ->
  SSize = maps:get(messenger_receive_logsize, Args, ?MESSENGER_DEFAULT_RECEIVE_LOGSIZE),
  RSize = maps:get(messenger_send_logsize, Args, ?MESSENGER_DEFAULT_SEND_LOGSIZE),
  #messenger_state{
    sent = cyclic_queue:new(SSize), % send
    received = cyclic_queue:new(RSize) % receive
  }.

update_plugin({'receive', Msg}, State, P = #messenger_state{received = R}) ->
  caffe:log(State, "receive <- ~s", [Msg]),
  {State, P#messenger_state{received = cyclic_queue:put(Msg, R)}};
update_plugin({'receive_control', Type, Msg}, State, P) ->
  caffe:log(State, "receive_control ~w <- ~w", [Type, Msg]),
  {State, P};
update_plugin({'send', Vertex, Msg}, State, P = #messenger_state{sent = S}) ->
  PID = graph_state:get_outgoing(Vertex, State),
  caffe:log(State, "send ~s -> ~w", [Msg, Vertex]),
  PID ! {'basic', Msg}, % send basic message along outgoing PID
  {State, P#messenger_state{sent = cyclic_queue:put(Msg, S)}};
update_plugin({'send_control', Type, Vertex, Msg}, State, P) ->
  PID = graph_state:get_outgoing(Vertex, State),
  caffe:log(State, "send_control ~w:~w -> ~w", [Type, Msg, Vertex]),
  PID ! {'control', Type, Msg}, % send control message along outgoing PID
  {State, P};
update_plugin(_, _, _) -> ignore.
