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
-define(MESSENGER_DEFAULT_RECEIVE_TIME, 50).

%% API

-export([send_message/3, send_control_message/4]).                 % use these to send user-based messages or control messages
-export([get_recently_received/1, get_recently_sent/1]).      % utility functions
-export([wait_message/1, wait_message/2, wait_messages/2]).
-export([dependencies/0, new_plugin/1, update_plugin/3, format/1]).     % implementation - should not be called directly.

-export_type([accepts/0]).

%% standard for plugins - list out the messages that this plugin listens to
-type control_type() :: atom().
-type accepts()
  :: {'send', caffe_graph:vertex(), any()}
   | {'receive', caffe_graph:vertex(), any()}
   | {'send_control', control_type(), caffe_graph:vertex(), any()}
   | {'receive_control', control_type(), caffe_graph:vertex(), any()}.

%% plugin state
-record(messenger_state, {
  sent,
  sent_control,
  received,
  received_control,
  receive_time
}).

%% public accessors

% user/basic messages
send_message(Msg, Vertex, State) ->
  caffe:process_event({'send', Vertex, Msg}, State).
receive_message(Msg, Vertex, State) ->
  caffe:process_event({'receive', Vertex, Msg}, State).

% control messages - we associate a type with each control message
send_control_message(Msg, Type, Vertex, State) ->
  caffe:process_event({'send_control', Type, Vertex, Msg}, State).
receive_control_message(Msg, Type, Vertex, State) ->
  caffe:process_event({'receive_control', Type, Vertex, Msg}, State).

get_recently_sent(State) ->
  #messenger_state{sent = S} = caffe:get_plugin_state(State, ?MODULE),
  cyclic_queue:all(S).
get_recently_received(State) ->
  #messenger_state{received = R} = caffe:get_plugin_state(State, ?MODULE),
  cyclic_queue:all(R).

%% implementation

format(#messenger_state{sent = S, received = R, sent_control = SC, received_control = RC}) ->
  #{sent             => cyclic_queue:all(S),
    received         => cyclic_queue:all(R),
    sent_control     => cyclic_queue:all(SC),
    received_control => cyclic_queue:all(RC)}.

dependencies() -> [
  graph_state
].

new_plugin(Args) ->
  SSize = maps:get(messenger_receive_logsize, Args, ?MESSENGER_DEFAULT_RECEIVE_LOGSIZE),
  RSize = maps:get(messenger_send_logsize, Args, ?MESSENGER_DEFAULT_SEND_LOGSIZE),
  RcvTime = maps:get(messenger_receive_time, Args, ?MESSENGER_DEFAULT_RECEIVE_TIME),
  #messenger_state{
    sent = cyclic_queue:new(SSize), % send
    received = cyclic_queue:new(RSize), % receive
    sent_control = cyclic_queue:new(SSize),
    received_control = cyclic_queue:new(RSize),
    receive_time = RcvTime
  }.

update_plugin({'receive', Vertex, Msg}, State, P = #messenger_state{received = R}) ->
  caffe:log(State, "receive <- ~w:~s", [Vertex, Msg]),
  {State, P#messenger_state{received = cyclic_queue:put({Msg, Vertex}, R)}};
update_plugin({'receive_control', Type, Vertex, Msg}, State, P = #messenger_state{received_control = RC}) ->
  caffe:log(State, "receive_control ~w <- ~w:~w", [Type, Vertex, Msg]),
  {State, P#messenger_state{received_control = cyclic_queue:put({Type, Msg, Vertex}, RC)}};
update_plugin({'send', Vertex, Msg}, State, P = #messenger_state{sent = S}) ->
  Sender = graph_state:get_vertex(State),
  PID = graph_state:get_outgoing(Vertex, State),
  caffe:log(State, "send ~s -> ~w", [Msg, Vertex]),
  PID ! {'basic', Sender, Msg}, % send basic message along outgoing PID
  {State, P#messenger_state{sent = cyclic_queue:put({Msg, Vertex}, S)}};
update_plugin({'send_control', Type, Vertex, Msg}, State, P = #messenger_state{sent_control = SC}) ->
  Sender = graph_state:get_vertex(State),
  PID = graph_state:get_outgoing(Vertex, State),
  caffe:log(State, "send_control ~w:~w -> ~w", [Type, Msg, Vertex]),
  PID ! {'control', Type, Sender, Msg}, % send control message along outgoing PID
  {State, P#messenger_state{sent_control = cyclic_queue:put({Type, Msg, Vertex}, SC)}};
update_plugin(_, _, _) -> ignore.

% wait for any incoming batched messages and process it
wait_message(State) ->
  P = caffe:get_plugin_state(?MODULE, State),
  receive Message -> process_message(Message, State)
  after P#messenger_state.receive_time -> State
  end.

% wait for a specific incoming message and process it
wait_message(State, Message) ->
  P = caffe:get_plugin_state(?MODULE, State),
  % todo see if we can reorder this into one receive & using guards
  case Message of
    {basic, Basic} ->
      receive {basic, Vertex, Basic} -> process_message({basic, Vertex, Basic}, State)
      after P#messenger_state.receive_time -> State
      end;
    {basic, Vertex, Basic} ->
      receive {basic, Vertex, Basic} -> process_message({basic, Vertex, Basic}, State)
      after P#messenger_state.receive_time -> State
      end;
    {control, Type} ->
      receive {control, Type, Vertex, Control} -> process_message({control, Type, Vertex, Control}, State)
      after P#messenger_state.receive_time -> State
      end;
    {control, Vertex, Type} ->
      receive {control, Type, Vertex, Control} -> process_message({control, Type, Vertex, Control}, State)
      after P#messenger_state.receive_time -> State
      end
  end.

% waits for all listed messages to be received
wait_messages(State, [Message|Messages]) ->
  wait_messages(wait_message(State, Message), Messages);
wait_messages(State, []) -> State.

% process a signal - we have two types - basic and control messages
% these produce different events via the messenger module
process_message({'basic', Vertex, Msg}, State) -> receive_message(Msg, Vertex, State);
process_message({'control', Type, Vertex, Msg}, State) -> receive_control_message(Msg, Type, Vertex, State);
process_message(Any, _) -> throw({'badmatch', Any}).