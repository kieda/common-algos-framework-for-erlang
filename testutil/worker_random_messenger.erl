%%%-------------------------------------------------------------------
%%% @author zkieda
%%% @copyright (C) 2021, <COMPANY>
%%% @doc
%%%   Is a user defined function that simply sends a unique string message to a random outgoing channel
%%%   at a user-specified interval
%%% @end
%%% Created : 12. Apr 2021 10:27 PM
%%%-------------------------------------------------------------------
-module(worker_random_messenger).
-author("zkieda").

%% API
-export([new_state/1, update_state/2]).
-define(DEFAULT_MESSAGE_PROBABILITY, 0.25). % 25% chance of sending a message
-define(DEFAULT_WAIT_TIME, 2500). % 25 millis after sending a message

new_state(VertexArgs) ->
  Probability = maps:get(worker_random_message_probability, VertexArgs, ?DEFAULT_MESSAGE_PROBABILITY),
  WaitTime = maps:get(worker_random_message_wait_time, VertexArgs, ?DEFAULT_WAIT_TIME),
  #{unique_id => 0, message_probability => Probability, wait_time => WaitTime}.

next_id(#{unique_id := I}) -> I.
inc_id(State = #{unique_id := I}) ->
  maps:update(unique_id, I + 1, State).

update_state(State, UserState = #{message_probability := P, wait_time := T}) ->
  SendMessagesTo = lists:filter(fun(_) -> rand:uniform() =< P end, graph_state:get_outgoing(State)),
  lists:foldl(fun(V, {StateA, UserStateA}) ->
                Message = lists:concat([graph_state:get_vertex(StateA), next_id(UserStateA)]),
                StateB = messenger:send_message(Message, V, StateA),
                UserStateB = inc_id(UserStateA),
                timer:sleep(T),
                {StateB, UserStateB}
              end, {State, UserState}, SendMessagesTo).
