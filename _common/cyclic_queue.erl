%%%-------------------------------------------------------------------
%%% @author zkieda
%%% @copyright (C) 2021, <COMPANY>
%%% @doc
%%%   very simplistic cyclic array implementation. Fixed size and backed by an array.
%%%   Adding new elements beyond the max size will override existing values
%%% @end
%%% Created : 01. May 2021 4:14 PM
%%%-------------------------------------------------------------------
-module(cyclic_queue).

%% API
-export([new/1, put/2, max_size/1, poll/1, poll/2, all/1]).

% data representation
-record(cyclic_queue, {
  index = 0,
  count = 0,
  data
}).

% creates a new cyclic array of fixed size
new(MaxSize) -> #cyclic_queue{
  data = array:new([{fixed, true}, {size, MaxSize}])
}.

% retrieves max size of this cyclic queue
max_size(#cyclic_queue{data = D}) -> array:size(D).

% puts the next item in the queue.
put(Item, A = #cyclic_queue{index = I, count = C, data = D}) ->
  D2 = array:set(I, Item, D),
  #cyclic_queue{
    index = next_index(A),
    count = min(C + 1, max_size(A)),
    data  = D2
  }.

% gets Count elements from the queue.
% Throws an exception if we try to get more elements than total amount put,
% or if we try to get more elements than max size
poll(_, 0) -> [];
poll(A = #cyclic_queue{count = C, data = D}, Count) when Count >= 0 andalso Count =< C ->
  [array:get(previous_index(A), D) | poll(A#cyclic_queue{index = previous_index(A)}, Count - 1)].

% get top element
poll(A) -> poll(A, 1).

% gets all elements, returns a list
all(CyclicArray = #cyclic_queue{count = C}) -> poll(CyclicArray, C).


%% private functions
next_index(A = #cyclic_queue{index = I}) ->
  (I + 1) rem max_size(A).
previous_index(A = #cyclic_queue{index = I}) ->
  if I =:= 0 -> max_size(A) - 1;
     true -> I - 1
  end.

