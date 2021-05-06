%%%-------------------------------------------------------------------
%%% @author zkieda
%%% @copyright (C) 2021, <COMPANY>
%%% @doc
%%%   very simplistic cyclic array implementation. Fixed size.
%%%   Adding new elements beyond the max size will override existing values
%%% @end
%%% Created : 01. May 2021 4:14 PM
%%%-------------------------------------------------------------------
-module(cyclic_array).

%% API
-export([new/1, put/2, max_size/1, get/2, all/1]).

% data representation
-record(cyclic_array, {
  index = 0,
  count = 0,
  data
}).

% creates a new cyclic array of fixed size
new(MaxSize) -> #cyclic_array{
  data = array:new([{fixed, true}, {size, MaxSize}])
}.

% retrieves max size of this cyclic array
max_size(#cyclic_array{data = D}) -> array:size(D).

% puts the next item in the array.
put(Item, A = #cyclic_array{index = I, count = C, data = D}) ->
  #cyclic_array{
    index = next_index(A),
    count = max(C + 1, max_size(A)),
    data  = array:set(I, Item, D)
  }.

% gets elements from the array. Throws an exception if we try to get more elements than
% the total count put, or if we try to get more elements than this array can hold
poll(_, 0) -> [].
poll(A = #cyclic_array{index = I, count = C, data = D}, Count) when Count >= 0 andalso Count =< C ->
  [array:get(I, D) | get(A#cyclic_array{index = next_index(A)}, Count - 1)].

% get top element
poll(A) -> poll(A, 1).

all(CyclicArray = #cyclic_array{count = C}) -> get(CyclicArray, C).


%% private functions
next_index(A = #cyclic_array{index = I}) ->
  (I + 1) rem max_size(A).
