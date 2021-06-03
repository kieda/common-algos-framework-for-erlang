%%%-------------------------------------------------------------------
%%% @author zkieda
%%% @copyright (C) 2021, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 02. Apr 2021 2:43 AM
%%%-------------------------------------------------------------------
-module(caffe_util).
-author("zkieda").

-export([is_exported/3, get_exported/2]).
-export([get_exported_default/3, get_exported_default/4]).
-export([apply_function_spec/2]).
-export([table/1]).
% functions for generating and analyzing diffs. Used analyze changes in the state
-export([diff_deep/2, diff_is_same/1, get_diff_added/1, get_diff_removed/1, get_diff_changed/1]).

% helper functions for analyzing the result of a diff
get_diff_added({added, Added}) -> Added;
get_diff_added({added, Added, _, _}) -> Added.

get_diff_removed({_, _, removed, Removed}) -> Removed;
get_diff_removed({removed, Removed}) -> Removed;
get_diff_removed({removed, Removed, _, _}) -> Removed.

get_diff_changed({changed, A, B}) -> {A, B};
get_diff_changed({_, _, changed, Diff}) -> Diff.

% after we call diff_deep, helper function to determine if a given diff is the same
diff_is_same(Any) ->
  Any == same orelse (is_map(Any) andalso maps:size(Any) == 0).

% takes a tuple and current index. Constructs a list of differences
diff_deep_tuple_samesize_helper(_, _, Max, Max) -> []; % base case - end of tuple when Idx == Max
diff_deep_tuple_samesize_helper(A, B, Max, Idx) -> % recursive case
  DiffIdx = diff_deep(element(Idx + 1, A), element(Idx + 1, B)),
  [DiffIdx|diff_deep_tuple_samesize_helper(A, B, Max, Idx + 1)].

% wraps the recursive function, so if everything is the same in the tuple we just return 'same'
diff_deep_tuple_samesize(A, B) ->
  Max = max(tuple_size(A), tuple_size(B)),
  Diff = diff_deep_tuple_samesize_helper(A, B, Max, 0),
  Same = lists:all(fun diff_is_same/1, Diff),
  if Same -> same;
     true -> list_to_tuple(Diff)
  end.
diff_deep_tuple(A, B) ->
  SizeA = tuple_size(A),
  SizeB = tuple_size(B),
  Diff = diff_deep_tuple_samesize(A, B),
  if SizeA == SizeB -> Diff;
     SizeA < SizeB  ->
       Added = list_to_tuple(lists:nthtail(tuple_to_list(B), SizeA)),
       if Diff == same -> {added, Added};
         true -> {added, Added, changed, Diff}
       end;
     SizeA > SizeB  ->
       Removed = list_to_tuple(lists:nthtail(tuple_to_list(A), SizeB)),
       if Diff == same -> {removed, Removed};
         true -> {removed, Removed, changed, Diff}
       end
  end.
diff_deep_map(A, B) ->
  {AKeys, BKeys} = {maps:keys(A), maps:keys(B)},
  CommonKeys = ordsets:to_list(ordsets:intersection(
    ordsets:from_list(AKeys), ordsets:from_list(BKeys)
  )),
  % represents key/value pairs added, removed, and changed on the map (respectively)
  Added = maps:map(fun(_, Val) -> {added, Val} end, maps:without(AKeys, B)),
  Removed = maps:map(fun(_, Val) -> {removed, Val} end, maps:without(BKeys, A)),
  Changed = maps:from_list(lists:map(fun(Key) -> {Key, diff_deep(maps:get(Key, A), maps:get(Key, B))} end, CommonKeys)),
  % remove non-interesting entries in the map (diff is the same)
  FilteredChanges = maps:filter(fun(_, V) -> not diff_is_same(V) end, Changed),
  maps:merge(maps:merge(Added, Removed), FilteredChanges).
% we don't recurse into lists - we just display which elems were added and removed.
diff_deep_list(A, B) ->
  Removed  = A -- B,
  Added    = B -- A,
  case {Removed, Added} of
    {[], []} -> same;
    {[], _ } -> {added, Added};
    {_ , []} -> {removed, Removed};
    {_ , _ } -> {added, Added, removed, Removed}
  end.

% diff maps or whatever deep
diff_deep(A, B) when is_tuple(A) andalso is_tuple(B) ->
  diff_deep_tuple(A, B);
diff_deep(A, B) when is_map(A) andalso is_map(B) ->
  diff_deep_map(A, B);
diff_deep(A, B) when is_list(A) andalso is_list(B) ->
  diff_deep_list(A, B);
diff_deep(A, B) ->
  if A == B -> same;
     true -> {changed, A, B}
  end.

get_exported_default(M, F, Default) -> get_exported_default(M, F, [], Default).
get_exported_default(M, F, Args, Default) ->
  case is_exported(M, F, length(Args)) of
    true  -> apply(M, F, Args);
    false -> Default
  end.

is_exported(M, F, A) ->
  case erlang:module_loaded(M) of
    false -> code:ensure_loaded(M);
    true -> ok
  end,
  erlang:function_exported(M, F, A).

-spec get_exported(module(), sets:set(atom())) -> #{atom() => [Arity::non_neg_integer()]}.
get_exported(M, Functions) ->
  case erlang:module_loaded(M) of
    false -> code:ensure_loaded(M);
    true -> ok
  end,
  ModuleExports = lists:keyfind(exports, 1, erlang:get_module_info(M)),
  ExportList = case ModuleExports of
    {exports, E} -> E;
    _ -> []
  end,
  Matching = lists:filter(fun({F, _}) -> sets:is_element(F, Functions) end, ExportList),
  lists:foldr(fun({F, Arity}, Map) -> maps:update_with(F, fun(A) -> [Arity|A] end, [Arity], Map) end, maps:new(), Matching).

apply_function_spec({anonymous, Fun}, Args) -> apply(Fun, Args);
apply_function_spec({named, ModuleName, FunctionName}, Args) -> apply(ModuleName, FunctionName, Args).

% array of arrays -> array of maps w/header as keys
table([Header|Rows]) ->
  lists:map(fun(Row) -> maps:from_list(lists:zip(Header, Row)) end, Rows).

