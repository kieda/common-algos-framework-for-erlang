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

%% API
-export([is_exported/3, get_exported/2]).
-export([diff_deep/2, apply_function_spec/2]).

% diff maps or whatever deep
diff_deep(A, B) ->
  #{}.

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