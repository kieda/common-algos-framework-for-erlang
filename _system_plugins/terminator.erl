%%%-------------------------------------------------------------------
%%% @author zkieda
%%% @copyright (C) 2021, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 02. Apr 2021 9:17 PM
%%%-------------------------------------------------------------------
-module(terminator).
-author("zkieda").

%% API
-export([new_plugin/2, update_plugin/2, should_exit/1]).

new_plugin(_, _) -> #{
  terminate => false
}.

update_plugin({'receive', 'terminate'}, Plugin) -> maps:put(terminate, true, Plugin);
update_plugin(_, _) -> ignore.

should_exit(#{terminate := Val}) -> Val.
