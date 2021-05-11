%%%-------------------------------------------------------------------
%%% @author zkieda
%%% @copyright (C) 2021, <COMPANY>
%%% @doc
%%%   Simple plugin that exits the
%%% @end
%%% Created : 02. Apr 2021 9:17 PM
%%%-------------------------------------------------------------------
-module(terminator).
-author("zkieda").

%% API
-export([new_plugin/2, update_plugin/2, should_exit/1]).
-export_type([accepts/0]).
-type accepts()
  :: {'receive_control', 'terminate', any()}.

-record(terminator_state, {
  terminate = false
}).

new_plugin(_, _) -> #terminator_state{
  terminate = false
}.

update_plugin({'receive', 'terminate', _}, Plugin) -> Plugin#terminator_state{terminate = true};
update_plugin({'receive_control', 'terminate', _}, Plugin) -> Plugin#terminator_state{terminate = true};
update_plugin(_, _) -> ignore.

should_exit(State) ->
  #terminator_state{terminate = Val} = caffe:get_plugin_state(?MODULE, State),
  Val.
