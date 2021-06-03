%%%-------------------------------------------------------------------
%%% @author zkieda
%%% @doc
%%%   Simple plugin that exits the
%%% @end
%%% Created : 02. Apr 2021 9:17 PM
%%%-------------------------------------------------------------------
-module(terminator).
-author("zkieda").

%% API
-export([should_exit/1, terminate/1]).
-export([new_plugin/1, update_plugin/2, dependencies/0]).
-export_type([accepts/0]).
-type accepts() :: {'receive', 'terminate'}
  | {'receive_control', 'terminator', boolean()}.

-record(terminator_state, {
  terminate = false
}).

dependencies() -> [
  messenger
].
new_plugin(_) -> #terminator_state{
  terminate = false
}.

update_plugin({'receive', 'terminate'}, Plugin) -> Plugin#terminator_state{terminate = true};
update_plugin({'receive_control', 'terminator', true}, Plugin) -> Plugin#terminator_state{terminate = true};
update_plugin(_, _) -> ignore.

should_exit(State) ->
  #terminator_state{terminate = Val} = caffe:get_plugin_state(?MODULE, State),
  Val.

terminate(State) ->
  % simulate receiving a terminate message
  messenger:receive_message(terminate, State).