%%%-------------------------------------------------------------------
%%% @author zkieda
%%% @copyright (C) 2021, <COMPANY>
%%% @doc
%%%   This plugin is used to add or remove plugins on a given vertex.
%%%   There are some plugins that are added by default and cannot be removed,
%%%   these are the system plugins (this folder). For example, the plugin_manager
%%%   will not allow you to remove itself.
%%% @end
%%% Created : 01. Apr 2021 8:16 PM
%%%-------------------------------------------------------------------
-module(plugin_manager).
-author("zkieda").

%% API
-export([]).

new_plugin(VertexArgs, State) -> #{}.
