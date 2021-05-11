%%%-------------------------------------------------------------------
%%% @author zkieda
%%% @copyright (C) 2021, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 08. May 2021 2:24 PM
%%%-------------------------------------------------------------------
-module(ezpr).
-author("zkieda").

%% API
-export([par/2, bullet/3]).

-type bullet_option() :: {separator, string()}.
-type bullet_options() :: [bullet_option()].

-spec bullet(string(), string(), bullet_options()) -> string().
bullet(Item, Body, [{separator, Sep}]) ->
  BulletLength = string:length(Item) + string:length(Sep),
  par(lists:concat([Item, Sep, Body]),
    [{style, rest}, {indent, BulletLength}]).

-type par_option() :: {indent, non_neg_integer()} % in spaces. Default 4
  | {trim, leading | trailing | none | both}      % if we should trim lines before adding indentation
  | {style, first | rest | all}.                  % first line get indented, last lines get indented, or just all of em?
-type par_options() :: [par_option()] | par_option().

-spec par(string(), par_options()) -> string().
par(Str, O) ->
  Options = maps:from_list(O),
  Indent = maps:get(indent, Options, 4),
  Style = maps:get(style, Options, all),
  Trim = maps:get(trim, Options, none),

  Lines = string:split(Str, "\n", all),
  LinesTrimmed = lists:map(
    fun(Line) -> case Trim of
                   none -> Line;
                   TrimStyle -> string:trim(Line, TrimStyle)
                 end
    end, Lines),
  LinesFormatted = case Style of
    all   ->  par_all(LinesTrimmed, Indent);
    first ->  par_first(LinesTrimmed, Indent);
    rest  ->  case LinesTrimmed of
                [] -> [];
                [First|Rest] -> [First|par_all(Rest, Indent)]
              end
  end,
  Final = join_lines(LinesFormatted),
  Final.

join_lines([]) -> "";
join_lines([First|Rest]) -> string:concat(First ++ "\n", join_lines(Rest)).

par_first([First|Rest], Indent) -> [lists:concat([ lists:duplicate(Indent, $\s),  First ]) |Rest];
par_first([], _) -> [].

par_all([First|Rest], Indent) -> [lists:concat([ lists:duplicate(Indent, $\s),  First ])|par_all(Rest, Indent)];
par_all([], _) -> [].
