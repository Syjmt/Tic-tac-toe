-module(ttt_board).
-include("ttt_board.hrl").
-export([new_board/0,
         new_board/1,
         number_of_spaces/1,
         value_at/2,
         set_space_value/3]).

list_of_empty_spaces(RemainingSpaces) when RemainingSpaces > 0 ->
  [?EMPTY_SPACE | list_of_empty_spaces(RemainingSpaces - 1)];
list_of_empty_spaces(_) -> [].

new_board() ->
  new_board(?DEFAULT_BOARD_SIZE).
new_board(BoardSize) ->
  list_of_empty_spaces(BoardSize * BoardSize).

number_of_spaces(Board) ->
  length(Board).

value_at(SpaceIndex, Board) ->
  lists:nth(SpaceIndex, Board).

set_space_value(1, UpdatedValue, [_ | Rest]) ->
  [UpdatedValue | Rest];
set_space_value(SpaceIndex, UpdatedValue, Board) when SpaceIndex =:= length(Board) ->
  lists:droplast(Board) ++ [UpdatedValue];
set_space_value(SpaceIndex, UpdatedValue, Board) ->
  lists:sublist(Board, SpaceIndex-1) ++ [UpdatedValue] ++ lists:nthtail(SpaceIndex, Board).
