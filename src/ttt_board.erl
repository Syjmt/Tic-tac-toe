-module(ttt_board).
-include("ttt_board.hrl").
-export([new_board/0,
         new_board/1,
         to_list/1,
         number_of_spaces/1,
         value_at/2,
         set_space_value/3,
         has_empty_space/1,
         rows/1,
         columns/1,
         diagonals/1]).

list_of_empty_spaces(RemainingSpaces) when RemainingSpaces > 0 ->
  [?EMPTY_SPACE | list_of_empty_spaces(RemainingSpaces - 1)];
list_of_empty_spaces(_) -> [].

new_board() ->
  new_board(?DEFAULT_BOARD_SIZE).
new_board(BoardSize) when is_integer(BoardSize) ->
  list_of_empty_spaces(BoardSize * BoardSize);
new_board(ListOfBoardValues) when is_list(ListOfBoardValues) ->
  ListOfBoardValues.

to_list(Board) ->
  Board.

number_of_spaces(Board) ->
  length(Board).

value_at(SpaceIndex, Board) ->
  lists:nth(SpaceIndex, Board).

set_space_value(1, UpdatedValue, [_ | RemainingBoardValues]) ->
  [UpdatedValue | RemainingBoardValues];
set_space_value(SpaceIndex, UpdatedValue, Board) when SpaceIndex =:= length(Board) ->
  lists:droplast(Board) ++ [UpdatedValue];
set_space_value(SpaceIndex, UpdatedValue, Board) ->
  lists:sublist(Board, SpaceIndex-1) ++
  [UpdatedValue] ++
  lists:nthtail(SpaceIndex, Board).

has_empty_space(Board) ->
  lists:any(fun(Space) ->
    Space =:= ?EMPTY_SPACE
  end, Board).

rows(Board) ->
  split_rows(board_size(Board), Board).

board_size(Board) ->
  round(math:sqrt(number_of_spaces(Board))).

split_rows(_, []) -> [];
split_rows(BoardSize, RemainingBoardValues) ->
  {NewRow, LeftoverValues} = lists:split(BoardSize, RemainingBoardValues),
  [NewRow | split_rows(BoardSize, LeftoverValues)].

columns(Board) ->
  BoardSize = board_size(Board),
  ListOfStartIndices = lists:seq(1, BoardSize),
  split_columns(ListOfStartIndices, BoardSize, Board).

split_columns([], _, _) -> [];
split_columns([StartIndex | RemainingIndices], BoardSize, Board) ->
  NewColumn = every_nth_space(StartIndex, BoardSize, Board),
  [NewColumn | split_columns(RemainingIndices, BoardSize, Board)].

every_nth_space(_, _, []) -> [];
every_nth_space(StartIndex, Step, Board) ->
  TrimmedBoard = lists:nthtail(Step, Board),
  [value_at(StartIndex, Board) | every_nth_space(StartIndex, Step, TrimmedBoard)].

diagonals(Board) ->
  BoardRows = rows(Board),
  [board_diagonal(BoardRows, 1), board_diagonal(lists:reverse(BoardRows), 1)].

board_diagonal([], _) -> [];
board_diagonal([CurrentRow | RemainingRows], SpaceIndex) ->
  [value_at(SpaceIndex, CurrentRow) | board_diagonal(RemainingRows, SpaceIndex + 1)].
