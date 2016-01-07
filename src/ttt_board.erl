-module(ttt_board).
-include("ttt_board.hrl").
-export([new_board/0,
         new_board/1,
         to_list/1,
         number_of_spaces/1,
         value_at/2,
         update_board/3,
         number_of_empty_spaces/1,
         has_empty_space/1,
         rows/1,
         columns/1,
         diagonals/1,
         indices_of/2]).

new_board() ->
  new_board(?DEFAULT_BOARD_SIZE).
new_board(BoardSize) when is_integer(BoardSize) ->
  list_of_empty_spaces(BoardSize * BoardSize);
new_board(ListOfBoardValues) when is_list(ListOfBoardValues) ->
  ListOfBoardValues.

list_of_empty_spaces(RemainingSpaces) when RemainingSpaces > 0 ->
  [?EMPTY_SPACE | list_of_empty_spaces(RemainingSpaces - 1)];
list_of_empty_spaces(_) -> [].

to_list(Board) ->
  Board.

number_of_spaces(Board) ->
  length(to_list(Board)).

value_at(SpaceIndex, Board) ->
  lists:nth(SpaceIndex, to_list(Board)).

update_board(Index, Value, Board) ->
  new_board(set_space_value(Index, Value, to_list(Board))).

set_space_value(1, Value, [_ | RemainingBoardValues]) ->
  [Value | RemainingBoardValues];
set_space_value(SpaceIndex, Value, ListOfBoardValues) when
  SpaceIndex =:= length(ListOfBoardValues) ->
  lists:droplast(ListOfBoardValues) ++ [Value];
set_space_value(SpaceIndex, Value, ListOfBoardValues) ->
  lists:sublist(ListOfBoardValues, SpaceIndex-1) ++
  [Value] ++
  lists:nthtail(SpaceIndex, ListOfBoardValues).

number_of_empty_spaces(Board) -> number_of_empty_spaces(to_list(Board), 0).
number_of_empty_spaces([], Count) -> Count;
number_of_empty_spaces([?EMPTY_SPACE | RemainingBoardValues], Count) ->
  number_of_empty_spaces(RemainingBoardValues, Count + 1);
number_of_empty_spaces([_ | RemainingBoardValues], Count) ->
  number_of_empty_spaces(RemainingBoardValues, Count).

has_empty_space(Board) -> number_of_empty_spaces(Board) > 0.

rows(Board) ->
  split_rows(board_size(Board), to_list(Board)).

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
  TrimmedBoard = lists:nthtail(Step, to_list(Board)),
  [value_at(StartIndex, Board) | every_nth_space(StartIndex, Step, TrimmedBoard)].

diagonals(Board) ->
  BoardRows = rows(Board),
  [board_diagonal(BoardRows, 1), board_diagonal(lists:reverse(BoardRows), 1)].

board_diagonal([], _) -> [];
board_diagonal([CurrentRow | RemainingRows], SpaceIndex) ->
  [value_at(SpaceIndex, CurrentRow) | board_diagonal(RemainingRows, SpaceIndex + 1)].

indices_of(Value, Board) -> indices_of(Value, to_list(Board), [], 1).
indices_of(_, [], Indices, _) -> Indices;
indices_of(Value, [Value | RemainingBoardValues], Indices, CurrentIndex) ->
  indices_of(Value, RemainingBoardValues, Indices++ [CurrentIndex], CurrentIndex + 1);
indices_of(Value, [_ | RemainingBoardValues], Indices, CurrentIndex) ->
  indices_of(Value, RemainingBoardValues, Indices, CurrentIndex + 1).
