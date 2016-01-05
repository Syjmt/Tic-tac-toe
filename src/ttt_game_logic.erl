-module(ttt_game_logic).
-include("ttt_board.hrl").
-export([is_legal_move/2,
         is_game_over/1]).

is_legal_move(SpaceIndex, Board) ->
  ttt_board:value_at(SpaceIndex, Board) =:= ?EMPTY_SPACE.

is_game_over(Board) ->
  not ttt_board:has_empty_space(Board) or
  has_winner(ttt_board:rows(Board)) or
  has_winner(ttt_board:columns(Board)) or
  has_winner(ttt_board:diagonals(Board)).

has_winner([]) -> false;
has_winner([[?EMPTY_SPACE | _] | RemainingSpaces]) ->
  has_winner(RemainingSpaces);
has_winner([CurrentSpaces | RemainingSpaces]) ->
  [FirstSpaceValue | _] = CurrentSpaces,
  HasWinner = all_space_values_equal(FirstSpaceValue, CurrentSpaces),
  if HasWinner -> true;
     true -> has_winner(RemainingSpaces)
  end.

all_space_values_equal(Value, Spaces) ->
  lists:all(fun(Space) -> Space =:= Value end, Spaces).
