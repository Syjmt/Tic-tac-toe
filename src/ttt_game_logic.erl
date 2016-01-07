-module(ttt_game_logic).
-include("ttt_board.hrl").
-include("ttt_player.hrl").
-export([is_legal_move/2,
         current_player/1,
         is_game_over/1,
         winner/1]).

is_legal_move(SpaceIndex, Board) ->
  ttt_board:value_at(SpaceIndex, Board) =:= ?EMPTY_SPACE.

current_player(Board) ->
  NumberOfPlayerMoves = ttt_board:number_of_spaces(Board) -
                        ttt_board:number_of_empty_spaces(Board),
  IsPlayer1Turn = (NumberOfPlayerMoves rem 2) =:= 0,
  case IsPlayer1Turn of
    true -> ?PLAYER_1;
    _Else -> ?PLAYER_2
  end.

is_game_over(Board) ->
  not ttt_board:has_empty_space(Board) or
  has_winner(Board).

has_winner(Board) ->
  check_winner(ttt_board:rows(Board)) or
  check_winner(ttt_board:columns(Board)) or
  check_winner(ttt_board:diagonals(Board)).

check_winner([]) -> false;
check_winner([[?EMPTY_SPACE | _] | RemainingSpaces]) ->
  check_winner(RemainingSpaces);
check_winner([CurrentSpaces | RemainingSpaces]) ->
  [FirstSpaceValue | _] = CurrentSpaces,
  HasWinner = all_space_values_equal(FirstSpaceValue, CurrentSpaces),
  if HasWinner -> true;
     true -> check_winner(RemainingSpaces)
  end.

all_space_values_equal(Value, Spaces) ->
  lists:all(fun(Space) -> Space =:= Value end, Spaces).

winner(Board) -> winner(Board, has_winner(Board)).
winner(_, false) -> none;
winner(Board, true) ->
  swap_players(current_player(Board)).

swap_players(?PLAYER_1) -> ?PLAYER_2;
swap_players(_) -> ?PLAYER_1.
