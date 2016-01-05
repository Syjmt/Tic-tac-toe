-module(ttt_game_logic).
-include("ttt_board.hrl").
-export([is_legal_move/2]).

is_legal_move(SpaceIndex, Board) ->
  ttt_board:value_at(SpaceIndex, Board) =:= ?EMPTY_SPACE.
