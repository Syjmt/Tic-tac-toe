-module(ttt_api).
-include("ttt_api.hrl").
-include("ttt_player.hrl").
-export([new_game/1,
         board/1,
         board_rows/1,
         computer_move/1,
         current_player/1,
         is_game_over/1,
         is_human_player/1,
         is_legal_move/2,
         move/2,
         number_of_spaces/1,
         player_1_value/0,
         player_2_value/0,
         winner/1]).

new_game(GameMode) ->
  case GameMode of
    ?HUMAN_V_HUMAN ->
      #{?PLAYER_1 => ?HUMAN,
        ?PLAYER_2 => ?HUMAN,
        board => ttt_board:new_board()};
    ?HUMAN_V_COMPUTER ->
      #{?PLAYER_1 => ?HUMAN,
        ?PLAYER_2 => ?COMPUTER,
        board => ttt_board:new_board()};
    ?COMPUTER_V_HUMAN->
      #{?PLAYER_1 => ?COMPUTER,
        ?PLAYER_2 => ?HUMAN,
        board => ttt_board:new_board()};
    _ ->
      #{?PLAYER_1 => ?COMPUTER,
        ?PLAYER_2 => ?COMPUTER,
        board => ttt_board:new_board()}
  end.

board(GameState) -> maps:get(board, GameState).
board_rows(GameState) -> ttt_board:rows(board(GameState)).
computer_move(GameState) -> maps:put(board, ttt_ai:move(board(GameState)), GameState).
current_player(GameState) -> ttt_game_logic:current_player(board(GameState)).
is_game_over(GameState) -> ttt_game_logic:is_game_over(board(GameState)).
is_human_player(GameState) -> maps:get(current_player(GameState), GameState) =:= ?HUMAN.
is_legal_move(Index, GameState) -> ttt_game_logic:is_legal_move(Index, board(GameState)).
move(Index, GameState) -> maps:put(board, ttt_board:update_board(Index, current_player(GameState), board(GameState)), GameState).
number_of_spaces(GameState) -> ttt_board:number_of_spaces(board(GameState)).
player_1_value() -> ?PLAYER_1.
player_2_value() -> ?PLAYER_2.
winner(GameState) -> ttt_game_logic:winner(board(GameState)).
