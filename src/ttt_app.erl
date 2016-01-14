-module(ttt_app).
-include("ttt_app.hrl").
-include("ttt_player.hrl").
-export([start/0]).

start() ->
  printer:print_menu(),
  GameMode = input_reader:get_input_value(?GAME_MODE),
  GameState = ttt_api:new_game(GameMode),
  game_loop(GameState).

game_loop(GameState) ->
  game_loop(GameState, ttt_api:is_game_over(GameState)).

game_loop(GameState, IsGameOver) when not IsGameOver ->
  printer:print_board(ttt_api:board_rows(GameState)),
  printer:print_player_turn(ttt_api:current_player(GameState)),
  UpdatedGameState = make_move(GameState),
  game_loop(UpdatedGameState, ttt_api:is_game_over(UpdatedGameState));
game_loop(GameState, _) ->
  printer:print_board(ttt_api:board_rows(GameState)),
  printer:print_winner(GameState).

make_move(GameState)->
  case ttt_api:is_human_player(GameState) of
    true ->
      PlayerMove = input_reader:get_legal_player_move(GameState),
      UpdatedGameState = ttt_api:move(PlayerMove, GameState);
    _ ->
      UpdatedGameState = ttt_api:computer_move(GameState)
  end.
