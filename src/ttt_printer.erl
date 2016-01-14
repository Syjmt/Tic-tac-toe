-module(ttt_printer).
-include("ttt_app.hrl").
-export([print_menu/0,
         print_board/1,
         print_player_turn/1,
         print_winner/1]).

print_menu() ->
  io:format(?MENU_MESSAGE).

print_board(BoardRows) ->
  StartIndex = 1,
  print_board(BoardRows, StartIndex).
print_board([], _) -> ok;
print_board([CurrentRow | RemainingRows], Index) ->
  io:format("~n"),
  print_row(CurrentRow, Index),
  print_board(RemainingRows, Index+length(CurrentRow)).

print_row([], _) -> ok;
print_row([CurrentValue | RemainingValues], Index) ->
  Player1Value = ttt_api:player_1_value(),
  Player2Value = ttt_api:player_2_value(),
  case CurrentValue of
    Player1Value -> io:format("[ ~s ]", [?PLAYER_1_MARK]);
    Player2Value -> io:format("[ ~s ]", [?PLAYER_2_MARK]);
    _ -> io:format("[ ~w ]", [Index])
  end,
  print_row(RemainingValues, Index+1).

print_player_turn(CurrentPlayer) ->
  Player1Value = ttt_api:player_1_value(),
  case CurrentPlayer of
    Player1Value -> io:format(?PLAYER_1_TURN_MESSAGE);
    _ -> io:format(?PLAYER_2_TURN_MESSAGE)
  end.

print_winner(GameState) ->
  Winner = ttt_api:winner(GameState),
  Player1Value = ttt_api:player_1_value(),
  Player2Value = ttt_api:player_2_value(),
  case Winner of
    Player1Value -> io:format(?PLAYER_1_WINS_MESSAGE);
    Player2Value -> io:format(?PLAYER_2_WINS_MESSAGE);
    _ -> io:format(?CATS_GAME_MESSAGE)
  end.
