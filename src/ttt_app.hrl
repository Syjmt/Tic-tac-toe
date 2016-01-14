-define(PLAYER_1_MARK, "X").
-define(PLAYER_2_MARK, "O").
-define(CATS_GAME_MESSAGE, "~nCats game!").
-define(PLAYER_1_WINS_MESSAGE, "~nPlayer 1 wins!").
-define(PLAYER_2_WINS_MESSAGE, "~nPlayer 2 wins!").
-define(INPUT_INVALID_MESSAGE, "Invalid input. Please input an integer between ~w and ~w.~n").
-define(INPUT_OUT_OF_BOUNDS_MESSAGE, "~w is an invalid option. Please input an integer between ~w and ~w.~n").
-record(input_type, {lower_bound,
                     upper_bound,
                     invalid_message = ?INPUT_INVALID_MESSAGE,
                     out_of_bounds_message = ?INPUT_OUT_OF_BOUNDS_MESSAGE,
                     illegal_message = ok,
                     prompt_message}).
-define(GAME_MODE_MIN, 1).
-define(GAME_MODE_MAX, 4).
-define(GAME_MODE_PROMPT_MESSAGE, "Please select a game mode: ").
-define(GAME_MODE, #input_type{lower_bound = ?GAME_MODE_MIN,
                               upper_bound = ?GAME_MODE_MAX,
                               prompt_message = ?GAME_MODE_PROMPT_MESSAGE}).
-define(PLAYER_MOVE_MIN, 1).
-define(PLAYER_MOVE_MAX(GameState), ttt_api:number_of_spaces(GameState)).
-define(PLAYER_MOVE_PROMPT_MESSAGE, "Please enter a board space value: ").
-define(PLAYER_MOVE_ILLEGAL_MESSAGE, "That space is already occupied! Choose another space!~n").
-define(PLAYER_MOVE(GameState), #input_type{lower_bound = ?PLAYER_MOVE_MIN,
                                            upper_bound = ?PLAYER_MOVE_MAX(GameState),
                                            illegal_message = ?PLAYER_MOVE_ILLEGAL_MESSAGE,
                                            prompt_message = ?PLAYER_MOVE_PROMPT_MESSAGE}).
-define(PLAYER_1_TURN_MESSAGE, "~nPlayer 1's turn.~n").
-define(PLAYER_2_TURN_MESSAGE, "~nPlayer 2's turn.~n").

-define(MENU_MESSAGE, "/////////////////~n" ++
                      "// Tic Tac Toe //~n" ++
                      "/////////////////~n~n" ++
                      "Game modes:~n" ++
                      " 1. Player v Player~n" ++
                      " 2. Player v Computer~n" ++
                      " 3. Computer v Player~n" ++
                      " 4. Computer v Computer~n").
