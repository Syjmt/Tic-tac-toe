-module(ttt_api_test).
-include("ttt_common.hrl").
-include("../src/ttt_api.hrl").
-include("../src/ttt_player.hrl").

can_start_a_new_human_v_human_game_test_() ->
  GameStateMap = ttt_api:new_game(?HUMAN_V_HUMAN),

  [?_assertEqual(maps:get(?PLAYER_1, GameStateMap), ?HUMAN),
   ?_assertEqual(maps:get(?PLAYER_2, GameStateMap), ?HUMAN),
   ?_assertEqual(maps:get(board, GameStateMap), ttt_board:new_board())].

can_start_a_new_human_v_computer_game_test_() ->
  GameStateMap = ttt_api:new_game(?HUMAN_V_COMPUTER),

  [?_assertEqual(maps:get(?PLAYER_1, GameStateMap), ?HUMAN),
   ?_assertEqual(maps:get(?PLAYER_2, GameStateMap), ?COMPUTER),
   ?_assertEqual(maps:get(board, GameStateMap), ttt_board:new_board())].

can_start_a_new_computer_v_human_game_test_() ->
  GameStateMap = ttt_api:new_game(?COMPUTER_V_HUMAN),

  [?_assertEqual(maps:get(?PLAYER_1, GameStateMap), ?COMPUTER),
   ?_assertEqual(maps:get(?PLAYER_2, GameStateMap), ?HUMAN),
   ?_assertEqual(maps:get(board, GameStateMap), ttt_board:new_board())].

can_start_a_new_computer_v_computer_game_test_() ->
  GameStateMap = ttt_api:new_game(?COMPUTER_V_COMPUTER),

  [?_assertEqual(maps:get(?PLAYER_1, GameStateMap), ?COMPUTER),
   ?_assertEqual(maps:get(?PLAYER_2, GameStateMap), ?COMPUTER),
   ?_assertEqual(maps:get(board, GameStateMap), ttt_board:new_board())].

can_determine_if_human_move_test_() ->
  GameStateMap = ttt_api:new_game(?HUMAN_V_COMPUTER),
  UpdatedGameStateMap = ttt_api:move(1, GameStateMap),

  [?_assert(ttt_api:is_human_player(GameStateMap)),
   ?_assertNot(ttt_api:is_human_player(UpdatedGameStateMap))].
