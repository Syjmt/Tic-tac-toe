-module(ttt_game_logic_test).
-include("../src/ttt_board.hrl").
-include("../src/ttt_player.hrl").
-include("ttt_common.hrl").

can_check_if_a_move_is_legal_test_() ->
  NewBoard = ttt_board:new_board([?EMPTY_SPACE, ?PLAYER_1]),

  [?_assert(ttt_game_logic:is_legal_move(1, NewBoard)),
   ?_assertNot(ttt_game_logic:is_legal_move(2, NewBoard))].

can_return_the_current_player_test_() ->
  Player1Turn = ttt_board:new_board([?EMPTY_SPACE, ?EMPTY_SPACE]),
  Player2Turn = ttt_board:new_board([?PLAYER_1, ?EMPTY_SPACE]),

  [?_assertEqual(?PLAYER_1, ttt_game_logic:current_player(Player1Turn)),
   ?_assertEqual(?PLAYER_2, ttt_game_logic:current_player(Player2Turn))].

is_game_over_returns_false_for_an_incomplete_test() ->
  IncompleteGame = ttt_board:new_board([?PLAYER_1,    ?EMPTY_SPACE, ?EMPTY_SPACE,
                                        ?EMPTY_SPACE, ?EMPTY_SPACE, ?EMPTY_SPACE,
                                        ?EMPTY_SPACE, ?EMPTY_SPACE, ?EMPTY_SPACE]),

  ?assertNot(ttt_game_logic:is_game_over(IncompleteGame)).

is_game_over_returns_true_for_a_cats_game_test() ->
  CatsGame = ttt_board:new_board([?PLAYER_1, ?PLAYER_2, ?PLAYER_1,
                                  ?PLAYER_1, ?PLAYER_2, ?PLAYER_2,
                                  ?PLAYER_2, ?PLAYER_1, ?PLAYER_1]),

  ?assert(ttt_game_logic:is_game_over(CatsGame)).

is_game_over_returns_true_for_a_horizontal_win_test_() ->
  HorizontalWin1 = ttt_board:new_board([?PLAYER_1,    ?PLAYER_1,    ?PLAYER_1,
                                        ?EMPTY_SPACE, ?EMPTY_SPACE, ?EMPTY_SPACE,
                                        ?EMPTY_SPACE, ?EMPTY_SPACE, ?EMPTY_SPACE]),
  HorizontalWin2 = ttt_board:new_board([?EMPTY_SPACE, ?EMPTY_SPACE, ?EMPTY_SPACE,
                                        ?PLAYER_2,    ?PLAYER_2,    ?PLAYER_2,
                                        ?EMPTY_SPACE, ?EMPTY_SPACE, ?EMPTY_SPACE]),
  HorizontalWin3 = ttt_board:new_board([?EMPTY_SPACE, ?EMPTY_SPACE, ?EMPTY_SPACE,
                                        ?EMPTY_SPACE, ?EMPTY_SPACE, ?EMPTY_SPACE,
                                        ?PLAYER_1,    ?PLAYER_1,    ?PLAYER_1]),

  [?_assert(ttt_game_logic:is_game_over(HorizontalWin1)),
   ?_assert(ttt_game_logic:is_game_over(HorizontalWin2)),
   ?_assert(ttt_game_logic:is_game_over(HorizontalWin3))].

is_game_over_returns_true_for_a_vertical_win_test_() ->
  VerticalWin1 =   ttt_board:new_board([?PLAYER_2, ?EMPTY_SPACE, ?EMPTY_SPACE,
                                        ?PLAYER_2, ?EMPTY_SPACE, ?EMPTY_SPACE,
                                        ?PLAYER_2, ?EMPTY_SPACE, ?EMPTY_SPACE]),
  VerticalWin2 =   ttt_board:new_board([?EMPTY_SPACE, ?PLAYER_1, ?EMPTY_SPACE,
                                        ?EMPTY_SPACE, ?PLAYER_1, ?EMPTY_SPACE,
                                        ?EMPTY_SPACE, ?PLAYER_1, ?EMPTY_SPACE]),
  VerticalWin3 =   ttt_board:new_board([?EMPTY_SPACE, ?EMPTY_SPACE, ?PLAYER_2,
                                        ?EMPTY_SPACE, ?EMPTY_SPACE, ?PLAYER_2,
                                        ?EMPTY_SPACE, ?EMPTY_SPACE, ?PLAYER_2]),

  [?_assert(ttt_game_logic:is_game_over(VerticalWin1)),
   ?_assert(ttt_game_logic:is_game_over(VerticalWin2)),
   ?_assert(ttt_game_logic:is_game_over(VerticalWin3))].

is_game_over_returns_true_for_a_diagonal_win_test_() ->
  DiagonalWin1 = ttt_board:new_board([?PLAYER_1,    ?EMPTY_SPACE, ?EMPTY_SPACE,
                                      ?EMPTY_SPACE, ?PLAYER_1,    ?EMPTY_SPACE,
                                      ?EMPTY_SPACE, ?EMPTY_SPACE, ?PLAYER_1]),
  DiagonalWin2 = ttt_board:new_board([?EMPTY_SPACE, ?EMPTY_SPACE, ?PLAYER_2,
                                      ?EMPTY_SPACE, ?PLAYER_2,    ?EMPTY_SPACE,
                                      ?PLAYER_2,    ?EMPTY_SPACE, ?EMPTY_SPACE]),

  [?_assert(ttt_game_logic:is_game_over(DiagonalWin1)),
   ?_assert(ttt_game_logic:is_game_over(DiagonalWin2))].

winner_returns_none_for_an_incomplete_game_test() ->
  IncompleteGame = ttt_board:new_board([?PLAYER_1,    ?EMPTY_SPACE, ?EMPTY_SPACE,
                                        ?EMPTY_SPACE, ?EMPTY_SPACE, ?EMPTY_SPACE,
                                        ?EMPTY_SPACE, ?EMPTY_SPACE, ?EMPTY_SPACE]),

  ?assertEqual(none, ttt_game_logic:winner(IncompleteGame)).

winner_returns_none_for_a_cats_game_test() ->
  CatsGame = ttt_board:new_board([?PLAYER_1, ?PLAYER_2, ?PLAYER_1,
                                  ?PLAYER_1, ?PLAYER_2, ?PLAYER_2,
                                  ?PLAYER_2, ?PLAYER_1, ?PLAYER_1]),

  ?assertEqual(none, ttt_game_logic:winner(CatsGame)).

winner_returns_player_value_if_a_winner_exists_test_() ->
  HorizontalWin = ttt_board:new_board([?PLAYER_1,    ?PLAYER_1,    ?PLAYER_1,
                                       ?EMPTY_SPACE, ?PLAYER_2,    ?PLAYER_2,
                                       ?EMPTY_SPACE, ?EMPTY_SPACE, ?EMPTY_SPACE]),
  VerticalWin =   ttt_board:new_board([?PLAYER_2, ?PLAYER_1,    ?PLAYER_1,
                                       ?PLAYER_2, ?EMPTY_SPACE, ?PLAYER_1,
                                       ?PLAYER_2, ?EMPTY_SPACE, ?EMPTY_SPACE]),

  [?_assertEqual(?PLAYER_1, ttt_game_logic:winner(HorizontalWin)),
   ?_assertEqual(?PLAYER_2, ttt_game_logic:winner(VerticalWin))].
