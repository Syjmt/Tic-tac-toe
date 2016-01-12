-module(ttt_board_test).
-include("../src/ttt_board.hrl").
-include("ttt_common.hrl").

new_board_returns_a_board_test() ->
  NewBoard = ttt_board:new_board(),

  ?assertNotEqual(ok, NewBoard).

to_list_returns_the_board_as_a_list_test() ->
  NewBoard = ttt_board:new_board(),
  BoardAsList = ttt_board:to_list(NewBoard),

  ?assert(is_list(BoardAsList)).

can_return_number_of_spaces_on_board_test() ->
  NewBoard = ttt_board:new_board(),
  BoardAsList = ttt_board:to_list(NewBoard),

  ?assertEqual(length(BoardAsList), ttt_board:number_of_spaces(NewBoard)).

new_board_defaults_to_size_3_if_no_size_provided_test() ->
  NewBoard = ttt_board:new_board(),

  ?assertEqual(9, ttt_board:number_of_spaces(NewBoard)).

new_board_can_take_a_list_of_board_values_test() ->
  ExpectedBoard = [1, 2, 3],
  NewBoard = ttt_board:new_board(ExpectedBoard),

  ?assertEqual(ExpectedBoard, NewBoard).

can_return_value_of_a_space_test() ->
  NewBoard = ttt_board:new_board(),

  ?assertEqual(ttt_board:value_at(1, NewBoard), ?EMPTY_SPACE).

new_board_has_empty_spaces_test() ->
  NewBoard = ttt_board:new_board(),
  BoardAsList = ttt_board:to_list(NewBoard),

  lists:foreach(fun(SpaceIndex) ->
    ?assertEqual(?EMPTY_SPACE, ttt_board:value_at(SpaceIndex, NewBoard)),
    ?assertEqual(?EMPTY_SPACE, lists:nth(SpaceIndex, BoardAsList))
    end, lists:seq(1, ttt_board:number_of_spaces(NewBoard))).

can_update_board_test_() ->
  NewBoard = ttt_board:new_board(),
  UpdatedBoard1 = ttt_board:update_board(1, not_empty, NewBoard),
  UpdatedBoard2 = ttt_board:update_board(5, not_empty, NewBoard),
  UpdatedBoard3 = ttt_board:update_board(9, not_empty, NewBoard),

  [?_assertEqual(ttt_board:value_at(1, UpdatedBoard1), not_empty),
   ?_assertEqual(ttt_board:value_at(5, UpdatedBoard2), not_empty),
   ?_assertEqual(ttt_board:value_at(9, UpdatedBoard3), not_empty)].

can_return_the_number_of_empty_spaces_test() ->
  NewBoard = ttt_board:new_board([?EMPTY_SPACE, not_empty, not_empty, ?EMPTY_SPACE]),

  ?assertEqual(2, ttt_board:number_of_empty_spaces(NewBoard)).

can_return_the_indices_of_a_value_test_() ->
  NoEmptySpaces = ttt_board:new_board([not_empty, not_empty]),
  SomeEmptySpaces = ttt_board:new_board([?EMPTY_SPACE, not_empty]),
  AllEmptySpaces = ttt_board:new_board([?EMPTY_SPACE, ?EMPTY_SPACE]),

  [?_assertEqual([], ttt_board:indices_of(?EMPTY_SPACE, NoEmptySpaces)),
   ?_assertEqual([1], ttt_board:indices_of(?EMPTY_SPACE, SomeEmptySpaces)),
   ?_assertEqual([1,2], ttt_board:indices_of(?EMPTY_SPACE, AllEmptySpaces))].

can_check_if_board_has_empty_spaces_test() ->
  BoardWithEmptySpace = ttt_board:new_board([?EMPTY_SPACE]),
  BoardWithoutEmptySpace = ttt_board:new_board([not_empty]),

  [?_assert(ttt_board:has_empty_space(BoardWithEmptySpace)),
   ?_assertNot(ttt_board:has_empty_space(BoardWithoutEmptySpace))].

can_return_board_rows_test() ->
  ExpectedRows = [[player_1, ?EMPTY_SPACE, ?EMPTY_SPACE],
                  [player_1, player_2,     ?EMPTY_SPACE],
                  [player_2, ?EMPTY_SPACE, player_1]],
  NewBoard = ttt_board:new_board([player_1, ?EMPTY_SPACE, ?EMPTY_SPACE,
                                  player_1, player_2,     ?EMPTY_SPACE,
                                  player_2, ?EMPTY_SPACE, player_1]),

  ?assertEqual(ExpectedRows, ttt_board:rows(NewBoard)).

can_return_board_columns_test() ->
  ExpectedColumns = [[player_1,     player_1,     player_2],
                     [?EMPTY_SPACE, player_2,     ?EMPTY_SPACE],
                     [?EMPTY_SPACE, ?EMPTY_SPACE, player_1]],
  NewBoard = ttt_board:new_board([player_1, ?EMPTY_SPACE, ?EMPTY_SPACE,
                                  player_1, player_2,     ?EMPTY_SPACE,
                                  player_2, ?EMPTY_SPACE, player_1]),

  ?assertEqual(ExpectedColumns, ttt_board:columns(NewBoard)).

can_return_board_diagonals_test() ->
  ExpectedDiagonals = [[player_1, player_2, player_1],
                       [player_2, player_2, ?EMPTY_SPACE]],
  NewBoard = ttt_board:new_board([player_1, ?EMPTY_SPACE, ?EMPTY_SPACE,
                                  player_1, player_2,     ?EMPTY_SPACE,
                                  player_2, ?EMPTY_SPACE, player_1]),

  ?assertEqual(ExpectedDiagonals, ttt_board:diagonals(NewBoard)).
