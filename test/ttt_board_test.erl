-module(ttt_board_test).
-include("../src/ttt_board.hrl").
-include("ttt_common.hrl").

% Needed to separate test for number_of_spaces and new_board()
% Update if board representation changes
new_board(ListOfBoardValues) ->
  ListOfBoardValues.

new_board_returns_a_board_test() ->
  NewBoard = ttt_board:new_board(),

  ?assertNotEqual(ok, NewBoard).

can_return_number_of_spaces_on_board_test() ->
  NewBoard = new_board([1, 2, 3]),

  ?assertEqual(3, ttt_board:number_of_spaces(NewBoard)).

new_board_defaults_to_size_3_if_no_size_provided_test() ->
  NewBoard = ttt_board:new_board(),

  ?assertEqual(9, ttt_board:number_of_spaces(NewBoard)).

new_board_can_take_a_board_size_test() ->
  NewBoard = ttt_board:new_board(4),

  ?assertEqual(16, ttt_board:number_of_spaces(NewBoard)).

can_return_value_of_a_space_test() ->
  NewBoard = ttt_board:new_board(),

  ?assertEqual(ttt_board:value_at(1, NewBoard), ?EMPTY_SPACE).

new_board_has_empty_spaces_test() ->
  NewBoard = ttt_board:new_board(),

  lists:foreach(fun(SpaceIndex) ->
    ?assertEqual(?EMPTY_SPACE, ttt_board:value_at(SpaceIndex, NewBoard))
    end, lists:seq(1, ttt_board:number_of_spaces(NewBoard))).

can_set_value_of_a_space_test_() ->
  NewBoard = ttt_board:new_board(),
  UpdatedBoard1 = ttt_board:set_space_value(1, not_empty, NewBoard),
  UpdatedBoard2 = ttt_board:set_space_value(5, not_empty, NewBoard),
  UpdatedBoard3 = ttt_board:set_space_value(9, not_empty, NewBoard),

  [?_assertEqual(ttt_board:value_at(1, UpdatedBoard1), not_empty),
   ?_assertEqual(ttt_board:value_at(5, UpdatedBoard2), not_empty),
   ?_assertEqual(ttt_board:value_at(9, UpdatedBoard3), not_empty)].
