-module(ttt_game_logic_test).
-include("../src/ttt_board.hrl").
-include("ttt_common.hrl").

% Makes testing easier. Update if board representation changes.
new_board(ListOfBoardValues) ->
  ListOfBoardValues.

can_check_if_a_move_is_legal_test_() ->
  NewBoard = new_board([?EMPTY_SPACE, ?PLAYER_1]),

  [?_assert(ttt_game_logic:is_legal_move(1, NewBoard)),
   ?_assertNot(ttt_game_logic:is_legal_move(2, NewBoard))].
