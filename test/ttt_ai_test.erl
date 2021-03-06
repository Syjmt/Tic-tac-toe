-module(ttt_ai_test).
-include("../src/ttt_board.hrl").
-include("../src/ttt_player.hrl").
-include("ttt_common.hrl").


takes_center_or_corner_space_for_first_move_test() ->
  Board = ttt_board:new_board(),
  ComputerPlayerValue = ?PLAYER_1,
  UpdatedBoard = ttt_ai:move(Board),
  MovedToCenterOrCorner = (ComputerPlayerValue =:= ttt_board:value_at(1, UpdatedBoard)) or
                          (ComputerPlayerValue =:= ttt_board:value_at(3, UpdatedBoard)) or
                          (ComputerPlayerValue =:= ttt_board:value_at(5, UpdatedBoard)) or
                          (ComputerPlayerValue =:= ttt_board:value_at(7, UpdatedBoard)) or
                          (ComputerPlayerValue =:= ttt_board:value_at(9, UpdatedBoard)),

  ?assert(MovedToCenterOrCorner).

can_make_winning_move_test_() ->
  Board1 = ttt_board:new_board([?PLAYER_1,    ?PLAYER_2,    ?PLAYER_1,
                                ?PLAYER_1,    ?PLAYER_2,    ?EMPTY_SPACE,
                                ?EMPTY_SPACE, ?EMPTY_SPACE, ?EMPTY_SPACE]),
  Board2 = ttt_board:new_board([?PLAYER_1,    ?PLAYER_2,    ?PLAYER_2,
                                ?EMPTY_SPACE, ?PLAYER_1,    ?EMPTY_SPACE,
                                ?EMPTY_SPACE, ?EMPTY_SPACE, ?EMPTY_SPACE]),
  UpdatedBoard1 = ttt_ai:move(Board1),
  UpdatedBoard2 = ttt_ai:move(Board2),

  [?_assertEqual(?PLAYER_2, ttt_game_logic:winner(UpdatedBoard1)),
   ?_assertEqual(?PLAYER_1, ttt_game_logic:winner(UpdatedBoard2))].

can_block_a_loss_test_() ->
  Board1 = ttt_board:new_board([?PLAYER_1,    ?PLAYER_1,    ?EMPTY_SPACE,
                                ?PLAYER_2,    ?EMPTY_SPACE, ?EMPTY_SPACE,
                                ?EMPTY_SPACE, ?EMPTY_SPACE, ?EMPTY_SPACE]),
  Board2 = ttt_board:new_board([?PLAYER_1,    ?EMPTY_SPACE, ?EMPTY_SPACE,
                                ?EMPTY_SPACE, ?PLAYER_2,    ?EMPTY_SPACE,
                                ?PLAYER_1,    ?EMPTY_SPACE, ?EMPTY_SPACE]),
  UpdatedBoard1 = ttt_ai:move(Board1),
  UpdatedBoard2 = ttt_ai:move(Board2),
  SpaceToBlock1 = ttt_board:value_at(3, UpdatedBoard1),
  SpaceToBlock2 = ttt_board:value_at(4, UpdatedBoard2),

  [?_assertEqual(?PLAYER_2, SpaceToBlock1),
   ?_assertEqual(?PLAYER_2, SpaceToBlock2)].

will_take_an_immediate_win_over_blocking_a_loss_test() ->
  Board = ttt_board:new_board([?PLAYER_1,    ?PLAYER_1,    ?EMPTY_SPACE,
                               ?PLAYER_2,    ?PLAYER_2,    ?EMPTY_SPACE,
                               ?EMPTY_SPACE, ?EMPTY_SPACE, ?EMPTY_SPACE]),
  ComputerPlayerValue = ?PLAYER_1,
  UpdatedBoard = ttt_ai:move(Board) ,

  ?assertEqual(ComputerPlayerValue, ttt_game_logic:winner(UpdatedBoard)).

will_block_a_fork_test() ->
  Board = ttt_board:new_board([?PLAYER_1,    ?EMPTY_SPACE, ?EMPTY_SPACE,
                               ?EMPTY_SPACE, ?PLAYER_2,    ?EMPTY_SPACE,
                               ?EMPTY_SPACE, ?EMPTY_SPACE, ?PLAYER_1]),
  ComputerPlayerValue = ?PLAYER_2,
  UpdatedBoard = ttt_ai:move(Board),
  IsForkBlocked = ttt_board:value_at(3, UpdatedBoard) =:= ttt_board:value_at(7, UpdatedBoard),

  ?assert(IsForkBlocked).

play_out_a_game_test() ->
  Result = play_until_completion(ttt_board:new_board()),
  IsCatsGame = ttt_game_logic:is_game_over(Result) and
    (ttt_game_logic:winner(Result) =:= none),

  ?assert(IsCatsGame).

play_until_completion(Board) -> play_until_completion(Board, 9).
play_until_completion(Board, 0) -> Board;
play_until_completion(Board, RemainingTurns) ->
  UpdatedBoard = ttt_ai:move(Board),
  play_until_completion(UpdatedBoard, RemainingTurns-1).
