-module(ttt_ai).
-include("ttt_board.hrl").
-export([make_move/1]).
-define(DEFAULT_BEST_MOVE_VALUE, {infinity, -1}).
-define(INDICES_OF_EMPTY_SPACES(Board), ttt_board:indices_of(?EMPTY_SPACE, Board)).
-define(SCORE_TO_MINIMIZE, "ScoreToMinimize").
-define(SCORE_TO_MAXIMIZE, "ScoreToMaximize").
-define(MIN(MiniMaxMap), maps:get(?SCORE_TO_MINIMIZE, MiniMaxMap)).
-define(MAX(MiniMaxMap), maps:get(?SCORE_TO_MAXIMIZE, MiniMaxMap)).

make_move(Board) ->
  BestMove = find_best_move(Board),
  MyPlayerValue = ttt_game_logic:current_player(Board),
  ttt_board:update_board(BestMove, MyPlayerValue, Board).

find_best_move(Board) ->
  DefaultBestIndex = 0,
  find_best_move(Board, ?INDICES_OF_EMPTY_SPACES(Board), ?DEFAULT_BEST_MOVE_VALUE, DefaultBestIndex).
find_best_move(_, [], _, BestIndex) -> BestIndex;
find_best_move(Board, [CurrentIndex | RemainingIndices], BestMoveValue, BestIndex) ->
  CurrentPlayer = ttt_game_logic:current_player(Board),
  UpdatedBoard = ttt_board:update_board(CurrentIndex, CurrentPlayer, Board),
  IsGameOver = ttt_game_logic:is_game_over(UpdatedBoard),
  CurrentMoveValue = mini_max_value(CurrentPlayer, UpdatedBoard, IsGameOver),
  case is_better_move(BestMoveValue, CurrentMoveValue) of
    true ->
      find_best_move(Board, RemainingIndices, CurrentMoveValue, CurrentIndex);
    _Else ->
      find_best_move(Board, RemainingIndices, BestMoveValue, BestIndex)
  end.

mini_max_value(MyPlayerValue, Board, IsGameOver) when IsGameOver ->
  case ttt_game_logic:winner(Board) of
    none -> #{?SCORE_TO_MINIMIZE => 0,
              ?SCORE_TO_MAXIMIZE => 0};
    MyPlayerValue -> #{?SCORE_TO_MINIMIZE => 0,
                       ?SCORE_TO_MAXIMIZE => ttt_board:number_of_empty_spaces(Board) + 1};
    _Else -> #{?SCORE_TO_MINIMIZE => ttt_board:number_of_empty_spaces(Board) + 1,
               ?SCORE_TO_MAXIMIZE => 0}
  end;
mini_max_value(MyPlayerValue, Board, _) ->
  CurrentPlayer = ttt_game_logic:current_player(Board),
  PossibleMoves = lists:map(fun(Index) ->
    UpdatedBoard = ttt_board:update_board(Index, CurrentPlayer, Board),
    IsGameOver = ttt_game_logic:is_game_over(UpdatedBoard),
    mini_max_value(MyPlayerValue, UpdatedBoard, IsGameOver)
  end, ?INDICES_OF_EMPTY_SPACES(Board)),
  get_best_score(MyPlayerValue, CurrentPlayer, PossibleMoves).

is_better_move(?DEFAULT_BEST_MOVE_VALUE, _) -> true;
is_better_move(OldMove, NewMove) -> is_better_move(OldMove, NewMove, true).
is_better_move(OldMove, NewMove, IsMyTurn) when IsMyTurn ->
  HasSmallerMin = ?MIN(NewMove) < ?MIN(OldMove),
  HasSameMinBiggerMax = (?MIN(NewMove) =:= ?MIN(OldMove)) and (?MAX(NewMove) > ?MAX(OldMove)),
  HasSmallerMin or HasSameMinBiggerMax;
is_better_move(OldMove, NewMove, _) ->
  is_better_move(#{?SCORE_TO_MINIMIZE => ?MAX(OldMove), ?SCORE_TO_MAXIMIZE => ?MIN(OldMove)},
                 #{?SCORE_TO_MINIMIZE => ?MAX(NewMove), ?SCORE_TO_MAXIMIZE => ?MIN(NewMove)}).

get_best_score(MyPlayerValue, CurrentPlayer, [CurrentMove | RemainingMoves]) ->
  get_best_score(MyPlayerValue, CurrentPlayer, RemainingMoves, CurrentMove).
get_best_score(_, _, [], BestMove) -> BestMove;
get_best_score(MyPlayerValue, CurrentPlayer, [CurrentMove | RemainingMoves], BestMove) ->
  case is_better_move(BestMove, CurrentMove, MyPlayerValue =:= CurrentPlayer) of
    true ->
      get_best_score(MyPlayerValue, CurrentPlayer, RemainingMoves, CurrentMove);
    _Else ->
      get_best_score(MyPlayerValue, CurrentPlayer, RemainingMoves, BestMove)
  end.
