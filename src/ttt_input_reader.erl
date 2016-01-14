-module(ttt_input_reader).
-include("ttt_app.hrl").
-export([get_input_value/1,
         get_legal_player_move/1]).

get_input_value(InputType) -> get_input_value(InputType, none).
get_input_value(InputType, none) ->
  InputString = io:get_line(InputType#input_type.prompt_message),
  case string:to_integer(InputString) of
    {InputValue, "\n"} -> get_input_value(InputType, InputValue);
    _ ->
      io:format(InputType#input_type.invalid_message, [InputType#input_type.lower_bound,
                                                       InputType#input_type.upper_bound]),
      get_input_value(InputType, none)
  end;
get_input_value(InputType, InputValue) when
  (InputValue >= InputType#input_type.lower_bound) and
  (InputValue =< InputType#input_type.upper_bound) -> InputValue;
get_input_value(InputType, InputValue) ->
  io:format(InputType#input_type.out_of_bounds_message, [InputValue,
                                                         InputType#input_type.lower_bound,
                                                         InputType#input_type.upper_bound]),
  get_input_value(InputType, none).

get_legal_player_move(GameState) ->
  PlayerMove = get_input_value(?PLAYER_MOVE(GameState)),
  case ttt_api:is_legal_move(PlayerMove, GameState) of
    true -> PlayerMove;
    _ ->
      io:format(?PLAYER_MOVE(GameState)#input_type.illegal_message),
      get_legal_player_move(GameState)
  end.
