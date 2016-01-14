# Tic-tac-toe

Written in [Erlang - OTP 18.2.1](http://www.erlang.org/download_release/32) and uses [Rebar](https://github.com/rebar/rebar) to build.

# Running the tests

Run the following command in the project folder: `./rebar compile eunit` 

# Running the application

Make sure you compile the application first (by either running the tests or running `./rebar compile`). 
In the project folder, load up erl by running the command: `erl -pa ebin`. Start the application by running: `ttt_app:start().`. (It is important to include the '.')

# Project description

The project is composed of the main game logic and an application that uses the game logic. 
The game logic includes:
  - ttt_board.erl 
  - ttt_game_logic.erl 
  - ttt_ai.erl
  - ttt_api.erl
  
The application includes:
  - ttt_app.erl 
  - ttt_input_reader.erl 
  - ttt_printer.erl  

Along with many of the .erl files are corresponding .hrl files (header files) where I define constants and macros.
