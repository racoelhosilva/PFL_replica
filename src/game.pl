:- use_module(library(random)).
:- use_module(library(system)).

:- include(display).
:- include(logic).

% The main predicate, play/0, must be in the game.pl file and must give access to the game menu,
% which allows configuring the game type (H/H, H/PC, PC/H, or PC/PC), difficulty level(s) to be used
% by the artificial player(s), among other possible parameters, and start the game cycle.
play :-
    home, background(BG), background_color_rgb(BG), text(TEXT), text_color_rgb(TEXT), clear_screen,
    display_title,
    display_menu(GameConfig),
    clear_screen,
    initial_state(GameConfig, State),
    save_input_position(State),
    display_game(State),
    game_loop(State), !.

% game_loop(+State)
game_loop(State) :-
    game_over(State, Winner), !,
    display_winner(State, Winner), 
    reset, show_cursor.

game_loop(State) :-
    get_state_difficulty(State, Difficulty),
    choose_move(State, Difficulty, Move),
    move(State, Move, IntState),
    display_game(IntState),
    sleep(1),
    game_loop(IntState).

% initial_state(+GameConfig, -GameState)
% This predicate receives a desired game configuration and
% returns the corresponding initial game state. Game configuration includes the type of each player
% and other parameters such as board size, use of optional rules, player names, or other information
% to provide more flexibility to the game. The game state describes a snapshot of the current game
% state, including board configuration (typically using list of lists with different atoms for the different
% pieces), identifies the current player (the one playing next), and possibly captured pieces and/or
% pieces yet to be played, or any other information that may be required, depending on the game.
initial_state(GameConfig, state(Board, white, none, GameConfig)) :- new_board(Board).


% display_game(+GameState)
% This predicate receives the current game state (including the player
% who will make the next move) and prints the game state to the terminal. Appealing and intuitive
% visualizations will be valued. Flexible game state representations and visualization predicates will
% also be valued, for instance those that work with any board size. For uniformization purposes,
% coordinates should start at (1,1) at the lower left corner
display_game(State) :-
    home, background(BG), background_color_rgb(BG), text(TEXT), text_color_rgb(TEXT),
    hide_cursor,
    display_title,
    state_board(State, Board),
    display_board(Board),
    background(BG), background_color_rgb(BG), text(TEXT), text_color_rgb(TEXT),   
    display_player(State).
    %State = state(Board, Player, KingEaten, GameConfig),
    %value(State, white, WhiteValue), value(State, black, BlackValue), 
    %nl, write(WhiteValue), write(' '), write(BlackValue), nl,

% move(+GameState, +Move, -NewGameState)
% This predicate is responsible for move validation and
% execution, receiving the current game state and the move to be executed, and (if the move is valid)
% returns the new game state after the move is executed.
move(GameState, Move, NewGameState) :-
    execute_move(GameState, Move, IntermediateGameState),
    switch_player(IntermediateGameState, NewGameState).

% valid_moves(+GameState, -ListOfMoves)
% This predicate receives the current game state, and
% returns a list of all possible valid moves.
valid_moves(GameState, ListOfMoves) :-
    findall(Move, valid_move(GameState, Move), ListOfMoves).

% game_over(+GameState, -Winner)
% This predicate receives the current game state, and verifies
% whether the game is over, in which case it also identifies the winner (or draw). Note that this
% predicate should not print anything to the terminal.
game_over(State, Winner) :-
    king_eaten(State, OppositeColor),
    opposite_color(OppositeColor, Winner).
game_over(State, white) :-
    state_board(State, Board),
    size(Board, Size),
    board_piece(Board, Size-Size, white_king).
game_over(State, black) :-
    state_board(State, Board),
    board_piece(Board, 1-1, black_king).

% value(+GameState, +Player, -Value)
% This predicate receives the current game state and returns a
% value measuring how good/bad the current game state is to the given Player.
value(GameState, Player, Value) :- evaluate_state(Player, GameState, Value).

% choose_move(+GameState, +Level, -Move)
% This predicate receives the current game state and
% returns the move chosen by the computer player. Level 1 should return a random valid move. Level
% 2 should return the best play at the time (using a greedy algorithm), considering the evaluation of
% the game state as determined by the value/3 predicate. For human players, it should interact with
% the user to read the move.
choose_move(GameState, 0, Move) :-  % User chooses move
    get_move(GameState, Move).
choose_move(GameState, 1, Move) :-  % Choose random move
    valid_moves(GameState, Moves),
    random_member(Move, Moves).
choose_move(GameState, 2, Move) :-  % Choose best (greedy) move
    valid_moves(GameState, Moves),
    evaluate_moves(GameState, Moves, EvaluatedMoves),
    max_member(compare_keys, _-Move, EvaluatedMoves).
    