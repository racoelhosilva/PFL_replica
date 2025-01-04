:- use_module(ansi).
:- use_module(library(random)).
:- use_module(library(system)).

:- ensure_loaded(display).
:- ensure_loaded(evaluate).
:- ensure_loaded(logic).

% play
% It launches the main game menu, prompting the user to choose the gam mode, player names and
% difficulty levels (for computer players), and then starts the game loop.
play :-
    home, background(BG), background_color_rgb(BG), clear_screen,
    display_title,
    display_menu(GameConfig),
    clear_screen,
    initial_state(GameConfig, State),
    save_input_position(State),
    overlay_game(State),
    game_loop(State), !.

% game_loop(+GameState)
% This predicate is responsible for executing the game loop, consisting of
% choosing/asking the next move to execute, executing it, displaying the updated
% game state, and verifying if the game is over, continuing the cycle otherwise.
game_loop(GameState) :-
    game_over(GameState, Winner), !,
    display_winner(GameState, Winner), 
    reset, show_cursor.

game_loop(GameState) :-
    state_config(GameState, GameConfig),
    state_player(GameState, Player),
    config_difficulty(GameConfig, Player, Difficulty),
    choose_move(GameState, Difficulty, Move),
    move(GameState, Move, IntermediateGameState),
    overlay_game(IntermediateGameState),
    display_history_move(GameState, Move),
    sleep(1),
    game_loop(IntermediateGameState).

% initial_state(+GameConfig, -GameState)
% Generates the initial state of the game, and associates it with the given game
% configuration.
initial_state(GameConfig, state(Board, white, none, 0, GameConfig)) :- new_board(Board).

% display_game(+GameState)
% Displays the game state to the terminal.
display_game(State) :-
    home, background(BG), background_color_rgb(BG), clear_screen,
    overlay_game(State),
    reset, show_cursor.

% move(+GameState, +Move, -NewGameState)
% Verifies if the given move is valid, and if so, executes it, returning the
% resulting game state.
move(GameState, Move, NewGameState) :-
    valid_move(GameState, Move),
    execute_move(GameState, Move, IntermediateGameState),
    switch_player(IntermediateGameState, IntermediateGameState2),
    increase_state_move_counter(IntermediateGameState2, NewGameState).

% valid_moves(+GameState, -ListOfMoves)
% Returns a list of all valid moves for the current player in the given game state.
valid_moves(GameState, ListOfMoves) :- findall(Move, valid_move(GameState, Move), ListOfMoves).

% game_over(+GameState, -Winner)
% Verifies if the game is over, returning the winner in such case.
game_over(GameState, Winner) :- final_state(GameState, Winner).

% value(+GameState, +Player, -Value)
% Returns a value for the given game state, measuring how good or bad the
% current game state is  for the given player.
value(GameState, Player, Value) :- evaluate_state(Player, GameState, Value).

% choose_move(+GameState, +Level, -Move)
% Chooses a move by a computer player, or reads the move from a human player.
% To read the move from a human player, the level 0 should be specified. All
% other levels (1-3) are used for computer players. Level 1 corresponds to
% choosing a random valid move, level 2 chooses the move that gives the best
% immediate position (greedy), and level 3 chooses the best move according to
% the minimax algorithm.
choose_move(GameState, 0, Move) :-  % User chooses move
    get_move(GameState, Move).
choose_move(GameState, 1, Move) :-  % Choose random move
    valid_moves(GameState, Moves),
    random_member(Move, Moves).
choose_move(GameState, 2, Move) :-  % Choose best (greedy) move
    best_greedy_move(GameState, Move).
choose_move(GameState, 3, Move) :-  % Choose best (minimax) move
    best_minimax_move(GameState, 3, Move).
