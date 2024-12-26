:- include(display).
:- include(logic).

% The main predicate, play/0, must be in the game.pl file and must give access to the game menu,
% which allows configuring the game type (H/H, H/PC, PC/H, or PC/PC), difficulty level(s) to be used
% by the artificial player(s), among other possible parameters, and start the game cycle.
play :-
    initial_state('', State),
    get_state_board(State, Board),
    place_piece(Board, 1-1, 1-7, NewBoard),
    set_state_board(State, NewBoard, NewState),
    execute_move(NewState, step(1-7, vertical), NewNewState),
    display_game(NewNewState).

% initial_state(+GameConfig, -GameState)
% This predicate receives a desired game configuration and
% returns the corresponding initial game state. Game configuration includes the type of each player
% and other parameters such as board size, use of optional rules, player names, or other information
% to provide more flexibility to the game. The game state describes a snapshot of the current game
% state, including board configuration (typically using list of lists with different atoms for the different
% pieces), identifies the current player (the one playing next), and possibly captured pieces and/or
% pieces yet to be played, or any other information that may be required, depending on the game.
initial_state(_GameConfig, state(Board, whites, false, false)) :- new_board(Board).


% display_game(+GameState)
% This predicate receives the current game state (including the player
% who will make the next move) and prints the game state to the terminal. Appealing and intuitive
% visualizations will be valued. Flexible game state representations and visualization predicates will
% also be valued, for instance those that work with any board size. For uniformization purposes,
% coordinates should start at (1,1) at the lower left corner
display_game(state(Board, Player, _, _)) :-
    display_board(Board),
    display_player(Player),
    nl.

% move(+GameState, +Move, -NewGameState)
% This predicate is responsible for move validation and
% execution, receiving the current game state and the move to be executed, and (if the move is valid)
% returns the new game state after the move is executed.
move(_GameState, _Move, _NewGameState) :- throw('Not implemented').

% valid_moves(+GameState, -ListOfMoves)
% This predicate receives the current game state, and
% returns a list of all possible valid moves.
valid_moves(_GameState, _ListOfMoves) :- throw('Not implemented').

% game_over(+GameState, -Winner)
% This predicate receives the current game state, and verifies
% whether the game is over, in which case it also identifies the winner (or draw). Note that this
% predicate should not print anything to the terminal.
game_over(_GameState, _Winner) :- throw('Not implemented').

% value(+GameState, +Player, -Value)
% This predicate receives the current game state and returns a
% value measuring how good/bad the current game state is to the given Player.
value(_GameState, _Player) :- throw('Not implemented').

% choose_move(+GameState, +Level, -Move)
% This predicate receives the current game state and
% returns the move chosen by the computer player. Level 1 should return a random valid move. Level
% 2 should return the best play at the time (using a greedy algorithm), considering the evaluation of
% the game state as determined by the value/3 predicate. For human players, it should interact with
% the user to read the move.
choose_move(_GameState, _Level, _Move) :- throw('Not implemented').