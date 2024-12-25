:- include(board).
:- include(display).

play.

% initial_state(+GameConfig, -GameState)
initial_state(_GameConfig, _GameState) :- throw('Not implemented').

% display_game(+GameState)
display_game(_GameState) :- throw('Not implemented').

% move(+GameState, +Move, -NewGameState)
move(_GameState, _Move, _NewGameState) :- throw('Not implemented').

% valid_moves(+GameState, -ListOfMoves)
valid_moves(_GameState, _ListOfMoves) :- throw('Not implemented').

% game_over(+GameState, -Winner)
game_over(_GameState, _Winner) :- throw('Not implemented').

% value(+GameState, +Player, -Value)
value(_GameState, _Player) :- throw('Not implemented').

% choose_move(+GameState, +Level, -Move)
choose_move(_GameState, _Level, _Move) :- throw('Not implemented').