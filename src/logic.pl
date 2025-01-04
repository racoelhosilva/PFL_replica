:- ensure_loaded(board).

% state_board(+State, -Board)
% Returns the board of a state.
state_board(state(Board, _Player, _KingEaten, _MoveCounter, _GameConfig), Board).

% set_state_board(+State, +Board, -NewState)
% Sets the board of a state.
set_state_board(state(_OldBoard, Player, KingEaten, MoveCounter, GameConfig), Board, state(Board, Player, KingEaten, MoveCounter, GameConfig)).

% state_player(+State, -Player)
% Returns the player of a state.
state_player(state(_Board, Player, _KingEaten, _MoveCounter, _GameConfig), Player).

% set_state_player(+State, +Player, -NewState)
% Sets the state's player.
set_state_player(state(Board, _OldPlayer, KingEaten, MoveCounter, GameConfig), Player, state(Board, Player, KingEaten, MoveCounter, GameConfig)).

% switch_player(?State, ?NewState)
% Switches the state's player
switch_player(State, NewState) :-
    state_player(State, Player),
    opposite_color(Player, OtherPlayer),
    set_state_player(State, OtherPlayer, NewState).

% king_eaten(+State, -KingEaten)
% Returns the color of the king that was previously eaten.
% If no king was eaten, returns none.
% This predicate is used to determine one of the winning conditions.
king_eaten(state(_Board, _Player, KingEaten, _MoveCounter, _GameConfig), KingEaten).

% set_king_eaten(+State, +KingEaten, -NewState)
% Sets the color of the king that was eaten.
set_king_eaten(state(Board, Player, _OldKingEaten, MoveCounter, GameConfig), KingEaten, state(Board, Player, KingEaten, MoveCounter, GameConfig)).

% state_move_counter(+State, -MoveCounter)
% Returns the move counter of a state.
state_move_counter(state(_, _, _, MoveCounter, _), MoveCounter).

% increase_state_move_counter(+State, -NewState)
% Increases the move counter of a state.
increase_state_move_counter(state(Board, Player, KingEaten, MoveCounter, GameConfig), state(Board, Player, KingEaten, NextMove, GameConfig)) :-
    NextMove is MoveCounter + 1.

% state_config(+State, -GameConfig)
% Returns the game configuration, associated with a game state.
state_config(state(_Board, _Player, _KingEaten, _MoveCounter, GameConfig), GameConfig).

% set_state_config(+State, +GameConfig, -NewState)
% Sets the game configuration of a state.
set_state_config(state(Board, Player, KingEaten, MoveCounter, _OldGameConfig), GameConfig, state(Board, Player, KingEaten, MoveCounter, GameConfig)).

% final_state(+State, -Winner)
% Verifies if a state is a final state, returning the winner in such case.
final_state(State, Winner) :-
    king_eaten(State, OppositeColor),
    opposite_color(OppositeColor, Winner).
final_state(State, white) :-
    state_board(State, Board),
    size(Board, Size),
    board_piece(Board, Size-Size, white_king).
final_state(State, black) :-
    state_board(State, Board),
    board_piece(Board, 1-1, black_king).

% verify_and_set_king_eaten(+Piece, +State, -NewState)
% Verifies if a replaced piece is a king and sets the king eaten accordingly.
verify_and_set_king_eaten(Piece, State, State) :- \+ king(Piece), !.
verify_and_set_king_eaten(Piece, State, NewState) :-
    piece_color(Piece, Color),
    set_king_eaten(State, Color, NewState).

% execute_move(+State, +Move, -NewState)
% Executes a move in a game state.
execute_move(State, step(Pos, Direction), NewState) :-
    state_board(State, Board),
    state_player(State, Player),
    new_position(Player, Direction, Pos, Board, NewPos),
    board_piece(Board, NewPos, ReplacedPiece),
    place_piece(Board, Pos, NewPos, NewBoard),
    set_state_board(State, NewBoard, TempState),
    verify_and_set_king_eaten(ReplacedPiece, TempState, NewState).

execute_move(State, transform(Pos), NewState) :-
    state_board(State, Board),
    convert_to_king(Board, Pos, NewBoard),
    set_state_board(State, NewBoard, NewState).

% new_position(+Color, +Direction, +Position, +Board, -NewPosition)
% Returns the new position of a piece after moving in a certain direction.
% Respecting the game rules, the new position is the first position in the
% direction of movement not occupied by a friendly piece. If no such position
% exists, the predicate fails.
% To determine such position, the predicate tries to advance the current
% position, one step at a time, until the board bounds are reached or a position
% not occupied by a piece of the same color is found, using tail recursion.
new_position(_Color, _Direction, Position, Board, _NewPosition) :- \+ in_bounds(Board, Position), !, fail.
new_position(Color, _Direction, Position, Board, Position) :-
    board_piece_color(Board, Position, OtherColor),
    OtherColor \= Color, !.

new_position(white, vertical, Col-Row, Board, Col-NewRow) :-
    NextRow is Row + 1,
    new_position(white, vertical, Col-NextRow, Board, Col-NewRow).
new_position(white, horizontal, Col-Row, Board, NewCol-Row) :-
    NextCol is Col + 1,
    new_position(white, horizontal, NextCol-Row, Board, NewCol-Row).
new_position(white, diagonal, Col-Row, Board, NewCol-NewRow) :-
    NextCol is Col + 1,
    NextRow is Row + 1,
    new_position(white, diagonal, NextCol-NextRow, Board, NewCol-NewRow).

new_position(black, vertical, Col-Row, Board, Col-NewRow) :-
    NextRow is Row - 1,
    new_position(black, vertical, Col-NextRow, Board, Col-NewRow).
new_position(black, horizontal, Col-Row, Board, NewCol-Row):-
    NextCol is Col - 1,
    new_position(black, horizontal, NextCol-Row, Board, NewCol-Row).
new_position(black, diagonal, Col-Row, Board, NewCol-NewRow) :-
    NextCol is Col - 1,
    NextRow is Row - 1,
    new_position(black, diagonal, NextCol-NextRow, Board, NewCol-NewRow).

% player_can_move_at(+Player, +Position, +Board)
% Checks if a player can move at a certain position.
% The predicate checks if the position is within the board bounds, if it is
% currently occupied by a piece and if the piece is of the player's color.
player_can_move_at(Player, Position, Board) :-
    in_bounds(Board, Position),
    board_piece_color(Board, Position, Player).

% blocks_sight(?Color, ?Piece)
% Checks if a piece blocks the sight of a king. According to the game rules,
% the only pieces that block the sight of a king are pieces of the opposite
% color.
blocks_sight(Color, Piece) :-
    opposite_color(Color, OppositeColor),
    piece_color(Piece, OppositeColor).

% seen_by_king(?Color, +Board, +Position)
% Checks if a piece is seen by the king of a certain color.
seen_by_king(Color, Board, Position) :- seen_by_king(Color, _Direction, Board, Position), !.

% seen_by_king(?Color, ?Direction, +Board, +Position)
% Auxiliary predicate for seen_by_king/3, which checks if a piece is seen by
% the king of a certain color in a certain direction.
% The strategy used on this predicate is very similar to the one used in new_position/5,
% going through each position in the respective direction backwards, until the board bounds
% are reached or a piece that blocks the sight of the king is found, using tail recursion.
seen_by_king(_Color, _Direction, Board, Position) :- \+ in_bounds(Board, Position), !, fail.

seen_by_king(Color, _Direction, Board, Position) :- 
    board_piece(Board, Position, Piece),
    blocks_sight(Color, Piece), !, fail.
seen_by_king(_Color, _Direction, Board, Position) :- 
    board_piece(Board, Position, Piece),
    king(Piece), !.

seen_by_king(white, vertical, Board, Col-Row) :-
    NextRow is Row - 1,
    seen_by_king(white, vertical, Board, Col-NextRow).
seen_by_king(white, horizontal, Board, Col-Row) :-
    NextCol is Col - 1,
    seen_by_king(white, horizontal, Board, NextCol-Row).
seen_by_king(white, diagonal, Board, Col-Row) :-
    NextCol is Col - 1,
    NextRow is Row - 1,
    seen_by_king(white, diagonal, Board, NextCol-NextRow).

seen_by_king(black, vertical, Board, Col-Row) :-
    NextRow is Row + 1,
    seen_by_king(black, vertical, Board, Col-NextRow).
seen_by_king(black, horizontal, Board, Col-Row) :-
    NextCol is Col + 1,
    seen_by_king(black, horizontal, Board, NextCol-Row).
seen_by_king(black, diagonal, Board, Col-Row) :-
    NextCol is Col + 1,
    NextRow is Row + 1,
    seen_by_king(black, diagonal, Board, NextCol-NextRow).

% valid_move(+State, ?Move)
% Checks if a move is valid in a certain game state, with respect to the game
% rules and the current player.
valid_move(State, step(Position, Direction)) :-
    state_board(State, Board),
    state_player(State, Player),
    player_can_move_at(Player, Position, Board),
    new_position(Player, Direction, Position, Board, _NewPosition).

valid_move(State, transform(Position)) :-
    state_board(State, Board),
    state_player(State, Player),
    player_can_move_at(Player, Position, Board),
    board_piece(Board, Position, Piece),
    \+ king(Piece),
    seen_by_king(Player, Board, Position).

% valid_piece_moves(+State, +Position, -Moves)
% Returns the valid moves for the piece at a given position in a certain game
% state.
valid_piece_moves(State, Position, Moves) :-
    findall(Move, valid_piece_move(State, Position, Move), Moves).

% valid_piece_move(+State, +Position, ?Move)
% Checks if a move is valid for a piece at a given position in a certain game
% state.
valid_piece_move(State, Position, step(Position, Direction)) :-
    valid_move(State, step(Position, Direction)).
valid_piece_move(State, Position, transform(Position)) :-
    valid_move(State, transform(Position)).
