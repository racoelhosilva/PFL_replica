:- ensure_loaded(board).

% state_board(+State, -Board)
state_board(state(Board, _Player, _KingEaten, _MoveCounter, _GameConfig), Board).

% set_state_board(+State, +Board, -NewState)
set_state_board(state(_OldBoard, Player, KingEaten, MoveCounter, GameConfig), Board, state(Board, Player, KingEaten, MoveCounter, GameConfig)).

% state_player(+State, -Player)
state_player(state(_Board, Player, _KingEaten, _MoveCounter, _GameConfig), Player).

% set_state_player(+State, +Player, -NewState)
set_state_player(state(Board, _OldPlayer, KingEaten, MoveCounter, GameConfig), Player, state(Board, Player, KingEaten, MoveCounter, GameConfig)).

% switch_player(?State, ?NewState)
switch_player(State, NewState) :-
    state_player(State, Player),
    opposite_color(Player, OtherPlayer),
    set_state_player(State, OtherPlayer, NewState).

% king_eaten(+State, -KingEaten)
king_eaten(state(_Board, _Player, KingEaten, _MoveCounter, _GameConfig), KingEaten).

% set_king_eaten(+State, +KingEaten, -NewState)
set_king_eaten(state(Board, Player, _OldKingEaten, MoveCounter, GameConfig), KingEaten, state(Board, Player, KingEaten, MoveCounter, GameConfig)).

% state_move(+State, -MoveCounter)
state_move(state(_, _, _, MoveCounter, _), MoveCounter).

% increase_state_move(+State, -NewState)
increase_state_move(state(Board, Player, KingEaten, MoveCounter, GameConfig), state(Board, Player, KingEaten, NextMove, GameConfig)) :-
    NextMove is MoveCounter + 1.

% state_config(+State, -GameConfig)
state_config(state(_Board, _Player, _KingEaten, _MoveCounter, GameConfig), GameConfig).

% set_state_config(+State, +GameConfig, -NewState)
set_state_config(state(Board, Player, KingEaten, MoveCounter, _OldGameConfig), GameConfig, state(Board, Player, KingEaten, MoveCounter, GameConfig)).

% final_state(+State, -Winner)
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
verify_and_set_king_eaten(Piece, State, State) :- \+ king(Piece), !.
verify_and_set_king_eaten(Piece, State, NewState) :-
    piece_color(Piece, Color),
    set_king_eaten(State, Color, NewState).

% execute_move(+State, +Move, -NewState)
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

% new_position(+Color, +Direction, +PiecePosition, +Board, -NewPosition)
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
player_can_move_at(Player, Position, Board) :-
    in_bounds(Board, Position),
    board_piece_color(Board, Position, Player).

% blocks_sight(?Color, ?Piece)
blocks_sight(Color, Piece) :-
    opposite_color(Color, OppositeColor),
    piece_color(Piece, OppositeColor).

seen_by_king(Color, Board, Position) :- seen_by_king(Color, _Direction, Board, Position), !.

% seen_by_king(?Color, ?Direction, +Board, +Position)
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

% valid_move(+State, -Move)
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
    seen_by_king(Player, Board, Position).  % DOuble negation used to prevent unification of _Direction

% valid_piece_moves(+State, +Piece, -Moves)
valid_piece_moves(State, Piece, Moves) :-
    findall(Move, valid_piece_move(State, Piece, Move), Moves).

% valid_piece_move(+State, +Piece, -Move)
valid_piece_move(State, Piece, step(Piece, Direction)) :-
    valid_move(State, step(Piece, Direction)).
valid_piece_move(State, Piece, transform(Piece)) :-
    valid_move(State, transform(Piece)).
