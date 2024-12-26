:- include(board).

% get_state_board(+State, -Board)
get_state_board(state(Board, _, _, _), Board).

% set_state_board(+State, +NewBoard, -NewState).
set_state_board(state(_, Player, WhiteKingEaten, BlackKingEaten), NewBoard, state(NewBoard, Player, WhiteKingEaten, BlackKingEaten)).

% get_state_player(+State, -Player).
get_state_player(state(_, Player, _, _), Player).

% execute_move(+State, +Move, -NewState)
execute_move(State, step(Row-Col, Direction), NewState) :-
    get_state_player(State, whites), !,
    get_state_board(State, Board),
    get_board(Board, Row-Col, Piece),
    piece_color(Piece, Color),
    new_position(Color, Direction, Board, Row-Col, NewRow-NewCol),
    place_piece(Board, Row-Col, NewRow-NewCol, NewBoard),
    set_state_board(State, NewBoard, NewState).


% new_position(+Color, +Direction, +Board, +PiecePosition, +NewPosition)
% TODO: Maybe optimize this
new_position(_, _, Board, Position, _) :- \+ in_bounds(Board, Position) , !, fail.
new_position(Color, _, Board, Position, Position) :- 
    get_board(Board, Position, OtherPiece),
    piece_color(OtherPiece, OtherColor),
    OtherColor \= Color, !.

new_position(white, vertical, Board, Row-Col, Row-NewCol) :-
    NextCol is Col + 1,
    new_position(white, vertical, Board, Row-NextCol, Row-NewCol).
new_position(white, horizontal, Board, Row-Col, NewRow-Col) :-
    NextRow is Row + 1,
    new_position(white, horizontal, Board, NextRow-Col, NewRow-Col).
new_position(white, diagonal, Board, Row-Col, NewRow-NewCol) :-
    NextRow is Row + 1,
    NextCol is Col + 1,
    new_position(white, diagonal, Board, NextRow-NextCol, NewRow-NewCol).

new_position(black, vertical, Board, Row-Col, Row-NewCol) :-
    NextCol is Col - 1,
    new_position(white, vertical, Board, Row-NextCol, Row-NewCol).
new_position(black, horizontal, Board, Row-Col, NewRow-Col) :-
    NextRow is Row - 1,
    new_position(white, horizontal, Board, NextRow-Col, NewRow-Col).
new_position(black, diagonal, Board, Row-Col, NewRow-NewCol) :-
    NextRow is Row - 1,
    NextCol is Col - 1,
    new_position(white, diagonal, Board, NextRow-NextCol, NewRow-NewCol).
