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
    new_position(Color, Direction, Row-Col, NewRow-NewCol),
    place_piece(Board, Row-Col, NewRow-NewCol, NewBoard),
    set_state_board(State, NewBoard, NewState).

% new_position(+Color, +Direction, +PiecePosition, +NewPosition)
new_position(white, vertical, Row-Col, Row-NewCol) :-
    NewCol is Col + 1.
new_position(white, horizontal, Row-Col, NewRow-Col) :-
    NewRow is Row + 1.
new_position(white, diagonal, Row-Col, NewRow-NewCol) :-
    NewRow is Row + 1,
    NewCol is Col + 1.

new_position(black, vertical, Row-Col, Row-NewCol) :-
    NewCol is Col - 1.
new_position(black, horizontal, Row-Col, NewRow-Col) :-
    NewRow is Row - 1.
new_position(black, diagonal, Row-Col, NewRow-NewCol) :-
    NewRow is Row - 1,
    NewCol is Col - 1.
