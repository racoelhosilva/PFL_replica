:- include(utils).

new_board([
    [empty, empty, empty, empty, black, black, black, black_king],
    [empty, empty, empty, empty, black, black, black, black],
    [empty, empty, empty, empty, empty, empty, black, black],
    [empty, empty, empty, empty, empty, empty, black, black],
    [white, white, empty, empty, empty, empty, empty, empty],
    [white, white, empty, empty, empty, empty, empty, empty],
    [white, white, white, white, empty, empty, empty, empty],
    [white_king, white, white, white, empty, empty, empty, empty]
]).

put_piece(Board, IRow-ICol, FRow-FCol, NewBoard) :-
    get_board(Board, IRow-ICol, Piece),
    set_board(Board, FRow-FCol, Piece, TempBoard),
    set_board(TempBoard, IRow-ICol, empty, NewBoard).

