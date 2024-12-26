:- include(utils).

new_board([
    [white_king, white, white, white, empty, empty, empty, empty],
    [white, white, white, white, empty, empty, empty, empty],
    [white, white, empty, empty, empty, empty, empty, empty],
    [white, white, empty, empty, empty, empty, empty, empty],
    [empty, empty, empty, empty, empty, empty, black, black],
    [empty, empty, empty, empty, empty, empty, black, black],
    [empty, empty, empty, empty, black, black, black, black],
    [empty, empty, empty, empty, black, black, black, black_king]
]).  % Mirrored vertically so that the lower left corner matches the coordinates (1, 1)

put_piece(Board, IRow-ICol, FRow-FCol, NewBoard) :-
    get_board(Board, IRow-ICol, Piece),
    set_board(Board, FRow-FCol, Piece, TempBoard),
    set_board(TempBoard, IRow-ICol, empty, NewBoard).

