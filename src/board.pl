:- include(utils).

new_board([
    [white_king, white_piece, white_piece, white_piece, empty, empty, empty, empty],
    [white_piece, white_piece, white_piece, white_piece, empty, empty, empty, empty],
    [white_piece, white_piece, empty, empty, empty, empty, empty, empty],
    [white_piece, white_piece, empty, empty, empty, empty, empty, empty],
    [empty, empty, empty, empty, empty, empty, black_king, black_king],
    [empty, empty, empty, empty, empty, empty, black_king, black_king],
    [empty, empty, empty, empty, black_king, black_king, black_king, black_king],
    [empty, empty, empty, empty, black_king, black_king, black_king, black_king]
]).  % Mirrored vertically so that the lower left corner matches the coordinates (1, 1)

% piece_color(?Piece, ?Color)
piece_color(white_piece, white).
piece_color(white_king, white).
piece_color(black_piece, black).
piece_color(black_king, black).

% is_king(?Piece)
is_king(white_king).
is_king(black_king).

% place_piece(+Board, +InitialPosition, +FinalPosition, -NewBoard)
place_piece(Board, IRow-ICol, FRow-FCol, NewBoard) :-
    get_board(Board, IRow-ICol, Piece),
    set_board(Board, FRow-FCol, Piece, TempBoard),
    set_board(TempBoard, IRow-ICol, empty, NewBoard).
