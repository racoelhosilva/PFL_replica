:- use_module(library(between)).
:- include(utils).

new_board([
    [white_king, white_piece, white_piece, white_piece, empty, empty, empty, empty],
    [white_piece, white_piece, white_piece, white_piece, empty, empty, empty, empty],
    [white_piece, white_piece, empty, empty, empty, empty, empty, empty],
    [white_piece, white_piece, empty, empty, empty, empty, empty, empty],
    [empty, empty, empty, empty, empty, empty, black_piece, black_piece],
    [empty, empty, empty, empty, empty, empty, black_piece, black_piece],
    [empty, empty, empty, empty, black_piece, black_piece, black_piece, black_piece],
    [empty, empty, empty, empty, black_piece, black_piece, black_piece, black_king]
]).  % Mirrored vertically so that the lower left corner matches the coordinates (1, 1)

% piece_color(?Piece, ?Color)
piece_color(white_piece, white).
piece_color(white_king, white).
piece_color(black_piece, black).
piece_color(black_king, black).
piece_color(empty, none).

% is_king(?Piece)
is_king(white_king).
is_king(black_king).

% in_bounds(+Board, +Position)
in_bounds(_Board, Row-Col) :- between(1, 8, Row), between(1, 8, Col).

% place_piece(+Board, +InitialPosition, +FinalPosition, -NewBoard)
place_piece(Board, IRow-ICol, FRow-FCol, NewBoard) :-
    get_board(Board, IRow-ICol, Piece),
    set_board(Board, FRow-FCol, Piece, TempBoard),
    set_board(TempBoard, IRow-ICol, empty, NewBoard).

% as_king(?NormalPiece, ?KingPiece).
as_king(white_piece, white_king).
as_king(black_piece, black_king).

% convert_to_king(+Board, +Position, -NewBoard)
convert_to_king(Board, Row-Col, NewBoard) :-
    get_board(Board, Row-Col, Piece),
    as_king(Piece, KingPiece),
    set_board(Board, Row-Col, KingPiece, NewBoard).
