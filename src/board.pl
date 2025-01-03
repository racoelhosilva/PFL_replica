:- use_module(library(between)).
:- include(utils).

% new_board(+Board)
new_board(board([
    [white_king, white_piece, white_piece, white_piece, empty, empty, empty, empty],
    [white_piece, white_piece, white_piece, white_piece, empty, empty, empty, empty],
    [white_piece, white_piece, empty, empty, empty, empty, empty, empty],
    [white_piece, white_piece, empty, empty, empty, empty, empty, empty],
    [empty, empty, empty, empty, empty, empty, black_piece, black_piece],
    [empty, empty, empty, empty, empty, empty, black_piece, black_piece],
    [empty, empty, empty, empty, black_piece, black_piece, black_piece, black_piece],
    [empty, empty, empty, empty, black_piece, black_piece, black_piece, black_king]
], 8)).  % Mirrored vertically so that the lower left corner matches the coordinates (1, 1)

% piece_color(?Piece, ?Color)
piece_color(white_piece, white).
piece_color(white_king, white).
piece_color(black_piece, black).
piece_color(black_king, black).
piece_color(empty, none).

% opposite_color(?Color, ?OppositeColor)
opposite_color(white, black).
opposite_color(black, white).

% king(?Piece)
king(white_king).
king(black_king).

% size(+Board, -Size)
size(board(_Board, Size), Size).

% in_bounds(+Board, +Position)
in_bounds(board(_Board, Size), Col-Row) :- between(1, Size, Row), between(1, Size, Col).

% board_piece(+Board, +Position, -Piece)
board_piece(board(Board, _Size), Position, Piece) :-
    matrix_at(Board, Position, Piece).

% place_piece(+Board, +InitialPosition, +FinalPosition, -NewBoard)
place_piece(board(Board, Size), InitialPosition, FinalPosition, board(NewBoard, Size)) :-
    matrix_at(Board, InitialPosition, Piece),
    set_matrix_at(Board, FinalPosition, Piece, TempBoard),
    set_matrix_at(TempBoard, InitialPosition, empty, NewBoard).

% board_piece_color(+Board, +Position, -Color)
board_piece_color(board(Board, _Size), Position, Color) :-
    matrix_at(Board, Position, Piece),
    piece_color(Piece, Color).

% as_king(?NormalPiece, ?KingPiece).
as_king(white_piece, white_king).
as_king(black_piece, black_king).

% convert_to_king(+Board, +Position, -NewBoard)
convert_to_king(board(Board, Size), Position, board(NewBoard, Size)) :-
    matrix_at(Board, Position, Piece),
    as_king(Piece, KingPiece),
    set_matrix_at(Board, Position, KingPiece, NewBoard).
