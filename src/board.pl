:- use_module(library(between)).

:- ensure_loaded(utils).

% new_board(+Board)
% Creates a new standard board.
new_board(board([
    [white_king, white_piece, white_piece, white_piece, empty, empty, empty, empty],
    [white_piece, white_piece, white_piece, white_piece, empty, empty, empty, empty],
    [white_piece, white_piece, empty, empty, empty, empty, empty, empty],
    [white_piece, white_piece, empty, empty, empty, empty, empty, empty],
    [empty, empty, empty, empty, empty, empty, black_piece, black_piece],
    [empty, empty, empty, empty, empty, empty, black_piece, black_piece],
    [empty, empty, empty, empty, black_piece, black_piece, black_piece, black_piece],
    [empty, empty, empty, empty, black_piece, black_piece, black_piece, black_king]
], 8)).  % Mirrored vertically so that the lower left corner matches the coordinates (1, 1).

intermediate_board(board([
    [white_king,empty,white_piece,white_piece,empty,empty,empty,empty],
    [empty,empty,white_piece,white_piece,empty,empty,empty,empty],
    [empty,empty,empty,empty,empty,empty,empty,empty],
    [empty,white_piece,empty,empty,empty,black_piece,black_piece,empty],
    [empty,white_piece,empty,white_piece,white_piece,empty,black_piece,empty],
    [empty,white_piece,empty,empty,empty,empty,black_piece,empty],
    [empty,empty,black_piece,black_piece,black_piece,black_piece,empty,empty],
    [empty,empty,empty,empty,empty,black_piece,empty,black_king]
],8)).

final_board(board([
    [black_piece,empty,empty,empty,empty,empty,empty,empty],
    [empty,empty,empty,empty,empty,empty,empty,empty],
    [empty,empty,black_piece,empty,empty,empty,empty,empty],
    [empty,empty,empty,empty,empty,white_king,empty,empty],
    [empty,white_piece,empty,empty,black_piece,empty,empty,empty],
    [empty,empty,empty,black_piece,empty,empty,white_piece,empty],
    [empty,empty,empty,empty,empty,empty,black_king,empty],
    [empty,empty,empty,empty,empty,empty,white_piece,empty]
],8)).

% piece_color(?Piece, ?Color)
% Returns the color of a piece.
piece_color(white_piece, white).
piece_color(white_king, white).
piece_color(black_piece, black).
piece_color(black_king, black).
piece_color(empty, none).

% opposite_color(?Color, ?OppositeColor)
% Returns the opposite color
opposite_color(white, black).
opposite_color(black, white).

% king(?Piece)
% Returns whether a piece is a king.
king(white_king).
king(black_king).

% size(+Board, -Size)
% Returns the size of a board.
size(board(_Board, Size), Size).

% in_bounds(+Board, ?Position)
% Checks if a position is within the board.
in_bounds(board(_Board, Size), Col-Row) :- between(1, Size, Row), between(1, Size, Col).

% board_piece(+Board, +Position, -Piece)
% Returns the piece at a position on the board.
board_piece(board(Board, _Size), Position, Piece) :-
    matrix_at(Board, Position, Piece).

% place_piece(+Board, +InitialPosition, +FinalPosition, -NewBoard)
% Moves a piece from one position to another.
% If the final position is occupied, the piece is captured by replacement.
% In addition, the original position is emptied.
place_piece(board(Board, Size), InitialPosition, FinalPosition, board(NewBoard, Size)) :-
    matrix_at(Board, InitialPosition, Piece),
    set_matrix_at(Board, FinalPosition, Piece, TempBoard),
    set_matrix_at(TempBoard, InitialPosition, empty, NewBoard).

% board_piece_color(+Board, +Position, -Color)
% Returns the color of a piece at a position on the board.
board_piece_color(board(Board, _Size), Position, Color) :-
    matrix_at(Board, Position, Piece),
    piece_color(Piece, Color).

% as_king(?NormalPiece, ?KingPiece).
% Returns the king piece corresponding to a normal piece.
as_king(white_piece, white_king).
as_king(black_piece, black_king).

% convert_to_king(+Board, +Position, -NewBoard)
% Converts a piece at a position into a king.
convert_to_king(board(Board, Size), Position, board(NewBoard, Size)) :-
    matrix_at(Board, Position, Piece),
    as_king(Piece, KingPiece),
    set_matrix_at(Board, Position, KingPiece, NewBoard).
