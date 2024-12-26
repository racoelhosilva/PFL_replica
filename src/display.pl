:- use_module(library(lists)).

display_cell(empty) :- write(.).
display_cell(white) :- write(w).
display_cell(white_king) :- write(+).
display_cell(black) :- write(b).
display_cell(black_king) :- write(*).

display_line(Line) :- 
    member(Cell, Line),
    display_cell(Cell),
    fail.
display_line(_Line) :- nl.

display_board(Board) :- 
    reverse(Board, RevBoard),
    member(Line, RevBoard),
    display_line(Line),
    fail.
display_board(_Board) :- nl.


