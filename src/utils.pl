:- use_module(library(lists)).

get_board(Board, Row-Col, Value) :-
    nth0(Col, Board, Line),
    nth0(Row, Line, Value).

set_line(Line, N, Value, NewLine) :-
    nth0(N, Line, _, LineRest),
    nth0(N, NewLine, Value, LineRest).

set_board(Board, Row-Col, Value, NewBoard) :-
    nth0(Col, Board, Line, BoardRest),
    set_line(Line, Row, Value, NewLine),
    nth0(Col, NewBoard, NewLine, BoardRest).