:- use_module(library(lists)).

get_board(Board, Row-Col, Value) :-
    nth1(Col, Board, Line),
    nth1(Row, Line, Value).

set_line(Line, N, Value, NewLine) :-
    nth1(N, Line, _, LineRest),
    nth1(N, NewLine, Value, LineRest).

set_board(Board, Row-Col, Value, NewBoard) :-
    nth1(Col, Board, Line, BoardRest),
    set_line(Line, Row, Value, NewLine),
    nth1(Col, NewBoard, NewLine, BoardRest).