:- use_module(library(lists)).

% matrix_at(+Matrix, +Position, -Value)
matrix_at(Matrix, Col-Row, Value) :-
    nth1(Row, Matrix, Line),
    nth1(Col, Line, Value).

% set_line_at(+Line, +Index, +Value, -NewLine)
set_line_at(Line, Index, Value, NewLine) :-
    nth1(Index, Line, _OldValue, LineRest),
    nth1(Index, NewLine, Value, LineRest).

% set_matrix_at(+Matrix, +Position, +Value, -NewMatrix)
set_matrix_at(Matrix, Col-Row, Value, NewMatrix) :-
    nth1(Row, Matrix, Line, MatrixRest),
    set_line_at(Line, Col, Value, NewLine),
    nth1(Row, NewMatrix, NewLine, MatrixRest).

% compare_keys(+Entry1, +Entry2)
compare_keys(Key1-_, Key2-_) :- Key1 =< Key2.
