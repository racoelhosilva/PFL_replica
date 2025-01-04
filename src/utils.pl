:- use_module(library(lists)).

% matrix_at(+Matrix, +Position, -Value)
% Gets the value of a matrix at a given position.
matrix_at(Matrix, Col-Row, Value) :-
    nth1(Row, Matrix, Line),
    nth1(Col, Line, Value).

% set_line_at(+Line, +Index, +Value, -NewLine)
% Sets a value at a given index in a line.
set_line_at(Line, Index, Value, NewLine) :-
    nth1(Index, Line, _OldValue, LineRest),
    nth1(Index, NewLine, Value, LineRest).

% set_matrix_at(+Matrix, +Position, +Value, -NewMatrix)
% Sets a value at a given position in a matrix.
set_matrix_at(Matrix, Col-Row, Value, NewMatrix) :-
    nth1(Row, Matrix, Line, MatrixRest),
    set_line_at(Line, Col, Value, NewLine),
    nth1(Row, NewMatrix, NewLine, MatrixRest).

% compare_keys(+Entry1, +Entry2)
% Compares two entries by their keys.
compare_keys(Key1-_, Key2-_) :- Key1 =< Key2.

% max_key_set(+EntryList, -MaxKeyValues)
% Returns all the values of the entry list with the maximum key.
max_key_set(EntryList, MaxKeyValues) :-
    findall(Key-Values, (
        bagof(Value, member(Key-Value, EntryList), Values)
    ), KeyValuesList),
    max_member(compare_keys, _-MaxKeyValues, KeyValuesList).
