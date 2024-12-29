:- use_module(library(lists)).

% get_matrix(+Matrix, +Position, -Value)
get_matrix(Matrix, Col-Row, Value) :-
    nth1(Row, Matrix, Line),
    nth1(Col, Line, Value).

% set_line(+Line, +Index, +Value, -NewLine)
set_line(Line, Index, Value, NewLine) :-
    nth1(Index, Line, _OldValue, LineRest),
    nth1(Index, NewLine, Value, LineRest).

% set_matrix(+Matrix, +Position, +Value, -NewMatrix)
set_matrix(Matrix, Col-Row, Value, NewMatrix) :-
    nth1(Row, Matrix, Line, MatrixRest),
    set_line(Line, Col, Value, NewLine),
    nth1(Row, NewMatrix, NewLine, MatrixRest).

% get_max_key(+EntryList, ?MaxEntry)
get_max_key([FirstEntry | ListTail], MaxEntry) :- get_max_key(ListTail, FirstEntry, MaxEntry).

% get_max_key(+EntryList, +Accumulator, ?MaxEntry)
get_max_key([], Accumulator, Accumulator).
get_max_key([Key-Value | ListTail], CurrKey-_, MaxEntry) :-
    Key > CurrKey, !,
    get_max_key(ListTail, Key-Value, MaxEntry).
get_max_key([_ | ListTail], CurrEntry, MaxEntry) :-
    get_max_key(ListTail, CurrEntry, MaxEntry).
