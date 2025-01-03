:- use_module(library(between)).
:- use_module(library(lists)).

% input_number(-Number)
% Reads a number from the input
input_number(Number):-
    input_number_aux(Number, 0).

% input_number_aux(-Number, +Accumulator)
% Auxiliar function to read a number from input
% Stops at line feed and skips unwanted characters
input_number_aux(Number, Number) :-
    peek_code(10), !, skip_line.
input_number_aux(Number, Accumulator):- 
    peek_code(Code),
    between(48, 57, Code), !,
    get_code(_),
    NewAccumulator is 10 * Accumulator + (Code - 48),
    input_number_aux(Number, NewAccumulator).
input_number_aux(Number, Accumulator) :-
    get_code(_),
    input_number_aux(Number, Accumulator).

% input_string(-String)
% Reads a string from the input
input_string(String):-
    input_string_aux(String, []).

% input_string_aux(-String, +Accumulator)
% Auxiliar function to read a string from input
% Stops at line feed and skips unwanted characters
input_string_aux(String, Accumulator) :-
    peek_code(10), !, skip_line,
    atom_codes(String, Accumulator).
input_string_aux(String, Accumulator):- 
    peek_code(Code),
    between(32, 126, Code), !,
    get_code(_),
    append(Accumulator, [Code], NewAccumulator),
    input_string_aux(String, NewAccumulator).
input_string_aux(String, Accumulator) :-
    get_code(_),
    input_string_aux(String, Accumulator).

% input_position(-Position, +Size)
% Reads a position from the input
input_position(Position, Size):-
    input_position_aux(Position, 0, 0, Size).

% input_position_aux(-Position, +Col, +Row, +Size)
% Auxiliar function to read a position from input
% Stops at line feed and skips unwanted characters
input_position_aux(Position, Col, Row, Size) :-
    peek_code(10), !, skip_line. 
input_position_aux(Position, Col, Row, Size) :-
    can_read_col(Col, Size),
    read_col(Col, Size, NewCol).
input_position_aux(Position, Col, Row, Size) :-
    can_read_row(Row, Size),
    read_row(Row, Size, NewRow).
input_position_aux(Position, Col, Row, Size) :-
    get_code(_),
    input_position_aux(Position, Row, Col, Size).

% can_read_col(+Col, +Size)
% Checks if it should read column inputs
can_read_col(0, _Size).
can_read_col(_Col, Size) :- Size > 26.

% read_col(+Col, +Size, -NewCol)
% Reads a column from the input
read_col(Col, Size, NewCol) :-
    peek_code(Code),
    UpperBoundCalc is 65 + Size - 1,
    UpperBound is min(UpperBoundCalc, 90),
    between(65, UpperBound, Code), !,
    get_code(_),
    NewCol is Col * 26 + (Code - 64),
    input_position_aux(Position, NewCol, Row, Size).
read_col(Col, Size, NewCol) :-
    peek_code(Code),
    UpperBoundCalc is 97 + Size - 1,
    UpperBound is min(UpperBoundCalc, 122),
    between(97, UpperBound, Code), !,
    get_code(_),
    NewCol is Col * 26 + (Code - 96),
    input_position_aux(Position, NewCol, Row, Size).

% can_read_row(+Row, +Size)
% Checks if it should read row inputs
can_read_row(0, _Size).
can_read_row(_Row, Size) :- Size > 10.

% read_row(+Row, +Size, -NewRow)
% Reads a row from the input
read_row(Row, Size, NewRow) :-
    peek_code(Code),
    UpperBoundCalc is 49 + Size - 1,
    UpperBound is min(UpperBoundCalc, 57),
    between(48, UpperBound, Code), !,
    get_code(_),
    NewRow is 10 * Row + (Code - 48),
    input_position_aux(Position, Col, NewRow, Size).
