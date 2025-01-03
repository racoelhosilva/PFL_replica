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

% input_position_aux(-Position, +Row, +Col, +Size)
% Auxiliar function to read a position from input
% Stops at line feed and skips unwanted characters
input_position_aux(Position, Row, Col, Size):-
    col_or_size_condition(Col, Size),
    peek_code(Code),
    UpperBoundCalc is 65 + Size - 1,
    UpperBound is min(UpperBoundCalc, 90),
    between(65, UpperBound, Code), !,
    get_code(_),
    NewCol is Col * 26 + (Code - 64),
    input_position_aux(Position, Row, NewCol, Size).
input_position_aux(Position, Row, Col, Size):-
    col_or_size_condition(Col, Size),
    peek_code(Code),
    UpperBoundCalc is 97 + Size - 1,
    UpperBound is min(UpperBoundCalc, 122),
    between(97, UpperBound, Code), !,
    get_code(_),
    NewCol is Col * 26 + (Code - 96),
    input_position_aux(Position, Row, NewCol, Size).
input_position_aux(Position, Row, Col, Size):-
    row_or_size_condition(Row, Size),
    peek_code(Code),
    UpperBoundCalc is 49 + Size - 1,
    UpperBound is min(UpperBoundCalc, 57),
    between(46, UpperBound, Code), !,
    get_code(_),
    NewRow is 10 * Row + (Code - 48),
    input_position_aux(Position, NewRow, Col, Size).
input_position_aux(Col-Row, Row, Col, _Size):-
    peek_code(10), !, skip_line.
input_position_aux(Position, Row, Col, Size):-
    get_code(_),
    input_position_aux(Position, Row, Col, Size).

% col_or_size_condition(+Col, +Size)
% Check if should read more column input
col_or_size_condition(0, Size).
col_or_size_condition(_Col, Size) :- Size > 26.

% row_or_size_condition(+Row, +Size)
% Check if should read more row input
row_or_size_condition(0, Size).
row_or_size_condition(_Row, Size) :- Size > 10.