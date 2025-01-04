:- use_module(library(between)).
:- use_module(library(lists)).

% read_number(-Number)
% Reads a number from the input.
read_number(Number):-
    read_number_aux(Number, 0).

% read_number_aux(-Number, +Accumulator)
% Auxiliar predicate to read a number from input.
% Stops at line feed and skips unwanted characters.
read_number_aux(Number, Number) :-
    peek_code(10), !, skip_line.
read_number_aux(Number, Accumulator):- 
    peek_code(Code),
    between(48, 57, Code), !,
    get_code(_),
    NewAccumulator is 10 * Accumulator + (Code - 48),
    read_number_aux(Number, NewAccumulator).
read_number_aux(Number, Accumulator) :-
    get_code(_),
    read_number_aux(Number, Accumulator).

% read_string(-String)
% Reads a string from the input.
read_string(String):-
    read_string_aux(String, []).

% read_string_aux(-String, +Accumulator)
% Auxiliar predicate to read a string from input.
% Stops at line feed and skips unwanted characters.
read_string_aux(String, Accumulator) :-
    peek_code(10), !, skip_line,
    atom_codes(String, Accumulator).
read_string_aux(String, Accumulator):- 
    peek_code(Code),
    between(32, 126, Code), !,
    get_code(_),
    append(Accumulator, [Code], NewAccumulator),
    read_string_aux(String, NewAccumulator).
read_string_aux(String, Accumulator) :-
    get_code(_),
    read_string_aux(String, Accumulator).

% read_position(-Position, +Size)
% Reads a position from the input.
% Letters are associated with columns and numbers are associated with rows.
% If the row or column only needs one character, the first valid character is considered.
% If they need multiple characters, all of them are considered in concatenation.
% Letters can be uppercase or lowercase and any order of rows/columns is considered equivalent.
% Unwanted characters are skipped until the end of the line.
read_position(Position, Size):-
    read_position_aux(Position, 0, 0, Size).

% read_position_aux(-Position, +Col, +Row, +Size)
% Auxiliar predicate to read a position from input.
% Stops at line feed and skips unwanted characters.
read_position_aux(Col-Row, Col, Row, _Size) :-
    peek_code(10), !, skip_line. 
read_position_aux(Position, Col, Row, Size) :-
    can_read_col(Col, Size),
    read_col(Col, Size, NewCol),
    read_position_aux(Position, NewCol, Row, Size).
read_position_aux(Position, Col, Row, Size) :-
    can_read_row(Row, Size),
    read_row(Row, Size, NewRow),
    read_position_aux(Position, Col, NewRow, Size).
read_position_aux(Position, Col, Row, Size) :-
    get_code(_),
    read_position_aux(Position, Col, Row, Size).

% can_read_col(+Col, +Size)
% Checks if it should read column inputs.
can_read_col(0, _Size).
can_read_col(_Col, Size) :- Size > 26.

% read_col(+Col, +Size, -NewCol)
% Reads a column from the input.
read_col(Col, Size, NewCol) :-
    peek_code(Code),
    UpperBoundCalc is 65 + Size - 1,
    UpperBound is min(UpperBoundCalc, 90),
    between(65, UpperBound, Code), !,
    get_code(_),
    NewCol is Col * 26 + (Code - 64).
read_col(Col, Size, NewCol) :-
    peek_code(Code),
    UpperBoundCalc is 97 + Size - 1,
    UpperBound is min(UpperBoundCalc, 122),
    between(97, UpperBound, Code), !,
    get_code(_),
    NewCol is Col * 26 + (Code - 96).

% can_read_row(+Row, +Size)
% Checks if it should read row inputs.
can_read_row(0, _Size).
can_read_row(_Row, Size) :- Size > 10.

% read_row(+Row, +Size, -NewRow)
% Reads a row from the input.
read_row(Row, Size, NewRow) :-
    peek_code(Code),
    UpperBoundCalc is 49 + Size - 1,
    UpperBound is min(UpperBoundCalc, 57),
    between(48, UpperBound, Code), !,
    get_code(_),
    NewRow is 10 * Row + (Code - 48).
