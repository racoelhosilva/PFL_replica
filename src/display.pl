:- use_module(library(lists)).

read_number(X):-
    read_number_aux(X, 0).

read_number_aux(X, Acc):- 
    get_code(C),
    between(48, 57, C), !,
    NextAcc is 10 * Acc + (C - 48),
    read_number_aux(X, NextAcc).
read_number_aux(X, X).

get_line(Result, Acc):-
    get_char(Char),
    Char \= '\n',
    append(Acc, [Char], Acc1),
    get_line(Result, Acc1).
get_line(Result, Acc):-
    atom_chars(Result, Acc).

get_menu_option(Min, Max, Context, Value):-
    format('~a between ~d and ~d: ', [Context, Min, Max]),
    repeat,
    read_number(Value),
    between(Min, Max, Value), !.

get_name(Context, Player):-
    format('Name for ~a: ', [Context]),
    get_line(Player, []).

get_gamemode(GameMode):-
    write('Please select the game mode:'), nl,
    write('\t1. Human vs. Human'), nl,
    write('\t2. Human vs. Computer'), nl,
    write('\t3. Computer vs. Human'), nl,
    write('\t4. Computer vs. Computer'), nl,
    get_menu_option(1, 4, 'Game mode', GameMode).

get_difficulty(Difficulty):-
    write('Please select the difficulty level:'), nl,
    write('\t1. Random'), nl,
    write('\t2. Greedy'), nl,
    get_menu_option(1, 2, 'Difficulty level', Difficulty).

display_menu(GameConfig) :- 
    write('Welcome to Replica!'), nl,
    get_gamemode(GameMode),
    display_options(GameMode, Player1, Player2), !,
    GameConfig = [GameMode, Player1, Player2].

display_options(1, [Name1, 0], [Name2, 0]) :-
    get_name('Player 1', Name1),
    get_name('Player 2', Name2).
display_options(2, [Name1, 0], [Name2, Difficulty]) :-
    get_name('Player 1', Name1),
    get_name('Computer', Name2),
    get_difficulty(Difficulty).
display_options(3, [Name1, Difficulty], [Name2, 0]) :-
    get_name('Computer', Name1),
    get_difficulty(Difficulty),
    get_name('Player 2', Name2).
display_options(4, [Name1, Difficulty1], [Name2, Difficulty2]) :-
    get_name('Computer 1', Name1),
    get_difficulty(Difficulty1),
    get_name('Computer 2', Name2),
    get_difficulty(Difficulty2).

display_cell(empty) :- write(.).
display_cell(white_piece) :- write(w).
display_cell(white_king) :- write(+).
display_cell(black_piece) :- write(b).
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
display_board(_Board).

display_player(white) :- write('Whites\' turn: '), nl.
display_player(black) :- write('Blacks\' turn: '), nl.   % Changing color according to the pieces would be nice :)
