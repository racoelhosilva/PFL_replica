:- use_module(library(lists)).

:- include(input).

get_option(Min, Max, Context, Value):-
    format('~a between ~d and ~d: ', [Context, Min, Max]),
    input_number(Value),
    between(Min, Max, Value), !.
get_option(Min, Max, Context, Value):-
    write('Invalid option! '),
    get_option(Min, Max, Context, Value).

get_name(Context, Player):-
    format('Name for ~a: ', [Context]),
    input_string(Player).

get_position(Position, Size):-
    write('Choose the position of the piece to move: '),
    input_position(Position, Size),
    Position = Row-Col,
    between(1, Size, Row),
    between(1, Size, Col), !.
get_position(Position, Size):-
    write('Invalid position! '),
    get_position(Position, Size).

get_move(State, Move) :-
    state_board(State, Board),
    size(Board, Size),
    get_position(Position, Size),
    valid_piece_moves(State, Position, PieceMoves),
    length(PieceMoves, Length),
    Length > 0, !,
    write('Valid moves: '), write(PieceMoves),nl,
    get_option(1, Length, 'Move', Index),
    nth1(Index, PieceMoves, Move), !,
    write('You selected: '), write(Move), nl.
get_move(State, Move) :-
    write('No piece in that position can move! '),
    get_move(State, Move).

get_gamemode(GameMode):-
    write('Please select the game mode:'), nl,
    write('\t1. Human vs. Human'), nl,
    write('\t2. Human vs. Computer'), nl,
    write('\t3. Computer vs. Human'), nl,
    write('\t4. Computer vs. Computer'), nl,
    get_option(1, 4, 'Game mode', GameMode).

get_difficulty(Difficulty):-
    write('Please select the difficulty level:'), nl,
    write('\t1. Random'), nl,
    write('\t2. Greedy'), nl,
    get_option(1, 2, 'Difficulty level', Difficulty).

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

display_cell(empty) :- write('|'), write(.).
display_cell(white_piece) :- write('|'), write(w).
display_cell(white_king) :- write('|'), write(+).
display_cell(black_piece) :- write('|'), write(b).
display_cell(black_king) :- write('|'), write(*).

display_line(Line, _LineNumber) :- 
    write('-----------------'), nl,
    member(Cell, Line),
    display_cell(Cell),
    fail.
display_line(_Line, LineNumber) :- write('| '), write(LineNumber), nl.

display_board_lines([], _).
display_board_lines([Line|Rest], LineNumber) :- 
    display_line(Line, LineNumber),
    NewLineNumber is LineNumber - 1,
    display_board_lines(Rest, NewLineNumber).

display_board(board(Board, _Size)) :- 
    
    reverse(Board, RevBoard),
    length(Board, TotalLines),
    display_board_lines(RevBoard, TotalLines),
    write('-----------------'), nl, 
    write(' A B C D E F G H'), nl, nl.

display_player(white) :- write('\tWhites\' turn').
display_player(black) :- write('\tBlacks\' turn').   % Changing color according to the pieces would be nice :)
