:- use_module(ansi).
:- use_module(library(lists)).

:- include(input).
:- include(interface).

get_option(Min, Max, Context, Value):-
    format('~a between ~d and ~d: ', [Context, Min, Max]),
    input_number(Value),
    between(Min, Max, Value), !.
get_option(Min, Max, Context, Value):-
    write('Invalid option! '),
    get_option(Min, Max, Context, Value).

get_option_game(Min, Max, Context, Value):-
    move_cursor_up(2),
    format('~a between ~d and ~d: ', [Context, Min, Max]),
    restore_cursor,
    write('> '),
    input_number(Value),
    between(Min, Max, Value), !.
get_option_game(Min, Max, Context, Value):-
    restore_cursor,
    move_cursor_up(1),
    write('Invalid option! '),
    restore_cursor,
    clear_to_end,
    get_option_game(Min, Max, Context, Value).

get_name(Context, Player):-
    format('Name for ~a: ', [Context]),
    input_string(Player).

get_position(Position, Size):-
    move_cursor_up(2),
    write('Position of the piece to move: '),
    restore_cursor,
    write('> '),
    input_position(Position, Size),
    Position = Row-Col,
    between(1, Size, Row),
    between(1, Size, Col), !.
get_position(Position, Size):-
    restore_cursor,
    move_cursor_up(1),
    write('Invalid position! '),
    restore_cursor,
    clear_to_end,
    get_position(Position, Size).

get_move(State, Move) :-
    state_board(State, Board),
    size(Board, Size),
    get_position(Position, Size),
    valid_piece_moves(State, Position, PieceMoves),
    length(PieceMoves, Length),
    Length > 0, !,
    move_cursor(20, 80),
    write('Valid moves: '), write(PieceMoves),
    restore_cursor,
    get_option_game(1, Length, 'Move', Index),
    nth1(Index, PieceMoves, Move), !,
    write('You selected: '), write(Move), nl.
get_move(State, Move) :-
    restore_cursor,
    move_cursor_up(1),
    write('No piece in that position can move! '),
    restore_cursor,
    clear_to_end,
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

put_cell_pixel :- write(' ').

put_cell_line(_Height, 0).
put_cell_line(Height, Width) :-
    put_cell_pixel,
    Width1 is Width - 1,
    put_cell_line(Height, Width1).

put_cell_aux(0, _).
put_cell_aux(Height, Width) :-
    put_cell_line(Height, Width),
    move_cursor_down(1),
    move_cursor_left(Width),
    Height1 is Height - 1,
    put_cell_aux(Height1, Width).

put_cell :-
    tile_width(Width),
    tile_height(Height),
    put_cell_aux(Height, Width),
    move_cursor_right(Width),
    move_cursor_up(Height).

draw_cell(Row, Col) :-
    Sum is Row + Col,
    Sum mod 2 =:= 0,
    background_color_rgb(255,255,255),
    put_cell.
draw_cell(Row, Col) :-
    Sum is Row + Col,
    Sum mod 2 =:= 1,
    background_color_rgb(0,0,0),
    put_cell.

draw_piece(Symbol) :-
    tile_height(Height),
    tile_width(Width),
    CenterX is (Width // 2) + 1,
    CenterY is (Height-1) // 2,
    move_cursor_left(CenterX),
    move_cursor_down(CenterY),
    write(Symbol),
    move_cursor_right(CenterX - 1),
    move_cursor_up(CenterY).

display_piece(empty) :- draw_piece(' ').
display_piece(white_piece) :- piece_white(Color), text_color_rgb(Color), bold, draw_piece('w').
display_piece(white_king) :- piece_white(Color), text_color_rgb(Color), bold, draw_piece('+').
display_piece(black_piece) :- piece_black(Color), text_color_rgb(Color), bold, draw_piece('b').
display_piece(black_king) :- piece_black(Color), text_color_rgb(Color), bold, draw_piece('*').

display_cell(Piece, Row, Col) :-
    draw_cell(Row, Col),
    display_piece(Piece).

display_line([], _Row, _Cols).
display_line([Cell|Rest], Row, Cols) :- 
    display_cell(Cell, Row, Cols),
    RestCols is Cols - 1,
    display_line(Rest, Row, RestCols).

display_board_aux([], _Rows, _Cols).
display_board_aux([Line|Rest], Rows, Cols) :- 
    display_border_vertical(Rows),
    display_line(Line, Rows, Cols),
    display_border_vertical(Rows),
    tile_height(Height),
    move_cursor_down(Height),
    tile_width(Width),
    NewCols is Cols + 2,
    move_cursor_left(NewCols * Width),
    RestRows is Rows - 1,
    display_board_aux(Rest, RestRows, Cols).

display_border_vertical(Row) :- 
    border_background(BackgroundColor), background_color_rgb(BackgroundColor),
    border_text(TextColor), text_color_rgb(TextColor), bold,
    put_cell,
    draw_piece(Row).

display_border_horizontal_aux(Width, Width).
display_border_horizontal_aux(Width, Col) :- 
    put_cell,
    Value is 65 + Col,
    char_code(AlphaCol, Value),
    draw_piece(AlphaCol),
    Col1 is Col + 1,
    display_border_horizontal_aux(Width, Col1).

display_border_horizontal(Size) :- 
    border_background(BackgroundColor), background_color_rgb(BackgroundColor),
    border_text(TextColor), text_color_rgb(TextColor), bold,
    put_cell,
    display_border_horizontal_aux(Size, 0),
    put_cell,
    tile_height(Height),
    move_cursor_down(Height),
    tile_width(Width),
    NewSize is Size + 2, 
    move_cursor_left(NewSize * Width).

display_board(board(Board, Size)) :- 
    move_cursor(10,4),
    display_border_horizontal(Size),
    reverse(Board, RevBoard),
    display_board_aux(RevBoard, Size, Size),
    display_border_horizontal(Size),
    nl.

display_winner(State, Winner) :-
    get_right_coordinate(State, Right),
    move_cursor(11, Right),
    get_state_name(State, Name),
    display_winner_aux(Name, Winner),
    restore_cursor.

display_winner_aux(Name, white) :- 
    piece_white(Color), text_color_rgb(Color), bold,
    clear_line, write(Name), write(' wins as white!').
display_winner_aux(Name, black) :- 
    piece_black(Color), text_color_rgb(Color), bold,
    clear_line, write(Name), write(' wins as black!').

display_player(State) :- 
    get_right_coordinate(State, Right),
    move_cursor(11, Right),
    get_state_name(State, Name),
    state_player(State, Player),
    display_player_aux(Name, Player),
    restore_cursor.

display_player_aux(Name, white) :- 
    piece_white(Color), text_color_rgb(Color), bold,
    clear_line, write(Name), write(' to play as white!').
display_player_aux(Name, black) :- 
    piece_black(Color), text_color_rgb(Color), bold,
    clear_line, write(Name), write(' to play as black!').

display_title :-
    nl,
    logo_color(Color), text_color_rgb(Color), bold,
    format('~|~t~a~t~120+', ' ______    _______  _______  ___      ___   _______  _______ '), nl,
    format('~|~t~a~t~120+', '|    _ |  |       ||       ||   |    |   | |       ||   _   |'), nl,
    format('~|~t~a~t~120+', '|   | ||  |    ___||    _  ||   |    |   | |       ||  |_|  |'), nl,
    format('~|~t~a~t~120+', '|   |_||_ |   |___ |   |_| ||   |    |   | |       ||       |'), nl,
    format('~|~t~a~t~120+', '|    __  ||    ___||    ___||   |___ |   | |      _||       |'), nl,
    format('~|~t~a~t~120+', '|   |  | ||   |___ |   |    |       ||   | |     |_ |   _   |'), nl,
    format('~|~t~a~t~120+', '|___|  |_||_______||___|    |_______||___| |_______||__| |__|'), nl,
    move_cursor(10, 1).

get_right_coordinate(state(board(_Board, Size), _Player, _KingEaten, _GameConfig), Value) :-
    tile_width(Width),
    Value is (Size + 2) * Width + 10.

get_bottom_coordinate(state(board(_Board, Size), _Player, _KingEaten, _GameConfig), Value) :-
    tile_height(Height),
    Value is (Size + 2) * Height + 13.

save_input_position(State) :-
    get_right_coordinate(State, Right),
    get_bottom_coordinate(State, Bottom),
    move_cursor(Bottom, Right),
    save_cursor.
