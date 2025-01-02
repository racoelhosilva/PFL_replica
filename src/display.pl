:- use_module(ansi).
:- use_module(library(lists)).

:- include(input).
:- include(interface).

get_option(Min, Max, Context, Value):-
    prompt_color(PromptColor), text_color_rgb(PromptColor),
    format('~a between ~d and ~d: ', [Context, Min, Max]),
    input_color(InputColor), text_color_rgb(InputColor),
    input_number(Value),
    between(Min, Max, Value), !,
    restore_cursor,
    clear_line, write(Context), write(' '), write(Value), write(' selected!'),
    restore_cursor,
    move_cursor_down(1), clear_line.
get_option(Min, Max, Context, Value):-
    restore_cursor,
    error_color(ErrorColor), text_color_rgb(ErrorColor),
    move_cursor_down(1),
    write('Invalid option!'),
    restore_cursor,
    get_option(Min, Max, Context, Value).

get_option_game(Min, Max, Context, Value):-
    move_cursor_up(2), clear_line,
    prompt_color(PromptColor), text_color_rgb(PromptColor),
    format('~a between ~d and ~d: ', [Context, Min, Max]),
    restore_cursor,
    prompt_color(PromptColor), text_color_rgb(PromptColor),
    write('> '),
    input_color(InputColor), text_color_rgb(InputColor),
    input_number(Value),
    between(Min, Max, Value), !,
    restore_cursor,
    move_cursor_up(1), clear_line.
get_option_game(Min, Max, Context, Value):-
    restore_cursor,
    error_color(ErrorColor), text_color_rgb(ErrorColor),
    move_cursor_up(1),
    write('Invalid option! '),
    restore_cursor,
    clear_line,
    get_option_game(Min, Max, Context, Value).

get_name(Context, Player):-
    prompt_color(PromptColor), text_color_rgb(PromptColor),
    format('    Name for ~a: ', [Context]),
    input_color(InputColor), text_color_rgb(InputColor),
    input_string(Player).

get_position(Position, Board):-
    move_cursor_up(2), clear_line,
    prompt_color(PromptColor), text_color_rgb(PromptColor),
    write('Position of the piece to move: '),
    restore_cursor,
    prompt_color(PromptColor), text_color_rgb(PromptColor),
    write('> '),
    input_color(InputColor), text_color_rgb(InputColor),
    size(Board, Size),
    input_position(Position, Size),
    in_bounds(Board, Position), !,
    restore_cursor,
    move_cursor_up(1), clear_line.
get_position(Position, Board):-
    restore_cursor,
    error_color(ErrorColor), text_color_rgb(ErrorColor),
    move_cursor_up(1),
    write('Invalid position! '),
    restore_cursor,
    clear_line,
    get_position(Position, Board).

display_valid_moves(Moves) :-
    length(Moves, Length),
    Shift is Length + 5,
    move_cursor_up(Shift),
    menu_header_color(HeaderColor), text_color_rgb(HeaderColor), bold,
    write('Valid moves:'),
    reset_bold,
    display_valid_moves_aux(Moves, Length, 1).

display_valid_moves_aux([], _, _).
display_valid_moves_aux([Move|Rest], Length, Index) :-
    restore_cursor,
    Shift is Length + 4 - Index,
    move_cursor_up(Shift),
    menu_options_color(OptionsColor), text_color_rgb(OptionsColor),
    write(Index), write('. '),
    display_valid_move(Move),
    Index1 is Index + 1,
    display_valid_moves_aux(Rest, Length, Index1).

display_valid_move(step(_Position, vertical)) :- 
    vertical_color(Color), text_color_rgb(Color),
    write('Vertical Step').
display_valid_move(step(_Position, horizontal)) :-
    horizontal_color(Color), text_color_rgb(Color),
    write('Horizontal Step').
display_valid_move(step(_Position, diagonal)) :-
    diagonal_color(Color), text_color_rgb(Color),
    write('Diagonal Step').
display_valid_move(transform(_Position)) :-
    transform_color(Color), text_color_rgb(Color),
    write('Transform').

clear_game_info(Length, Length).
clear_game_info(Length, Cur) :-
    clear_line,
    move_cursor_up(1),
    Cur1 is Cur + 1,
    clear_game_info(Length, Cur1).

get_move(State, Move) :-
    restore_cursor,
    state_board(State, Board),
    get_position(Position, Board),
    valid_piece_moves(State, Position, PieceMoves),
    length(PieceMoves, Length),
    Length > 0, !,
    restore_cursor,
    display_valid_moves(PieceMoves),
    restore_cursor,
    get_option_game(1, Length, 'Move', Index),
    nth1(Index, PieceMoves, Move), !,
    restore_cursor,
    length(PieceMoves, Length),
    clear_game_info(Length, -6).
get_move(State, Move) :-
    restore_cursor,
    error_color(ErrorColor), text_color_rgb(ErrorColor),
    move_cursor_up(1),
    write('No piece in that position can move! '),
    restore_cursor,
    clear_line,
    get_move(State, Move).

get_gamemode(GameMode):-
    write('    Please select the game mode:'), nl,
    reset_bold, 
    menu_options_color(OptionsColor), text_color_rgb(OptionsColor), 
    write('      1. Human vs. Human'), nl,
    write('      2. Human vs. Computer'), nl,
    write('      3. Computer vs. Human'), nl,
    write('      4. Computer vs. Computer'), nl,
    write('    '),
    save_cursor,
    get_option(1, 4, 'Game mode', GameMode), nl.

get_difficulty(Difficulty):-
    menu_header_color(HeaderColor), text_color_rgb(HeaderColor), bold,
    write('    Please select the difficulty level:'), nl,
    reset_bold,
    menu_options_color(OptionsColor), text_color_rgb(OptionsColor),
    write('      1. Random'), nl,
    write('      2. Greedy'), nl,
    write('    '),
    save_cursor,
    get_option(1, 2, 'Difficulty level', Difficulty), nl.

display_menu(game_config(GameMode, Player1, Player2)) :- 
    background(BackgroundColor), background_color_rgb(BackgroundColor),
    menu_header_color(HeaderColor), text_color_rgb(HeaderColor), bold,
    format('~|~t~a~t~120+', 'Welcome to Replica!'), nl, nl,
    get_gamemode(GameMode),
    display_options(GameMode, Player1, Player2), !.

display_options(1, player_info(Name1, 0), player_info(Name2, 0)) :-
    get_name('Player 1', Name1),
    get_name('Player 2', Name2).
display_options(2, player_info(Name1, 0), player_info(Name2, Difficulty)) :-
    get_name('Player 1', Name1),
    get_name('Computer', Name2),
    get_difficulty(Difficulty).
display_options(3, player_info(Name1, Difficulty), player_info(Name2, 0)) :-
    get_name('Computer', Name1),
    get_difficulty(Difficulty),
    get_name('Player 2', Name2).
display_options(4, player_info(Name1, Difficulty1), player_info(Name2, Difficulty2)) :-
    get_name('Computer 1', Name1),
    get_difficulty(Difficulty1),
    get_name('Computer 2', Name2),
    get_difficulty(Difficulty2).

put_cell_pixel :- write(' ').

put_cell_line(_Height, 0) :- !.
put_cell_line(Height, Width) :-
    put_cell_pixel,
    Width1 is Width - 1,
    put_cell_line(Height, Width1).

put_cell_aux(0, _) :- !.
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
    board_white(Color), background_color_rgb(Color),
    put_cell.
draw_cell(Row, Col) :-
    Sum is Row + Col,
    Sum mod 2 =:= 1,
    board_black(Color), background_color_rgb(Color),
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

draw_wide_piece(WideSymbol) :-
    tile_height(Height),
    tile_width(Width),
    CenterX is (Width // 2) + 1,
    CenterY is (Height-1) // 2,
    move_cursor_left(CenterX + 1),
    move_cursor_down(CenterY),
    write(WideSymbol),
    move_cursor_right(CenterX - 2),
    move_cursor_up(CenterY).

display_piece(empty) :- draw_piece(' ').
display_piece(white_piece) :- 
    piece_white(Color), text_color_rgb(Color), bold, 
    white_pawn_symbol(Symbol), draw_piece(Symbol).
display_piece(white_king) :- 
    piece_white(Color), text_color_rgb(Color), bold, 
    white_king_symbol(Symbol), draw_wide_piece(Symbol).
display_piece(black_piece) :- 
    piece_black(Color), text_color_rgb(Color), bold, 
    black_pawn_symbol(Symbol), draw_piece(Symbol).
display_piece(black_king) :- 
    piece_black(Color), text_color_rgb(Color), bold, 
    black_king_symbol(Symbol), draw_wide_piece(Symbol).

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

display_border_horizontal_aux(Width, Width) :- !.
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
    state_name(State, Winner, Name),
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
    state_player(State, Player),
    state_name(State, Player, Name),
    display_player_aux(Name, Player),
    restore_cursor.

display_player_aux(Name, white) :- 
    piece_white(Color), text_color_rgb(Color), bold,
    clear_line, write(Name), write(' to play as white!').
display_player_aux(Name, black) :- 
    piece_black(Color), text_color_rgb(Color), bold,
    clear_line, write(Name), write(' to play as black!').

display_value(State, Value) :- 
    get_right_coordinate(State, Right),
    get_bottom_coordinate(State, Bottom),
    NewBottom is Bottom - 1,
    move_cursor(NewBottom, 1),
    NewRight is Right - 3,
    value_color(Color), text_color_rgb(Color), bold,
    format('~|~tBoard Evaluation: ~8F~t~*+', [Value, NewRight]).

put_move(step(Col-Row, vertical)) :- 
    vertical_color(Color), text_color_rgb(Color), reset_bold,
    Value is 64 + Col, char_code(AlphaCol, Value),
    write(AlphaCol), write(Row), write('|').
put_move(step(Col-Row, horizontal)) :- 
    horizontal_color(Color), text_color_rgb(Color), reset_bold,
    Value is 64 + Col, char_code(AlphaCol, Value),
    write(AlphaCol), write(Row), write('-').
put_move(step(Col-Row, diagonal)) :- 
    diagonal_color(Color), text_color_rgb(Color), reset_bold,
    Value is 64 + Col, char_code(AlphaCol, Value),
    write(AlphaCol), write(Row), write('/').
put_move(transform(Col-Row)) :- 
    transform_color(Color), text_color_rgb(Color), reset_bold,
    Value is 64 + Col, char_code(AlphaCol, Value),
    write(AlphaCol), write(Row), write('x').

draw_move(white, Move, CurrentMove) :-
    notation_color(Color), text_color_rgb(Color), bold,
    format('~|~d.~t~3+ ', [CurrentMove]),
    put_move(Move).
draw_move(black, Move, _CurrentMove) :-
    move_cursor_right(8),
    put_move(Move).

display_move(State, Move) :-
    state_move(State, CurrentMove),
    get_right_coordinate(State, Right),
    get_bottom_coordinate(State, Bottom),
    VerticalBound is Bottom - 13 - 12,
    Row1 is CurrentMove // 2,
    CurrentRow is Row1 mod VerticalBound,
    CurrentCol is Right + (Row1 // VerticalBound) * 14,
    Row is 13 + CurrentRow,
    move_cursor(Row, CurrentCol),
    state_player(State, Player),
    CurrentTurn is Row1 + 1,
    draw_move(Player, Move, CurrentTurn),
    restore_cursor.

display_title :-
    nl,
    logo_color(Color), text_color_rgb(Color), bold,
    format('~|~t~a~t~120+', ' ______    _______  _______  ___      ___   _______  _______ '), nl,
    format('~|~t~a~t~120+', '|    _ |  |       ||       ||   |    |   | |       ||   _   |'), nl,
    format('~|~t~a~t~120+', '|   | ||  |    ___||    _  ||   |    |   | |      _||  |_|  |'), nl,
    format('~|~t~a~t~120+', '|   |_||_ |   |___ |   |_| ||   |    |   | |     |  |       |'), nl,
    format('~|~t~a~t~120+', '|    __  ||    ___||    ___||   |___ |   | |     |  |       |'), nl,
    format('~|~t~a~t~120+', '|   |  | ||   |___ |   |    |       ||   | |     |_ |   _   |'), nl,
    format('~|~t~a~t~120+', '|___|  |_||_______||___|    |_______||___| |_______||__| |__|'), nl,
    move_cursor(10, 1).

overlay_game(State) :-
    home, background(BG), background_color_rgb(BG),
    hide_cursor,
    display_title,
    state_board(State, Board),
    display_board(Board),
    background(BG), background_color_rgb(BG),
    display_player(State),
    value(State, white, Value),
    display_value(State, Value).

get_right_coordinate(State, Value) :-
    state_board(State, board(_Board, Size)),
    tile_width(Width),
    Value is (Size + 2) * Width + 10.

get_bottom_coordinate(State, Value) :-
    state_board(State, board(_Board, Size)),
    tile_height(Height),
    Value is (Size + 2) * Height + 12.

save_input_position(State) :-
    get_right_coordinate(State, Right),
    get_bottom_coordinate(State, Bottom),
    move_cursor(Bottom, Right),
    save_cursor.
