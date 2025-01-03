:- use_module(ansi).
:- use_module(library(lists)).

:- include(input).
:- include(theme).


/* General Display Functions */

% display_title
% Displays the game title
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

% save_input_position(+State)
% Saves the cursor position for input to be used later
save_input_position(State) :-
    get_right_coordinate(State, Right),
    get_bottom_coordinate(State, Bottom),
    move_cursor(Bottom, Right),
    save_cursor.

% get_right_coordinate(+State, -Value)
% Gets the right coordinate for the cursor input based on the board size
get_right_coordinate(State, Value) :-
    state_board(State, board(_Board, Size)),
    tile_width(Width),
    Value is (Size + 2) * Width + 10.

% get_bottom_coordinate(+State, -Value)
% Gets the bottom coordinate for the cursor input based on the board size
get_bottom_coordinate(State, Value) :-
    state_board(State, board(_Board, Size)),
    tile_height(Height),
    Value is (Size + 2) * Height + 12.


/* Menu Display Functions */

% display_menu(-GameConfig)
% Displays the game menu, allowing the user to select the game mode and the players' names
display_menu(game_config(GameMode, Player1, Player2)) :- 
    background(BackgroundColor), background_color_rgb(BackgroundColor),
    menu_header_color(HeaderColor), text_color_rgb(HeaderColor), bold,
    format('~|~t~a~t~120+', 'Welcome to Replica!'), nl, nl,
    get_gamemode(GameMode),
    display_options(GameMode, Player1, Player2), !.

% get_gamemode(-GameMode)
% Displays the game mode menu and reads the selected game mode
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

% display_options(+GameMode, -Player1, -Player2)
% Displays the player options menu based on the selected game mode and reads the selected options
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

% get_name(+Context, -Player)
% Prompts the user for a player name and reads the input
get_name(Context, Player):-
    prompt_color(PromptColor), text_color_rgb(PromptColor),
    format('    Name for ~a: ', [Context]),
    input_color(InputColor), text_color_rgb(InputColor),
    input_string(Player).

% get_difficulty(-Difficulty)
% Displays the difficulty level menu and reads the selected difficulty level
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

% get_option(+Min, +Max, +Context, -Value)
% Prompts the user for an option between Min and Max and reads the input
% Similar to get_game_option/4, but used for the game menu
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
    clear_line,
    write('Invalid option!'),
    restore_cursor,
    get_option(Min, Max, Context, Value).


/* Game Display Functions */

% overlay_game(+State)
% Displays the game screen overlay, assumes screen is already cleared and with background set
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


/* Board Display Functions */

% display_board(+Board)
% Displays the game board
display_board(board(Board, Size)) :- 
    move_cursor(10,4),
    display_border_horizontal(Size),
    reverse(Board, RevBoard),
    display_board_aux(RevBoard, Size, Size),
    display_border_horizontal(Size),
    nl.

% display_border_horizontal(+Size)
% Displays the horizontal border of the board
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

% display_border_horizontal_aux(+Width, +Col)
% Auxiliar function to display the horizontal border of the board
display_border_horizontal_aux(Width, Width) :- !.
display_border_horizontal_aux(Width, Col) :- 
    put_cell,
    Value is 65 + Col,
    char_code(AlphaCol, Value),
    draw_piece(AlphaCol),
    Col1 is Col + 1,
    display_border_horizontal_aux(Width, Col1).

% display_board_aux(+Board, +Rows, +Cols)
% Auxiliar function to display the game board
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

% display_border_vertical(+Row)
% Displays the vertical border of a specific row of the board
display_border_vertical(Row) :- 
    border_background(BackgroundColor), background_color_rgb(BackgroundColor),
    border_text(TextColor), text_color_rgb(TextColor), bold,
    put_cell,
    draw_piece(Row).

% display_line(+Line, +Row, +Cols)
% Displays a line of the game board
display_line([], _Row, _Cols).
display_line([Cell|Rest], Row, Cols) :- 
    display_cell(Cell, Row, Cols),
    RestCols is Cols - 1,
    display_line(Rest, Row, RestCols).

% display_cell(+Piece, +Row, +Col)
% Displays a board cell of the game board
display_cell(Piece, Row, Col) :-
    draw_cell(Row, Col),
    display_piece(Piece).

% draw_cell(+Row, +Col)
% Specifies whether a board cell should be white or black and draws it
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

% put_cell
% Obtains the tile width and height, puts a board cell on the screen 
% and moves the cursor to the next cell position
% Also used to draw the border of the board
put_cell :-
    tile_width(Width),
    tile_height(Height),
    put_cell_aux(Height, Width),
    move_cursor_right(Width),
    move_cursor_up(Height).

% put_cell_aux(+Height, +Width)
% Auxiliar function to put a board cell on the screen
put_cell_aux(0, _) :- !.
put_cell_aux(Height, Width) :-
    put_cell_line(Height, Width),
    move_cursor_down(1),
    move_cursor_left(Width),
    Height1 is Height - 1,
    put_cell_aux(Height1, Width).

% put_cell_line(+Height, +Width)
% Puts a line of a board cell on the screen
put_cell_line(_Height, 0) :- !.
put_cell_line(Height, Width) :-
    put_cell_pixel,
    Width1 is Width - 1,
    put_cell_line(Height, Width1).

% put_cell_pixel
% Puts a pixel of a board cell on the screen
put_cell_pixel :- write(' ').

% display_piece(+Piece)
% Sets the piece color and symbol and displays a piece on the screen
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

% draw_piece(+Symbol)
% Draws a piece on the screen over the center of a board cell
% Used for the pawn piece, empty cell and the border of the board
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

% draw_wide_piece(+WideSymbol)
% Draws a wide piece on the screen over the center of a board cell
% Used for the king piece
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


/* Turn Display Functions */

% display_player(+State)
% Displays the player to play
display_player(State) :- 
    get_right_coordinate(State, Right),
    move_cursor(11, Right),
    state_player(State, Player),
    state_name(State, Player, Name),
    display_player_aux(Name, Player),
    restore_cursor.

% display_player_aux(+Name, +Player)
% Auxiliar function to display the player to play
display_player_aux(Name, white) :- 
    piece_white(Color), text_color_rgb(Color), bold,
    clear_line, write(Name), write(' to play as white!').
display_player_aux(Name, black) :- 
    piece_black(Color), text_color_rgb(Color), bold,
    clear_line, write(Name), write(' to play as black!').

% display_winner(+State, +Winner)
% Displays the winner of the game
display_winner(State, Winner) :-
    get_right_coordinate(State, Right),
    move_cursor(11, Right),
    state_name(State, Winner, Name),
    display_winner_aux(Name, Winner),
    restore_cursor.

% display_winner_aux(+Name, +Winner)
% Auxiliar function to display the winner of the game
display_winner_aux(Name, white) :- 
    piece_white(Color), text_color_rgb(Color), bold,
    clear_line, write(Name), write(' wins as white!').
display_winner_aux(Name, black) :- 
    piece_black(Color), text_color_rgb(Color), bold,
    clear_line, write(Name), write(' wins as black!').


/* User Move Display Functions */

% get_move(+State, -Move)
% Reads a move from the input
% Starts by getting the position of the piece to move
% Then gets the valid moves for that piece and displays them
% Finally, reads the selected move from the input
% If no piece can move from the selected position, displays an error message and retries
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
    get_game_option(1, Length, 'Move', Index),
    nth1(Index, PieceMoves, Move), !,
    restore_cursor,
    length(PieceMoves, Length),
    clear_valid_moves(Length, -6).
get_move(State, Move) :-
    restore_cursor,
    error_color(ErrorColor), text_color_rgb(ErrorColor),
    move_cursor_up(1),
    clear_line,
    write('No piece in that position can move! '),
    restore_cursor,
    clear_line,
    get_move(State, Move).

% get_position(-Position, +Board)
% Prompts the user for a position and reads the input
% Checks if the position is valid
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
    clear_line,
    write('Invalid position! '),
    restore_cursor,
    clear_line,
    get_position(Position, Board).

% display_valid_moves(+Moves)
% Displays the valid moves for a piece
display_valid_moves(Moves) :-
    length(Moves, Length),
    Shift is Length + 5,
    move_cursor_up(Shift),
    menu_header_color(HeaderColor), text_color_rgb(HeaderColor), bold,
    write('Valid moves:'),
    reset_bold,
    display_valid_moves_aux(Moves, Length, 1).

% display_valid_moves_aux(+Moves, +Length, +Index)
% Auxiliar function to display the valid moves for a piece
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

% display_valid_move(+Move)
% Displays a valid move for a piece, with the corresponding color
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
    
% get_game_option(+Min, +Max, +Context, -Value)
% Prompts the user for an option between Min and Max and reads the input
% Similar to get_option/4, but used for in-game options
get_game_option(Min, Max, Context, Value):-
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
get_game_option(Min, Max, Context, Value):-
    restore_cursor,
    error_color(ErrorColor), text_color_rgb(ErrorColor),
    move_cursor_up(1),
    clear_line,
    write('Invalid option! '),
    restore_cursor,
    clear_line,
    get_game_option(Min, Max, Context, Value).

% clear_valid_moves(+Length, +Cur)
% Clears the valid move menu displayed on the screen
clear_valid_moves(Length, Length).
clear_valid_moves(Length, Cur) :-
    clear_line,
    move_cursor_up(1),
    Cur1 is Cur + 1,
    clear_valid_moves(Length, Cur1).


/* Move History Display Functions */

% display_history_move(+State, +Move)
% Displays a move on the history section of the screen
display_history_move(State, Move) :-
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
    draw_history_move(Player, Move, CurrentTurn),
    restore_cursor.

% draw_history_move(+Color, +Move, +CurrentMove)
% Draws current move on the history section of the screen
draw_history_move(white, Move, CurrentMove) :-
    notation_color(Color), text_color_rgb(Color), bold,
    format('~|~d.~t~3+ ', [CurrentMove]),
    put_history_move_code(Move).
draw_history_move(black, Move, _CurrentMove) :-
    move_cursor_right(8),
    put_history_move_code(Move).

% put_history_move_code(+Move)
% Puts a move on the screen, with the corresponding color
put_history_move_code(step(Col-Row, vertical)) :- 
    vertical_color(Color), text_color_rgb(Color), reset_bold,
    Value is 64 + Col, char_code(AlphaCol, Value),
    write(AlphaCol), write(Row), write('|').
put_history_move_code(step(Col-Row, horizontal)) :- 
    horizontal_color(Color), text_color_rgb(Color), reset_bold,
    Value is 64 + Col, char_code(AlphaCol, Value),
    write(AlphaCol), write(Row), write('-').
put_history_move_code(step(Col-Row, diagonal)) :- 
    diagonal_color(Color), text_color_rgb(Color), reset_bold,
    Value is 64 + Col, char_code(AlphaCol, Value),
    write(AlphaCol), write(Row), write('/').
put_history_move_code(transform(Col-Row)) :- 
    transform_color(Color), text_color_rgb(Color), reset_bold,
    Value is 64 + Col, char_code(AlphaCol, Value),
    write(AlphaCol), write(Row), write('x').


/* Evaluation Display Functions */

% display_value(+State, +Value)
% Displays the board evaluation value
display_value(State, Value) :- 
    get_right_coordinate(State, Right),
    get_bottom_coordinate(State, Bottom),
    NewBottom is Bottom - 1,
    move_cursor(NewBottom, 1),
    NewRight is Right - 3,
    value_color(Color), text_color_rgb(Color), bold,
    format('~|~tBoard Evaluation: ~8F~t~*+', [Value, NewRight]).
