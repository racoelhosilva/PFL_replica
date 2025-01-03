/* Text Configurations */

% Background color of the application
background(color(34,36,36)).

% Color of the logo on top of the interface
logo_color(color(58, 90, 64)).

% Color of the menu header text
menu_header_color(color(88, 129, 87)).

% Color of the menu options text
menu_options_color(color(58, 90, 64)).

% Color of the prompt text in the interface
prompt_color(color(163, 177, 138)).

% Color of the input text in the interface
input_color(color(218, 215, 205)).

% Color of the error text in the interface
error_color(color(255, 0, 0)).

% Color of the board evaluation in the interface
value_color(color(250, 162, 117)).

/* Board Configurations */

% Color of "white" tiles on the board
board_white(color(255,255,255)).

% Color of "black" tiles on the board
board_black(color(0,0,0)).

% Color of the "white" pieces on the board
piece_white(color(255,0,0)).

% Symbols of the "white" pieces
white_pawn_symbol('W').
white_king_symbol('*W*').

% Color of the "black" pieces on the board
piece_black(color(0,255,0)).

% Symbols of the "black" pieces
black_pawn_symbol('B').
black_king_symbol('*B*').

% Color of the border of the board
border_background(color(59, 27, 4)).

% Color of the text on the border of the board
border_text(color(199, 133, 12)).

% Specifies the tile width and height
% Changing this might slightly break the interface
% It is recommended to keep the width and height as odd numbers
% The default values are 7 and 3
tile_width(7).
tile_height(3).

/* Notation Configurations */

% Color of the notation index
notation_color(color(255, 255, 255)).

% Color of the vertical moves
vertical_color(color(231, 111, 81)).

% Color of the horizontal moves
horizontal_color(color(42, 157, 143)).

% Color of the diagonal moves
diagonal_color(color(233, 196, 106)).

% Color of the transformation moves
transform_color(color(206, 106, 133)).
