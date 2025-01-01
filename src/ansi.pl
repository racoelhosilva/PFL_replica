:- module(ansi, [
    reset/0,
    bold/0,
    faint/0,
    reset_bold/0,
    italic/0,
    reset_italic/0,
    underline/0,
    reset_underline/0,
    blink/0,
    reset_blink/0,
    invert/0,
    reset_invert/0,
    hidden/0,
    reset_hidden/0,
    strikethrough/0,
    reset_strikethrough/0,
    text_color/1,
    background_color/1,
    text_color_indexed/1,
    background_color_indexed/1,
    text_color_rgb/3,
    text_color_rgb/1,
    background_color_rgb/3,
    background_color_rgb/1,
    home/0,
    move_cursor/2,
    move_cursor_up/1,
    move_cursor_down/1,
    move_cursor_right/1,
    move_cursor_left/1,
    clear_screen/0,
    clear_to_start/0,
    clear_to_end/0,
    clear_line/0,
    clear_line_start/0,
    clear_line_full/0,
    hide_cursor/0,
    show_cursor/0,
    save_cursor/0,
    restore_cursor/0
]).

:- use_module(library(between)).

% Text Formatting
reset :- write('\e[0m').
bold :- write('\e[1m').
faint :- write('\e[2m').
reset_bold :- write('\e[22m').
italic :- write('\e[3m').
reset_italic :- write('\e[23m').
underline :- write('\e[4m').
reset_underline :- write('\e[24m').
blink :- write('\e[5m').
reset_blink :- write('\e[25m').
invert :- write('\e[7m').
reset_invert :- write('\e[27m').
hidden :- write('\e[8m').
reset_hidden :- write('\e[28m').
strikethrough :- write('\e[9m').
reset_strikethrough :- write('\e[29m').

% Colors
text_color(Color) :- ansi_color_seq(Color, text).
background_color(Color) :- ansi_color_seq(Color, background).

ansi_color_seq(Color, Type) :-
    color_code(Color, Type, Code),
    format('\e[~dm', [Code]).

text_color_indexed(Index) :- format('\e[38;5;~dm', [Index]).
background_color_indexed(Index) :- format('\e[48;5;~dm', [Index]).

text_color_rgb(R, G, B) :- format('\e[38;2;~d;~d;~dm', [R, G, B]).
text_color_rgb(color(R, G, B)) :- format('\e[38;2;~d;~d;~dm', [R, G, B]).
background_color_rgb(R, G, B) :- format('\e[48;2;~d;~d;~dm', [R, G, B]).
background_color_rgb(color(R, G, B)) :- format('\e[48;2;~d;~d;~dm', [R, G, B]).

% Cursor Control
home :- write('\e[H').
move_cursor(Row, Col) :- format('\e[~d;~dH', [Row, Col]).
move_cursor_up(N) :- format('\e[~dA', [N]).
move_cursor_down(N) :- format('\e[~dB', [N]).
move_cursor_right(N) :- format('\e[~dC', [N]).
move_cursor_left(N) :- format('\e[~dD', [N]).

% Clear Screen/Lines
clear_screen :- write('\e[2J').
clear_to_start :- write('\e[1J').
clear_to_end :- write('\e[0J').
clear_line :- write('\e[K').
clear_line_start :- write('\e[1K').
clear_line_full :- write('\e[2K').

% Cursor Visibility and Positioning
hide_cursor :- write('\e[?25l').
show_cursor :- write('\e[?25h').
save_cursor :- write('\e[s').
restore_cursor :- write('\e[u').

% Mapping Colors to ANSI Codes
color_code(black, text, 30).
color_code(red, text, 31).
color_code(green, text, 32).
color_code(yellow, text, 33).
color_code(blue, text, 34).
color_code(magenta, text, 35).
color_code(cyan, text, 36).
color_code(white, text, 37).
color_code(default, text, 39).
color_code(bright_black, text, 90).
color_code(bright_red, text, 91).
color_code(bright_green, text, 92).
color_code(bright_yellow, text, 93).
color_code(bright_blue, text, 94).
color_code(bright_magenta, text, 95).
color_code(bright_cyan, text, 96).
color_code(bright_white, text, 97).

color_code(black, background, 40).
color_code(red, background, 41).
color_code(green, background, 42).
color_code(yellow, background, 43).
color_code(blue, background, 44).
color_code(magenta, background, 45).
color_code(cyan, background, 46).
color_code(white, background, 47).
color_code(default, background, 49).
color_code(bright_black, background, 100).
color_code(bright_red, background, 101).
color_code(bright_green, background, 102).
color_code(bright_yellow, background, 103).
color_code(bright_blue, background, 104).
color_code(bright_magenta, background, 105).
color_code(bright_cyan, background, 106).
color_code(bright_white, background, 107).
