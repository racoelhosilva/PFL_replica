:- use_module(library(aggregate)).

:- include(board).

% get_state_board(+State, -Board)
get_state_board(state(Board, _, _, _), Board).

% set_state_board(+State, +NewBoard, -NewState)
set_state_board(state(_, Player, WhiteKingEaten, BlackKingEaten), NewBoard, state(NewBoard, Player, WhiteKingEaten, BlackKingEaten)).

% get_state_player(+State, -Player)
get_state_player(state(_, Player, _, _), Player).

% set_state_player(+State, +Player, -NewState)
set_state_player(state(Board, _, WhiteKingEaten, BlackKingEaten), NewPlayer, state(Board, NewPlayer, WhiteKingEaten, BlackKingEaten)).

% king_eaten(+Color, +State)
king_eaten(white, state(_, _, true, _)).
king_eaten(black, state(_, _, _, true)).

% set_king_eaten(+Color, +State, -NewState)
set_king_eaten(white, state(Board, Player, _, BlackKingEaten), WhiteKingEaten, state(Board, Player, WhiteKingEaten, BlackKingEaten)).
set_king_eaten(black, state(Board, Player, WhiteKingEaten, _), BlackKingEaten, state(Board, Player, WhiteKingEaten, BlackKingEaten)).

% player_can_move_at(+State, +Position)
player_can_move_at(State, Position) :-
    get_state_board(State, Board),
    get_state_player(State, Player),
    piece_at_is(Board, Position, Player).

% verify_and_set_king_eaten(+Piece, +State, -NewState)
verify_and_set_king_eaten(Piece, State, State) :- \+ is_king(Piece), !.
verify_and_set_king_eaten(Piece, State, NewState) :-
    piece_color(Piece, Color),
    set_king_eaten(Color, State, true, NewState).

% execute_move(+State, +Move, -NewState)
execute_move(State, step(Row-Col, Direction), NewState) :-
    player_can_move_at(State, Row-Col),
    get_state_board(State, Board),
    get_state_player(State, Player),
    new_position(Player, Direction, Board, Row-Col, NewRow-NewCol),
    get_board(Board, NewRow-NewCol, ReplacedPiece),
    place_piece(Board, Row-Col, NewRow-NewCol, NewBoard),
    set_state_board(State, NewBoard, IntState),
    verify_and_set_king_eaten(ReplacedPiece, IntState, NewState).

execute_move(State, transform(Row-Col), NewState) :-
    player_can_move_at(State, Row-Col),
    get_state_board(State, Board),
    convert_to_king(Board, Row-Col, NewBoard),
    set_state_board(State, NewBoard, NewState).

% switch_player(?State, ?NewState)
switch_player(State, NewState) :-
    get_state_player(State, Player),
    opposite_color(Player, OtherPlayer),
    set_state_player(State, OtherPlayer, NewState).

% new_position(+Color, +Direction, +Board, +PiecePosition, +NewPosition)
% TODO: Maybe optimize this
new_position(_, _, Board, Position, _) :- \+ in_bounds(Board, Position) , !, fail.
new_position(Color, _, Board, Position, Position) :- 
    piece_at_is(Board, Position, OtherColor),
    OtherColor \= Color, !.

new_position(white, vertical, Board, Row-Col, Row-NewCol) :-
    NextCol is Col + 1,
    new_position(white, vertical, Board, Row-NextCol, Row-NewCol).
new_position(white, horizontal, Board, Row-Col, NewRow-Col) :-
    NextRow is Row + 1,
    new_position(white, horizontal, Board, NextRow-Col, NewRow-Col).
new_position(white, diagonal, Board, Row-Col, NewRow-NewCol) :-
    NextRow is Row + 1,
    NextCol is Col + 1,
    new_position(white, diagonal, Board, NextRow-NextCol, NewRow-NewCol).

new_position(black, vertical, Board, Row-Col, Row-NewCol) :-
    NextCol is Col - 1,
    new_position(black, vertical, Board, Row-NextCol, Row-NewCol).
new_position(black, horizontal, Board, Row-Col, NewRow-Col) :-
    NextRow is Row - 1,
    new_position(black, horizontal, Board, NextRow-Col, NewRow-Col).
new_position(black, diagonal, Board, Row-Col, NewRow-NewCol) :-
    NextRow is Row - 1,
    NextCol is Col - 1,
    new_position(black, diagonal, Board, NextRow-NextCol, NewRow-NewCol).

% blocks_sight(?Color, ?Piece)
blocks_sight(Color, Piece) :-
    opposite_color(Color, OppositeColor),
    piece_color(Piece, OppositeColor).

% seen_by_king(?Color, ?Direction, +Board, +Position)
seen_by_king(_Color, _Direction, Board, Row-Col) :- \+ in_bounds(Board, Row-Col), !, fail.

seen_by_king(Color, _Direction, Board, Row-Col) :- 
    get_board(Board, Row-Col, Piece),
    blocks_sight(Color, Piece), !, fail.
seen_by_king(_Color, _Direction, Board, Row-Col) :- 
    get_board(Board, Row-Col, Piece),
    is_king(Piece), !.

seen_by_king(white, vertical, Board, Row-Col) :-
    NextRow is Row - 1,
    seen_by_king(white, vertical, Board, NextRow-Col).
seen_by_king(white, horizontal, Board, Row-Col) :-
    NextCol is Col - 1,
    seen_by_king(white, horizontal, Board, Row-NextCol).
seen_by_king(white, diagonal, Board, Row-Col) :-
    NextRow is Row - 1,
    NextCol is Col - 1,
    seen_by_king(white, diagonal, Board, NextRow-NextCol).

seen_by_king(black, vertical, Board, Row-Col) :-
    NextRow is Row + 1,
    seen_by_king(black, vertical, Board, NextRow-Col).
seen_by_king(black, horizontal, Board, Row-Col) :-
    NextCol is Col + 1,
    seen_by_king(black, horizontal, Board, Row-NextCol).
seen_by_king(black, diagonal, Board, Row-Col) :-
    NextRow is Row + 1,
    NextCol is Col + 1,
    seen_by_king(black, diagonal, Board, NextRow-NextCol).

% valid_move(+State, ?Move)
valid_move(State, step(Position, Direction)) :-
    get_state_board(State, Board),
    in_bounds(Board, Position),
    player_can_move_at(State, Position),
    get_state_player(State, Player),
    new_position(Player, Direction, Board, Position, _NewPosition).

valid_move(State, transform(Position)) :-
    get_state_board(State, Board),
    in_bounds(Board, Position),
    player_can_move_at(State, Position),
    get_board(Board, Position, Piece),
    \+ is_king(Piece),
    get_state_player(State, Player),
    seen_by_king(Player, _, Board, Position).

% evaluate_piece(+Color, +Piece, +Position, ?Value)
evaluate_piece(_Color, empty, _Position, 0) :- !.

evaluate_piece(white, white-king, 8-8, 15) :- !.
evaluate_piece(black, black_king, 1-1, 15) :- !.

evaluate_piece(white, Piece, Row-Col, Value) :-
    piece_color(Piece, white), !,
    Value is 2 + 3 / ((8 - Row) + (8 - Col) + 1).
evaluate_piece(black, Piece, Row-Col, Value) :-
    piece_color(Piece, black), !,
    Value is 2 + 3 / ((Row - 1) + (Col - 1) + 1).

evaluate_piece(Color, Piece, Row-Col, Value) :-
    opposite_color(Color, OppositeColor),
    evaluate_piece(OppositeColor, Piece, Row-Col, OppositeValue),
    Value is -OppositeValue.

% evaluate_line(+Color, +Line, +Row, ?Value)
evaluate_line(Color, Line, Row, Value) :- evaluate_line(Color, Line, Row, 1, 0, Value).

% evaluate_line(+Color, +Line, +Row, +Col, +Accumulator, ?Value)
evaluate_line(_Color, [], _Row, _Col, Acc, Acc).
evaluate_line(Color, [Piece | LineTail], Row, Col, Acc, Value) :-
    evaluate_piece(Color, Piece, Row-Col, PieceValue),
    NewCol is Col + 1,
    NewAcc is Acc + PieceValue,
    evaluate_line(Color, LineTail, Row, NewCol, NewAcc, Value).

% evaluate_board(+Color, +Board, ?Value)
evaluate_board(Color, Board, Value) :- evaluate_board(Color, Board, 1, 0, Value).

% evaluate_board(+Color, +Board, +Row, +Accumulator, ?Value)
evaluate_board(_Color, [], _Row, Acc, Acc).
evaluate_board(Color, [Line | BoardTail], Row, Acc, Value) :-
    evaluate_line(Color, Line, Row, LineValue),
    NewRow is Row + 1,
    NewAcc is Acc + LineValue,
    evaluate_board(Color, BoardTail, NewRow, NewAcc, Value).

% evaluate_state(+Color, +State, ?Value)
evaluate_state(Color, State, Value) :-
    opposite_color(Color, OppositeColor),
    king_eaten(OppositeColor, State), !,
    set_king_eaten(OppositeColor, State, false, NewState),
    evaluate_state(Color, NewState, SubValue),
    Value is SubValue + 5.

evaluate_state(Color, State, Value) :-
    king_eaten(Color, State), !,
    set_king_eaten(Color, State, false, NewState),
    evaluate_state(Color, NewState, SubValue),
    Value is SubValue - 5.

evaluate_state(Color, State, Value) :-
    get_state_board(State, Board),
    evaluate_board(Color, Board, Value).

% evaluate_move(+State, +Move, ?EvaluatedMove)
evaluate_move(State, Move, Value-Move) :-
    get_state_player(State, Player),
    execute_move(State, Move, PossibleState),
    evaluate_state(Player, PossibleState, Value).
    
% evaluate_moves(+State, +Moves, ?EvaluatedMoves)
evaluate_moves(State, Moves, EvaluatedMoves) :-
    maplist(evaluate_move(State), Moves, EvaluatedMoves).
