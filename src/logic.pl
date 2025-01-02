:- include(board).

% state_board(+State, -Board)
state_board(state(Board, _Player, _KingEaten, _CurrentMove, _GameConfig), Board).

% set_state_board(+State, +Board, -NewState)
set_state_board(state(_OldBoard, Player, KingEaten, CurrentMove, GameConfig), Board, state(Board, Player, KingEaten, CurrentMove, GameConfig)).

% state_player(+State, -Player)
state_player(state(_Board, Player, _KingEaten, _CurrentMove, _GameConfig), Player).

% set_state_player(+State, +Player, -NewState)
set_state_player(state(Board, _OldPlayer, KingEaten, CurrentMove, GameConfig), Player, state(Board, Player, KingEaten, CurrentMove, GameConfig)).

% state_difficulty(+State, -Difficulty)
state_difficulty(state(_, white, _, _, game_config(_, player_info(_, Difficulty), _)), Difficulty).
state_difficulty(state(_, black, _, _, game_config(_, _, player_info(_, Difficulty))), Difficulty).

% state_name(+State, +Player, -Name)
state_name(state(_, _, _, _, game_config(_, player_info(Name, _), _)), white, Name).
state_name(state(_, _, _, _, game_config(_, _, player_info(Name, _))), black, Name).

% state_move(+State, -CurrentMove)
state_move(state(_, _, _, CurrentMove, _), CurrentMove).

% increase_state_move(+State, -NewState)
increase_state_move(state(Board, Player, KingEaten, CurrentMove, GameConfig), state(Board, Player, KingEaten, NextMove, GameConfig)) :-
    NextMove is CurrentMove + 1.

% king_eaten(+State, +KingEaten)
king_eaten(state(_Board, _Player, KingEaten, _CurrentMove, _GameConfig), KingEaten).

% set_king_eaten(+State, +KingEaten, -NewState)
set_king_eaten(state(Board, Player, _OldKingEaten, CurrentMove, GameConfig), KingEaten, state(Board, Player, KingEaten, CurrentMove, GameConfig)).

% verify_and_set_king_eaten(+Piece, +State, -NewState)
verify_and_set_king_eaten(Piece, State, State) :- \+ king(Piece), !.
verify_and_set_king_eaten(Piece, State, NewState) :-
    piece_color(Piece, Color),
    set_king_eaten(State, Color, NewState).

% execute_move(+State, +Move, -NewState)
execute_move(State, step(Pos, Direction), NewState) :-
    state_board(State, Board),
    state_player(State, Player),
    new_position(Player, Direction, Pos, Board, NewPos),
    board_piece(Board, NewPos, ReplacedPiece),
    place_piece(Board, Pos, NewPos, NewBoard),
    set_state_board(State, NewBoard, TempState),
    verify_and_set_king_eaten(ReplacedPiece, TempState, NewState).

execute_move(State, transform(Pos), NewState) :-
    state_board(State, Board),
    convert_to_king(Board, Pos, NewBoard),
    set_state_board(State, NewBoard, NewState).

% switch_player(?State, ?NewState)
switch_player(State, NewState) :-
    state_player(State, Player),
    opposite_color(Player, OtherPlayer),
    set_state_player(State, OtherPlayer, NewState).

% new_position(+Color, +Direction, +PiecePosition, +Board, -NewPosition)
new_position(_Color, _Direction, Position, Board, _NewPosition) :- \+ in_bounds(Board, Position), !, fail.
new_position(Color, _Direction, Position, Board, Position) :-
    board_piece_color(Board, Position, OtherColor),
    OtherColor \= Color, !.

new_position(white, vertical, Col-Row, Board, Col-NewRow) :-
    NextRow is Row + 1,
    new_position(white, vertical, Col-NextRow, Board, Col-NewRow).
new_position(white, horizontal, Col-Row, Board, NewCol-Row) :-
    NextCol is Col + 1,
    new_position(white, horizontal, NextCol-Row, Board, NewCol-Row).
new_position(white, diagonal, Col-Row, Board, NewCol-NewRow) :-
    NextCol is Col + 1,
    NextRow is Row + 1,
    new_position(white, diagonal, NextCol-NextRow, Board, NewCol-NewRow).

new_position(black, vertical, Col-Row, Board, Col-NewRow) :-
    NextRow is Row - 1,
    new_position(black, vertical, Col-NextRow, Board, Col-NewRow).
new_position(black, horizontal, Col-Row, Board, NewCol-Row):-
    NextCol is Col - 1,
    new_position(black, horizontal, NextCol-Row, Board, NewCol-Row).
new_position(black, diagonal, Col-Row, Board, NewCol-NewRow) :-
    NextCol is Col - 1,
    NextRow is Row - 1,
    new_position(black, diagonal, NextCol-NextRow, Board, NewCol-NewRow).

% player_can_move_at(+Player, +Position, +Board)
player_can_move_at(Player, Position, Board) :-
    in_bounds(Board, Position),
    board_piece_color(Board, Position, Player).

% blocks_sight(?Color, ?Piece)
blocks_sight(Color, Piece) :-
    opposite_color(Color, OppositeColor),
    piece_color(Piece, OppositeColor).

% seen_by_king(?Color, ?Direction, +Board, +Position)
seen_by_king(_Color, _Direction, Board, Position) :- \+ in_bounds(Board, Position), !, fail.

seen_by_king(Color, _Direction, Board, Position) :- 
    board_piece(Board, Position, Piece),
    blocks_sight(Color, Piece), !, fail.
seen_by_king(_Color, _Direction, Board, Position) :- 
    board_piece(Board, Position, Piece),
    king(Piece), !.

seen_by_king(white, vertical, Board, Col-Row) :-
    NextRow is Row - 1,
    seen_by_king(white, vertical, Board, Col-NextRow).
seen_by_king(white, horizontal, Board, Col-Row) :-
    NextCol is Col - 1,
    seen_by_king(white, horizontal, Board, NextCol-Row).
seen_by_king(white, diagonal, Board, Row-Col) :-
    NextCol is Col - 1,
    NextRow is Row - 1,
    seen_by_king(white, diagonal, Board, NextCol-NextRow).

seen_by_king(black, vertical, Board, Col-Row) :-
    NextRow is Row + 1,
    seen_by_king(black, vertical, Board, Col-NextRow).
seen_by_king(black, horizontal, Board, Col-Row) :-
    NextCol is Col + 1,
    seen_by_king(black, horizontal, Board, NextCol-Row).
seen_by_king(black, diagonal, Board, Col-Row) :-
    NextCol is Col + 1,
    NextRow is Row + 1,
    seen_by_king(black, diagonal, Board, NextCol-NextRow).

% valid_move(+State, -Move)
valid_move(State, step(Position, Direction)) :-
    state_board(State, Board),
    state_player(State, Player),
    player_can_move_at(Player, Position, Board),
    new_position(Player, Direction, Position, Board, _NewPosition).

valid_move(State, transform(Position)) :-
    state_board(State, Board),
    state_player(State, Player),
    player_can_move_at(Player, Position, Board),
    board_piece(Board, Position, Piece),
    \+ king(Piece),
    seen_by_king(Player, _, Board, Position).

% valid_piece_moves(+State, +Position, -Moves)
valid_piece_moves(State, Piece, Moves) :-
    findall(Move, valid_piece_move(State, Piece, Move), Moves).

% valid_piece_move(+State, +Piece, -Move)
valid_piece_move(State, Piece, step(Piece, Direction)) :-
    valid_move(State, step(Piece, Direction)).
valid_piece_move(State, Piece, transform(Piece)) :-
    valid_move(State, transform(Piece)).

% evaluate_piece(+Color, +BoardSize, +Piece, +Position, -Value)
evaluate_piece(_Color, _BoardSize, empty, _Position, 0) :- !.

evaluate_piece(white, BoardSize, white-king, BoardSize-BoardSize, 15) :- !.
evaluate_piece(black, _BoardSize, black_king, 1-1, 15) :- !.

evaluate_piece(white, BoardSize, Piece, Row-Col, Value) :-
    piece_color(Piece, white), !,
    Value is 2 + 3 / ((BoardSize - Row) + (BoardSize - Col) + 1).
evaluate_piece(black, _BoardSize, Piece, Row-Col, Value) :-
    piece_color(Piece, black), !,
    Value is 2 + 3 / ((Row - 1) + (Col - 1) + 1).

evaluate_piece(Color, BoardSize, Piece, Row-Col, Value) :-
    opposite_color(Color, OppositeColor),
    evaluate_piece(OppositeColor, BoardSize, Piece, Row-Col, OppositeValue),
    Value is -OppositeValue.

% evaluate_line(+Color, +BoardSize, +Line, +Row, -Value)
evaluate_line(Color, BoardSize, Line, Row, Value) :- evaluate_line(Color, BoardSize, Line, Row, 1, 0, Value).

% evaluate_line(+Color, +BoardSize, +Line, +Row, +Col, +Accumulator, -Value)
evaluate_line(_Color, _BoardSize, [], _Row, _Col, Acc, Acc).
evaluate_line(Color, BoardSize, [Piece | LineTail], Row, Col, Acc, Value) :-
    evaluate_piece(Color, BoardSize, Piece, Row-Col, PieceValue),
    NewCol is Col + 1,
    NewAcc is Acc + PieceValue,
    evaluate_line(Color, BoardSize, LineTail, Row, NewCol, NewAcc, Value).

% evaluate_board(+Color, +Board, -Value)
evaluate_board(Color, board(Board, BoardSize), Value) :- evaluate_board(Color, BoardSize, Board, 1, 0, Value).

% evaluate_board(+Color, +BoardSize, +Board, +Row, +Accumulator, -Value)
evaluate_board(_Color, _BoardSize, [], _Row, Acc, Acc).
evaluate_board(Color, BoardSize, [Line | BoardTail], Row, Acc, Value) :-
    evaluate_line(Color, BoardSize, Line, Row, LineValue),
    NewRow is Row + 1,
    NewAcc is Acc + LineValue,
    evaluate_board(Color, BoardSize, BoardTail, NewRow, NewAcc, Value).

% evaluate_state(+Color, +State, -Value)
evaluate_state(Color, State, Value) :-
    opposite_color(Color, OppositeColor),
    king_eaten(State, OppositeColor), !,
    set_king_eaten(State, none, NewState),
    evaluate_state(Color, NewState, SubValue),
    Value is SubValue + 5.

evaluate_state(Color, State, Value) :-
    king_eaten(State, Color), !,
    set_king_eaten(State, none, NewState),
    evaluate_state(Color, NewState, SubValue),
    Value is SubValue - 5.

evaluate_state(Color, State, Value) :-
    state_board(State, Board),
    evaluate_board(Color, Board, Value).

% evaluate_move(+State, +Move, -EvaluatedMove)
evaluate_move(State, Move, Value-Move) :-
    state_player(State, Player),
    execute_move(State, Move, PossibleState),
    evaluate_state(Player, PossibleState, Value).
    
% evaluate_moves(+State, +Moves, -EvaluatedMoves)
evaluate_moves(State, Moves, EvaluatedMoves) :-
    maplist(evaluate_move(State), Moves, EvaluatedMoves).
