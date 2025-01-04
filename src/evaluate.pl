:- ensure_loaded(logic).
:- ensure_loaded(utils).

% evaluate_piece(+Color, +BoardSize, +Piece, +Position, -Value)
evaluate_piece(_Color, _BoardSize, empty, _Position, 0) :- !.

evaluate_piece(white, BoardSize, Piece, Col-Row, Value) :-
    piece_color(Piece, white), !,
    Value is 2 + 3 / ((BoardSize - Row) + (BoardSize - Col) + 1).
evaluate_piece(black, _BoardSize, Piece, Col-Row, Value) :-
    piece_color(Piece, black), !,
    Value is 2 + 3 / ((Row - 1) + (Col - 1) + 1).

evaluate_piece(Color, BoardSize, Piece, Position, Value) :-
    opposite_color(Color, OppositeColor),
    evaluate_piece(OppositeColor, BoardSize, Piece, Position, OppositeValue),
    Value is -OppositeValue.

% evaluate_line(+Color, +BoardSize, +Line, +Row, -Value)
evaluate_line(Color, BoardSize, Line, Row, Value) :- evaluate_line(Color, BoardSize, Line, 1, Row, 0, Value), !.

% evaluate_line(+Color, +BoardSize, +Line, +Col, +Row, +Accumulator, -Value)
evaluate_line(_Color, _BoardSize, [], _Col, _Row, Acc, Acc).
evaluate_line(Color, BoardSize, [Piece | LineTail], Col, Row, Acc, Value) :-
    evaluate_piece(Color, BoardSize, Piece, Col-Row, PieceValue),
    NewCol is Col + 1,
    NewAcc is Acc + PieceValue,
    evaluate_line(Color, BoardSize, LineTail, NewCol, Row, NewAcc, Value).

% evaluate_board(+Color, +Board, -Value)
evaluate_board(Color, board(Board, BoardSize), Value) :- evaluate_board(Color, BoardSize, Board, 1, 0, Value), !.

% evaluate_board(+Color, +BoardSize, +Board, +Row, +Accumulator, -Value)
evaluate_board(_Color, _BoardSize, [], _Row, Acc, Acc).
evaluate_board(Color, BoardSize, [Line | BoardTail], Row, Acc, Value) :-
    evaluate_line(Color, BoardSize, Line, Row, LineValue),
    NewRow is Row + 1,
    NewAcc is Acc + LineValue,
    evaluate_board(Color, BoardSize, BoardTail, NewRow, NewAcc, Value).

% evaluate_state(+Color, +State, -Value)
evaluate_state(Color, State, 60.0) :-
    final_state(State, Color), !.

evaluate_state(Color, State, -60.0) :-
    opposite_color(Color, OppositeColor),
    final_state(State, OppositeColor), !.

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

best_minimax_move(State, Depth, Move) :-
    NextDepth is Depth - 1,
    findall(Value-Move, (
        valid_move(State, Move),
        state_player(State, Player),
        execute_move(State, Move, IntermediateState),
        switch_player(IntermediateState, NewState),
        minimax(Player, NewState, NextDepth, min, Value)
    ), EvaluatedMoves),
    max_key_set(EvaluatedMoves, BestMoves),
    random_member(Move, BestMoves).

% minimax(+State, +MaxDepth, +Maximize, -Value)
minimax(CurrentPlayer, State, _Depth, _Maximize, Value) :-
    final_state(State, _Winner), !,
    evaluate_state(CurrentPlayer, State, Value).
minimax(CurrentPlayer, State, 0, _Maximize, Value) :- 
    evaluate_state(CurrentPlayer, State, Value).
minimax(CurrentPlayer, State, Depth, Maximize, Value) :-
    Depth > 0,
    bagof(Move, valid_move(State, Move), Moves),
    maplist(evaluate_minimax_move(CurrentPlayer, State, Depth, Maximize), Moves, Values),
    compare_minimax_values(Maximize, Values, Value).

switch_maximize(max, min).
switch_maximize(min, max).

evaluate_minimax_move(CurrentPlayer, State, Depth, Maximize, Move, Value) :-
    execute_move(State, Move, IntermediateState),
    switch_player(IntermediateState, NewState),
    NextDepth is Depth - 1,
    switch_maximize(Maximize, NewMaximize),
    minimax(CurrentPlayer, NewState, NextDepth, NewMaximize, Value), !.

compare_minimax_values(max, Values, Value) :- max_member(Value, Values).
compare_minimax_values(min, Values, Value) :- min_member(Value, Values).
