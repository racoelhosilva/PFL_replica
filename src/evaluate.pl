:- ensure_loaded(logic).
:- ensure_loaded(utils).

% evaluate_piece(+Color, +BoardSize, +Piece, +Position, -Value)
% Evaluates the value of a piece.
% The predicate accepts the position of the piece so to make the evaluation
% position-dependent (to give more value to pieces closer to the opposite
% corner of the board).
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
% Calculates the value of all pieces in a line.
evaluate_line(Color, BoardSize, Line, Row, Value) :- evaluate_line(Color, BoardSize, Line, 1, Row, 0, Value).

% evaluate_line(+Color, +BoardSize, +Line, +Col, +Row, +Accumulator, -Value)
% Auxiliary predicate for evaluating all the pieces in a line, using tail recursion.
evaluate_line(_Color, _BoardSize, [], _Col, _Row, Acc, Acc).
evaluate_line(Color, BoardSize, [Piece | LineTail], Col, Row, Acc, Value) :-
    evaluate_piece(Color, BoardSize, Piece, Col-Row, PieceValue),
    NewCol is Col + 1,
    NewAcc is Acc + PieceValue,
    evaluate_line(Color, BoardSize, LineTail, NewCol, Row, NewAcc, Value).

% evaluate_board(+Color, +Board, -Value)
% Determines the value of all pieces in the board.
evaluate_board(Color, board(Board, BoardSize), Value) :- evaluate_board(Color, BoardSize, Board, 1, 0, Value).

% evaluate_board(+Color, +BoardSize, +Board, +Row, +Accumulator, -Value)
% Auxiliary predicate for evaluating all the pieces in the board, using tail recursion.
evaluate_board(_Color, _BoardSize, [], _Row, Acc, Acc).
evaluate_board(Color, BoardSize, [Line | BoardTail], Row, Acc, Value) :-
    evaluate_line(Color, BoardSize, Line, Row, LineValue),
    NewRow is Row + 1,
    NewAcc is Acc + LineValue,
    evaluate_board(Color, BoardSize, BoardTail, NewRow, NewAcc, Value).

% evaluate_state(+Color, +State, -Value)
% Determines the value of a state.
% The predicate first checks if the state is a final state, and if so, returns
% the best possible value in favor of the winning player. Otherwise, it evaluates
% every piece of the board.
evaluate_state(Color, State, 60.0) :-
    final_state(State, Color), !.

evaluate_state(Color, State, -60.0) :-
    opposite_color(Color, OppositeColor),
    final_state(State, OppositeColor), !.

evaluate_state(Color, State, Value) :-
    state_board(State, Board),
    evaluate_board(Color, Board, Value).

% evaluate_move(+State, +Move, -EvaluatedMove)
% Evaluates a move, by executing it and evaluating the resulting state.
% The evaluated move is a pair consisting of the move's value and the move itself.
evaluate_move(State, Move, Value-Move) :-
    state_player(State, Player),
    execute_move(State, Move, PossibleState),
    evaluate_state(Player, PossibleState, Value).
    
% evaluate_moves(+State, +Moves, -EvaluatedMoves)
% Evaluates a list of moves.
evaluate_moves(State, Moves, EvaluatedMoves) :-
    maplist(evaluate_move(State), Moves, EvaluatedMoves).

% best_greedy_move(+State, -Move)
% Chooses the move that gives the best immediate advantage.
% If multiple moves give the same maximum value, one is chosen randomly.
best_greedy_move(State, Move) :-
    findall(SomeMove, valid_move(State, SomeMove), Moves),
    evaluate_moves(State, Moves, EvaluatedMoves),
    max_key_set(EvaluatedMoves, MaxMoves),
    random_member(Move, MaxMoves).

% best_minimax_move(+State, +Depth, -Move)
% Chooses the move that gives the best advantage, using a minimax algorithm
% with a given depth. If multiple moves give the same maximum value, one is
% chosen at random.
best_minimax_move(State, Depth, Move) :-
    findall(Value-Move, (
        valid_move(State, Move),
        state_player(State, Player),
        evaluate_minimax_move(Player, State, Depth, max, Move, Value)
    ), EvaluatedMoves),
    max_key_set(EvaluatedMoves, BestMoves),
    random_member(Move, BestMoves).

% minimax(+State, +MaxDepth, +Maximize, -Value)
% Minimax algorithm for evaluating a state.
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

% switch_maximize(?State, ?NewState)
% Used by the minimax algorithm to switch the maximize state.
switch_maximize(max, min).
switch_maximize(min, max).

% evaluate_minimax_move(+CurrentPlayer, +State, +MaxDepth, +Maximize, +Move, -Value)
% Evaluates a move using the minimax algorithm.
evaluate_minimax_move(CurrentPlayer, State, MaxDepth, Maximize, Move, Value) :-
    execute_move(State, Move, IntermediateState),
    switch_player(IntermediateState, NewState),
    NextMaxDepth is MaxDepth - 1,
    switch_maximize(Maximize, NewMaximize),
    minimax(CurrentPlayer, NewState, NextMaxDepth, NewMaximize, Value), !.

% compare_minimax_values(+Maximize, +Values, -Value)
% Compares the values of the minimax algorithm to determine the best move,
% based on the maximize state.
compare_minimax_values(max, Values, Value) :- max_member(Value, Values).
compare_minimax_values(min, Values, Value) :- min_member(Value, Values).
