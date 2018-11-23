% To run it, try:
% Connect4.

:- use_module(library(clpfd)).

% the starting state for every connect4 game
connect4_start(gameBoard([['*','*','*','*','*','*'],
	                      ['*','*','*','*','*','*'],
	                      ['*','*','*','*','*','*'],
	                      ['*','*','*','*','*','*'],
	                      ['*','*','*','*','*','*'],
	                      ['*','*','*','*','*','*'],
	                      ['*','*','*','*','*','*']])).

% checks for row
rowWin(X, gameBoard([H|T])) :-
	(  checkRow(X, 0, H)
    -> true
    ;  rowWin(X, gameBoard(T))
     ).

checkRow(_, 4, _).
checkRow(X, N, [X|T]) :-
	N1 is N+1,
	checkRow(X, N1, T).
checkRow(X, _, [H|T]) :-
	dif(X, H),
	checkRow(X, 0, T).

% checks for column
columnWin(X, gameBoard(GB)) :-
	transpose(GB, Trans),
	rowWin(X, gameBoard(Trans)).

getDiags(Matx, Diags) :-
	addRowsToDiags([], Matx, 0, Diags).

% checks for / diagonal
diagWin1(X, gameBoard(GB)) :-
	getDiags(GB, Diags),
	rowWin(X, gameBoard(Diags)).

% checks for \ diagonal
diagWin2(X, gameBoard(GB)) :-
	rotateMatx(GB, Rotated),
	getDiags(Rotated, Diags),
	rowWin(X, gameBoard(Diags)).

rotateMatx(X, Z) :-
	transpose(X, Y),
	maplist(reverse, Y, Z).

addRowsToDiags(Diags, [], _, Diags).
addRowsToDiags(Prev, [H|T], N, Res) :-
	addRowEltsToDiags(Prev, H, N, 0, NewDiags),
	N1 is N+1,
	addRowsToDiags(NewDiags, T, N1, Res).

addRowEltsToDiags(Diags, [], _, _, Diags).
addRowEltsToDiags(Prev, [H|T], RowN, N, Res) :-
	Index is RowN+N,
	set(Prev, Index, H, NewDiags),
	N1 is N+1,
	addRowEltsToDiags(NewDiags, T, RowN, N1, Res).

set([], 0, X, [[X]]).
set([H|[]], 1, X, Res) :- append([H], [[X]], Res).
set([H|T], 0, X, [[X|H]|T]).
set([H|T], I, X, [H|R]):- I > -1, NI is I-1, set(T, NI, X, R), !.

% test :- connect4_start(X),
%	diagWin2('X', X).
