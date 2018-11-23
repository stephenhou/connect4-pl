% To run it, try:
% play(X).

:- use_module(library(clpfd)).

% the starting state for every connect4 game
connect4_start(gameBoard([['*','*','*','*','*','*'],
	                      ['*','*','*','*','*','*'],
	                      ['*','*','*','*','*','*'],
	                      ['*','*','*','*','*','*'],
	                      ['*','*','*','*','*','*'],
	                      ['*','*','*','*','*','*'],
						  ['*','*','*','*','*','*']], [7, 7, 7, 7, 7, 7])).

% checks for row
rowWin(X, [H|T]) :-
	(  checkRow(X, 0, H)
    -> true
    ;  rowWin(X, T)
     ).

checkRow(_, 4, _).
checkRow(X, N, [X|T]) :-
	N1 is N+1,
	checkRow(X, N1, T).
checkRow(X, _, [H|T]) :-
	dif(X, H),
	checkRow(X, 0, T).

% checks for column
columnWin(X, GB) :-
	transpose(GB, Trans),
	rowWin(X, Trans).

getDiags(Matx, Diags) :-
	addRowsToDiags([], Matx, 0, Diags).

% checks for / diagonal
diagWin1(X, GB) :-
	getDiags(GB, Diags),
	rowWin(X, Diags).

% checks for \ diagonal
diagWin2(X, GB) :-
	rotateMatx(GB, Rotated),
	getDiags(Rotated, Diags),
	rowWin(X, Diags).

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

starting_player(0).
starting_player(1).

replace(I, L, E, K) :-
  nth1(I, L, _, R),
  nth1(I, K, E, R).


print_board(gameBoard(X, _)):-
  write("Choose column 1-6 to place piece"),
  nl,
  nl,
  print_board_rows(X).


print_board_rows([]) :- nl.
print_board_rows([H|T]):-
  write("  "),
  print_board_element(H),
  nl,
  print_board_rows(T).


print_board_element([]).
print_board_element([H|T]) :-
  write(H),
  write("   "),
  print_board_element(T).


play(Input) :-
  repeat,
  write("Who starts? 0=you, 1=computer"),
  nl,
  read(Input),
  (Input = 0 -> player_start, !;
  (Input = 1 -> computer_start, !;
  (fail_player_select))), !.

fail_player_select :-
  write("Invalid player to start. Please pick 0 or 1"),
  nl,
  fail.

player_start :-
  connect4_start(X),
  play_move('X', X), !.

computer_start :-
  connect4_start(X),
  play_move('O', X), !.

check_win(X, Board) :-
  rowWin(X, Board);
  columnWin(X, Board);
  diagWin1(X, Board);
  diagWin2(X, Board).

play_move(_, gameBoard(Board, Spaces)) :-
  check_board_full(Spaces),
  print_board(gameBoard(Board, _)),
  write("DRAW").

play_move('X', gameBoard(Board, _)) :-
  check_win('X', Board),
  print_board(gameBoard(Board, _)),
  write("COMPUTER WON").

play_move('O', gameBoard(Board, _)) :-
  check_win('O', Board),
  print_board(gameBoard(Board, _)),
  write("YOU WON").

play_move('O', gameBoard(Board, Spaces)) :-
  computer_move(gameBoard(Board, Spaces), Decision),
  update_height(Decision, Spaces, UpdatedSpaces, Height),
  update_board(Decision, Height, Board, 'O', UpdatedBoard),
  play_move('X', gameBoard(UpdatedBoard, UpdatedSpaces)).

play_move('X', gameBoard(Board, Spaces)) :- 
  print_board(gameBoard(Board, Spaces)),
  read_player_input(Input, Spaces),
  update_height(Input, Spaces, UpdatedSpaces, Height),
  update_board(Input, Height, Board, 'X', UpdatedBoard),
  play_move('O', gameBoard(UpdatedBoard, UpdatedSpaces)).

computer_move(gameBoard(Board, Spaces), Decision) :-
  %TODO incorporate check win check lose decisions
  repeat,
  random(1, 7, Decision),
  nth1(Decision, Spaces, Column),
  valid_height(Column), !.

check_board_full(Spaces) :-
  sort(0, @>=, Spaces,  Sorted),
  nth1(1, Sorted, Elem),
  \+valid_height(Elem).

read_player_input(Input, Spaces) :-
  repeat,
  read(Input),
  check_input(Input, Spaces), !.

update_height(Input, Spaces, UpdatedSpaces, Elem) :-
  nth1(Input, Spaces, Elem),
  replace(Input, Spaces, Elem1, UpdatedSpaces),
  Elem1 is Elem - 1.

update_board(Input, Height, Board, Player, UpdatedBoard) :-
  nth1(Height, Board, Row),
  replace(Input, Row, Player, UpdatedRow),
  replace(Height, Board, UpdatedRow, UpdatedBoard).
  
check_input(Input, Spaces) :- 
  valid_column(Input),
  nth1(Input, Spaces, Elem),
  valid_height(Elem).

valid_column(1).
valid_column(2).
valid_column(3).
valid_column(4).
valid_column(5).
valid_column(6).

valid_height(1).
valid_height(2).
valid_height(3).
valid_height(4).
valid_height(5).
valid_height(6).
valid_height(7).
