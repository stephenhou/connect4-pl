% To run it, try:
% play(Start).

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
rowWin(X, [H|T], N) :-
	(  checkRow(X, 0, H, N)
    -> true
    ;  rowWin(X, T, N)
     ).

checkRow(_, N, _, N).
checkRow(X, N, [X|T], Lim) :-
	N1 is N+1,
	N < Lim,
	checkRow(X, N1, T, Lim).
checkRow(X, N, [H|T], Lim) :-
	dif(X, H),
	N < Lim,
	checkRow(X, 0, T, Lim).

% checks for column
columnWin(X, GB, N ) :-
	transpose(GB, Trans),
	rowWin(X, Trans, N).

getDiags(Matx, Diags) :-
	addRowsToDiags([], Matx, 0, Diags).

% checks for / diagonal
diagWin1(X, GB, N) :-
	getDiags(GB, Diags),
	rowWin(X, Diags, N).

% checks for \ diagonal
diagWin2(X, GB, N) :-
	rotateMatx(GB, Rotated),
	getDiags(Rotated, Diags),
	rowWin(X, Diags, N).

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

check_win(X, Board, N) :-
  rowWin(X, Board, N);
  columnWin(X, Board, N);
  diagWin1(X, Board, N);
  diagWin2(X, Board, N).

play_move(_, gameBoard(Board, Spaces)) :-
  check_board_full(Spaces),
  print_board(gameBoard(Board, _)),
  write("DRAW").

play_move(_, gameBoard(Board, _)) :-
  check_win('X', Board, 4),
  print_board(gameBoard(Board, _)),
  write("YOU WON").

play_move(_, gameBoard(Board, _)) :-
  check_win('O', Board, 4),
  print_board(gameBoard(Board, _)),
  write("COMPUTER WON").

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

%test(Decision) :- connect4_start(X),
%  chooseCompMove(6, Decision, X, 0, 4).

chooseCompMove(_, 0, _, _, 0, _) :- false.
chooseCompMove(_, N, N, _, 1, _).
chooseCompMove(X, N, Decision, gameBoard(Board, Spaces), 0, Target) :-
  nth1(N, Spaces, Column),
  (  valid_height(Column)
  -> update_height(N, Spaces, _, Height),
     update_board(N, Height, Board, X, UpdatedBoard),
     (  check_win(X, UpdatedBoard, Target)
     -> chooseCompMove(X, N, Decision, _, 1, Target)
     ;  N1 is N-1,
        chooseCompMove(X, N1, Decision, gameBoard(Board, Spaces), 0, Target)
     )
  ;  N1 is N-1,
     chooseCompMove(X, N1, Decision, gameBoard(Board, Spaces), 0, Target)
  ).
  

computer_move(gameBoard(Board, Spaces), Decision) :-
  chooseCompMove('O', 6, Decision, gameBoard(Board, Spaces), 0, 4);
  chooseCompMove('X', 6, Decision, gameBoard(Board, Spaces), 0, 4);
  chooseCompMove('O', 6, Decision, gameBoard(Board, Spaces), 0, 3);
  chooseCompMove('X', 6, Decision, gameBoard(Board, Spaces), 0, 3);
  chooseCompMove('O', 6, Decision, gameBoard(Board, Spaces), 0, 2);
  chooseCompMove('X', 6, Decision, gameBoard(Board, Spaces), 0, 2);
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
