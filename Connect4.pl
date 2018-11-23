% To run it, try:
% play(X).

% the starting state for every connect4 game
connect4_start(gameBoard([['*','*','*','*','*','*'],
	                      ['*','*','*','*','*','*'],
	                      ['*','*','*','*','*','*'],
	                      ['*','*','*','*','*','*'],
	                      ['*','*','*','*','*','*'],
	                      ['*','*','*','*','*','*'],
	                      ['*','*','*','*','*','*']], [7, 0, 0, 0, 0, 0, 0])).

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

check_win(Board) :-
  %TODO
  false.

play_move('X', gameBoard(Board, _)) :-
  check_win(Board),
  print_board(gameBoard(Board, _)),
  write("COMPUTER WON").

play_move('O', gameBoard(Board, _)) :-
  check_win(Board),
  print_board(gameBoard(Board, _)),
  write("YOU WON").

play_move('O', gameBoard(Board, Spaces)) :-
  computer_move(gameBoard(Board, Spaces), Decision),
  update_height(Decision, Spaces, UpdatedSpaces, Height),
  update_board(Decision, Height, Board, 'O', UpdatedBoard),
  play_move('X', gameBoard(UpdatedBoard, UpdatedSpaces)).

play_move(_, gameBoard(Board, Spaces)) :-
  check_board_full(Spaces),
  print_board(gameBoard(Board, _)),
  write("DRAW").

play_move('X', gameBoard(Board, Spaces)) :- 
  print_board(gameBoard(Board, Spaces)),
  read_player_input(Input, Spaces),
  update_height(Input, Spaces, UpdatedSpaces, Height),
  update_board(Input, Height, Board, 'X', UpdatedBoard),
  play_move('O', gameBoard(UpdatedBoard, UpdatedSpaces)).

computer_move(gameBoard(Board, Spaces), Decision) :-
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


