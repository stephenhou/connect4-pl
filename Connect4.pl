% To run it, try:
% play(X).

% the starting state for every connect4 game
connect4_start(gameBoard([['*','*','*','*','*','*'],
	                      ['*','*','*','*','*','*'],
	                      ['*','*','*','*','*','*'],
	                      ['*','*','*','*','*','*'],
	                      ['*','*','*','*','*','*'],
	                      ['*','*','*','*','*','*'],
	                      ['*','*','*','*','*','*']], [7, 7, 7, 7, 7, 7, 7])).

starting_player(0).
starting_player(1).

replace(I, L, E, K) :-
  nth1(I, L, _, R),
  nth1(I, K, E, R).


print_board(gameBoard(X, _)):-
  write("Choose column 1-7 to place piece"),
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

play_move('O', gameBoard(_, _)) :-
  write("Computer playing").

play_move('X', gameBoard(Board, Spaces)) :- 
  print_board(gameBoard(Board, Spaces)),
  read_player_input(Input),
  update_height(Input, Spaces, UpdatedSpaces, Height),
  update_board(Input, Height, Board, 'X', UpdatedBoard),
  play_move('O', gameBoard(UpdatedBoard, UpdatedSpaces)).

read_player_input(Input) :- 
  read(Input).

update_height(Input, Spaces, UpdatedSpaces, Elem) :-
  nth1(Input, Spaces, Elem),
  replace(Input, Spaces, Elem1, UpdatedSpaces),
  Elem1 is Elem - 1.

update_board(Input, Height, Board, Player, UpdatedBoard) :-
  nth1(Height, Board, Row),
  replace(Input, Row, Player, UpdatedRow),
  replace(Height, Board, UpdatedRow, UpdatedBoard).
  
check_input(Input, Spaces) :- 
  nth0(Input1, Spaces, Elem),
  write(Input1),
  write(Elem),
  Input1 is Input - 1,
  (Input > 7 -> fail, !;
  (Input < 1 -> fail, !;
  (Elem < 0 -> fail))), !.


