% To run it, try:
% play.

% the starting state for every connect4 game
connect4_start(gameBoard([['*','*','*','*','*','*'],
	                      ['*','*','*','*','*','*'],
	                      ['*','*','*','*','*','*'],
	                      ['*','*','*','*','*','*'],
	                      ['*','*','*','*','*','*'],
	                      ['*','*','*','*','*','*'],
	                      ['*','*','*','*','*','*']])).

print_board(gameBoard(X)):-
  write("Choose column 1-7 to place piece"),
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

play :-
  connect4_start(X),
  print_board(X).

