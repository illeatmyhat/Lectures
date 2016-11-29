:- use_module(library(clpfd)).

% Based on http://programmablelife.blogspot.co.at/2012/07/adventures-in-declarative-programming.html

sudoku(Rows) :-
	% append/2 flattens the first argument into the second,
	% giving a single list of all the squares in the puzzle.
	append(Rows, Values),
	Values ins 1..9,
	% maplist/2 corresponds to "all"; it is true if the given predicate function is true
	% when applied to all elements of the given list.
	maplist(all_different, Rows),

	% transpose/2 does a matrix transpose on the puzzle to give a list of
	% columns instead of rows.
	transpose(Rows, Cols),
	maplist(all_different, Cols),

	% The tricky part: doing the "squares". First give each row a name.
	Rows = [A, B, C, D, E, F, G, H, I],
	% Check that rows A, B, C can be grouped 3 at a time to produce	9 different values.
	squares(A, B, C),
	squares(D, E, F),
	squares(G, H, I),

	maplist(label, Rows).

squares([], [], []).
% Given three lists of at least 3 elements, see if the first 3 elements
% of each list together make a list of 9 different values. Then see if
% the same is true of the remaining elements of each list.
squares([A,B,C|T1], [D,E,F|T2], [G,H,I|T3]) :-
	all_different([A,B,C,D,E,F,G,H,I]),
	squares(T1, T2, T3).










