:- use_module(library(clpfd)).

% Given a Sudoku puzzle as a list of lists, determine if the puzzle has
% a solution, where _ indicates an unknown square.
sudoku(Puzzle) :-
	% This is a verbose solution that will be improved upon.
	% Label each row as A..I.
	Puzzle = [A, B, C, D, E, F, G, H, I],

	% Each row is made of 9 values.
	A = [A1, A2, A3, A4, A5, A6, A7, A8, A9],
	B = [B1, B2, B3, B4, B5, B6, B7, B8, B9],
	C = [C1, C2, C3, C4, C5, C6, C7, C8, C9],
	D = [D1, D2, D3, D4, D5, D6, D7, D8, D9],
	E = [E1, E2, E3, E4, E5, E6, E7, E8, E9],
	F = [F1, F2, F3, F4, F5, F6, F7, F8, F9],
	G = [G1, G2, G3, G4, G5, G6, G7, G8, G9],
	H = [H1, H2, H3, H4, H5, H6, H7, H8, H9],
	I = [I1, I2, I3, I4, I5, I6, I7, I8, I9],

	% Each column is also 9 values, using the variables from the rows.
	Co1 = [A1, B1, C1, D1, E1, F1, G1, H1, I1],
	Co2 = [A2, B2, C2, D2, E2, F2, G2, H2, I2],
	Co3 = [A3, B3, C3, D3, E3, F3, G3, H3, I3],
	Co4 = [A4, B4, C4, D4, E4, F4, G4, H4, I4],
	Co5 = [A5, B5, C5, D5, E5, F5, G5, H5, I5],
	Co6 = [A6, B6, C6, D6, E6, F6, G6, H6, I6],
	Co7 = [A7, B7, C7, D7, E7, F7, G7, H7, I7],
	Co8 = [A8, B8, C8, D8, E8, F8, G8, H8, I8],
	Co9 = [A9, B9, C9, D9, E9, F9, G9, H9, I9],

	% The 9 "squares" are also of 9 values.
	Sq1 = [A1, A2, A3, B1, B2, B3, C1, C2, C3],
	Sq2 = [A4, A5, A6, B4, B5, B6, C4, C5, C6],
	Sq3 = [A7, A8, A9, B7, B8, B9, C7, C8, C9],
	Sq4 = [D1, D2, D3, E1, E2, E3, F1, F2, F3],
	Sq5 = [D4, D5, D6, E4, E5, E6, F4, F5, F6],
	Sq6 = [D7, D8, D9, E7, E8, E9, F7, F8, F9],
	Sq7 = [G1, G2, G3, H1, H2, H3, I1, I2, I3],
	Sq8 = [G4, G5, G6, H4, H5, H6, I4, I5, I6],
	Sq9 = [G7, G8, G9, H7, H8, H9, I7, I8, I9],

	% "ins" forces each element of a list to be in a particular range.
	% It is part of "constraint logic programming".
	A ins 1..9,
	B ins 1..9,
	C ins 1..9,
	D ins 1..9,
	E ins 1..9,
	F ins 1..9,
	G ins 1..9,
	H ins 1..9,
	I ins 1..9,

	% All items in a row must be different.
	all_different(A),
	all_different(B),
	all_different(C),
	all_different(D),
	all_different(E),
	all_different(F),
	all_different(G),
	all_different(H),
	all_different(I),
	% All items in a column must be different.
	all_different(Co1),
	all_different(Co2),
	all_different(Co3),
	all_different(Co4),
	all_different(Co5),
	all_different(Co6),
	all_different(Co7),
	all_different(Co8),
	all_different(Co9),
	% All items in a square must be different.
	all_different(Sq1),
	all_different(Sq2),
	all_different(Sq3),
	all_different(Sq4),
	all_different(Sq5),
	all_different(Sq6),
	all_different(Sq7),
	all_different(Sq8),
	all_different(Sq9),

	% At this point Prolog will report success, but won't give all the values
	% of the unknown variables. "label" will force this.
	label(A),
	label(B),
	label(C),
	label(D),
	label(E),
	label(F),
	label(G),
	label(H),
	label(I).

% To solve a puzzle, run this query:
/*
Puzzle = [
   [8,_,_,_,_,_,_,_,_],
   [_,_,3,6,_,_,_,_,_],
   [_,7,_,_,9,_,2,_,_],
   [_,5,_,_,_,7,_,_,_],
   [_,_,_,_,4,5,7,_,_],
   [_,_,_,1,_,_,_,3,_],
   [_,_,1,_,_,_,_,6,8],
   [_,_,8,5,_,_,_,1,_],
   [_,9,_,_,_,_,4,_,_]
   ],
   Puzzle = [A,B,C,D,E,F,G,H,I],
   sudoku([A,B,C,D,E,F,G,H,I]).
*/
% Prolog will print the values of the variables A through I,
% which will be the rows of the solved puzzle.
