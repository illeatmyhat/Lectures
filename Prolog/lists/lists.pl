% Check if a list contains the number 0.
contains0([0|_]). % if the head is 0, we don't care about the tail.
contains0([_|T]) :- contains0(T). % if the head is not 0, check the tail.

% Check if a list contains the given value.
contains([X|_], X).
contains([_|T], X) :- contains(T, X).

% Check if the given list has the given length.
mylength([], 0).
mylength([_|T], L) :- length(T, S), L is S + 1.

% Check if the sum of the elements of a list matches the given value.
sum([], 0).
sum([H|T], S) :- sum(T, D), S is D + H.


% mylength and sum are not tail-recursive, so let's try making them so.
mylength2([H|T], L) :- mylength2([H|T], L, 0). % the last param will accumulate.
mylength2([], L, L). % base case of recursion.
mylength2([_|T], L, A) :- S is A + 1, mylength2(T, L, S).

sum2([H|T], S) :- sum2([H|T], S, 0).
sum2([], S, S).
sum2([H|T], Sum, Acc) :- N is Acc + H, sum2(T, Sum, N).

% append
append([], L, L).
append([H|T], L, [H|Result]) :- append(T, L, Result).

evens([], []).
evens([H|T], [H|E]) :- H rem 2 =:= 0, evens(T, E).
evens([_|T], E) :- evens(T, E).

% ordered: if the list is in nondecreasing order.
ordered([X]). % list of 1 element is ordered.
ordered([X,Y|T]) :- X =< Y, ordered([Y|T]). % list of at least 2 elements is ordered if the first is <= second, and rest is ordered.

% quicksort: if the given list, when sorted, is equal to S.
quicksort([X|Xs], S) :-
	partition(Xs, X, Littles, Bigs),
	quicksort(Littles, Ls),
	quicksort(Bigs, Bs),
	append(Ls, [X|Bs], S).
quicksort([], []).

% partition: if the given list, when partitioned around Y, results in
% one list of elements smaller than Y, and one list of elements larger
% than Y.
partition([], Y, [], []).
partition([X|Xs], Y, [X|Ls], Bs) :- X =< Y, partition(Xs, Y, Ls, Bs).
partition([X|Xs], Y, Ls, [X|Bs]) :- X > Y, partition(Xs, Y, Ls, Bs).


