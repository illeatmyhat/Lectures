parent(tom, bob).
parent(bob, pat).
parent(bob, ada).
parent(pat, jim).
parent(ada, michelle).

male(tom).
male(bob).
male(jim).
male(pat).
female(ada).
female(michelle).

sibling(X, Y) :- parent(Z, X), parent(Z, Y), X \= Y.
brother(X, Y) :- male(X), sibling(X, Y).
aunt(X, Y) :- female(X), parent(Z, Y), sibling(X, Z).

ancestor(X, Y) :- parent(X, Y).
ancestor(X, Y) :- parent(Z, Y), ancestor(X, Z).

