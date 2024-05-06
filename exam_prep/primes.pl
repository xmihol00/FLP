prime(X, Y) :- X - 1 =:= Y.
prime(X, Y) :- X mod Y =\= 0, Z is Y + 1, prime(X, Z).
prime(2) :- !.
prime(X) :- var(X), !, num(X), prime(X, 2), !.
prime(X) :- X > 2, prime(X, 2), !.

num(3).
num(X) :- num(Y), X is Y + 1.
