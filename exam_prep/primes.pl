primeTest(X, Y) :- X - 1 =:= Y.
primeTest(X, Y) :- X mod Y =\= 0, Z is Y + 1, primeTest(X, Z).
primeTest(2).
primeTest(X) :- num(X), primeTest(X, 2).
primeTest(X) :- primeTest(X, 2), !.

num(3).
num(X) :- \+ number(X), num(Y), X is Y + 1.

inf(3).
inf(X) :- inf(Y), X is Y + 1.
inf(X) :- inf(X).

smallerThan(3, 2) :- !.
smallerThan(X, Y) :- Y is X - 1.
smallerThan(X, Y) :- XX is X - 1, smallerThan(XX, Y).

isNotPrime(X) :- smallerThan(X, Y1), smallerThan(X, Y2), X is Y1 * Y2.
isPrime(X) :- isNotPrime(X), !, fail.
isPrime(_).

prime(2).
prime(X) :- inf(X), isPrime(X).
