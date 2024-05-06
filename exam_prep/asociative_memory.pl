:- dynamic mem/3.

dict(Dict, Key, Value) :- \+ var(Dict), \+ var(Key), \+ var(Value), (retract(mem(Dict, Key, _)); true), assertz(mem(Dict, Key, Value)), !.
dict(Dict, Key, Value) :- \+ var(Dict), mem(Dict, Key, Value).

member(_, []) :- fail.
member(X, [X|_]) :- !.
member(X, [_|T]) :- member(X, T).

:- dynamic mem/2.

keyval(K, V) :- \+ var(K), \+ var(V), !, \+ mem(K, _), assertz(mem(K, V)).
keyval(K, V) :- \+ var(K), var(V), !, mem(K, V).
keyval(Ks, V) :- var(Ks), \+ var(V), !, collect(V, [], Ks).
collect(V, Found, New) :- mem(K, V), \+ member(K, Found), !, Next = [K|Found], collect(V, Next, New).
collect(_, X, X).
