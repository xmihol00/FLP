:- dynamic mem/2.

notElem([], _).
notElem([H|_], H) :- !, fail.
notElem([_|T], X) :- notElem(T, X).

keyVal(K, V) :- \+ var(K), \+ var(V), \+ mem(K, _), assertz(mem(K, V)), !.
keyVal(K, V) :- \+ var(K), var(V), mem(K, V), !.
keyVal(Ks, V) :- var(Ks), \+ var(V), keyVal([], Ks, V), !.
keyVal(Kin, Kout, V) :- mem(K, V), notElem(Kin, K), !, keyVal([K|Kin], Kout, V). 
keyVal(Kin, Kin, _).

getAll(KVs) :- getAll([], KVs).
getAll(KVsIn, KVsOut) :- mem(K, V), notElem(KVsIn, (K, V)), !, getAll([(K, V)|KVsIn], KVsOut).
getAll(KVsIn, KVsIn).

fromList([]).
fromList([(K, V)|T]) :- keyVal(K, V), !, fromList(T).
fromList([_|T]) :- fromList(T).

remKey(K) :- mem(K, _), retract(mem(K, _)).
remAll :- retract(mem(_, _)), !, remAll.
remAll :- true.
