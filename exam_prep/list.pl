
append([], X, X).
append([H|T1], X, [H|T2]) :- append(T1, X, T2).

listFlatten([], []).
listFlatten([H|T], Out) :- listFlattenDeep(H, O1), listFlatten(T, O2), !, append(O1, O2, Out).
listFlattenDeep([H|T], Out) :- !, listFlattenDeep(H, O1), listFlatten(T, O2), !, append(O1, O2, Out).
listFlattenDeep([], []).
listFlattenDeep(X, [X]).

isPrefix(X, Y) :- append(X, _, Y).
dropX(X, Y, Z) :- append(X, Z, Y).

elem(E, L) :- append(_, [E|_], L), !.
removeElem(E, L, R) :- append(S, [E|T], L), !, append(S, T, R).
removeAllElems(E, L, R) :- append(S, [E|T], L), !, append(S, T, RR), removeAllElems(E, RR, R).
removeAllElems(_, L, L).

elemVar(_, []) :- !, fail.
elemVar(E, [H|_]) :- \+ var(H), E = H, !.
elemVar(E, [_|T]) :- elemVar(E, T).

removeVarElem(E, [H|T], T) :- \+ var(H), E = H, !.
removeVarElem(E, [H|T], [H|R]) :- removeElem(E, T, R).