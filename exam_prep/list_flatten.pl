
append([], X, X).
append([H|T1], X, [H|T2]) :- append(T1, X, T2).

listFlatten([], []).
listFlatten([H|T], Out) :- listFlattenDeep(H, O1), listFlatten(T, O2), !, append(O1, O2, Out).
listFlattenDeep([H|T], Out) :- !, listFlattenDeep(H, O1), listFlatten(T, O2), !, append(O1, O2, Out).
listFlattenDeep([], []).
listFlattenDeep(X, [X]).