
append([], X, X).
append([H|T1], X, [H|T2]) :- append(T1, X, T2).

elem(E, L) :- append(_, [E|_], L), !.

replaceAll(O, N, IL, OL) :- append(S, [O|T], IL), append(S, [N|T], ILL), !, replaceAll(O, N, ILL, OL).
replaceAll(O, N, [O], [N]).
replaceAll(_, _, L, L).

removeAll(E, IL, OL) :- append(S, [E|T], IL), append(S, T, ILL), !, removeAll(E, ILL, OL).
removeAll(_, L, L).

intersect([H|T], S2, I) :- removeAll(H, S2, SS2), !, intersect(T, SS2, I).
intersect([_|T], S2, I) :- intersect(T, S2, I).
intersect([], S2, S2).

/* replace all unbound occurrences of Old with New */
substitute(l(S, E), Old, New, l(S, O)) :- \+ elem(New, S), intersect(S, E, I), elem(Old, I), !, replaceAll(Old, New, E, R), substituteH(R, Old, New, O).
substitute(X, _, _, X).
substituteH([], _, _, []).
substituteH([l(S, E)|T], Old, New, [l(S, O)|TT]) :- substitute(l(S, E), Old, New, l(S, O)), !, substituteH(T, Old, New, TT).
substituteH([H|T], Old, New, [H|TT]) :- substituteH(T, Old, New, TT).

/* replace all bound occurrences of Old with New */
/* TODO */


printLambda(l(S, E)) :- write("λ"), printLambdaH(l(S, E)), !.
printLambdaH(l([], [])) :- !.
printLambdaH(l([H], R)) :- write(H), write("."), printLambdaH(l([], R)), !.
printLambdaH(l([H|T], R)) :- write(H), printLambdaH(l(T, R)), !.
printLambdaH(l([], [l(S, E)|T])) :- write("("), printLambda(l(S, E)), write(")"), printLambdaH(l([], T)), !.
printLambdaH(l([], [H|T])) :- write(H), printLambdaH(l([], T)), !.

/* substitute(l([x,y,z],[a,x,z,l([b],[a,b]),a,l([a,v],[a,d]),y]), a, q, L). */
/* substitute(l([a],[a,b]), b, a, L). */