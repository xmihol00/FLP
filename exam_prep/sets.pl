
myMember(_, []) :- !, fail.
myMember(X, [H|T]) :- X \== H, !, myMember(X, T).
myMember(X, [X|_]) :- !.

myAppend([], X, X) :- !.
myAppend([H|T], X, Y) :- myAppend(T, X, YY), Y = [H|YY], !.
myAppend(Y, X, [H|T]) :- myAppend(YY, X, T), Y = [H|YY], !.

mySplit(List, Pivot, Left, Right) :- append(Left, [Pivot|Right], List).

less5(X) :- X =< 5.

splitCall(_, [], [], []).
splitCall(F, [H|T], Left, Right) :- call(F, H), !, splitCall(F, T, L, Right), Left = [H|L].
splitCall(_, Right, [], Right).

universum([], []).
universum([H|T], U) :- universum(T, UU), uniqueAppend(H, UU, U).

uniqueAppend([], A, A) :- !.
uniqueAppend([H|T], R, A) :- myMember(H, R), !, uniqueAppend(T, R, A).
uniqueAppend([H|T], R, A) :- RR = [H|R], uniqueAppend(T, RR, A).

setComplement([], U, U) :- !.
setComplement([Hs|Ts], U, D) :- myAppend(Start, [Hs|End], U), !, myAppend(Start, End, UU), setComplement(Ts, UU, D).

complements(Sets, Diffs) :- universum(Sets, U), complements(Sets, U, Diffs).
complements([], _, []) :- !.
complements([H|T], U, Diffs) :- setComplement(H, U, D), !, complements(T, U, Ds), Diffs = [D|Ds]. 

