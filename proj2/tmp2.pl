
edge("A", "B").
edge("B", "C").
edge("C", "D").
edge("D", "E").
edge("E", "A").
edge("A", "D").
edge("E", "C").

insertUntil(Element, N, Xs, Ys) :-
    append(Start, End, Xs),
    length(Start, M),
    N > M,
    append(Start, [Element|End], Ys).

hamilton_cycle(Cycle) :- permutation(["C", "D", "E"], P), insertUntil("B", 2, P, Inserted), Full=["A"|Inserted], is_cycle(Full), Cycle=Full.

is_cycle([A | T]) :- is_cycle([A | T], A).
is_cycle([A, B | T], X) :- edge(A, B), is_cycle([B | T], X). 
is_cycle([A], X) :- edge(A, X).

main :- findall(C, hamilton_cycle(C), Cycles), writeln(Cycles).