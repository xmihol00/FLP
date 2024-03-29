read_line(Line, LastChar) :-
	get_char(LastChar),
	(
        (
            LastChar == end_of_file;
	        (char_code(LastChar, Code), Code == 10)
        ), Line = [], !;
		read_line(L, _), Line = [LastChar | L]
    ).

parse_input :- read_line(L, C), 
        ( 
            C == end_of_file;
	        parse_line(L, A, B), assert(edge(A, B)), assert(edge(B, A)), assert(node(A)), assert(node(B)), parse_input();
            parse_input()
	    ).

parse_line([A, ' ', B], A, B).
parse_line(_, _, _) :- fail.

all_nodes(Nodes) :- setof(X, node(X), Nodes).
all_edges(Edges) :- setof([X, Y], edge(X, Y), Edges).

hamilton_cycle(Start, Cycle) :- 
    all_nodes(Nodes),
    length(Nodes, NodesLen),
    hamilton_cycle(Start, Start, [Start], NodesLen, Cycle).
hamilton_cycle(Start, Current, Visited, CycleLen, Cycle) :- 
    edge(Current, Next), 
    \+ member(Next, Visited), 
    hamilton_cycle(Start, Next, [Next | Visited], CycleLen, Cycle).
hamilton_cycle(Start, Current, Visited, CycleLen, Cycle) :- 
    length(Visited, CycleLen), 
    edge(Current, Start),
    Cycle = Visited.

rotate_list([], _, []).
rotate_list(List, Pivot, Rotated) :-
    append(Left, [Pivot|Right], List),
    append([Pivot|Right], Left, Rotated).

unique_hamilton_cycle(Start, UniqueCycle) :- 
    hamilton_cycle(Start, Cycle),
    rotate_list(Cycle, Start, RotatedCycle),
    [_ | RotatedTail] = RotatedCycle,
    reverse(RotatedTail, ReversedRotatedTail),
    (
        (\+ current_predicate(found_cycles/1));
        (\+ found_cycles(RotatedTail), \+ found_cycles(ReversedRotatedTail))
    ),
    assert(found_cycles(RotatedTail)),
    UniqueCycle = RotatedCycle.

insertUntil(Element, N, Xs, Ys) :-
    append(Start, End, Xs),
    length(Start, M),
    N > M,
    append(Start, [Element|End], Ys).

hamilton_cycle_odd(Nodes, Cycle) :- 
    [Head1, Head2 | Tail]=Nodes, 
    permutation(Tail, P), 
    insertUntil(Head2, 2, P, Inserted), 
    Full=[Head1|Inserted], 
    is_cycle(Full), 
    Cycle=Full.

hamilton_cycle_even(Nodes, Cycle) :- 
    [Head1, Head2, Head3 | Tail] = Nodes,
    length(Nodes, Len),
    OddLen is Len-1,
    permutation(Tail, P), 
    insertUntil(Head3, 2, P, Inserted), 
    Partial=[Head1|Inserted], 
    insertUntil(Head2, OddLen, Partial, Full),
    is_cycle(Full), 
    Cycle=Full.

hamilton_cycle_small(Nodes, Cycles) :- 
    length(Nodes, Len),
    (
        (Len =:= 1, [H1] = Nodes, edge(H1, H1), Cycles = [Nodes]);
        (Len =:= 3, [H1, H2, H3] = Nodes, edge(H1, H2), edge(H2, H3), edge(H3, H1), Cycles = [Nodes]);
        (Len =:= 2, [H1, H2] = Nodes, edge(H1, H2), edge(H2, H1), Cycles = [Nodes]);
        Cycles = []
    ).

is_cycle([A | T]) :- is_cycle([A | T], A).
is_cycle([A, B | T], X) :- edge(A, B), is_cycle([B | T], X). 
is_cycle([A], X) :- edge(A, X).

factorial(N, F) :- 
    N > 0, 
    Next is N - 1, 
    factorial(Next, Prev), 
    F is N * Prev.
factorial(0, 1).

power(X, N, Y) :- 
    N > 0, 
    Next is N - 1, 
    power(X, Next, Prev), 
    Y is X * Prev.
power(_, 0, 1).

find_cycles_via_nodes(Cycles) :-
    all_nodes(Nodes), 
    length(Nodes, NodesLen),
    sort(Nodes, SortedNodes),
    (
        NodesLen =< 3 ->
            hamilton_cycle_small(SortedNodes, UnrotatedCycles);
            (
                NodesLen /\ 1 =:= 1 -> 
                    findall(C, hamilton_cycle_odd(SortedNodes, C), UnrotatedCycles);
                    findall(C, hamilton_cycle_even(SortedNodes, C), UnrotatedCycles)
            )
    ),
    [Head | _] = SortedNodes,
    maplist({Head}/[L, O]>>(rotate_list(L, Head, O)), UnrotatedCycles, Cycles).

find_cycles_via_edges(Cycles) :-
    all_nodes(Nodes), 
    sort(Nodes, SortedNodes),
    [Head | _] = SortedNodes,
    findall(C, unique_hamilton_cycle(Head, C), Cycles).

print_cycles([]).
print_cycles([H|T]) :- print_cycle(H), print_cycles(T).

print_cycle([]).
print_cycle([H|T]) :- print_cycles([H|T], H).
print_cycles([H1, H2 | T], F) :- write(H1-H2), write(' '), print_cycles([H2 | T], F).
print_cycles([H], F) :- writeln(H-F).

test_print_cycles([]).
test_print_cycles([H|T]) :- test_print_cycle(H), test_print_cycles(T).

test_print_cycle([H|T]) :- write(H), write(' '), test_print_cycle(T).
test_print_cycle([]) :- nl.

main(Cycles) :-
    parse_input(),
    all_nodes(Nodes), 
    all_edges(Edges),
    length(Nodes, NodesLen),
    length(Edges, EdgesLen),
    factorial(NodesLen, F),
    EdgesPerNode is EdgesLen / NodesLen,
    power(EdgesPerNode, NodesLen, P),
    (
        F < P -> 
            find_cycles_via_nodes(Cycles);
            find_cycles_via_edges(Cycles)
    ).

main_print :-
    main(Cycles),
    print_cycles(Cycles).

main_test_print :-
    main(Cycles),
    test_print_cycles(Cycles).
    