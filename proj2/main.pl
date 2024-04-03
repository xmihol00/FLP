/* -----------------------------------------------------------------------------------------------------------------------
--   project: flp23-log (2nd project regarding hamiltonian cycles to Functional and Logic Programming course at FIT, BUT)
--    author: David Mihola (xmihol00)
--     email: xmihol00@stud.fit.vutbr.cz
--      date: 28. 4. 2024
-------------------------------------------------------------------------------------------------------------------------- */

/** 
 * @brief Reads a line from the input and returns it as a list of characters. The last character is also returned explicitly.
 * @param -Line The list of characters representing the line.
 * @param -LastChar The last character of the line.
 */
read_line(Line, LastChar) :-
    /* read a character from STDIN */
	get_char(LastChar),
	(   
        /* end recursion if the character is the end of file or a newline character */
        (
            LastChar == end_of_file;
	        (char_code(LastChar, Code), Code == 10)
        ), Line = [], !;
        /* continue reading the line (recursive call) and prepend the read character to the rest of the read line */
		read_line(L, _), Line = [LastChar | L]
    ).

/** 
 * @brief Parses the input and inserts non-duplicit nodes and edges into the database.
 */
parse_input :- read_line(L, C), 
        (
            /* 
            switch into 3 possible cases:
                a) end of file  -> stop parsing
                b) valid line   -> insert new (check for duplicates) edges and nodes to the database and continue parsing (recursive call)
                c) invalid line -> skip and continue parsing (recursive call)
            */
            C == end_of_file;
	        parse_line(L, A, B), 
                (
                    /* 
                    if the node predicate does not exists yet, then add any node to the database, 
                    or check if the first node exists and only add it to the database if it does not
                    */
                    (\+ current_predicate(node/1)), assert(node(A));
                    node(A);
                    assert(node(A))
                ), 
                (
                    /* 
                    check if the second node exists and only add it to the database if it does not
                    */
                    node(B); 
                    assert(node(B))
                ), 
                (
                    /*
                    if the edge predicate does not exists yet, then add any edge to the database,
                    or check if the edge exists and only add it to the database if it does not
                    */
                    (\+ current_predicate(edge/2)), assert(edge(A, B));
                    edge(A, B);
                    assert(edge(A, B))
                ), 
                (
                    /* 
                    check if the reversed edge exists and only add it to the database if it does not
                    */
                    edge(B, A); 
                    assert(edge(B, A))
                ), parse_input();
            parse_input()
	    ).

/**
 * @brief Parses a line and returns the two nodes. If the line is invalid, the predicate fails.
 * @param +Line The line to be parsed.
 * @param -A The first node.
 * @param -B The second node.
 */
parse_line([A, ' ', B], A, B).
parse_line(_, _, _) :- fail.

/** 
 * @brief Finds all nodes in the database and returns them as a list.
 * @param -Nodes The list of all nodes.
 */
all_nodes(Nodes) :- setof(X, node(X), Nodes).

/** 
 * @brief Finds all edges in the database and returns them as a list.
 * @param -Edges The list of all edges.
 */
all_edges(Edges) :- setof([X, Y], edge(X, Y), Edges).

/**
 * @brief Rotates a list by a given pivot so that the list starts with the pivot. The pivot must be in the list, or the list must be empty.
 * @param +List The list to be rotated.
 * @param +Pivot The pivot to rotate the list by.
 * @param -Rotated The rotated list.
 */
rotate_list(List, Pivot, Rotated) :-
    /* split the list by the pivot */
    append(Left, [Pivot|Right], List),
    /* append the left part to the right part in reverse order */
    append([Pivot|Right], Left, Rotated).
rotate_list([], _, []).

/**
 * @brief Inserts an element into a list at a position smaller than a given index. The index must not be larger than the length of the list.
 *        Note: call the predicate with findall/3 to get all possible insertions.
 * @param +Element The element to be inserted.
 * @param +N The index to insert until.
 * @param +Xs The list to be inserted into.
 * @param -Ys The list with the inserted element.
 */
insertUntil(Element, N, Xs, Ys) :-
    /* split the list into two arbitrary parts */
    append(Start, End, Xs),
    /* get the lengh of the first part */
    length(Start, M),
    /* check if the length is smaller than the given index */
    N > M,
    /* insert the given element between the first and second part of the list */
    append(Start, [Element|End], Ys).

/** 
 * @brief Finds a hamiltonian cycle starting at a given node using edges as the space of search.
 * @param +Start The node to start the cycle at.
 * @param -Cycle The list of nodes representing the cycle.
 */
/* the main/starting predicate */
hamiltonian_cycle(Start, Cycle) :-
    /* retreive all nodes from the database */
    all_nodes(Nodes),
    /* get the number of nodes */
    length(Nodes, NodesLen),
    /* begin the recursive search */
    hamiltonian_cycle(Start, Start, [Start], NodesLen, Cycle).
/* the recursive predicate (only if the current path can continue) */
hamiltonian_cycle(Start, Current, Visited, CycleLen, Cycle) :-
    /* get a next node connected to the current node, all possible next nodes will be eventualy retreived, when the follwing predicates fail */
    edge(Current, Next), 
    /* check if the next node has not been visited yet */
    \+ member(Next, Visited), 
    /* add the next node to the visited nodes and continue the search */
    hamiltonian_cycle(Start, Next, [Next | Visited], CycleLen, Cycle).
/* the end of recursion predicate, i.e. cycle found */
hamiltonian_cycle(Start, Current, Visited, CycleLen, Cycle) :-
    /* check if the path has the required length, i.e. same as the number of nodes in the database */
    length(Visited, CycleLen), 
    /* check if there is an edge between the last and the first node */
    edge(Current, Start),
    /* cycle found */
    Cycle = Visited.

/**
 * @brief Finds a unique hamiltonian cycle with each call starting at a given node, which must not change in successive calls, using nodes as the space of search.
 * @param +Start The node to start the cycle at.
 * @param -UniqueCycle The list of nodes representing the cycle.
 */
unique_hamiltonian_cycle(Start, UniqueCycle) :- 
    /* find any hamiltonian cycle */
    hamiltonian_cycle(Start, Cycle),
    /* convert the cycle to its canonical form, i.e. specific starting element, rest of the list and reversed rest of the list, see README.md for more detail */
    rotate_list(Cycle, Start, RotatedCycle),
    [_ | RotatedTail] = RotatedCycle,
    reverse(RotatedTail, ReversedRotatedTail),
    /* succedess if therese is no cycle in the database yet, or the cycle is not in the database yet */
    (
        (\+ current_predicate(found_cycles/1));
        (\+ found_cycles(RotatedTail), \+ found_cycles(ReversedRotatedTail))
    ),
    /* add the not yet found cycle to the database */
    assert(found_cycles(RotatedTail)),
    /* unique cycle found */
    UniqueCycle = RotatedCycle.

/**
 * @brief Finds a unique hamiltonian cycle with each call in a graph with odd number of nodes using nodes as the space of search.
 * @param +Nodes The list of nodes to search the cycle in.
 * @param +Len The length of the list of nodes.
 * @param -UniqueCycle The list of nodes representing the cycle.
 */
hamiltonian_cycle_odd(Nodes, Len, UniqueCycle) :- 
    /* get the position of the node in the middle of the list */
    HalfLen is Len // 2,
    /* take out the first two nodes from the node list to break the permutation symmetry, see README.md for more detail */
    [Head1, Head2|Tail]=Nodes, 
    /* find a permutation of the rest of the nodes */
    permutation(Tail, Permutation), 
    /* extend the permutation non-symmetrically */
    insertUntil(Head2, HalfLen, Permutation, Inserted), 
    /* append the starting node, which is the same for all potential cycles */
    Full=[Head1|Inserted], 
    /* check whether the current permutation is a cycle, i.e. edges exist in between the nodes */
    is_cycle(Full), 
    /* unique cycle found */
    UniqueCycle=Full.

/**
 * @brief Finds a unique hamiltonian cycle with each call in a graph with even number of nodes using nodes as the space of search.
 * @param +Nodes The list of nodes to search the cycle in.
 * @param +Len The length of the list of nodes.
 * @param -UniqueCycle The list of nodes representing the cycle.
 */
hamiltonian_cycle_even(Nodes, Len, UniqueCycle) :- 
    /* get the length of the list without one node first node, i.e. the length as if the list had odd length */
    OddLen is Len - 1,
    /* get the position of the node in the middle of the odd length list  */
    HalfLen is (Len - 1) // 2,
    /* take out the first three nodes from the node list to break the permutation symmetry, see README.md for more detail */
    [Head1, Head2, Head3|Tail] = Nodes,
    /* find a permutation of the rest of the nodes */
    permutation(Tail, P), 
    /* extend the permutation non-symmetrically */
    insertUntil(Head3, HalfLen, P, Inserted), 
    /* append the starting node, which is the same for all potential cycles */
    Partial=[Head1|Inserted], 
    /* extend the permutation non-symmetrically again */
    insertUntil(Head2, OddLen, Partial, Full),
    /* check whether the current permutation is a cycle, i.e. edges exist in between the nodes */
    is_cycle(Full), 
    /* unique cycle found */
    UniqueCycle=Full.

/**
 * @brief Finds all unique hamiltonian cycles in a graph with 1, 2 or 3 nodes.
 * @param +Nodes The list of nodes to search the cycle in.
 * @param -UniqueCycles The list of lists of nodes representing unique cycles.
 */
hamiltonian_cycle_small(Nodes, UniqueCycles) :- 
    length(Nodes, Len),
    (
        /* switch based on the number of nodes and return found cycle */
        (Len =:= 1, [H1] = Nodes, edge(H1, H1), UniqueCycles = [Nodes]);
        (Len =:= 2, [H1, H2] = Nodes, edge(H1, H2), edge(H2, H1), UniqueCycles = [Nodes]);
        (Len =:= 3, [H1, H2, H3] = Nodes, edge(H1, H2), edge(H2, H3), edge(H3, H1), UniqueCycles = [Nodes]);
        /* no cycle found or the graph has more than 3 nodes */
        UniqueCycles = []
    ).

/**
 * @brief Checks whether a list of nodes represents a cycle in a graph.
 * @param +Nodes The list of nodes to check.
 */
/* the main/starting predicate */
is_cycle([First|Rest]) :- is_cycle([First|Rest], First).
/* the recursive predicate (only if the expected path exists) */
is_cycle([First, Second|Rest], Initial) :- edge(First, Second), is_cycle([Second|Rest], Initial). 
/* the end of recursion predicate, i.e. cycle exists */
is_cycle([Last], Initial) :- edge(Last, Initial).

/**
 * @brief Calculates the factorial of a given number.
 * @param +Number The number to calculate the factorial of.
 * @param -Factorial The factorial of the given number.
 */
/* recursive case */
factorial(Number, Factorial) :- 
    Number > 0, 
    Next is Number - 1, 
    factorial(Next, Previous), 
    Factorial is Number * Previous.
/* base case */
factorial(0, 1).
/* return 0 instead of fail, when negative input is passed */
factorial(Negative, 0) :- Negative < 0.

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
            hamiltonian_cycle_small(SortedNodes, UnrotatedCycles);
            (
                NodesLen /\ 1 =:= 1 -> 
                    findall(C, hamiltonian_cycle_odd(SortedNodes, NodesLen, C), UnrotatedCycles);
                    findall(C, hamiltonian_cycle_even(SortedNodes, NodesLen, C), UnrotatedCycles)
            )
    ),
    [Head | _] = SortedNodes,
    maplist({Head}/[L, O]>>(rotate_list(L, Head, O)), UnrotatedCycles, Cycles).

find_cycles_via_edges(Cycles) :-
    all_nodes(Nodes), 
    sort(Nodes, SortedNodes),
    [Head | _] = SortedNodes,
    findall(C, unique_hamiltonian_cycle(Head, C), Cycles).

print_cycle([]).
print_cycle([Head|Tail]) :- print_cycles([Head|Tail], Head).

print_cycles([]).
print_cycles([Head|Tail]) :- print_cycle(Head), print_cycles(Tail).
print_cycles([Head1, Head2 | Tail], First) :- write(Head1-Head2), write(' '), print_cycles([Head2 | Tail], First).
print_cycles([Last], First) :- writeln(Last-First).

test_print_cycle([Head|Tail]) :- write(Head), write(' '), test_print_cycle(Tail).
test_print_cycle([]) :- nl.

test_print_cycles([]).
test_print_cycles([Head|Tail]) :- test_print_cycle(Head), test_print_cycles(Tail).

main1(Cycles) :-
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

main2(Cycles) :-
    parse_input(),
    all_nodes(Nodes), 
    all_edges(Edges),
    length(Nodes, NodesLen),
    NodesLenDec is NodesLen - 1,
    length(Edges, EdgesLen),
    factorial(NodesLenDec, F),
    EdgesPerNode is EdgesLen / NodesLen,
    power(EdgesPerNode, NodesLen, P),
    (
        F < P -> 
            find_cycles_via_nodes(Cycles);
            find_cycles_via_edges(Cycles)
    ).


main_print :-
    main1(Cycles),
    print_cycles(Cycles).

main_test_print :-
    main1(Cycles),
    test_print_cycles(Cycles).

main_nodes_test_print :-
    parse_input(),
    find_cycles_via_nodes(Cycles),
    test_print_cycles(Cycles).

main_edges_test_print :-
    parse_input(),
    find_cycles_via_edges(Cycles),
    test_print_cycles(Cycles).

main_combined1_test_print :-
    main1(Cycles),
    test_print_cycles(Cycles).

main_combined2_test_print :-
    main2(Cycles),
    test_print_cycles(Cycles).
