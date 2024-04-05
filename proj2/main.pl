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
parse_input :- 
    /* ensure even less capable prolog compilers (like swipl on merlin) do not print prompt */
    prompt(_, ''), 
    /* read a line from STDIN */
    read_line(L, C), 
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
                ), parse_input; 
            parse_input
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
rotate_list(Pivot, List, Rotated) :-
    /* split the list by the pivot */
    append(Left, [Pivot | Right], List),
    /* append the left part to the right part in reverse order */
    append([Pivot | Right], Left, Rotated).
rotate_list(_, [], []).

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
    append(Start, [Element | End], Ys).

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
    length(Nodes, NodesLength),
    /* begin the recursive search */
    hamiltonian_cycle(Start, Start, [Start], NodesLength, Cycle).
/* the recursive predicate (only if the current path can continue) */
hamiltonian_cycle(Start, Current, Visited, CycleLength, Cycle) :-
    /* get a next node connected to the current node, all possible next nodes will be eventualy retreived, when the follwing predicates fail */
    edge(Current, Next), 
    /* check if the next node has not been visited yet */
    \+ member(Next, Visited), 
    /* add the next node to the visited nodes and continue the search */
    hamiltonian_cycle(Start, Next, [Next | Visited], CycleLength, Cycle).
/* the end of recursion predicate, i.e. cycle found */
hamiltonian_cycle(Start, Current, Visited, CycleLength, Cycle) :-
    /* check if the path has the required length, i.e. same as the number of nodes in the database */
    length(Visited, CycleLength), 
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
    rotate_list(Start, Cycle, RotatedCycle),
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
 * @param +Length The length of the list of nodes.
 * @param -UniqueCycle The list of nodes representing the cycle.
 */
hamiltonian_cycle_odd(Nodes, Length, UniqueCycle) :- 
    /* get the position of the node in the middle of the list */
    HalfLength is Length // 2,
    /* take out the first two nodes from the node list to break the permutation symmetry, see README.md for more detail */
    [Head1, Head2 | Tail]=Nodes, 
    /* find a permutation of the rest of the nodes */
    permutation(Tail, Permutation), 
    /* extend the permutation non-symmetrically */
    insertUntil(Head2, HalfLength, Permutation, Inserted), 
    /* append the starting node, which is the same for all potential cycles */
    Full=[Head1 | Inserted], 
    /* check whether the current permutation is a cycle, i.e. edges exist in between the nodes */
    is_cycle(Full), 
    /* unique cycle found */
    UniqueCycle=Full.

/**
 * @brief Finds a unique hamiltonian cycle with each call in a graph with even number of nodes using nodes as the space of search.
 * @param +Nodes The list of nodes to search the cycle in.
 * @param +Length The length of the list of nodes.
 * @param -UniqueCycle The list of nodes representing the cycle.
 */
hamiltonian_cycle_even(Nodes, Length, UniqueCycle) :- 
    /* get the length of the list without one node first node, i.e. the length as if the list had odd length */
    OddLength is Length - 1,
    /* get the position of the node in the middle of the odd length list  */
    HalfLength is (Length - 1) // 2,
    /* take out the first three nodes from the node list to break the permutation symmetry, see README.md for more detail */
    [Head1, Head2, Head3 | Tail] = Nodes,
    /* find a permutation of the rest of the nodes */
    permutation(Tail, P), 
    /* extend the permutation non-symmetrically */
    insertUntil(Head3, HalfLength, P, Inserted), 
    /* append the starting node, which is the same for all potential cycles */
    Partial=[Head1 | Inserted], 
    /* extend the permutation non-symmetrically again */
    insertUntil(Head2, OddLength, Partial, Full),
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
    length(Nodes, Length),
    (
        /* switch based on the number of nodes and return found cycle */
        (Length =:= 1, [H1] = Nodes, edge(H1, H1), UniqueCycles = [Nodes]);
        (Length =:= 2, [H1, H2] = Nodes, edge(H1, H2), edge(H2, H1), UniqueCycles = [Nodes]);
        (Length =:= 3, [H1, H2, H3] = Nodes, edge(H1, H2), edge(H2, H3), edge(H3, H1), UniqueCycles = [Nodes]);
        /* no cycle found or the graph has more than 3 nodes */
        UniqueCycles = []
    ).

/**
 * @brief Checks whether a list of nodes represents a cycle in a graph.
 * @param +Nodes The list of nodes to check.
 */
/* the main/starting predicate */
is_cycle([First | Rest]) :- is_cycle([First | Rest], First).
/* the recursive predicate (only if the expected path exists) */
is_cycle([First, Second | Rest], Initial) :- edge(First, Second), is_cycle([Second | Rest], Initial). 
/* the end of recursion predicate, i.e. cycle exists */
is_cycle([Last], Initial) :- edge(Last, Initial).

/**
 * @brief Calculates the factorial of a given number.
 * @param +Number The number to calculate the factorial of.
 * @param -Factorial The factorial of the given number.
 */
/* base case */
factorial(0, 1).
/* return 0 instead of fail, when negative input is passed */
factorial(Negative, 0) :- Negative < 0.
/* ensure the factorial does not crash on systems with bound integers to 64 bits (e.g. merlin) */
factorial(TooLarge, Factorial) :- TooLarge > 20, Factorial is 18446744073709551615.
/* recursive case */
factorial(Number, Factorial) :- 
    Next is Number - 1, 
    factorial(Next, Previous), 
    Factorial is Number * Previous.

/**
 * @brief Calculates the power of a given base on a given exponent.
 * @param +Base The base of the power.
 * @param +Exponent The exponent of the power.
 * @param -Power The power of the base on the exponent.
 */
/* base case */
power(_, 0, 1).
/* recursive case */
power(Base, Exponent, Power) :- 
    Next is Exponent - 1, 
    power(Base, Next, Previous), 
    (
        /* ensure the power does not crash on systems with bound integers to 64 bits (e.g. merlin) */
        18446744073709551615 / Base < Previous -> 
            Power is 18446744073709551615;
            Power is Base * Previous
    ).

/**
 * @brief Finds all unique hamiltonian cycles in a graph using nodes as the space of search.
 * @param -Cycles The list of lists of nodes representing cycles.
 */
find_cycles_via_nodes(Cycles) :-
    /* retreive all nodes from the database */
    all_nodes(Nodes), 
    /* get the number of nodes in the database */
    length(Nodes, NodesLength),
    /* sort the nodes to produce more readable output */
    sort(Nodes, SortedNodes),
    (
        NodesLength =< 3 ->
            /* find all unique cycles in a graph with 1, 2 or 3 nodes */
            hamiltonian_cycle_small(SortedNodes, UnrotatedCycles);
            /* find all unique cycles in a graph with more than 3 nodes */
            (
                NodesLength /\ 1 =:= 1 -> 
                    /* find all unique cycles in a graph with odd number of nodes */
                    findall(C, hamiltonian_cycle_odd(SortedNodes, NodesLength, C), UnrotatedCycles);
                    /* find all unique cycles in a graph with even number of nodes */
                    findall(C, hamiltonian_cycle_even(SortedNodes, NodesLength, C), UnrotatedCycles)
            )
    ),
    [Head | _] = SortedNodes,
    /* rotate all the results to start with the same node */
    maplist(rotate_list(Head), UnrotatedCycles, Cycles).

/**
 * @brief Finds all unique hamiltonian cycles in a graph using edges as the space of search.
 * @param -Cycles The list of lists of nodes representing cycles.
 */
find_cycles_via_edges(Cycles) :-
    /* retreive all nodes from the database */
    all_nodes(Nodes), 
    /* sort the nodes to produce more readable output */
    sort(Nodes, SortedNodes),
    [Head | _] = SortedNodes,
    /* find all unique cycles in a graph and ensure they all start with the same node */
    findall(C, unique_hamiltonian_cycle(Head, C), Cycles).

/**
 * @brief Prints a hamiltonian cycle in the required format by the assignment.
 * @param +Cycle The list of nodes representing the cycle.
 */
print_cycle([]).
print_cycle([Head | Tail]) :- print_cycles([Head | Tail], Head).

/**
 * @brief Prints multiple hamiltonian cycles each on a separate line in the required format by the assignment.
 * @param +Cycles The list of lists of nodes representing the cycles.
 */
/* main/starting predicate */
print_cycles([Head | Tail]) :- print_cycle(Head), print_cycles(Tail).
/* no cycles to print (base case) */
print_cycles([]).
/* recursive case */
print_cycles([Head1, Head2 | Tail], First) :- write(Head1-Head2), write(' '), print_cycles([Head2 | Tail], First).
/* base case */
print_cycles([Last], First) :- writeln(Last-First).

/**
 * @brief Prints a hamiltonian cycle in a test format.
 * @param +Cycles The list of nodes representing the cycles.
 */
/* recursive case */
test_print_cycle([Head | Tail]) :- write(Head), write(' '), test_print_cycle(Tail).
/* base case */
test_print_cycle([]) :- nl.

/**
 * @brief Prints multiple hamiltonian cycles each on a separate line in a test format.
 * @param +Cycles The list of lists of nodes representing the cycles.
 */
/* recursive case */
test_print_cycles([Head | Tail]) :- test_print_cycle(Head), test_print_cycles(Tail).
/* base case */
test_print_cycles([]).

/* --------------------------------------------------------------------------------
-- main predicates to test, analyze performance and for the final submission
-- see the Makefile and README.md for more details
----------------------------------------------------------------------------------- */

main(Cycles) :-
    /* read the input and parse it */
    parse_input,
    /* retreive all nodes from the database */
    all_nodes(Nodes),
    /* retreive all edges from the database */
    all_edges(Edges),
    /* get the number of nodes and edges */
    length(Nodes, NodesLength),
    length(Edges, EdgesLength),
    /* compute the time complexity of the node based search */
    factorial(NodesLength, Factorial),
    /* compute the average number of edges per node */
    EdgesPerNode is EdgesLength / NodesLength,
    /* compute the time complexity of the edge based search (deduplication part of the complexity is negligable, therefore omitted) */
    power(EdgesPerNode, NodesLength, Power),
    (
        /* choose the solution with smaller time complexity */
        Factorial < Power -> 
            find_cycles_via_nodes(Cycles);
            find_cycles_via_edges(Cycles)
    ).

/**
 * @brief Main predicate for the submission, called when build with 'make'.
 */
main_print :-
    main(Cycles),
    print_cycles(Cycles),
    halt.

/**
 * @brief Main predicate for testing, called when build with 'make test'.
 */
main_test_print :-
    main(Cycles),
    test_print_cycles(Cycles),
    halt.

/* --------------------------------------------------------------------------------
-- Main predicates for performance analysis, see README.md. 
----------------------------------------------------------------------------------- */
/* called when build with 'make test_nodes' */
main_nodes_test_print :-
    parse_input,
    find_cycles_via_nodes(Cycles),
    test_print_cycles(Cycles),
    halt.

/* called when build with 'make test_edges' */
main_edges_test_print :-
    parse_input,
    find_cycles_via_edges(Cycles),
    test_print_cycles(Cycles),
    halt.

/* called when build with 'make test_combined' */
main_combined_test_print :-
    main(Cycles),
    test_print_cycles(Cycles),
    halt.
