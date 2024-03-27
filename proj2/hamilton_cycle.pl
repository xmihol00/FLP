
main :- parse_input(), hamilton_cycles(AC), remove_duplicates(AC, DC), writeln(DC).

parse_line([A, ' ', B], A, B).
parse_line(_, _, _) :- fail.

all_nodes(Nodes) :- setof(X, node(X), Nodes).
all_edges(Edges) :- setof([X, Y], edge(X, Y), Edges).

parse_input :- read_line(L, C), 
        ( 
            C == end_of_file;
	        parse_line(L, A, B), assert(edge(A, B)), assert(edge(B, A)), assert(node(A)), assert(node(B)), parse_input();
            parse_input()
	    ).

first_same_permutations(Permutations) :-
    all_nodes([A | T]),
    findall(P, permutation(T, P), PU),
    maplist({A}/[L, [A|L]]>>true, PU, Permutations).

hamilton_cycles(Cycles) :- first_same_permutations(P), include(is_cycle, P, Cycles).

is_cycle([A | T]) :- is_cycle([A | T], A).
is_cycle([A, B | T], X) :- edge(A, B), is_cycle([B | T], X). 
is_cycle([A], X) :- edge(A, X).

remove_duplicates(AllCycles, DedupCycles) :- 
    maplist([[_|Rest], Rest]>>true, AllCycles, DedupCycles),
    inner_list_length(DedupCycles, L),
    (
        L /\ 1 =:= 1 -> 
            writeln("Odd"); 
            writeln("Even")
    ),
    Middle is L // 2,
    writeln(Middle).

// TODO group by

inner_list_length([A|T], L) :- length(A, L).
