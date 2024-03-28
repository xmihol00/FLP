
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
    maplist([[_|Rest], Rest]>>true, AllCycles, Tails),
    [[HH|_]|_] = AllCycles,
    inner_list_length(Tails, L),
    (
        L /\ 1 =:= 1 -> 
            M is L // 2, writeln(Tails), 
                create_tuples(M, Tails, Tups), 
                findall(Group, group_by(Key, Value, member((Key, Value), Tups), Group), Groups), 
                maplist([[H|_], H]>>true, Groups, XX),
                maplist({HH}/[[U | T], [HH, U | T]]>>true, XX, DedupCycles);
            writeln("Even")
    ).

create_tuples(N, Lists, Tuples) :- maplist({N}/[List, Tuple]>>(nth0(N, List, Elem), Tuple = (Elem, List)), Lists, Tuples).

inner_list_length([A|_], L) :- length(A, L).
