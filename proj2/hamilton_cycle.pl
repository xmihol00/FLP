
main :- parse_input(), hamilton_cycles(Cycles), writeln(Cycles), print_cycles(Cycles).

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

hamilton_cycles(Cycles) :- make_permutations(P), include(is_cycle, P, Cycles).

is_cycle([A | T]) :- is_cycle([A | T], A).
is_cycle([A, B | T], X) :- edge(A, B), is_cycle([B | T], X). 
is_cycle([A], X) :- edge(A, X).

insertUntil(Element, N, Xs, Ys) :-
    append(Start, End, Xs),
    length(Start, M),
    N > M,
    append(Start, [Element|End], Ys).

flatten([], []).
flatten([H|T], Flat) :- flatten(T, FlatT), append(H, FlatT, Flat).

make_permutations(UniquePerms) :- 
    all_nodes(List), 
    length(List, Len),
    sort(List, SortedList),
    [Head | _] = SortedList,
    (
        Len /\ 1 =:= 1 -> 
            [Head1, Head2 | Tail] = SortedList,
                Half is (Len) // 2,
                findall(P, permutation(Tail, P), Perms), 
                maplist({Half, Head2}/[L, Inserted]>>(findall(Ys, insertUntil(Head2, Half, L, Ys), Inserted)), Perms, Lists),
                flatten(Lists, FlattenLists),
                maplist({Head1}/[[H|T], Concatenated]>>(Concatenated=[Head1, H | T]), FlattenLists, UnrotatedUniquePerms);
            [Head1, Head2, Head3 | Tail] = SortedList,
                Half is (Len-1) // 2,
                findall(P, permutation(Tail, P), Perms), 
                maplist({Half, Head3}/[L, Inserted]>>(findall(Ys, insertUntil(Head3, Half, L, Ys), Inserted)), Perms, Lists),
                flatten(Lists, FlattenLists),
                HalfPlus is Len-1,
                maplist({Head2}/[[H|T], Concatenated]>>(Concatenated=[Head2, H | T]), FlattenLists, MappedLists),
                maplist({HalfPlus, Head1}/[L, Inserted]>>(findall(Ys, insertUntil(Head1, HalfPlus, L, Ys), Inserted)), MappedLists, InsertedAgain),
                flatten(InsertedAgain, UnrotatedUniquePerms)
    ), maplist({Head}/[L, O]>>(rotate_list(L, Head, O)), UnrotatedUniquePerms, UniquePerms).

create_tuples(N, Lists, Tuples) :- maplist({N}/[List, Tuple]>>(nth0(N, List, Elem), Tuple = (Elem, List)), Lists, Tuples).

inner_list_length([A|_], L) :- length(A, L).

rotate_list([], _, []).
rotate_list(List, Pivot, Rotated) :-
    append(Left, [Pivot|Right], List),
    append([Pivot|Right], Left, Rotated).

print_cycles([]).
print_cycles([H|T]) :- print_cycle(H), print_cycles(T).

print_cycle([]).
print_cycle([H|T]) :- print_cycles([H|T], H).
print_cycles([H1, H2 | T], F) :- write(H1-H2), write(' '), print_cycles([H2 | T], F).
print_cycles([H], F) :- writeln(H-F).
