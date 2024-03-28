student_grade(alice, 90).
student_grade(bob, 85).
student_grade(charlie, 90).
student_grade(diana, 85).
student_grade(eve, 95).



create_tuples(N, Lists, Tuples) :- maplist({N}/[List, Tuple]>>(nth0(N, List, Elem), Tuple = (Elem, List)), Lists, Tuples).

create_lists(L) :- L = [[a,1,u],[b,2,v],[c,1,x],[d,2,z],[e,3,zz]].
create_list(L) :- L = [1, 2, 3, 4, 5].

gps(G) :- create_lists(L), create_tuples(1, L, Tups), group_by(A, T, member((A, T), Tups), G).

insertUntil(Element, N, Xs, Ys) :-
    append(Start, End, Xs),
    length(Start, M),
    N > M,
    append(Start, [Element|End], Ys).

flatten([], []).
flatten([H|T], Flat) :- flatten(T, FlatT), append(H, FlatT, Flat).

make_permutations(UniquePerms) :- 
    create_list(List), 
    [Head1, Head2 | Tail] = List,
    length(Tail, Len),
    Half is (Len+1) // 2,
    findall(P, permutation(Tail, P), Perms), 
    maplist({Half, Head2}/[L, Inserted]>>(findall(Ys, insertUntil(Head2, Half, L, Ys), Inserted)), Perms, Lists),
    flatten(Lists, FlattenLists),
    writeln(FlattenLists),
    maplist({Head1}/[[H|T], Concatenated]>>(Concatenated=[Head1, H | T]), FlattenLists, UniquePerms).

unique_perms() :- make_permutations(A), writeln(A), length(A, L), writeln(L).

 /*   
    ins(Lists) :- maplist([Idx, List]>>(insertAt(1, Idx, [a,b,c,d,e], List)), [0, 1, 2], Lists). 
    findall(Ys, insertUntil(1,2,[a,b,c,d,e],Ys), Lists)
    maplist([L, Inserted]>>(findall(Ys, insertUntil(6, 2, L, Ys), Inserted)), [[a,1,u],[b,2,v]], Lists).
 */