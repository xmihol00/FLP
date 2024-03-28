student_grade(alice, 90).
student_grade(bob, 85).
student_grade(charlie, 90).
student_grade(diana, 85).
student_grade(eve, 95).



create_tuples(N, Lists, Tuples) :- maplist({N}/[List, Tuple]>>(nth0(N, List, Elem), Tuple = (Elem, List)), Lists, Tuples).

create_lists(L) :- L = [[a,1,u],[b,2,v],[c,1,x],[d,2,z],[e,3,zz]].
create_list(L) :- L = [2, 3, 4, 5].

gps(G) :- create_lists(L), create_tuples(1, L, Tups), group_by(A, T, member((A, T), Tups), G).

unique_perms(Perm) :- create_list(L), findall(P, permutation(L, P), Perm). 
