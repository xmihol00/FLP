treeInsert(X, empty, (empty, X, empty)).
treeInsert((Xk, Xv), (L, (Yk, Yv), R), (NewL, (Yk, Yv), R)) :- Xk < Yk, !, treeInsert((Xk, Xv), L, NewL).
treeInsert((Xk, Xv), (L, (Yk, Yv), R), (L, (Yk, Yv), NewR)) :- Xk > Yk, !, treeInsert((Xk, Xv), R, NewR).

buildTree(List, Tree) :- buildTree(List, empty, Tree).
buildTree([H|T], StartTree, FinalTree) :- treeInsert(H, StartTree, NewTree), !, buildTree(T, NewTree, FinalTree), !.
buildTree([], FinalTree, FinalTree) :- !.

inOrderTreePrint(Tree) :- inOrderTreePrint(Tree, "").
inOrderTreePrint(empty, _).
inOrderTreePrint((L, (Xk, Xv), R), Indent) :- string_concat(Indent, "  ", NewIndent), 
                                       inOrderTreePrint(L, NewIndent), 
                                       write(Indent), write(Xk), write(": "), writeln(Xv), 
                                       inOrderTreePrint(R, NewIndent).

getTree((((empty, (1, "This"), empty), (2, "is"), (empty, (3, "unfortunately"), empty)), (4, "really"), (empty, (5, "pathetic"), (empty, (22, ":("), empty)))).
buildTree1(T) :- buildTree([(5, "either"), (6, ":D"), (3, "much"), (4, "better"), (12, "..."), (1, ['N', 'O', 'T'])], T).
buildTree2(T) :- buildTree([(5, "a"), (6, "b"), (3, "a"), (4, "b"), (12, "c"), (1, "e"), (-5, "a")], T).

getKeys(_, empty, []).
getKeys(V, (L, (K, V), R), [K|Ks]) :- getKeys(V, L, LKs), getKeys(V, R, RKs), append(LKs, RKs, Ks), !.
getKeys(V, (L, _, R), Ks) :- getKeys(V, L, LKs), getKeys(V, R, RKs), append(LKs, RKs, Ks), !.

gT :- getTree(T), inOrderTreePrint(T).
bT :- buildTree1(T), inOrderTreePrint(T).

myAppend([], L, L).
myAppend([H|T], X, [H|Y]) :- !, myAppend(T, X, Y).
myAppend([H|Y], X, [H|T]) :- !, myAppend(Y, X, T).