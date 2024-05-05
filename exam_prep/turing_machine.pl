init :- assertz(tol([d, c, b, a])), assertz(tor([])), assertz(head(blank)).
state(Head, Right, Left) :- head(Head), tol(Right), tor(Left).

shl :- 
    tol([Left|RestLeft]), retract(tol(_)), assertz(tol(RestLeft)), 
    head(H), retract(head(_)), assertz(head(Left)), 
    tor(RightRest), retract(tor(_)), assertz(tor([H|RightRest])), !.

shr :- 
    tor([]), !, 
    head(H), retract(head(_)), assertz(head(blank)), 
    tol(Left), retract(tol(_)), assertz(tol([H|Left])).
shr :- 
    tor([Right|RightRest]), retract(tor(_)), assertz(tor(RightRest)),
    head(H), retract(head(_)), assertz(head(Right)), 
    tol(Left), retract(tol(_)), assertz(tol([H|Left])).

moveStart :- shl, !, moveStart.
moveStart :- \+ shl.

ttol(X) :- (\+ current_predicate(starting_state/3), !, tol(L), tor(R), head(H), assertz(starting_state(L, R, H)), ttol(X));
           (shl, !, ttol(X));
           (\+ shl, !, tor(RestRight), head(FH), X = [FH|RestRight], retract(tor(_)), retract(tol(_)), retract(head(_)), 
                       starting_state(L, R, H), assertz(tol(L)), assertz(tor(R)), assertz(head(H)), abolish(starting_state/3)).

/* [move(d, shl), move(blank, shr), move(blank, shl), move(c, shr), move(d, shr)] */
findmove([], _) :- fail.
findmove([move(Symbol, Action)|Moves], A) :- (head(H), H \= Symbol, !, findmove(Moves, A));
                                             (Action == shl, tol([]), !, findmove(Moves, A));
                                             (A = move(Symbol, Action));
                                             (findmove(Moves, A)).
