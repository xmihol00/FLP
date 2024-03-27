
main :- parse_input(), hamilton_cycle().

parse_line([A, ' ', B], A, B).
parse_line(_, _, _) :- fail.

all_nodes(Nodes) :- setof(X, node(X), Nodes).
all_edges(Edges) :- setof([X, Y], edge(X, Y), Edges).

parse_input :- read_line(L, C), 
        ( 
            C == end_of_file ;
	        parse_line(L, A, B), assert(edge(A, B)), assert(edge(B, A)), assert(node(A)), assert(node(B)), parse_input() ;
            parse_input()
	    ).

hamilton_cycle :- all_nodes(Nodes), permutation(Nodes, P), check_cycle(P), writeln(P).
check_cycle([A, B | T]) :- edge(A, B), check_cycle([B | T]).
