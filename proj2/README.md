# FLP-LOG (2nd project to Functional and Logic Programming course at FIT, BUT)
```
  case: Search of unique hamiltonian cycles in a graph implemented in Prolog.
author: David Mihola (xmihol00)
 email: xmihol00@stud.fit.vutbr.cz
  date: 28. 4. 2024
```

## Directory structure
```
├── hand_solved_graphs/    - graphs with unique hamiltonian cycles found by hand and scripts to test them against the implemented algorithms
├── main.pl                - implementation of the algorithm with multiple main functions for various tests
├── Makefile               - commands for building executables launching different main functions
├── performance_graphs/    - generated graphs without solutions to test the performance of the implemented algorithms, scripts to evaluate the performance and performance plots 
└── README.md
```

## Implementation details
There are two solutions for the problem implemented. Both solutions use the Prolog database for storing nodes and edges, which should ensure their O(1) lookup, considering that the database is a reasonably balanced hash table.

### Solution using edges as the search space
This is the common solution. Paths without repeating nodes starting and finishing in a given node are search and then deduplicated to ensure uniqueness of each solution. The time complexity, considering that existence of an edge can be evaluated in O(1), of this algorithm is $O(V^E)$, where $V$ is the number of nodes in a graph and $E$ is the average branching factor.

### Solution using nodes as the search space
This is a less common solution.