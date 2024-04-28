#-------------------------------------------------------------------------------------------------------------------------
#    project: flp23-log (2nd project regarding hamiltonian cycles to Functional and Logic Programming course at FIT, BUT)
# assignment: Hamiltonian cycles
#     author: David Mihola (xmihol00)
#      email: xmihol00@stud.fit.vutbr.cz
#       date: 28. 4. 2024 (academic year 2023/2024)
#-------------------------------------------------------------------------------------------------------------------------

# Usage:
# make or make all   - compiles the project as the intended output 
# make test          - compiles the project with different output better readable for humans and better for testing purposes
# make test_nodes    - compiles the project using only the node solution, see README.md, with the test output
# make test_edges    - compiles the project using only the edge solution, see README.md, with the test output
# make test_combined - compiles the project using the combined solution, see README.md, with the test output
#
# make clean         - removes all the compiled files and outputs of tests scripts
# make pack          - creates an archive for submitting the project

CC=swipl
CFLAGS=-G12g -T4g -L2g #--stack-limit=12g

SRC=main.pl
TARGET=flp23-log  # the intendet output of the project
MAIN=main_print

# the following targets are used for testing purposes
MAIN_TEST=main_test_print
TARGET_TEST=test-flp23-log

MAIN_TEST_NODES = main_nodes_test_print
TARGET_TEST_NODES = test-flp23-log-nodes

MAIN_TEST_EDGES = main_edges_test_print
TARGET_TEST_EDGES = test-flp23-log-edges

MAIN_TEST_COMBINED = main_combined_test_print
TARGET_TEST_COMBINED = test-flp23-log-combined

# the archive name for submitting the project
PACK_TARGET=flp-log-xmihol00.zip

.PHONY: all clean test test_nodes test_edges test_combined pack

all:
	$(CC) $(CFLAGS) -o $(TARGET) -g $(MAIN) -c $(SRC)

test:
	$(CC) $(CFLAGS) -o $(TARGET_TEST) -g $(MAIN_TEST) -c $(SRC)

test_nodes:
	$(CC) $(CFLAGS) -o $(TARGET_TEST_NODES) -g $(MAIN_TEST_NODES) -c $(SRC)

test_edges:
	$(CC) $(CFLAGS) -o $(TARGET_TEST_EDGES) -g $(MAIN_TEST_EDGES) -c $(SRC)

test_combined:
	$(CC) $(CFLAGS) -o $(TARGET_TEST_COMBINED) -g $(MAIN_TEST_COMBINED) -c $(SRC)

clean:
	rm -f $(TARGET) $(TARGET_TEST) $(TARGET_TEST_NODES) $(TARGET_TEST_EDGES) $(TARGET_TEST_COMBINED) $(PACK_TARGET) *.o *.out *.tmp

pack:
	zip -r $(PACK_TARGET) Makefile README.md main.pl performance_graphs/ hand_solved_graphs/