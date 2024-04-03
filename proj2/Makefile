CC=swipl
CFLAGS=--stack-limit=12g -G12g

SRC=main.pl
TARGET=flp23-log
MAIN=main_print

MAIN_TEST=main_test_print
TARGET_TEST=test-flp23-log

MAIN_TEST_NODES = main_nodes_test_print
TARGET_TEST_NODES = test-flp23-log-nodes

MAIN_TEST_EDGES = main_edges_test_print
TARGET_TEST_EDGES = test-flp23-log-edges

MAIN_TEST_COMBINED1 = main_combined1_test_print
TARGET_TEST_COMBINED1 = test-flp23-log-combined1

MAIN_TEST_COMBINED2 = main_combined2_test_print
TARGET_TEST_COMBINED2 = test-flp23-log-combined2

.PHONY: all clean test test_nodes test_edges test_combined1 test_combined2

all:
	$(CC) $(CFLAGS) -o $(TARGET) -g $(MAIN) -c $(SRC)

test:
	$(CC) $(CFLAGS) -o $(TARGET_TEST) -g $(MAIN_TEST) -c $(SRC)

test_nodes:
	$(CC) $(CFLAGS) -o $(TARGET_TEST_NODES) -g $(MAIN_TEST_NODES) -c $(SRC)

test_edges:
	$(CC) $(CFLAGS) -o $(TARGET_TEST_EDGES) -g $(MAIN_TEST_EDGES) -c $(SRC)

test_combined1:
	$(CC) $(CFLAGS) -o $(TARGET_TEST_COMBINED1) -g $(MAIN_TEST_COMBINED1) -c $(SRC)

test_combined2:
	$(CC) $(CFLAGS) -o $(TARGET_TEST_COMBINED2) -g $(MAIN_TEST_COMBINED2) -c $(SRC)

clean:
	rm -f $(TARGET) $(TARGET_TEST) $(TARGET_TEST_NODES) $(TARGET_TEST_EDGES) $(TARGET_TEST_COMBINED1) $(TARGET_TEST_COMBINED2) *.o