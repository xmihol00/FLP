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

MAIN_TEST_COMBINED = main_combined_test_print
TARGET_TEST_COMBINED = test-flp23-log-combined

.PHONY: all clean test test_nodes test_edges test_combined 

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
	rm -f $(TARGET) $(TARGET_TEST) $(TARGET_TEST_NODES) $(TARGET_TEST_EDGES) $(TARGET_TEST_COMBINED) *.o