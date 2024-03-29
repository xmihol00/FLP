CC=swipl
CFLAGS=

SRC=main.pl
TARGET=flp23-log
MAIN=main_print

MAIN_TEST=main_test_print
TARGET_TEST=test-flp23-log

.PHONY: all clean

all:
	$(CC) $(CFLAGS) -o $(TARGET) -g $(MAIN) -c $(SRC)

test:
	$(CC) $(CFLAGS) -o $(TARGET_TEST) -g $(MAIN_TEST) -c $(SRC)

clean:
	rm -f $(TARGET) *.o