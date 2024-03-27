CC=swipl
CFLAGS=

SRC=hamilton_cycle.pl input2.pl
TARGET=flp23-log
MAIN=main

.PHONY: all clean

all:
	$(CC) $(CFLAGS) -o $(TARGET) -g $(MAIN) -c $(SRC)

clean:
	rm -f $(TARGET) *.o