CC=swipl
CFLAGS=-g -Wall

SRC=*.pl
TARGET=flp23-log

.PHONY: all clean

all:
	$(CC) $(CFLAGS) -o $(TARGET) -c $(SRC)

clean:
	rm -f $(TARGET) *.o