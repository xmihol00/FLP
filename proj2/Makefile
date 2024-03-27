CC=swipl
CFLAGS=

SRC=hamilton_circle.pl input2.pl
TARGET=flp23-log
MAIN=find_hamilton_circle

.PHONY: all clean

all:
	$(CC) $(CFLAGS) -o $(TARGET) -g $(MAIN) -c $(SRC)

clean:
	rm -f $(TARGET) *.o