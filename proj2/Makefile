CC=swipl
CFLAGS=-g -Wall

SRC = *.pl

.PHONY: all clean

TARGET = flp23-log

all: $(TARGET)

$(TARGET): $(SRC)
    $(CC) $(CFLAGS) -o $(TARGET) -c $(SRC)

# Clean rule
clean:
    rm -f $(TARGET) *.o
