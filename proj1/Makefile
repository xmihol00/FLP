CC=ghc
CFLAGS=-Wall -O3 #-Wextra
SRC=*.hs

.PHONY: all clean pack

all:
	$(CC) $(CFLAGS) $(SRC) -o flp-fun

clean:
	rm flp-fun *.hi *.o

pack:
	zip -r xmihol00.zip *.hs Makefile README.md ground_truth/ datasets/ trained/ values/ figs/ test_flp-fun.py generate_ref.py cart_tree.py run_all_tests.sh