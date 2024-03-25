CC=ghc
CFLAGS=-Wall -O3 #-Wextra
SRC=*.hs

.PHONY: all clean pack

all:
	$(CC) $(CFLAGS) $(SRC) -o flp-fun
	$(CC) $(CFLAGS) $(SRC) -o flp-ref

clean:
	rm flp-fun flp-ref *.hi *.o

pack:
	zip -r xmihol00.zip *.hs Makefile README.md ground_truth/ datasets/ trained/ values/ figs/ edge_datasets/ edge_ground_truth/ edge_trained/ edge_values/ edge_test_flp-fun.py test_flp-fun.py generate_ref.py cart_tree.py run_all_tests.sh