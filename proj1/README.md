# FLP-FUN (1st project to Functional and Logic Programming course at FIT, BUT)
```
author: David Mihola (xmihol00)
 email: xmihol00@stud.fit.vutbr.cz
  date: 31. 3. 2024
```
## Directory structure
```
├── cart_tree.py            - python implementation of a decision tree
├── datasets/               - cleaned datasets prepared for testing
├── generate_ref.py         - script to generate a reference classification output
├── ground_truth/           - correct classification results
├── main.hs                 - main file of the Haskell decision tree implementation
├── Makefile                - Makefile to build the project
├── parsing.hs              - file with parsing function for the Haskell decision tree implementation
├── README.md
├── test_flp-fun.py         - test script of the Haskell decision tree implementation
├── test_runs               - test results with different optimizations
│   ├── all_params/
│   ├── default_tests/
│   ├── max_depth/
│   ├── min_samples_leaf/
│   └── min_samples_split/
├── trained/                - trained decision trees
├── tree.hs                 - file with three functions for the Haskell decision tree implementation
└── values/                 - samples for decision tree classification
```

## Additions to the basic implementation
This implementation supports 3 mechanisms preventing overfitting, which can be specified with additional arguments:
1. **max depth** - the maximum depth of the trained tree. No more splits are performed after the maximum depth is reached and the most occurring class is selected as a leaf node. Run as e.g.: `./flp-fun -2 datasets/housing1.csv -md 5`
2. **min samples split** - the minimum number of samples required to split an internal node. If an internal node has fewer samples than *min samples split*, the split will not be performed, and the node will become a leaf node. This can be check simply by the number of samples in the node. Run as e.g.: `./flp-fun -2 datasets/housing1.csv -mss 5`
3. **min samples leaf** - minimum number of samples required to be at a leaf node. If a split results in a leaf node with fewer samples than *min samples leaf*, the split is ignored, and the node is turned into a leaf node even if the split would result in a better training score. Run as e.g.: `./flp-fun -2 datasets/housing1.csv -msl 5`

Multiple additional arguments can be specified as well, e.g.:
```
./flp-fun -2 datasets/penguins_all.csv -msl 3 -mss 2
./flp-fun -2 datasets/penguins_all.csv -mss 3 -md 5
./flp-fun -2 datasets/penguins_all.csv -md 5 -msl 3 
./flp-fun -2 datasets/penguins_all.csv -md 5 -msl 3 -mss 7
```

## Sources
Algorithms used for training of a decision tree:
* https://medium.com/@riyapatadiya/gini-index-cart-decision-algorithm-in-machine-learning-1a4ed5d6140d
* https://medium.com/@dyahayusekarkinasih/how-c4-5-algorithm-handling-continuous-features-43fbebcd619b

