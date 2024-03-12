#############################################################################################################################
#   project: flp-fun (1st project regarding decision trees to Functional and Logic Programming course at FIT, BUT)
#    author: David Mihola (xmihol00)
#     email: xmihol00@stud.fit.vutbr.cz
#      date: 31. 3. 2024
# file info: Script to visualize a decision tree and to test an implementation of a decision tree in an imperative language.
#############################################################################################################################

# Usage: python cart_tree.py <dataset_file_path> 
# datasets are available in the datasets directory

import numpy as np
from sklearn.tree import DecisionTreeClassifier
from sklearn.tree import export_graphviz
import graphviz
import sys
import pandas as pd

MAX_DEPTH = 2**31 - 1
MIN_SAMPLES_LEAF = 1
MIN_SAMPLES_SPLIT = 2

def visualize_tree(tree, feature_names, class_names, tree_name):
    dot_data = export_graphviz(tree, out_file=None, feature_names=feature_names, class_names=class_names,
                               filled=True, rounded=True, special_characters=True)
    graph = graphviz.Source(dot_data)
    graph.render(tree_name, cleanup=True, format="png")
    return graph

def calculate_gini(y):
    classes = set([x[0] for x in y])
    gain = 0
    for cls in classes:
        gain += (np.sum([x[0] == cls for x in y]) / len(y)) ** 2
    return 1 - gain

class Node:
    def __init__(self, idx, threshold, left, right):
        self.idx = idx
        self.threshold = threshold
        self.left = left
        self.right = right

def create_tree(training_data, features, max_depth=None):
    """
    Imperative implementation of a decision tree
    """
    if max_depth is not None:
        if max_depth == 0:
            return max(set([x[0] for x in features]), key=[x[0] for x in features].count)
        max_depth -= 1

    if len(set([x[0] for x in features])) == 1:
        return features[0][0]
    
    best_gini = float("inf")
    for i, feature_data in enumerate(training_data):
        feature_data_sorted = sorted([feature_data[features[i][1]] for i in range(len(features))])
        thresholds = [(feature_data_sorted[i] + feature_data_sorted[i + 1]) / 2 for i in range(len(feature_data_sorted) - 1)]
        for threshold in thresholds:
            features_count = len(features)
            left = [feature for feature in features if feature_data[feature[1]] < threshold]
            right = [feature for feature in features if feature_data[feature[1]] >= threshold]
            gini = len(left) / features_count * calculate_gini(left) + len(right) / features_count * calculate_gini(right)
            if gini < best_gini:
                best_gini = gini
                best_threshold = threshold
                best_left = left
                best_right = right
                best_idx = i

    node = Node(best_idx, best_threshold, None, None)
    node.left = create_tree(training_data, best_left, max_depth)
    node.right = create_tree(training_data, best_right, max_depth)
    return node

def print_tree(tree, indent=0):
    if isinstance(tree, Node):
        print("  " * indent + f"Feature {tree.idx}, threshold {tree.threshold:.2f}")
        print_tree(tree.left, indent + 1)
        print_tree(tree.right, indent + 1)
    else:
        print("  " * indent + f"Class {tree}")

if __name__ == "__main__":
    if len(sys.argv) > 1:
        dataset = sys.argv[1]
    else:
        dataset = "datasets/example3.csv"

    data = pd.read_csv(dataset).to_numpy()
    features_count = data.shape[1] - 1
    X = data[:, :features_count].astype(float)
    y = data[:, features_count]

    label_map = {label: i for i, label in enumerate(sorted(set(y)))}
    y = np.array([label_map[label] for label in y])

    clf = DecisionTreeClassifier(max_depth=MAX_DEPTH, min_samples_leaf=MIN_SAMPLES_LEAF, min_samples_split=MIN_SAMPLES_SPLIT)
    clf.fit(X, y)

    class_names = list(map(str, label_map.keys()))
    visualize_tree(clf, [f"Feature {i}" for i in range(features_count)], class_names, dataset.split("/")[-1].split(".")[0])

    training_data = data[:, :features_count].T.astype(float)
    features = data[:, -1].flatten()
    features = [(features[i], i) for i in range(len(features))]
    tree = create_tree(training_data, features, MAX_DEPTH)
    print_tree(tree)
