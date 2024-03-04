import os
import pandas as pd
import numpy as np
from sklearn.model_selection import train_test_split
import argparse
from sklearn.tree import DecisionTreeClassifier
import time
import datetime

# usage e.g.: python test_flp-fun.py -ds p i h -md 5 10 -msl 5 5 -fa "-d 5" "-d 10"
# or just   : python test_flp-fun.py

DATASETS_MAP = {
    'p': "penguins_cleaned.csv",
    'i': "    iris_cleaned.csv",
    'h': " housing_cleaned.csv",
    'w': "   wines_cleaned.csv",
    # TODO add some more maybe synthetic datasets to test something specific
}

SPLITS = [0.0000001, 0.1, 0.2, 0.3, 0.4, 0.5] # test/train splits of the datasets

if __name__ == "__main__":
    parser = argparse.ArgumentParser(description="Run tests on flp-fun")
    parser.add_argument("--datasets", "-ds", type=str, nargs='+', default=['p', 'i', 'h', 'w'], help="Data sets to use for testing. p for penguins, i for iris, b for boston housing, w for wines. Default is all.")
    parser.add_argument("--max_depth", "-md", type=int, nargs='+', default=[], help="Maximum depth of reference decision tree. Default is 2^31.")
    parser.add_argument("--min_samples_split", "-mss", type=int, nargs='+', default=[], help="Minimum samples to split a node. Default is 2.")
    parser.add_argument("--min_samples_leaf", "-msl", type=int, nargs='+', default=[], help="Minimum samples to be a leaf. Default is 1.")
    parser.add_argument("--flp_args", "-fa", type=str, nargs='+', default=[], help="Arguments to pass to flp-fun. Default is none.")
    parser.add_argument("--seed", "-s", type=int, default=42)
    parser.add_argument("--random", "-r", action="store_true")

    args = parser.parse_args()
    args.flp_args = [""] if not args.flp_args else args.flp_args

    # fill in default values based on the number of flp-fun argument sets
    args.max_depth = args.max_depth + [2**31] * (len(args.flp_args) - len(args.max_depth))
    args.min_samples_split = args.min_samples_split + [2] * (len(args.flp_args) - len(args.min_samples_split))
    args.min_samples_leaf = args.min_samples_leaf + [1] * (len(args.flp_args) - len(args.min_samples_leaf))

    # get max argument len for output formatting
    arg_max_len = max(map(len, args.flp_args))

    if args.random:
        args.seed = int(datetime.datetime.now().timestamp())
    np.random.seed(args.seed)

    results_df = pd.DataFrame(columns=["dataset", "arguments", "split", "train_success_rate", "test_success_rate", "reference_test_success", "test_difference", "train_time", "test_time"])
    for dataset in [DATASETS_MAP[ds] for ds in args.datasets]:
        df = pd.read_csv(dataset.strip(), header=None)
        for split in SPLITS:
            df_train, df_test = train_test_split(df, test_size=split, random_state=args.seed)
            df_train.to_csv("training_data.tmp", index=False, header=False)
            df_test.to_csv("testing_data.tmp", index=False, header=False)
            train_y = df_train.pop(df_train.columns[-1]).to_numpy()
            train_X = df_train
            train_X.to_csv("train_X.tmp", index=False, header=False)
            test_y = df_test.pop(df_test.columns[-1]).to_numpy()
            test_X = df_test
            test_X.to_csv("test_X.tmp", index=False, header=False)

            # train, evaluate, compare to a reference implementation
            for arguments, max_depth, min_samples_split, min_samples_leaf in zip(args.flp_args, args.max_depth, args.min_samples_split, args.min_samples_leaf):
                os.system(f"./flp-fun training_data.tmp {arguments} > trained_tree.tmp")

                train_start = time.time() 
                os.system(f"./flp-fun trained_tree.tmp train_X.tmp > predictions.tmp") # TODO possibly add arguments for flp-fun inference
                predictions = pd.read_csv("predictions.tmp", header=None).to_numpy().flatten()
                train_success_rate = (predictions == train_y).sum() / train_y.shape[0]
                train_time = time.time() - train_start

                test_start = time.time()
                os.system(f"./flp-fun trained_tree.tmp test_X.tmp > predictions.tmp") # TODO possibly add arguments for flp-fun inference
                predictions = pd.read_csv("predictions.tmp", header=None).to_numpy().flatten()
                test_success_rate = (predictions == test_y).sum() / test_y.shape[0]
                test_time = time.time() - test_start

                clf = DecisionTreeClassifier(max_depth=max_depth, min_samples_split=min_samples_split, min_samples_leaf=min_samples_split, random_state=args.seed)
                clf.fit(train_X, train_y)
                clf_predictions = clf.predict(test_X)
                reference_test_success = (clf_predictions == test_y).sum() / test_y.shape[0]

                test_difference = reference_test_success - test_success_rate
                arguments = arguments.rjust(arg_max_len, " ")
                results_df.loc[len(results_df.index)] = [dataset, arguments, round(split, 2), train_success_rate, test_success_rate, reference_test_success, test_difference, train_time, test_time]

                print(results_df.iloc[-1])

    results_df.to_csv("test_results.csv", index=False, float_format='% .3f')

    try:
        os.remove("training_data.tmp")
        os.remove("testing_data.tmp")
        os.remove("train_X.tmp")
        os.remove("test_X.tmp")
        os.remove("trained_tree.tmp")
        os.remove("predictions.tmp")
    except:
        pass