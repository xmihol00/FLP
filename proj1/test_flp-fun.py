import os
import pandas as pd
import numpy as np
from sklearn.model_selection import train_test_split
import argparse
from sklearn.tree import DecisionTreeClassifier
import time
import datetime
from glob import glob
from termcolor import colored

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
    parser.add_argument("--test_type", "-tt", type=str, default="both", choices=["both", "training", "inference", "b", "t", "i"], help="Type of tests to run. Default is both. b for both, t for training, i for inference.")
    parser.add_argument("--datasets", "-ds", type=str, nargs='+', default=['p', 'i', 'h', 'w'], choices=['p', 'i', 'h', 'w'], help="Data sets to use for testing. p for penguins, i for iris, b for boston housing, w for wines. Default is all.")
    parser.add_argument("--epsilon", "-e", type=float, default=0.1, help="Allowed difference to reference implementation. Default is 0.1.")
    parser.add_argument("--max_depth", "-md", type=int, nargs='+', default=[], help="Maximum depth of the reference decision tree. Default is 2^31.")
    # This parameter specifies the minimum number of samples required to split an internal node. If an internal node has fewer samples than min_samples_split, 
    # the split will not be performed, and the node will become a leaf node. This can be check simply by the number of samples in the node.
    parser.add_argument("--min_samples_split", "-mss", type=int, nargs='+', default=[], help="The minimum number of samples required to split a node. Default is 2.")
    # This parameter specifies the minimum number of samples required to be at a leaf node. If a split results in a leaf node with fewer samples than 
    # min_samples_leaf, the split is ignored, and the node is turned into a leaf node even if the split would result in a better training score.
    parser.add_argument("--min_samples_leaf", "-msl", type=int, nargs='+', default=[], help="The minimum number of samples required to be at a leaf node. Default is 1.")
    # Each of the flp arguments, passed as "quite a lot of arguments" will be coupled with the corresponding max_depth, min_samples_split, min_samples_leaf
    # at the same index in the lists. You can test your improved implementations this way. E.g. my implementation accepts additional parameters after the
    # file names.
    parser.add_argument("--flp_args", "-fa", type=str, nargs='+', default=[], help="Arguments to pass to flp-fun,. Default is none.")
    parser.add_argument("--no_files", "-nf", action="store_true", help="Do not save test output to files.")
    # ensure reproducibility
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

    if args.test_type in ["both", "b", "training", "t"]:
        print("Running training tests...")
        os.remove("flp-fun_training_stdout.tmp") if os.path.exists("flp-fun_training_stdout.tmp") else None
        os.remove("flp-fun_training_stderr.tmp") if os.path.exists("flp-fun_training_stderr.tmp") else None
        os.remove("flp-fun_parsing_stderr.tmp") if os.path.exists("flp-fun_parsing_stderr.tmp") else None
        
        all_pased = True
        passed_count = 0
        failed_count = 0
        total_count = 0
        parsing_failed_count = 0
        parsing_passed_count = 0
        results_df = pd.DataFrame(columns=["dataset", "arguments", "split", "train_success_rate", "test_success_rate", "reference_test_success", "test_difference", "train_time", "result"])
        for dataset in [DATASETS_MAP[ds] for ds in args.datasets]:
            df = pd.read_csv(dataset.strip(), header=None)
            for split in SPLITS:
                # split the data set to train and test sets
                df_train, df_test = train_test_split(df, test_size=split, random_state=args.seed)

                # save the training split to train a decision tree
                df_train.to_csv("training_data.tmp", index=False, header=False)

                # remove the target column from the data sets
                train_y = df_train.pop(df_train.columns[-1]).to_numpy()
                train_X = df_train
                test_y = df_test.pop(df_test.columns[-1]).to_numpy()
                test_X = df_test
                # save the test and train sets as samples to be predicted by the trained tree
                train_X.to_csv("train_X.tmp", index=False, header=False)
                test_X.to_csv("test_X.tmp", index=False, header=False)

                # train, evaluate, compare to a reference implementation
                for arguments, max_depth, min_samples_split, min_samples_leaf in zip(args.flp_args, args.max_depth, args.min_samples_split, args.min_samples_leaf):
                    # train the tree and save it
                    train_start = time.time() 
                    os.system(f"./flp-fun training_data.tmp {arguments} | tee -a flp-fun_training_stdout.tmp >trained_tree.tmp 2>flp-fun_training_stderr.tmp")
                    train_time = time.time() - train_start

                    if os.system(f"./flp-ref trained_tree.tmp --echo_tree >/dev/null 2>flp-fun_parsing_stderr.tmp"):
                        print("Trained tree could not be parsed by the reference implementation.")
                        parsing_failed_count += 1
                    else:
                        parsing_passed_count += 1

                    os.system(f"./flp-fun trained_tree.tmp train_X.tmp | tee -a flp-fun_training_stdout.tmp >predictions.tmp 2>flp-fun_training_stderr.tmp") # TODO possibly add arguments for flp-fun inference
                    predictions = pd.read_csv("predictions.tmp", header=None).to_numpy().flatten()
                    train_success_rate = (predictions == train_y).sum() / train_y.shape[0] # this should be 1.0 for completely overfitted trees on the training set

                    os.system(f"./flp-fun trained_tree.tmp test_X.tmp | tee -a flp-fun_training_stdout.tmp >predictions.tmp 2>flp-fun_training_stderr.tmp") # TODO possibly add arguments for flp-fun inference
                    predictions = pd.read_csv("predictions.tmp", header=None).to_numpy().flatten()
                    test_success_rate = (predictions == test_y).sum() / test_y.shape[0] # this will differ based on implementation

                    clf = DecisionTreeClassifier(max_depth=max_depth, min_samples_split=min_samples_split, min_samples_leaf=min_samples_split, random_state=args.seed)
                    clf.fit(train_X, train_y)
                    clf_predictions = clf.predict(test_X)
                    reference_test_success = (clf_predictions == test_y).sum() / test_y.shape[0]

                    test_difference = reference_test_success - test_success_rate # this can even be negative if the reference implementation is worse.
                    message = "PASSED"
                    if test_difference > args.epsilon:
                        all_pased = False
                        print(f"Test failed for dataset {dataset}, split {split}, arguments {arguments}. Difference is {test_difference}.")
                        message = "FAILED"
                        failed_count += 1
                    else:
                        passed_count += 1
                    total_count += 1

                    arguments = arguments.rjust(arg_max_len, " ")
                    results_df.loc[len(results_df.index)] = [
                        dataset, arguments, round(split, 2), train_success_rate, test_success_rate, reference_test_success, test_difference, train_time, message
                    ]


        if all_pased:
            print(colored("All training tests passed.", "green"))
        else:
            print(colored("Some training tests failed.", "red"))

        if not args.no_files:
            results_df.to_csv("training_tests_result.csv", index=False, float_format='% .3f')
            with open("training_tests_summary.txt", "w") as f:
                if all_pased:
                    f.write("All tests passed.\n")
                else:
                    f.write("Some tests failed.\n")
                f.write(f"Passed:         {passed_count}/{total_count}\n")
                f.write(f"Failed:         {failed_count}/{total_count}\n")
                f.write(f"Success rate:   {passed_count/total_count*100:.2f} %\n")
                f.write(f"Failed rate:    {failed_count/total_count*100:.2f} %\n")
                f.write(f"Parsing failed: {parsing_failed_count}/{total_count}\n")
                f.write(f"Parsing passed: {parsing_passed_count}/{total_count}\n")
                f.write(f"Results saved to training_tests_result.csv")

        try:
            os.remove("training_data.tmp")
            os.remove("train_X.tmp")
            os.remove("test_X.tmp")
            os.remove("trained_tree.tmp")
            os.remove("predictions.tmp")
        except:
            pass
    
    if args.test_type in ["both", "b", "inference", "i"]:
        print("Running inference tests...")
        os.remove("flp-fun_inference_stdout.tmp") if os.path.exists("flp-fun_inference_stdout.tmp") else None
        os.remove("flp-fun_inference_stderr.tmp") if os.path.exists("flp-fun_inference_stderr.tmp") else None
        
        all_pased = True
        passed_count = 0
        failed_count = 0
        total_count = 0

        for file in glob('trained/*.txt'):
            base_name = os.path.basename(file)
            os.system(f"./flp-fun trained/{base_name} values/{base_name} | tee -a flp-fun_inference_stdout.tmp >predictions.tmp 2>flp-fun_inference_stderr.tmp")
            predictions = pd.read_csv("predictions.tmp", header=None).to_numpy().flatten()
            ground_truth = pd.read_csv(f"ground_truth/{base_name}", header=None).to_numpy().flatten()
            same = np.array_equal(predictions, ground_truth)
            message = "PASSED"
            if not same:
                all_pased = False
                print(f"Test failed for file {file}.")
                message = "FAILED"
                failed_count += 1
            else:
                passed_count += 1
            total_count += 1

        if all_pased:
            print(colored("All inference tests passed.", "green"))
        else:
            print(colored("Some inference tests failed.", "red"))
        
        if not args.no_files:
            with open("inference_tests_summary.txt", "w") as f:
                if all_pased:
                    f.write("All tests passed.\n")
                else:
                    f.write("Some tests failed.\n")

                f.write(f"Passed:       {passed_count}/{total_count}\n")
                f.write(f"Failed:       {failed_count}/{total_count}\n")
                f.write(f"Success rate: {passed_count/total_count*100:.2f}%\n")
                f.write(f"Failed rate:  {failed_count/total_count*100:.2f}%\n")