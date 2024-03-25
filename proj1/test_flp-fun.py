#############################################################################################################################
#   project: flp-fun (1st project regarding decision trees to Functional and Logic Programming course at FIT, BUT)
#    author: David Mihola (xmihol00)
#     email: xmihol00@stud.fit.vutbr.cz
#      date: 31. 3. 2024
# file info: Script testing the Haskell implementation against a scikit-learn implementation of a decision tree classifier.
#############################################################################################################################

# usage e.g.: python3 ../test_flp-fun.py -ds p h w -md 4 8 -fa "-d 4" "-d 8" -tt t
#           : python3 ../test_flp-fun.py -ds p h w -mss 3 8 -fa "-mss 3" "-mss 8" -tt t
#           : python3 ../test_flp-fun.py -ds p h w -msl 4 7 -fa "-msl 4" "-msl 7" -tt t
#           : python3 ../test_flp-fun.py -ds p h w -md 4 8 -mss 3 8 -msl 4 7 -fa "-d 4 -mss 3 -msl 4" "-d 8 -mss 7 -msl 6" -tt t
#           : mkdir -p test_results && cd test_results && python3 ../test_flp-fun.py
# or just   : python3 ../test_flp-fun.py

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

DATASETS_MAP = {
    'p': "penguins_all.csv",
    'i': "    iris_all.csv",
    'h': " housing_all.csv",
    'w': "   wines_all.csv",
    # TODO add some more maybe synthetic datasets to test something specific
}

SPLITS = [0, 0.1, 0.2, 0.3, 0.4, 0.5] # test/train splits of the datasets

if __name__ == "__main__":
    parser = argparse.ArgumentParser(description="Run tests on flp-fun")
    parser.add_argument("--test_type", "-tt", type=str, default="both", choices=["both", "training", "inference", "b", "t", "i"], help="Type of tests to run. Default is both. b for both, t for training, i for inference.")
    parser.add_argument("--datasets", "-ds", type=str, nargs='+', default=['p', 'i', 'h', 'w'], choices=['p', 'i', 'h', 'w'], help="Data sets to use for testing. p for penguins, i for iris, h for housing, w for wines. Default is all.")
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
    # file names. You can adapt this approach.
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

    # ensure reproducibility or randomness
    if args.random:
        args.seed = int(datetime.datetime.now().timestamp())
    np.random.seed(args.seed)

    # make sure the script will find all the necessary files when run from a different directory
    script_path = os.path.dirname(__file__)
    if not script_path:
        script_path = "."
    
    strip_string = lambda x: x.strip() if isinstance(x, str) else x # strip string values in the dataframes

    all_training_passed = None
    if args.test_type in ["both", "b", "training", "t"]:
        print("Running training tests...")

        # remove old output files if they exist
        os.remove("flp-fun_training_stdout.out") if os.path.exists("flp-fun_training_stdout.out") else None
        os.remove("flp-fun_training_stderr.out") if os.path.exists("flp-fun_training_stderr.out") else None
        os.remove("flp-fun_parsing_stderr.out") if os.path.exists("flp-fun_parsing_stderr.out") else None
        
        # initialize the control variables
        all_training_passed = True
        passed_count = 0
        failed_count = 0
        total_count = 0
        results_df = pd.DataFrame(columns=["dataset", "arguments", "split", "train_success_rate", "test_success_rate", "reference_train_success_rate", "reference_test_success_rate", "test_difference", "train_time", "result"])

        for dataset in [DATASETS_MAP[ds] for ds in args.datasets]:
            df = pd.read_csv(f"{script_path}/datasets/{dataset.strip()}", header=None)
            for split in SPLITS:
                if split > 0:
                    # split the data set to train and test sets
                    df_train, df_test = train_test_split(df, test_size=split, random_state=args.seed)
                else:
                    df_train = df.copy()
                    df_test = df.copy()

                # save the training split to train a decision tree
                df_train.to_csv("training_data.tmp", index=False, header=False)

                # remove the target column from the data sets
                df_train[df_train.columns[-1]] = df_train[df_train.columns[-1]].apply(strip_string)
                train_y = df_train.pop(df_train.columns[-1]).to_numpy()
                train_X = df_train
                df_test[df_test.columns[-1]] = df_test[df_test.columns[-1]].apply(strip_string)
                test_y = df_test.pop(df_test.columns[-1]).to_numpy()
                test_X = df_test
                # save the test and train sets as samples to be predicted by the trained tree
                train_X.to_csv("train_X.tmp", index=False, header=False)
                test_X.to_csv("test_X.tmp", index=False, header=False)

                # train, evaluate, compare to a reference implementation
                for arguments, max_depth, min_samples_split, min_samples_leaf in zip(args.flp_args, args.max_depth, args.min_samples_split, args.min_samples_leaf):
                    # train the tree and save it
                    train_start = time.time() 
                    os.system(f"{script_path}/flp-fun -2 training_data.tmp {arguments} | tee -a flp-fun_training_stdout.out >trained_tree.tmp 2>flp-fun_training_stderr.out")
                    train_time = time.time() - train_start

                     # predict the train set, the accuracy should be very high
                    os.system(f"{script_path}/flp-fun -1 trained_tree.tmp train_X.tmp | tee -a flp-fun_training_stdout.out >predictions.tmp 2>flp-fun_training_stderr.out") # TODO possibly add arguments for flp-fun inference
                    train_predictions = pd.read_csv("predictions.tmp", header=None).apply(strip_string).to_numpy().flatten()
                    train_success_rate = (train_predictions == train_y).sum() / train_y.shape[0] # this should be 1.0 for completely overfitted trees on the training set

                    # predict the test set
                    os.system(f"{script_path}/flp-fun -1 trained_tree.tmp test_X.tmp | tee -a flp-fun_training_stdout.out >predictions.tmp 2>flp-fun_training_stderr.out") # TODO possibly add arguments for flp-fun inference
                    test_predictions = pd.read_csv("predictions.tmp", header=None).apply(strip_string).to_numpy().flatten()
                    test_success_rate = (test_predictions == test_y).sum() / test_y.shape[0] # this will differ based on implementation

                    # reference implementation
                    clf = DecisionTreeClassifier(max_depth=max_depth, min_samples_split=min_samples_split, min_samples_leaf=min_samples_split, random_state=args.seed)
                    clf.fit(train_X, train_y)
                    clf_test_predictions = clf.predict(test_X)
                    clf_train_predictions = clf.predict(train_X)
                    reference_test_success = (clf_test_predictions == test_y).sum() / test_y.shape[0]
                    reference_train_success = (clf_train_predictions == train_y).sum() / train_y.shape[0]

                    #misses = train_predictions != train_y
                    #indices = np.where(misses)[0]
                    #train_misses = train_predictions[misses]
                    #clf_train_misses = clf_train_predictions[clf_train_predictions != train_y]
                    #if train_misses.size > 0:
                    #    print(f"Train misses: {train_misses}, actual: {clf_train_misses}", indices)
                    #    exit(1)

                    # compare prediction with the reference, this can even be negative if the reference implementation is worse.
                    test_difference = reference_test_success - test_success_rate
                    # evaluate the results
                    message = "PASSED"
                    if test_difference > args.epsilon:
                        all_training_passed = False
                        print(f"Test failed for dataset {dataset}, split {split}, arguments {arguments}. Difference is {test_difference}.")
                        print(f"Ground truth: {test_y}")
                        print(f"Reference:    {clf_test_predictions}")
                        print(f"Actual:       {test_predictions}")
                        message = "FAILED"
                        failed_count += 1
                    else:
                        passed_count += 1
                    total_count += 1

                    # write the results to a dataframe
                    arguments = arguments.rjust(arg_max_len, " ")
                    results_df.loc[len(results_df.index)] = [
                        dataset, arguments, round(split, 2), train_success_rate, test_success_rate, reference_train_success, reference_test_success, test_difference, train_time, message
                    ]


        if all_training_passed:
            print(colored("All training tests PASSED.", "green"))
        else:
            print(colored("Some training tests FAILED.", "red"))

        if not args.no_files:
            results_df.to_csv("training_tests_result.csv", index=False, float_format='% .3f')
            with open("training_tests_summary.txt", "w") as f:
                if all_training_passed:
                    f.write("All tests PASSED.\n")
                else:
                    f.write("Some tests FAILED.\n")
                f.write(f"Passed:         {passed_count}/{total_count}\n")
                f.write(f"Failed:         {failed_count}/{total_count}\n")
                f.write(f"Success rate:   {passed_count/total_count*100:.2f} %\n")
                f.write(f"Failed rate:    {failed_count/total_count*100:.2f} %\n")
                f.write(f"Results saved to training_tests_result.csv")

        try:
            os.remove("training_data.tmp") if os.path.exists("training_data.tmp") else None
            os.remove("train_X.tmp") if os.path.exists("train_X.tmp") else None
            os.remove("test_X.tmp") if os.path.exists("test_X.tmp") else None
            os.remove("trained_tree.tmp") if os.path.exists("trained_tree.tmp") else None
            os.remove("predictions.tmp") if os.path.exists("predictions.tmp") else None
        except:
            pass
    
    all_inference_passed = None
    if args.test_type in ["both", "b", "inference", "i"]:
        print("Running inference tests...")
        os.remove("flp-fun_inference_stdout.out") if os.path.exists("flp-fun_inference_stdout.out") else None
        os.remove("flp-fun_inference_stderr.out") if os.path.exists("flp-fun_inference_stderr.out") else None
        
        all_inference_passed = True
        passed_count = 0
        failed_count = 0
        total_count = 0
        
        # run inference tests from directories 'trained', 'values' and 'ground_truth', more tests can be added, the file names just need to match
        for file in glob(f"{script_path}/trained/*.txt"): 
            base_name = os.path.basename(file).replace(".txt", ".csv")
            
            # run the inference
            os.system(f"{script_path}/flp-fun -1 {file} {script_path}/values/{base_name} | tee -a flp-fun_inference_stdout.out >predictions.tmp 2>flp-fun_inference_stderr.out")

            # load the predictions and ground truth
            predictions = pd.read_csv("predictions.tmp", header=None).apply(strip_string).to_numpy().flatten()
            ground_truth = pd.read_csv(f"{script_path}/ground_truth/{base_name}", header=None).apply(strip_string).to_numpy().flatten()

            # compare the predictions to the ground truth, here any difference is a failure since the inference should be deterministic
            same = np.array_equal(predictions, ground_truth)
            message = "PASSED"
            if not same:
                all_inference_passed = False
                print(f"Test failed for file {file}.")
                message = "FAILED"
                failed_count += 1
            else:
                passed_count += 1
            total_count += 1

        if all_inference_passed:
            print(colored("All inference tests PASSED.", "green"))
        else:
            print(colored("Some inference tests FAILED.", "red"))
        
        if not args.no_files:
            with open("inference_tests_summary.txt", "w") as f:
                if all_inference_passed:
                    f.write("All tests PASSED.\n")
                else:
                    f.write("Some tests FAILED.\n")

                f.write(f"Passed:       {passed_count}/{total_count}\n")
                f.write(f"Failed:       {failed_count}/{total_count}\n")
                f.write(f"Success rate: {passed_count/total_count*100:.2f}%\n")
                f.write(f"Failed rate:  {failed_count/total_count*100:.2f}%\n")
        
        try:
            os.remove("predictions.tmp") if os.path.exists("predictions.tmp") else None
        except:
            pass

# return codes to evaluate the tests automatically
if all_training_passed is None and all_inference_passed is None:  # no tests were run
    exit(-1)
if all_training_passed is None:                                   # only inference tests were run
    if all_inference_passed:                                      # only inference tests were run and passed
        exit(0)
    else:                                                         # only inference tests were run and failed                   
        exit(2)
if all_inference_passed is None:                                  # only training tests were run
    if all_training_passed:                                       # only training tests were run and passed
        exit(0)
    else:                                                         # only training tests were run and failed
        exit(1)
if all_training_passed and all_inference_passed:                  # both tests were run and passed
    exit(0)
if not all_training_passed and not all_inference_passed:          # both tests were run and failed
    exit(3)

# cleanup leftover file if you dare: 'rm *.out *.tmp'