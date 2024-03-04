import os
import pandas as pd
import numpy as np
from sklearn.model_selection import train_test_split

DATASETS = ["penguins_cleaned.csv", "iris_cleaned.csv", "wines_cleaned.csv", "boston_housing_cleaned.csv"]
SPLITS = [0.0, 0.1, 0.2, 0.3, 0.4, 0.5]

for dataset in DATASETS:
    df = pd.read_csv(dataset, header=None)
    for split in SPLITS:
        df_train, df_test = train_test_split(df, test_size=split)
        df_train.to_csv("training_data.tmp", index=False, header=False)
        df_test.to_csv("testing_data.tmp", index=False, header=False)
        train_y = df_train.pop(df_train.columns[-1]).to_numpy()
        train_X = df_train
        train_X.to_csv("train_X.tmp", index=False, header=False)
        test_y = df_test.pop(df_test.columns[-1]).to_numpy()
        test_X = df_test
        test_X.to_csv("test_X.tmp", index=False, header=False)

        os.system(f"./flp-fun training_data.tmp > trained_tree.tmp")
        
        os.system(f"./flp-fun trained_tree.tmp train_X.tmp > predictions.tmp")
        predictions = pd.read_csv("predictions.tmp", header=None).to_numpy().flatten()
        train_success_rate = (predictions == train_y).sum() / train_y.shape[0]

        os.system(f"./flp-fun trained_tree.tmp test_X.tmp > predictions.tmp")
        predictions = pd.read_csv("predictions.tmp", header=None).to_numpy().flatten()
        test_success_rate = (predictions == test_y).sum() / test_y.shape[0] 
        
        print(f"{dataset} {split}:", train_success_rate, test_success_rate)
        exit()

try:
    os.remove("X.tmp")
    os.remove("trained_tree.tmp")
    os.remove("predictions.tmp")
except:
    pass