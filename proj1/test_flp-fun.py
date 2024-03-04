import os
import pandas as pd

DATASETS = ["iris_cleaned.csv", "penguins_cleaned.csv", "wines_cleaned.csv", "boston_housing_cleaned.csv"]

for dataset in DATASETS:
    df = pd.read_csv(dataset)
    y = df.pop(df.columns[-1])
    df.to_csv("X.tmp", index=False, header=False)
    os.system(f"./flp-fun {dataset} > trained_tree.tmp")
    os.system(f"./flp-fun trained_tree.tmp X.tmp > predictions.tmp")
    predictions = pd.read_csv("predictions.tmp", header=None)
    # compare predictions with y
    success_rate = (predictions == y).sum() / len(y)
    print(f"{dataset}:", success_rate)

try:
    os.remove("X.tmp")
    os.remove("trained_tree.tmp")
    os.remove("predictions.tmp")
except FileNotFoundError:
    pass