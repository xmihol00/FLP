import pandas as pd
import numpy as np
import sys

if __name__ == "__main__":
    dataset = sys.argv[1]

    df = pd.read_csv(dataset)
    df = df.dropna()

    if dataset == "penguins.csv":
        df.drop(columns=["island"], inplace=True)
        df = df[~df.apply(lambda row: row.astype(str).str.strip() == '.').any(axis=1)]
        df["sex"] = df["sex"].replace({"MALE": -0.5, "FEMALE": 0.5}).astype(np.float32)
        first_column = df.pop("species")
        df.insert(len(df.columns), "species", first_column)
        df.to_csv("penguins_cleaned.csv", index=False, header=False)

    elif dataset == "wines.csv":
        df = df.drop(columns=["Id"])
        df["quality"] = df["quality"].astype(str)
        df.to_csv("wines_cleaned.csv", index=False, header=False)
    
    elif dataset == "boston_housing.csv":
        df["MEDV"] = (df["MEDV"].astype(np.int32) // 10) * 10
        df.to_csv("boston_housing_cleaned.csv", index=False, header=False)
    
    elif dataset == "iris.csv":
        df.to_csv("iris_cleaned.csv", index=False, header=False)
