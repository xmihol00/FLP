import pandas as pd
from glob import glob
import os

for file in glob('data/*.txt'):
    base_name = os.path.basename(file)
    df = pd.read_csv(file)
    df = df.iloc[:, :-1]
    values_file = f"values/{base_name}"
    df.to_csv(values_file, index=False, header=False)
    os.system(f"./flp-fun trained/{base_name} {values_file} > inference/{base_name}")
