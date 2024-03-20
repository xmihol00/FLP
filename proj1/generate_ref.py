###############################################################################################################################
#   project: flp-fun (1st project regarding decision trees to Functional and Logic Programming course at FIT, BUT)
#    author: David Mihola (xmihol00)
#     email: xmihol00@stud.fit.vutbr.cz
#      date: 31. 3. 2024
# file info: Script to generate reference classification output.
###############################################################################################################################

import pandas as pd
from glob import glob
import os


for file in glob("edge_datasets/*.csv"):
    base_name = os.path.basename(file)
    df = pd.read_csv(file)
    df = df.iloc[:, :-1]
    values_file = f"values/{base_name}"
    df.to_csv(values_file, index=False, header=False)
    os.system(f"./flp-fun -2 {file} > edge_trained/{base_name}")
    # generate a reference classification output for each dataset
    os.system(f"./flp-fun -1 edge_trained/{base_name} {values_file} > edge_ground_truth/{base_name}")
