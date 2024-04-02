from time import time, sleep
from glob import glob
import os
import pandas as pd
import argparse
import subprocess

parser = argparse.ArgumentParser()
parser.add_argument("-r", "--rerun_all_tests", action="store_true", help="Rerun all tests.")

args = parser.parse_args()

if os.path.exists("performance_results.csv") and not args.rerun_all_tests:
    df = pd.read_csv("performance_results.csv")
    df.set_index("filename", inplace=True)
else:
    df = pd.DataFrame(columns=["filename", "test_type", "time", "ret_code"])
    df.set_index("filename", inplace=True)

for suffix in ["circle.txt", "fully_connected.txt"]:
    test_type = suffix.split(".")[0]
    for filename in glob(f"*{suffix}"):
        if filename not in df.index:
            start = time()
            ret_code = subprocess.run(f"../test-flp23-log <{filename} >/dev/null 2>/dev/null", shell=True, timeout=300)
            end = time()
            df.loc[filename] = [test_type, end - start, ret_code.returncode]
            df.to_csv("performance_results.csv")
            print(f"{filename} took {end - start} seconds, return code {ret_code.returncode}")
            sleep(1)
