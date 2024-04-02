from time import time, sleep
from glob import glob
import os
import pandas as pd
import argparse
import subprocess

parser = argparse.ArgumentParser()
parser.add_argument("-r", "--rerun_all_tests", action="store_true", help="Rerun all tests.")
parser.add_argument("-t", "--timeout", type=int, default=60, help="Timeout for each test.")

args = parser.parse_args()


for suffix in ["circle.txt", "fully_connected.txt"]:
    test_type = suffix.split(".")[0]
    csv_name = f"performance_results_{test_type}.csv"
    if os.path.exists(csv_name) and not args.rerun_all_tests:
        df = pd.read_csv(csv_name)
        df.set_index("file_name", inplace=True)
    else:
        df = pd.DataFrame(columns=["file_name", "test_type", "time", "result"])
        df.set_index("file_name", inplace=True)

    for filename in sorted(list(glob(f"*{suffix}"))):
        if filename not in df.index:
            start = time()
            try:
                ret_code = subprocess.run(f"timeout {args.timeout + 1} ../test-flp23-log <{filename} >/dev/null 2>/dev/null", shell=True, timeout=args.timeout)
                ret_message = "Success" if ret_code.returncode == 0 else "Failed"
            except subprocess.TimeoutExpired:
                ret_code = None
                ret_message = "Timeout"
            end = time()
            df.loc[filename] = [test_type, end - start, ret_message]
            df.to_csv(csv_name)
            print(f"{filename} took {end - start} seconds with {ret_message}")
            sleep(2)
