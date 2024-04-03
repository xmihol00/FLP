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

for suffix in ["circle.txt", "fully_connected.txt", "2D_mesh.txt", "2D_wrap-around_mesh.txt"]:
    test_type = suffix.split(".")[0]
    for implementation in ["nodes", "edges", "combined1", "combined2"]:
        os.system(f"cd .. && make test_{implementation}")
        csv_name = f"{implementation}_{test_type}_results.csv"
        if os.path.exists(csv_name) and not args.rerun_all_tests:
            df = pd.read_csv(csv_name)
            df.set_index("graph_name", inplace=True)
        else:
            df = pd.DataFrame(columns=["graph_name", "test_type", "time", "result"])
            df.set_index("graph_name", inplace=True)

        for filename in sorted(list(glob(f"*{suffix}"))):
            graph_name = filename.split("_")[0]
            if graph_name not in df.index or (df.loc[graph_name]["result"] == "Timeout" and df.loc[graph_name]["time"] < args.timeout):
                try:
                    command = f"timeout {args.timeout + 1} ../test-flp23-log-{implementation} <{filename} >/dev/null"
                    print(f"Launching: {command}")
                    start = time()
                    ret_code = subprocess.run(command, shell=True, timeout=args.timeout)
                    end = time()
                    if ret_code.returncode == 0:
                        ret_message = "Success"
                    else:
                        ret_message = "Fail"
                except subprocess.TimeoutExpired:
                    ret_code = None
                    ret_message = "Timeout"
                    end = start + args.timeout

                df.loc[graph_name] = [test_type, end - start, ret_message]
                df.to_csv(csv_name)
                print(f"{filename} took {end - start} seconds with {ret_message}")
                sleep(1)
            
