import matplotlib.pyplot as plt
import pandas as pd
from glob import glob

for test_type in ["circle"]: #, "fully_connected", "mesh"]:
    plt.figure(figsize=(10, 5))
    for results_filename, marker in zip(glob(f"*{test_type}_results.csv"), ['o', 's', 'D']):
        df = pd.read_csv(results_filename)
        df.set_index("file_name", inplace=True)
        plt.plot(df["time"], label=results_filename.split("_")[0], marker=marker, linestyle='-', markersize=5)
    plt.title(f"{test_type} test")
    plt.xlabel("nodes")
    plt.yscale("log")
    plt.ylabel("time (s)")
    plt.legend()
    plt.grid()
    plt.savefig(f"{test_type}_results.png", dpi=400)