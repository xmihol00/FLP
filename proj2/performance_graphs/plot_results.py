import matplotlib.colors as mcolors
import matplotlib.pyplot as plt
import pandas as pd
from glob import glob

marker_dict = {
    "Success": '',
    "Fail": "$F$",
    "Timeout": "$T$"
}

for test_type in ["circle", "fully_connected", "2D_mesh", "2D_wrap-around_mesh"]:
    plt.figure(figsize=(12, 8))
    for results_filename, marker, color in zip(sorted(glob(f"*_{test_type}_results.csv")), ['o', 's', 'D', '*'], mcolors.TABLEAU_COLORS):
        marker_dict["Success"] = marker
        df = pd.read_csv(results_filename)
        markers = [marker_dict[result] for result in df["result"]]
        for row in df.itertuples():
            plt.plot(row.Index, row.time, marker=marker_dict[row.result], markersize=10, color=color)
        df.set_index("graph_name", inplace=True)
        plt.plot(df["time"], label=results_filename.split("_")[0], marker='', linestyle='--', markersize=0, color=color)
    plt.title(f"{test_type.replace('_', ' ').capitalize()} graph performance comparison")
    plt.xlabel("nodes")
    plt.yscale("log")
    plt.ylabel("time (s)")
    plt.legend()
    plt.grid()
    plt.tight_layout()
    plt.savefig(f"{test_type}_results.png", dpi=400)