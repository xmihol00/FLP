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
    plt.figure(figsize=(10, 6))
    for results_filename, marker, color in zip(sorted(glob(f"*_{test_type}_results.csv")), ['s', 'D', '*', 'o'], mcolors.TABLEAU_COLORS):
        marker_dict["Success"] = marker
        df = pd.read_csv(results_filename)
        max_time = df["time"].max()
        for row in df.itertuples():
            plt.plot(row.Index, row.time, marker=marker_dict[row.result], markersize=10, color=color)
        df.set_index("graph_name", inplace=True)
        plt.plot(df["time"], label=results_filename.split("_")[0], marker='', linestyle='--', markersize=0, color=color)
    plt.title(f"{test_type.replace('_', ' ').capitalize()} graph performance comparison")
    plt.xlabel("nodes")
    plt.yscale("log")
    plt.ylabel("time (s)")
    plt.scatter([list(df.itertuples())[0].Index], [-5], marker="$F$", color="black", label="Fail", s=50)
    plt.scatter([list(df.itertuples())[0].Index], [-5], marker="$T$", color="black", label="Timeout", s=50)
    plt.legend()
    plt.grid()
    plt.tight_layout()
    plt.savefig(f"{test_type}_results.png", dpi=400)