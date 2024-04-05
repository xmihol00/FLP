import matplotlib.colors as mcolors
import matplotlib.pyplot as plt
import pandas as pd
from glob import glob

marker_dict = {
    "Success": '',
    "Fail": "$F$",
    "Timeout": "$T$"
}

fig, axes = plt.subplots(2, 2, figsize=(14, 8))
axes = axes.flatten()
for ax, test_type in zip(axes, ["circle", "fully_connected", "2D_mesh", "2D_wrap-around_mesh"]):
    for results_filename, marker, color in zip(sorted(glob(f"*_{test_type}_results.csv")), ['o', 's', 'D', '*'], mcolors.TABLEAU_COLORS):
        marker_dict["Success"] = marker
        df = pd.read_csv(results_filename)
        markers = [marker_dict[result] for result in df["result"]]
        for row in df.itertuples():
            ax.plot(row.Index, row.time, marker=marker_dict[row.result], markersize=6, color=color, alpha=0.75)
        df.set_index("graph_name", inplace=True)
        ax.plot(df["time"], label=results_filename.split("_")[0], marker='', linestyle='--', markersize=0, color=color, alpha=0.75)
    ax.set_title(f"{test_type.replace('_', ' ').capitalize()} graph performance comparison")
    ax.set_xlabel("nodes")
    ax.set_yscale("log")
    ax.set_ylabel("time [s]")
    ax.grid()
    
plt.legend()
plt.tight_layout()
plt.savefig(f"performance_results.png", dpi=400)