"""Data quality plot: raw poll vote shares over time with election results."""

import pandas as pd
import matplotlib.pyplot as plt
import matplotlib.dates as mdates
import numpy as np

PARTY_COLS = ["cdu_csu", "spd", "gruene", "fdp", "afd", "linke", "bsw", "sonstige"]

PARTY_LABELS = {
    "cdu_csu": "CDU/CSU", "spd": "SPD", "gruene": "GRÜNE", "fdp": "FDP",
    "afd": "AfD", "linke": "LINKE", "bsw": "BSW", "sonstige": "Sonstige"
}

PARTY_COLORS = {
    "CDU/CSU": "#000000", "SPD": "#E3000F", "GRÜNE": "#1AA037", "FDP": "#FFCC00",
    "AfD": "#009EE0", "LINKE": "#BE3075", "BSW": "#572B81", "Sonstige": "#AAAAAA"
}

PARTY_ORDER = list(PARTY_COLORS.keys())

# --- Load data ---
polls = pd.read_csv("data/polls.csv", parse_dates=["date_published"])
elections = pd.read_csv("data/results.csv", parse_dates=["election_date"])

# Pivot polls to long
polls_long = polls.melt(
    id_vars=["date_published", "pollster"], value_vars=PARTY_COLS,
    var_name="party_col", value_name="vote_share"
).dropna(subset=["vote_share", "date_published"])
polls_long["party"] = polls_long["party_col"].map(PARTY_LABELS)
polls_long["date"] = polls_long["date_published"]

# Pivot elections to long
elections_long = elections.melt(
    id_vars=["election_date"], value_vars=PARTY_COLS,
    var_name="party_col", value_name="vote_share"
).dropna(subset=["vote_share"])
elections_long["party"] = elections_long["party_col"].map(PARTY_LABELS)
elections_long["date"] = elections_long["election_date"]
elections_long["vote_share"] = elections_long["vote_share"] * 100

# --- Minimal theme setup ---
plt.rcParams.update({
    "font.family": "sans-serif",
    "font.size": 10,
    "axes.spines.top": False,
    "axes.spines.right": False,
    "axes.spines.left": False,
    "axes.grid": True,
    "axes.grid.axis": "y",
    "grid.color": "#E5E5E5",
    "grid.linewidth": 0.4,
    "xtick.color": "#555555",
    "ytick.color": "#555555",
    "axes.labelcolor": "#333333",
    "figure.facecolor": "white",
    "axes.facecolor": "white",
})

# --- Plot ---
fig, ax = plt.subplots(figsize=(16, 7))

for party in PARTY_ORDER:
    subset = polls_long[polls_long["party"] == party]
    if subset.empty:
        continue
    ax.scatter(subset["date"], subset["vote_share"],
               s=1.5, alpha=0.2, color=PARTY_COLORS[party], label=party, rasterized=True)

# Election results as diamonds
for party in PARTY_ORDER:
    subset = elections_long[elections_long["party"] == party]
    if subset.empty:
        continue
    ax.scatter(subset["date"], subset["vote_share"],
               s=60, marker="D", color=PARTY_COLORS[party],
               edgecolors="white", linewidths=0.5, zorder=5)

# Election date lines
for d in elections["election_date"]:
    ax.axvline(d, color="#BBBBBB", linewidth=0.4, linestyle=":", zorder=1)

ax.set_ylim(0, 55)
ax.set_ylabel("Vote share")
ax.yaxis.set_major_formatter(plt.FuncFormatter(lambda x, _: f"{x:.0f}%"))
ax.xaxis.set_major_locator(mdates.YearLocator(5))
ax.xaxis.set_minor_locator(mdates.YearLocator(1))
ax.xaxis.set_major_formatter(mdates.DateFormatter("%Y"))

ax.set_title("Raw polling data", fontsize=14, fontweight="bold", loc="left", pad=12)
ax.text(0, 1.02, "All polls from 8 institutes. Diamonds = election results.",
        transform=ax.transAxes, fontsize=9, color="#666666", va="bottom")

handles, labels = ax.get_legend_handles_labels()
legend = ax.legend(handles, labels, loc="lower center", bbox_to_anchor=(0.5, -0.12),
                   ncol=8, frameon=False, fontsize=9, markerscale=4)

plt.tight_layout()
plt.savefig("estimation/plt/data_quality/raw_vote_shares.png", dpi=200, bbox_inches="tight")
print("Saved: estimation/plt/data_quality/raw_vote_shares.png")
