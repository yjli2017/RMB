"""Step 7 — Descriptive + pairwise statistical tests on derived metrics.

Input  : awake.parquet, sleep.parquet (Step 4), mt.parquet (Step 3),
         metadata.parquet (Step 2)
Output : channel_stats.csv      — per-channel summary
         stats_results.csv      — pairwise Mann–Whitney across groups, per metric
         circadian_hourly.csv   — hourly population mean activity / sleep
         schema.json
         plots/channel_overview.{png,html}, plots/circadian.{png,html},
         plots/group_compare.{png,html}
"""

from __future__ import annotations

import os
from itertools import combinations
from typing import Dict, List

import numpy as np
import pandas as pd
import plotly.graph_objects as go
import matplotlib
matplotlib.use("Agg")
import matplotlib.pyplot as plt
from scipy import stats as sstats

from ..config import RMBConfig
from ..manifest import write_schema
from ..visualizer import (
    NATURE_COLORS,
    NATURE_PALETTE,
    add_day_boundaries,
    apply_nature_style,
    nature_plotly_layout,
    save_nature_figure,
)


STEP_NO = 7
STEP_NAME = "stats"


def _fly_col(monitor: str, channel: int) -> str:
    return f"{monitor}_Ch{channel}"


def run(config: RMBConfig, mt_path: str, awake_path: str, sleep_path: str,
        metadata_path: str) -> Dict:
    step_dir = config.step_dir(STEP_NO, STEP_NAME)
    plots_dir = os.path.join(step_dir, "plots")

    mt = pd.read_parquet(mt_path)
    awake = pd.read_parquet(awake_path)
    sleep = pd.read_parquet(sleep_path)
    md = pd.read_parquet(metadata_path)

    # Per-channel summary
    channel_stats = pd.DataFrame({
        "activity_mean": mt.mean(),
        "activity_std": mt.std(),
        "awake_proportion": awake.mean(),
        "sleep_proportion": sleep.mean(),
    })
    channel_stats.index.name = "fly_column"
    channel_stats_path = os.path.join(step_dir, "channel_stats.csv")
    channel_stats.to_csv(channel_stats_path)

    # Circadian hourly aggregate
    rows_per_hour = 60
    n_hours = mt.shape[0] // rows_per_hour
    hourly = []
    for h in range(n_hours):
        s = h * rows_per_hour
        e = (h + 1) * rows_per_hour
        hourly.append({
            "hour": h,
            "activity_mean": float(mt.iloc[s:e].values.mean()),
            "sleep_mean": float(sleep.iloc[s:e].values.mean()),
        })
    circadian_df = pd.DataFrame(hourly)
    circadian_path = os.path.join(step_dir, "circadian_hourly.csv")
    circadian_df.to_csv(circadian_path, index=False)

    # Pairwise group comparisons (Mann–Whitney U) on per-channel metrics
    results = []
    if "Group" in md.columns:
        col_to_group: dict = {}
        for _, r in md.iterrows():
            c = _fly_col(str(r["Monitor_number"]), int(r["Channel"]))
            if c in channel_stats.index:
                col_to_group[c] = str(r["Group"])
        if col_to_group:
            channel_stats_with_group = channel_stats.copy()
            channel_stats_with_group["Group"] = channel_stats_with_group.index.map(col_to_group)
            groups = sorted(g for g in channel_stats_with_group["Group"].dropna().unique())
            for metric in ("activity_mean", "awake_proportion", "sleep_proportion"):
                for a, b in combinations(groups, 2):
                    va = channel_stats_with_group.loc[channel_stats_with_group["Group"] == a, metric].dropna().values
                    vb = channel_stats_with_group.loc[channel_stats_with_group["Group"] == b, metric].dropna().values
                    if len(va) >= 2 and len(vb) >= 2:
                        u, p = sstats.mannwhitneyu(va, vb, alternative="two-sided")
                        results.append({"metric": metric, "group_a": a, "group_b": b,
                                        "n_a": int(len(va)), "n_b": int(len(vb)),
                                        "statistic": float(u), "p_value": float(p),
                                        "mean_a": float(np.mean(va)), "mean_b": float(np.mean(vb))})
    stats_results = pd.DataFrame(results)
    stats_results_path = os.path.join(step_dir, "stats_results.csv")
    stats_results.to_csv(stats_results_path, index=False)

    # Plot 1: channel overview
    fig, axes = plt.subplots(2, 2, figsize=(7.2, 5.2))
    axes[0, 0].hist(channel_stats["activity_mean"].values, bins=20, color=NATURE_PALETTE["blue"], alpha=0.9,
                    edgecolor="white", linewidth=0.5)
    axes[0, 0].set_title("Activity mean per channel", loc="left"); axes[0, 0].set_xlabel("Mean MT")
    axes[0, 1].hist(channel_stats["awake_proportion"].values, bins=20, color=NATURE_PALETTE["bluish_green"], alpha=0.9,
                    edgecolor="white", linewidth=0.5)
    axes[0, 1].set_title("Awake proportion per channel", loc="left"); axes[0, 1].set_xlabel("Awake fraction")
    axes[1, 0].scatter(channel_stats["activity_mean"].values,
                       channel_stats["awake_proportion"].values, alpha=0.72, s=16,
                       color=NATURE_PALETTE["dark_grey"], edgecolor="white", linewidth=0.25)
    axes[1, 0].set_title("Activity vs awake fraction", loc="left")
    axes[1, 0].set_xlabel("Activity mean"); axes[1, 0].set_ylabel("Awake fraction")
    axes[1, 1].hist(channel_stats["sleep_proportion"].values, bins=20, color=NATURE_PALETTE["purple"], alpha=0.9,
                    edgecolor="white", linewidth=0.5)
    axes[1, 1].set_title("Sleep proportion per channel", loc="left"); axes[1, 1].set_xlabel("Sleep fraction")
    for ax in axes.ravel():
        apply_nature_style(ax)
    p1_png = os.path.join(plots_dir, "channel_overview.png")
    save_nature_figure(fig, p1_png, dpi=config.plot_dpi)
    plt.close(fig)
    pf = go.Figure()
    pf.add_histogram(x=channel_stats["activity_mean"].values, name="activity_mean", opacity=0.7)
    pf.add_histogram(x=channel_stats["awake_proportion"].values, name="awake_proportion", opacity=0.7)
    pf.add_histogram(x=channel_stats["sleep_proportion"].values, name="sleep_proportion", opacity=0.7)
    nature_plotly_layout(pf, "Channel metric histograms", width=1000, height=480, barmode="overlay")
    p1_html = os.path.join(plots_dir, "channel_overview.html")
    pf.write_html(p1_html, include_plotlyjs="cdn")

    # Plot 2: circadian
    p2_png = os.path.join(plots_dir, "circadian.png")
    p2_html = os.path.join(plots_dir, "circadian.html")
    fig, (ax1, ax2) = plt.subplots(2, 1, figsize=(7.2, 4.8), sharex=True)
    ax1.plot(circadian_df["hour"], circadian_df["activity_mean"], "o-",
             color=NATURE_PALETTE["blue"], markersize=3, linewidth=1.1)
    ax1.set_title("Circadian activity (hourly population mean)", loc="left"); ax1.set_ylabel("Activity")
    ax2.plot(circadian_df["hour"], circadian_df["sleep_mean"], "o-",
             color=NATURE_PALETTE["purple"], markersize=3, linewidth=1.1)
    ax2.set_title("Circadian sleep (hourly population mean)", loc="left"); ax2.set_ylabel("Sleep fraction")
    ax2.set_xlabel("Hour")
    day_boundaries = [d * 24 for d in range(1, max(n_hours // 24, 1))]
    for ax in (ax1, ax2):
        add_day_boundaries(ax, day_boundaries)
        apply_nature_style(ax)
    save_nature_figure(fig, p2_png, dpi=config.plot_dpi)
    plt.close(fig)
    pf = go.Figure()
    pf.add_scatter(x=circadian_df["hour"], y=circadian_df["activity_mean"], mode="lines+markers",
                   name="activity_mean")
    pf.add_scatter(x=circadian_df["hour"], y=circadian_df["sleep_mean"], mode="lines+markers",
                   name="sleep_mean", yaxis="y2")
    nature_plotly_layout(pf, "Circadian aggregates", width=1100, height=500,
                         yaxis={"title": "Activity"},
                         yaxis2={"title": "Sleep fraction", "overlaying": "y", "side": "right"})
    pf.update_xaxes(title="Hour")
    pf.write_html(p2_html, include_plotlyjs="cdn")

    # Plot 3: group comparison (box of awake_proportion across groups)
    p3_png = os.path.join(plots_dir, "group_compare.png")
    p3_html = os.path.join(plots_dir, "group_compare.html")
    if "Group" in md.columns and len(stats_results) > 0:
        cs = channel_stats.copy()
        cs["Group"] = cs.index.map(col_to_group)
        cs = cs.dropna(subset=["Group"])
        fig, ax = plt.subplots(figsize=(6.4, 3.4))
        groups = list(cs.groupby("Group").groups.keys())
        data = [cs.loc[cs["Group"] == g, "awake_proportion"].values for g in groups]
        parts = ax.violinplot(data, positions=np.arange(len(groups)), widths=0.78,
                              showmeans=False, showmedians=False, showextrema=False)
        for i, body in enumerate(parts["bodies"]):
            body.set_facecolor(NATURE_COLORS[i % len(NATURE_COLORS)])
            body.set_edgecolor("white")
            body.set_alpha(0.28)
        bp = ax.boxplot(data, positions=np.arange(len(groups)), widths=0.24, patch_artist=True,
                        showfliers=False, medianprops={"color": NATURE_PALETTE["black"], "linewidth": 1.1},
                        boxprops={"facecolor": "white", "edgecolor": NATURE_PALETTE["dark_grey"], "linewidth": 0.8},
                        whiskerprops={"color": NATURE_PALETTE["dark_grey"], "linewidth": 0.8},
                        capprops={"color": NATURE_PALETTE["dark_grey"], "linewidth": 0.8})
        rng = np.random.default_rng(7)
        for i, vals in enumerate(data):
            jitter = rng.normal(i, 0.045, size=len(vals))
            ax.scatter(jitter, vals, s=12, color=NATURE_COLORS[i % len(NATURE_COLORS)], alpha=0.58,
                       edgecolor="white", linewidth=0.25, zorder=3)
        ax.set_xticks(np.arange(len(groups)))
        ax.set_xticklabels(groups, rotation=20, ha="right")
        ax.set_title("Awake proportion by Group", loc="left"); ax.set_ylabel("Awake fraction")
        ax.set_ylim(-0.02, 1.02)
        apply_nature_style(ax)
        save_nature_figure(fig, p3_png, dpi=config.plot_dpi)
        plt.close(fig)
        pf = go.Figure()
        for i, (g, sub) in enumerate(cs.groupby("Group")):
            pf.add_box(y=sub["awake_proportion"].values, name=str(g), boxpoints="all", jitter=0.35,
                       marker={"color": NATURE_COLORS[i % len(NATURE_COLORS)], "size": 4})
        nature_plotly_layout(pf, "Awake proportion by Group", width=900, height=460)
        pf.update_yaxes(title="Awake fraction")
        pf.write_html(p3_html, include_plotlyjs="cdn")
        plot_paths = [p1_png, p1_html, p2_png, p2_html, p3_png, p3_html]
    else:
        plot_paths = [p1_png, p1_html, p2_png, p2_html]

    schema = write_schema(
        step_dir,
        produced_by=f"step{STEP_NO:02d}_{STEP_NAME}",
        artifacts=[
            {"path": channel_stats_path, "format": "csv", "dataframe": channel_stats,
             "description": "Per-channel: activity_mean/std, awake/sleep proportion"},
            {"path": stats_results_path, "format": "csv", "dataframe": stats_results,
             "description": "Pairwise Mann–Whitney U tests across Groups"},
            {"path": circadian_path, "format": "csv", "dataframe": circadian_df,
             "description": "Hourly population activity_mean and sleep_mean"},
        ] + [{"path": p, "description": os.path.basename(p)} for p in plot_paths],
        extra={"n_pairwise_tests": int(len(stats_results))},
    )

    return {
        "step_dir": step_dir,
        "channel_stats_path": channel_stats_path,
        "stats_results_path": stats_results_path,
        "circadian_path": circadian_path,
        "schema": schema,
        "plots": plot_paths,
    }
