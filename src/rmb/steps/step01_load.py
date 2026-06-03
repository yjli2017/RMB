"""Step 1 — Load raw Monitor*.txt files.

Input  : config.data_dir containing Monitor*.txt (TAB-separated, no header).
Output : raw_monitors.parquet  — long-format
            columns = [monitor, timepoint, record_type, ch01..ch40]
         per_monitor_counts.csv — rows per record type per monitor
         schema.json
         plots/row_counts.{png,html}, plots/sample_raw_heatmap.{png,html}
"""

from __future__ import annotations

import os
from pathlib import Path
from typing import Dict

import numpy as np
import pandas as pd
import plotly.graph_objects as go
import matplotlib
matplotlib.use("Agg")
import matplotlib.pyplot as plt

from ..config import RMBConfig
from ..io import (
    discover_monitor_files,
    monitor_name,
    read_monitor_file,
    MONITOR_COL_TYPE,
    MONITOR_COL_DATA_START,
)
from ..manifest import write_schema
from ..visualizer import (
    NATURE_COLORS,
    NATURE_PALETTE,
    apply_nature_style,
    nature_plotly_layout,
    save_nature_figure,
)


STEP_NO = 1
STEP_NAME = "load"


def run(config: RMBConfig) -> Dict:
    step_dir = config.step_dir(STEP_NO, STEP_NAME)
    plots_dir = os.path.join(step_dir, "plots")

    files = discover_monitor_files(config.data_dir)
    if not files:
        raise FileNotFoundError(f"No Monitor*.txt files found in {config.data_dir}")

    long_frames = []
    counts = []
    max_channels = 0
    for fp in files:
        name = monitor_name(fp)
        raw = read_monitor_file(fp)
        type_col = raw.iloc[:, MONITOR_COL_TYPE].astype(str)
        n_channels = raw.shape[1] - MONITOR_COL_DATA_START
        max_channels = max(max_channels, n_channels)
        for marker in ("MT", "CT", "Pn"):
            sub = raw.loc[type_col == marker]
            counts.append({"monitor": name, "record_type": marker, "rows": int(len(sub))})
            if sub.empty:
                continue
            data = sub.iloc[:, MONITOR_COL_DATA_START:].copy()
            data = data.apply(pd.to_numeric, errors="coerce").fillna(0)
            data.columns = [f"ch{i+1:02d}" for i in range(data.shape[1])]
            data.insert(0, "monitor", name)
            data.insert(1, "record_type", marker)
            data.insert(2, "timepoint", np.arange(len(data)))
            long_frames.append(data)

    # Right-pad each long frame to max_channels so concat does not introduce NaN-from-misalignment
    target_cols = ["monitor", "record_type", "timepoint"] + [f"ch{i+1:02d}" for i in range(max_channels)]
    long_frames = [
        f.reindex(columns=target_cols, fill_value=0) for f in long_frames
    ]
    long_df = pd.concat(long_frames, ignore_index=True)
    counts_df = pd.DataFrame(counts)

    raw_path = os.path.join(step_dir, "raw_monitors.parquet")
    counts_path = os.path.join(step_dir, "per_monitor_counts.csv")
    long_df.to_parquet(raw_path, index=False)
    counts_df.to_csv(counts_path, index=False)

    # Plot 1: row counts per monitor × record type
    pivot = counts_df.pivot(index="monitor", columns="record_type", values="rows").fillna(0)
    fig, ax = plt.subplots(figsize=(6.8, 3.2))
    pivot.plot(kind="bar", ax=ax, color=NATURE_COLORS[:len(pivot.columns)], width=0.74,
               edgecolor="white", linewidth=0.5)
    ax.set_title("Rows per monitor × record type", loc="left")
    ax.set_xlabel("Monitor")
    ax.set_ylabel("Row count")
    ax.legend(frameon=False, title="Record type", ncol=len(pivot.columns), loc="upper center",
              bbox_to_anchor=(0.5, 1.20))
    ax.tick_params(axis="x", rotation=0)
    apply_nature_style(ax)
    p1_png = os.path.join(plots_dir, "row_counts.png")
    save_nature_figure(fig, p1_png, dpi=config.plot_dpi)
    plt.close(fig)

    pf = go.Figure()
    for i, col in enumerate(pivot.columns):
        pf.add_bar(name=col, x=pivot.index.astype(str), y=pivot[col].values,
                   marker={"color": NATURE_COLORS[i % len(NATURE_COLORS)]})
    nature_plotly_layout(pf, "Rows per monitor × record type", width=900, height=460,
                         barmode="group")
    pf.update_yaxes(title="Row count")
    p1_html = os.path.join(plots_dir, "row_counts.html")
    pf.write_html(p1_html, include_plotlyjs="cdn")

    # Plot 2: sample raw MT heatmap (first monitor's MT rows)
    first_monitor = long_df["monitor"].iloc[0]
    sample = long_df[(long_df["monitor"] == first_monitor) & (long_df["record_type"] == "MT")]
    sample_mat = sample.iloc[:, 3:].values  # ch01..chN
    p2_png = os.path.join(plots_dir, "sample_raw_heatmap.png")
    p2_html = os.path.join(plots_dir, "sample_raw_heatmap.html")
    fig, ax = plt.subplots(figsize=(7.2, 3.2))
    im = ax.imshow(sample_mat.T, aspect="auto", cmap="magma", interpolation="nearest", rasterized=True)
    ax.set_title(f"Raw MT activity — {first_monitor} ({sample_mat.shape[0]:,} × {sample_mat.shape[1]:,})", loc="left")
    ax.set_xlabel("Timepoint"); ax.set_ylabel("Channel")
    apply_nature_style(ax, grid="")
    cb = plt.colorbar(im, ax=ax, fraction=0.025, pad=0.015)
    cb.outline.set_visible(False)
    save_nature_figure(fig, p2_png, dpi=config.plot_dpi)
    plt.close(fig)
    pf = go.Figure(go.Heatmap(z=sample_mat.T, colorscale="Magma"))
    nature_plotly_layout(pf, f"Raw MT activity — {first_monitor}", width=1100, height=500)
    pf.update_xaxes(title="Timepoint")
    pf.update_yaxes(title="Channel")
    pf.write_html(p2_html, include_plotlyjs="cdn")

    schema_path = write_schema(
        step_dir,
        produced_by=f"step{STEP_NO:02d}_{STEP_NAME}",
        artifacts=[
            {"path": raw_path, "format": "parquet", "dataframe": long_df,
             "description": "long-form raw monitor data (one row per monitor×record_type×timepoint, 40 channels)"},
            {"path": counts_path, "format": "csv", "dataframe": counts_df,
             "description": "rows per monitor × record type"},
            {"path": p1_png, "description": "row counts bar chart (PNG)"},
            {"path": p1_html, "description": "row counts bar chart (HTML)"},
            {"path": p2_png, "description": "sample raw MT heatmap (PNG)"},
            {"path": p2_html, "description": "sample raw MT heatmap (HTML)"},
        ],
        extra={"monitor_files": [str(Path(f)) for f in files], "n_monitors": len(files)},
    )

    return {
        "step_dir": step_dir,
        "raw_path": raw_path,
        "counts_path": counts_path,
        "monitor_files": files,
        "schema": schema_path,
        "plots": [p1_png, p1_html, p2_png, p2_html],
    }
