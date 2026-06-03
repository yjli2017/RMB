"""Step 2 — Load or auto-generate metadata.

Input  : config.metadata_dir/*_metadata.csv (one per monitor).
Output : metadata.parquet — tidy DataFrame, one row per fly (Monitor_number×Channel)
         schema.json
         plots/group_breakdown.{png,html}, plots/missingness.{png,html}
"""

from __future__ import annotations

import os
from typing import Dict, List

import numpy as np
import pandas as pd
import plotly.graph_objects as go
import matplotlib
matplotlib.use("Agg")
import matplotlib.pyplot as plt

from ..config import RMBConfig
from ..io import load_metadata_dir, monitor_name
from ..manifest import write_schema
from ..metadata import RMBMetadataManager
from ..visualizer import (
    NATURE_COLORS,
    NATURE_PALETTE,
    apply_nature_style,
    nature_plotly_layout,
    save_nature_figure,
)


STEP_NO = 2
STEP_NAME = "metadata"


def run(config: RMBConfig, monitor_files: List[str]) -> Dict:
    step_dir = config.step_dir(STEP_NO, STEP_NAME)
    plots_dir = os.path.join(step_dir, "plots")
    mgr = RMBMetadataManager(config.metadata_dir)

    if not mgr.exists():
        mgr.generate_sample(monitor_files)

    md = load_metadata_dir(config.metadata_dir)
    md_path = os.path.join(step_dir, "metadata.parquet")
    # Normalize Monitor_number to monitor stem string (e.g. "Monitor36")
    if "Monitor_number" in md.columns:
        md["Monitor_number"] = md["Monitor_number"].astype(str)
    md.to_parquet(md_path, index=False)

    # Plot 1: counts by Group / Treatment / Sex
    candidates = [c for c in ("Group", "Treatment", "Sex", "Genotype") if c in md.columns]
    fig, axes = plt.subplots(1, max(len(candidates), 1), figsize=(3.0 * max(len(candidates), 1), 3.0))
    if len(candidates) == 1:
        axes = [axes]
    for j, (ax, col) in enumerate(zip(axes, candidates)):
        counts = md[col].fillna("NA").astype(str).value_counts()
        colors = [NATURE_COLORS[(i + j) % len(NATURE_COLORS)] for i in range(len(counts))]
        ax.bar(counts.index, counts.values, color=colors, alpha=0.95, edgecolor="white", linewidth=0.5)
        ax.set_title(f"Flies per {col}", loc="left"); ax.set_ylabel("Count")
        ax.tick_params(axis="x", rotation=25)
        apply_nature_style(ax)
        for i, v in enumerate(counts.values):
            ax.text(i, v + max(counts.values) * 0.025, str(int(v)), ha="center", fontsize=7.5)
    p1_png = os.path.join(plots_dir, "group_breakdown.png")
    save_nature_figure(fig, p1_png, dpi=config.plot_dpi)
    plt.close(fig)

    pf = go.Figure()
    for j, col in enumerate(candidates):
        counts = md[col].fillna("NA").astype(str).value_counts()
        pf.add_bar(name=col, x=[f"{col}:{k}" for k in counts.index], y=counts.values,
                   marker={"color": NATURE_COLORS[j % len(NATURE_COLORS)]})
    nature_plotly_layout(pf, "Flies per category", width=900, height=460, barmode="group")
    pf.update_yaxes(title="Count")
    p1_html = os.path.join(plots_dir, "group_breakdown.html")
    pf.write_html(p1_html, include_plotlyjs="cdn")

    # Plot 2: missingness per column
    miss = md.isna().mean().sort_values(ascending=False)
    fig, ax = plt.subplots(figsize=(7.2, 3.0))
    ax.bar(miss.index, miss.values * 100, color=NATURE_PALETTE["vermillion"], alpha=0.9,
           edgecolor="white", linewidth=0.5)
    ax.set_title("Missingness per column (%)", loc="left")
    ax.set_ylabel("% missing"); ax.tick_params(axis="x", rotation=45)
    apply_nature_style(ax)
    p2_png = os.path.join(plots_dir, "missingness.png")
    save_nature_figure(fig, p2_png, dpi=config.plot_dpi)
    plt.close(fig)
    pf = go.Figure(go.Bar(x=miss.index, y=miss.values * 100,
                          marker={"color": NATURE_PALETTE["vermillion"]}))
    nature_plotly_layout(pf, "Missingness per column (%)", width=900, height=420)
    pf.update_yaxes(title="% missing")
    p2_html = os.path.join(plots_dir, "missingness.html")
    pf.write_html(p2_html, include_plotlyjs="cdn")

    schema = write_schema(
        step_dir,
        produced_by=f"step{STEP_NO:02d}_{STEP_NAME}",
        artifacts=[
            {"path": md_path, "format": "parquet", "dataframe": md,
             "description": "tidy metadata (one row per fly: Monitor_number×Channel)"},
            {"path": p1_png, "description": "group breakdown (PNG)"},
            {"path": p1_html, "description": "group breakdown (HTML)"},
            {"path": p2_png, "description": "missingness per column (PNG)"},
            {"path": p2_html, "description": "missingness per column (HTML)"},
        ],
        extra={"n_flies": int(len(md)),
               "monitors_in_metadata": sorted(md["Monitor_number"].unique().tolist()) if "Monitor_number" in md.columns else []},
    )

    return {
        "step_dir": step_dir,
        "metadata_path": md_path,
        "metadata": md,
        "schema": schema,
        "plots": [p1_png, p1_html, p2_png, p2_html],
    }
