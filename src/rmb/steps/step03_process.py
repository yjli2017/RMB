"""Step 3 — Pivot raw long-form into wide MT / CT / PN matrices.

Input  : raw_monitors.parquet (Step 1)
Output : mt.parquet, ct.parquet, pn.parquet  — wide format
            rows = timepoint (per monitor), columns = "<Monitor>_Ch<N>"
         schema.json
         plots/sample_traces.{png,html}, plots/coverage.{png,html}
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
from ..manifest import write_schema
from ..visualizer import (
    NATURE_COLORS,
    NATURE_PALETTE,
    apply_nature_style,
    nature_plotly_layout,
    save_nature_figure,
)


STEP_NO = 3
STEP_NAME = "process"


def _channel_cols(long_df: pd.DataFrame) -> List[str]:
    return [c for c in long_df.columns if c.startswith("ch") and c[2:].isdigit()]


def _pivot(long_df: pd.DataFrame, marker: str, channel_cols: List[str]) -> pd.DataFrame:
    sub = long_df[long_df["record_type"] == marker]
    pieces = []
    monitors = sorted(sub["monitor"].unique())
    for m in monitors:
        block = sub[sub["monitor"] == m].sort_values("timepoint")
        wide = block[channel_cols].reset_index(drop=True)
        wide.columns = [f"{m}_Ch{i+1}" for i in range(len(channel_cols))]
        pieces.append(wide)
    if not pieces:
        return pd.DataFrame()
    min_len = min(p.shape[0] for p in pieces)
    pieces = [p.iloc[:min_len].reset_index(drop=True) for p in pieces]
    return pd.concat(pieces, axis=1)


def run(config: RMBConfig, raw_path: str) -> Dict:
    step_dir = config.step_dir(STEP_NO, STEP_NAME)
    plots_dir = os.path.join(step_dir, "plots")

    long_df = pd.read_parquet(raw_path)
    channel_cols = _channel_cols(long_df)

    mt = _pivot(long_df, "MT", channel_cols)
    ct = _pivot(long_df, "CT", channel_cols)
    pn = _pivot(long_df, "Pn", channel_cols)

    paths = {}
    for key, df in (("mt", mt), ("ct", ct), ("pn", pn)):
        p = os.path.join(step_dir, f"{key}.parquet")
        df.to_parquet(p, index=False)
        paths[key] = p

    # Plot 1: sample traces — first 4 channels of mt/ct/pn for first day or full
    n_show = min(mt.shape[0], 1440)
    sample_chs = mt.columns[:4]
    fig, axes = plt.subplots(3, 1, figsize=(7.2, 5.2), sharex=True)
    for row_i, (ax, (label, frame)) in enumerate(zip(axes, (("MT", mt), ("CT", ct), ("PN", pn)))):
        for i, c in enumerate(sample_chs):
            if c in frame.columns:
                ax.plot(np.arange(n_show), frame[c].iloc[:n_show].values, linewidth=0.75,
                        label=c, color=NATURE_COLORS[i % len(NATURE_COLORS)], alpha=0.95)
        ax.set_title(label, loc="left"); ax.set_ylabel(label)
        apply_nature_style(ax)
        if row_i == 0:
            ax.legend(fontsize=7, loc="upper center", ncol=4, frameon=False, bbox_to_anchor=(0.5, 1.32))
    axes[-1].set_xlabel("Timepoint (minutes)")
    p1_png = os.path.join(plots_dir, "sample_traces.png")
    save_nature_figure(fig, p1_png, dpi=config.plot_dpi)
    plt.close(fig)

    pf = go.Figure()
    for label, frame in (("MT", mt), ("CT", ct), ("PN", pn)):
        for c in sample_chs:
            if c in frame.columns:
                pf.add_scatter(x=np.arange(n_show), y=frame[c].iloc[:n_show].values,
                               mode="lines", name=f"{label}:{c}")
    nature_plotly_layout(pf, "Sample traces (first 4 channels)", width=1200, height=520)
    pf.update_xaxes(title="Timepoint")
    pf.update_yaxes(title="Value")
    p1_html = os.path.join(plots_dir, "sample_traces.html")
    pf.write_html(p1_html, include_plotlyjs="cdn")

    # Plot 2: coverage — non-NaN fraction per channel for MT
    coverage = (mt.notna() & (mt != 0)).mean()
    fig, ax = plt.subplots(figsize=(7.2, 2.6))
    ax.bar(np.arange(len(coverage)), coverage.values, color=NATURE_PALETTE["bluish_green"], alpha=0.95,
           edgecolor="white", linewidth=0.35)
    ax.set_title("Fraction of nonzero MT values per channel", loc="left")
    ax.set_xlabel("Channel"); ax.set_ylabel("Nonzero fraction")
    apply_nature_style(ax)
    p2_png = os.path.join(plots_dir, "coverage.png")
    save_nature_figure(fig, p2_png, dpi=config.plot_dpi)
    plt.close(fig)
    pf = go.Figure(go.Bar(x=list(coverage.index), y=coverage.values,
                          marker={"color": NATURE_PALETTE["bluish_green"]}))
    nature_plotly_layout(pf, "Fraction of nonzero MT values per channel", width=1200, height=420)
    pf.update_yaxes(title="Nonzero fraction")
    p2_html = os.path.join(plots_dir, "coverage.html")
    pf.write_html(p2_html, include_plotlyjs="cdn")

    schema = write_schema(
        step_dir,
        produced_by=f"step{STEP_NO:02d}_{STEP_NAME}",
        artifacts=[
            {"path": paths["mt"], "format": "parquet", "dataframe": mt,
             "description": "wide MT (movement) — rows=timepoint, cols=<Monitor>_Ch<N>"},
            {"path": paths["ct"], "format": "parquet", "dataframe": ct,
             "description": "wide CT (count) — rows=timepoint, cols=<Monitor>_Ch<N>"},
            {"path": paths["pn"], "format": "parquet", "dataframe": pn,
             "description": "wide PN (position) — rows=timepoint, cols=<Monitor>_Ch<N>"},
            {"path": p1_png, "description": "sample traces (PNG)"},
            {"path": p1_html, "description": "sample traces (HTML)"},
            {"path": p2_png, "description": "channel coverage (PNG)"},
            {"path": p2_html, "description": "channel coverage (HTML)"},
        ],
    )

    return {
        "step_dir": step_dir,
        "mt_path": paths["mt"],
        "ct_path": paths["ct"],
        "pn_path": paths["pn"],
        "schema": schema,
        "plots": [p1_png, p1_html, p2_png, p2_html],
    }
