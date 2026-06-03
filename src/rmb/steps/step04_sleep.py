"""Step 4 — Sleep / awake state derivation.

Input  : mt.parquet (from Step 3), config.sleep_threshold
Output : awake.parquet     — int8 (0/1) wide
         sleep.parquet     — int8 (0/1) wide, inverse of awake
         pn_awake.parquet  — pn × awake (position only when awake)
         schema.json
         plots/population_sleep.{png,html}, plots/sleep_raster.{png,html}
"""

from __future__ import annotations

import os
from typing import Dict

import numpy as np
import pandas as pd
import plotly.graph_objects as go
import matplotlib
matplotlib.use("Agg")
import matplotlib.pyplot as plt

from ..config import RMBConfig
from ..manifest import write_schema
from ..processor import RMBDataProcessor
from ..visualizer import (
    NATURE_PALETTE,
    add_day_boundaries,
    add_light_dark_bands,
    apply_nature_style,
    nature_plotly_layout,
    panel_label,
    plotly_colorscale,
    save_nature_figure,
    style_colorbar,
)


STEP_NO = 4
STEP_NAME = "sleep"


def run(config: RMBConfig, mt_path: str, pn_path: str) -> Dict:
    step_dir = config.step_dir(STEP_NO, STEP_NAME)
    plots_dir = os.path.join(step_dir, "plots")

    mt = pd.read_parquet(mt_path)
    pn = pd.read_parquet(pn_path)

    processor = RMBDataProcessor(position_range=config.position_range)
    awake = processor.movement_to_awake(mt, threshold=config.sleep_threshold)
    sleep = processor.flip_binary(awake)
    pn_awake = (pn * awake).astype(np.int16)

    awake_path = os.path.join(step_dir, "awake.parquet")
    sleep_path = os.path.join(step_dir, "sleep.parquet")
    pn_awake_path = os.path.join(step_dir, "pn_awake.parquet")
    awake.to_parquet(awake_path, index=False)
    sleep.to_parquet(sleep_path, index=False)
    pn_awake.to_parquet(pn_awake_path, index=False)

    # Plot 1: population sleep fraction over time
    pop_sleep = sleep.mean(axis=1).values
    pop_sem = sleep.sem(axis=1).fillna(0).values if sleep.shape[1] > 1 else np.zeros_like(pop_sleep)
    x = np.arange(len(pop_sleep))
    boundaries = list(range(config.minutes_per_day, len(pop_sleep), config.minutes_per_day))
    fig, ax = plt.subplots(figsize=(7.2, 2.9))
    add_light_dark_bands(ax, len(pop_sleep), config.minutes_per_day)
    ax.fill_between(x, pop_sleep - pop_sem, pop_sleep + pop_sem,
                    color=NATURE_PALETTE["slate"], alpha=0.22, lw=0)
    ax.plot(x, pop_sleep, color=NATURE_PALETTE["navy"], linewidth=1.3)
    add_day_boundaries(ax, boundaries)
    ax.set_title("Population sleep fraction over time", loc="left")
    ax.set_xlabel("Time (minutes)"); ax.set_ylabel("Sleep fraction")
    ax.set_ylim(0, 1)
    apply_nature_style(ax)
    p1_png = os.path.join(plots_dir, "population_sleep.png")
    save_nature_figure(fig, p1_png, dpi=config.plot_dpi)
    plt.close(fig)
    pf = go.Figure()
    pf.add_scatter(
        x=np.concatenate([x, x[::-1]]),
        y=np.concatenate([pop_sleep + pop_sem, (pop_sleep - pop_sem)[::-1]]),
        fill="toself", fillcolor="rgba(132,145,180,0.22)",
        line={"color": "rgba(0,0,0,0)"}, hoverinfo="skip", showlegend=False,
    )
    pf.add_scatter(x=x, y=pop_sleep, mode="lines", name="Sleep fraction",
                   line={"color": NATURE_PALETTE["navy"], "width": 2.2})
    for b in boundaries:
        pf.add_vline(x=b, line_color=NATURE_PALETTE["mid_grey"],
                     line_dash="dash", opacity=0.45)
    nature_plotly_layout(pf, "Population sleep fraction over time",
                         width=1200, height=420)
    pf.update_yaxes(title="Sleep fraction", range=[0, 1])
    pf.update_xaxes(title="Time (minutes)")
    p1_html = os.path.join(plots_dir, "population_sleep.html")
    pf.write_html(p1_html, include_plotlyjs="cdn")

    # Plot 2: sleep raster (channels × time)
    p2_png = os.path.join(plots_dir, "sleep_raster.png")
    p2_html = os.path.join(plots_dir, "sleep_raster.html")
    fig, ax = plt.subplots(figsize=(7.2, 3.9))
    im = ax.imshow(sleep.values.T, aspect="auto", cmap="rmb_sleep",
                   interpolation="nearest", rasterized=True, vmin=0, vmax=1)
    add_day_boundaries(ax, boundaries, color=NATURE_PALETTE["red"], alpha=0.55)
    ax.set_title(f"Sleep raster ({sleep.shape[0]:,} × {sleep.shape[1]:,})", loc="left")
    ax.set_xlabel("Time (minutes)"); ax.set_ylabel("Channel")
    apply_nature_style(ax, grid="")
    cb = plt.colorbar(im, ax=ax, fraction=0.025, pad=0.015, aspect=26,
                      ticks=[0, 1])
    cb.ax.set_yticklabels(["awake", "asleep"])
    style_colorbar(cb, label="State")
    save_nature_figure(fig, p2_png, dpi=config.plot_dpi)
    plt.close(fig)
    pf = go.Figure(go.Heatmap(
        z=sleep.values.T, zmin=0, zmax=1,
        colorscale=plotly_colorscale("rmb_sleep"),
        colorbar={"outlinewidth": 0, "thickness": 14, "len": 0.85,
                  "tickvals": [0, 1], "ticktext": ["awake", "asleep"],
                  "title": {"text": "State", "side": "right"}},
    ))
    nature_plotly_layout(pf, "Sleep raster", width=1200, height=520)
    pf.update_xaxes(title="Time (minutes)")
    pf.update_yaxes(title="Channel")
    pf.write_html(p2_html, include_plotlyjs="cdn")

    schema = write_schema(
        step_dir,
        produced_by=f"step{STEP_NO:02d}_{STEP_NAME}",
        artifacts=[
            {"path": awake_path, "format": "parquet", "dataframe": awake,
             "description": "binary awake mask (1=awake, 0=asleep) — int8 wide"},
            {"path": sleep_path, "format": "parquet", "dataframe": sleep,
             "description": "binary sleep mask (1=asleep, 0=awake) — int8 wide"},
            {"path": pn_awake_path, "format": "parquet", "dataframe": pn_awake,
             "description": "position values zeroed where the fly is asleep"},
            {"path": p1_png, "description": "population sleep over time (PNG)"},
            {"path": p1_html, "description": "population sleep over time (HTML)"},
            {"path": p2_png, "description": "per-channel sleep raster (PNG)"},
            {"path": p2_html, "description": "per-channel sleep raster (HTML)"},
        ],
        extra={"sleep_threshold": int(config.sleep_threshold),
               "awake_fraction": float(awake.values.mean()),
               "sleep_fraction": float(sleep.values.mean())},
    )

    return {
        "step_dir": step_dir,
        "awake_path": awake_path,
        "sleep_path": sleep_path,
        "pn_awake_path": pn_awake_path,
        "schema": schema,
        "plots": [p1_png, p1_html, p2_png, p2_html],
    }
