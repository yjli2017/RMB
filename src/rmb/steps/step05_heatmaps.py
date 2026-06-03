"""Step 5 — Heatmaps for MT, PN, Sleep, PN-when-Awake.

Input  : mt.parquet, pn.parquet (Step 3), sleep.parquet, pn_awake.parquet (Step 4)
Output : 4 × (PNG + HTML) heatmaps under plots/
         schema.json (no new data artifacts — these are review plots)
"""

from __future__ import annotations

import os
from typing import Dict

import pandas as pd

from ..config import RMBConfig
from ..manifest import write_schema
from ..visualizer import RMBVisualizer


STEP_NO = 5
STEP_NAME = "heatmaps"


def run(config: RMBConfig, mt_path: str, pn_path: str, sleep_path: str, pn_awake_path: str) -> Dict:
    step_dir = config.step_dir(STEP_NO, STEP_NAME)
    plots_dir = os.path.join(step_dir, "plots")
    viz = RMBVisualizer(minutes_per_day=config.minutes_per_day, dpi=config.plot_dpi)

    mt = pd.read_parquet(mt_path)
    pn = pd.read_parquet(pn_path)
    sleep = pd.read_parquet(sleep_path)
    pn_awake = pd.read_parquet(pn_awake_path)

    plots = []
    for name, df, cmap, title in (
        ("mt", mt, "plasma", "Movement (MT)"),
        ("pn", pn, "viridis", "Position (PN)"),
        ("sleep", sleep, "Reds", "Sleep state"),
        ("pn_awake", pn_awake, "coolwarm", "Position when awake"),
    ):
        png, html = viz.heatmap(df, title, os.path.join(plots_dir, f"heatmap_{name}"), cmap=cmap)
        plots.extend([png, html])

    schema = write_schema(
        step_dir,
        produced_by=f"step{STEP_NO:02d}_{STEP_NAME}",
        artifacts=[{"path": p, "description": os.path.basename(p)} for p in plots],
        extra={"note": "Heatmap step writes only review plots — no new data artifacts."},
    )
    return {"step_dir": step_dir, "schema": schema, "plots": plots}
