"""Step 6 — Position-frequency distributions, overall and per-day, with optional group breakdown.

Input  : pn_awake.parquet (Step 4), metadata.parquet (Step 2)
Output : freq_all.csv           — position × channel counts (all timepoints)
         freq_per_day.csv       — long-form (day, position, channel, count)
         freq_by_group.csv      — group mean proportion per position
         schema.json
         plots/freq_all.{png,html}, plots/freq_by_group.{png,html}
"""

from __future__ import annotations

import os
from typing import Dict, Optional

import numpy as np
import pandas as pd

from ..config import RMBConfig
from ..manifest import write_schema
from ..processor import RMBDataProcessor
from ..visualizer import RMBVisualizer


STEP_NO = 6
STEP_NAME = "frequency"


def _fly_to_column(monitor: str, channel: int) -> str:
    return f"{monitor}_Ch{channel}"


def run(config: RMBConfig, pn_awake_path: str, metadata_path: str) -> Dict:
    step_dir = config.step_dir(STEP_NO, STEP_NAME)
    plots_dir = os.path.join(step_dir, "plots")
    processor = RMBDataProcessor(position_range=config.position_range)
    viz = RMBVisualizer(minutes_per_day=config.minutes_per_day, dpi=config.plot_dpi)

    pn_awake = pd.read_parquet(pn_awake_path)
    md = pd.read_parquet(metadata_path)

    # Overall frequency
    freq_all = processor.frequency_distribution(pn_awake)
    freq_all_path = os.path.join(step_dir, "freq_all.csv")
    freq_all.to_csv(freq_all_path)

    # Per-day frequency
    rows = []
    n = pn_awake.shape[0]
    n_days = max(n // config.minutes_per_day, 1)
    for day in range(1, n_days + 1):
        s = (day - 1) * config.minutes_per_day
        e = min(day * config.minutes_per_day, n)
        chunk = pn_awake.iloc[s:e]
        day_freq = processor.frequency_distribution(chunk)
        long = day_freq.reset_index().melt(id_vars="index", var_name="channel", value_name="count")
        long.rename(columns={"index": "position"}, inplace=True)
        long.insert(0, "day", day)
        rows.append(long)
    freq_per_day = pd.concat(rows, ignore_index=True) if rows else pd.DataFrame()
    freq_per_day_path = os.path.join(step_dir, "freq_per_day.csv")
    freq_per_day.to_csv(freq_per_day_path, index=False)

    # Group breakdown — only if metadata has Group + Monitor_number + Channel
    group_table = pd.DataFrame()
    freq_by_group_path = os.path.join(step_dir, "freq_by_group.csv")
    if all(c in md.columns for c in ("Group", "Monitor_number", "Channel")):
        # Map fly column name → group
        col_to_group = {}
        for _, r in md.iterrows():
            col = _fly_to_column(str(r["Monitor_number"]), int(r["Channel"]))
            if col in freq_all.columns:
                col_to_group[col] = str(r["Group"])
        totals = freq_all.sum(axis=0).replace(0, np.nan)
        prop = freq_all.div(totals, axis=1).fillna(0)
        per_group_means = {}
        per_group_stds = {}
        for grp in sorted(set(col_to_group.values())):
            cols = [c for c, g in col_to_group.items() if g == grp]
            if cols:
                per_group_means[grp] = prop[cols].mean(axis=1)
                per_group_stds[f"{grp}_std"] = prop[cols].std(axis=1)
        group_table = pd.DataFrame({**per_group_means, **per_group_stds})
        group_table.index.name = "position"
        group_table.to_csv(freq_by_group_path)
    else:
        # Write an empty CSV with header so downstream tooling has something to read
        pd.DataFrame({"position": []}).to_csv(freq_by_group_path, index=False)

    # Plots
    p1_png, p1_html = viz.frequency_barplot(
        freq_all, "Position-frequency distribution (all data)",
        os.path.join(plots_dir, "freq_all"),
    )
    plots = [p1_png, p1_html]
    if not group_table.empty:
        p2_png, p2_html = viz.grouped_bar(
            group_table, "Position frequency by Group",
            os.path.join(plots_dir, "freq_by_group"),
            ylabel="Mean proportion",
        )
        plots.extend([p2_png, p2_html])

    schema = write_schema(
        step_dir,
        produced_by=f"step{STEP_NO:02d}_{STEP_NAME}",
        artifacts=[
            {"path": freq_all_path, "format": "csv", "dataframe": freq_all,
             "description": "Counts per position × channel (all timepoints)"},
            {"path": freq_per_day_path, "format": "csv", "dataframe": freq_per_day,
             "description": "Long-form: day, position, channel, count"},
            {"path": freq_by_group_path, "format": "csv",
             "dataframe": group_table if not group_table.empty else None,
             "description": "Mean proportion per position, columns = group means and *_std"},
        ] + [{"path": p, "description": os.path.basename(p)} for p in plots],
        extra={"position_range": list(config.position_range), "n_days": n_days},
    )

    return {
        "step_dir": step_dir,
        "freq_all_path": freq_all_path,
        "freq_per_day_path": freq_per_day_path,
        "freq_by_group_path": freq_by_group_path,
        "schema": schema,
        "plots": plots,
    }
