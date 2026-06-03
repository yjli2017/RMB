"""Reading Monitor*.txt files produced by TriKinetics DAM/MB systems.

Monitor file layout (tab-separated, no header):
    col 0 : sequential index (e.g. minute counter)
    col 1 : date string ("6 Jun 23")
    col 2 : time string ("18:19:00")
    col 3 : monitor status flag
    col 4 : monitor extras flag
    col 5 : monitor number (int)
    col 6 : light status / extras
    col 7 : record type, one of {"MT", "CT", "Pn"}
    col 8 : unused
    col 9 : light sensor reading (0/1)
    col 10..49 : 40 channel values
"""

from __future__ import annotations

import glob
import os
from pathlib import Path
from typing import List, Optional

import pandas as pd


MONITOR_COL_TYPE = 7
MONITOR_COL_DATA_START = 10
# Real-world TriKinetics files vary in channel count (32 for MB, 40 for older DAM).
# Pipeline detects channel count at load time rather than assuming a fixed value.


def discover_monitor_files(data_dir: str) -> List[str]:
    """Return sorted Monitor*.txt paths, excluding MonitorLC."""
    pattern = os.path.join(data_dir, "Monitor*.txt")
    files = [f for f in glob.glob(pattern) if "MonitorLC" not in f]
    return sorted(files)


def monitor_name(path: str) -> str:
    return Path(path).stem


def read_monitor_file(path: str) -> pd.DataFrame:
    """Read one Monitor*.txt as a raw DataFrame (no header)."""
    return pd.read_csv(path, sep="\t", header=None, low_memory=False)


def load_metadata_dir(metadata_dir: str) -> pd.DataFrame:
    """Concatenate all *_metadata.csv into one tidy DataFrame.

    Each row = one fly (Monitor_number, Channel). Returns empty DataFrame
    if no metadata files are present.
    """
    files = sorted(glob.glob(os.path.join(metadata_dir, "*_metadata.csv")))
    if not files:
        return pd.DataFrame()

    frames = []
    for f in files:
        df = pd.read_csv(f)
        df["__source_file"] = os.path.basename(f)
        frames.append(df)
    return pd.concat(frames, ignore_index=True)
