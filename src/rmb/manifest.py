"""Schema manifest writer.

Each step writes one `schema.json` next to its artifact(s) describing the
format, shape, dtypes, and basic summary stats so a reader can understand
the output without loading the data.
"""

from __future__ import annotations

import json
import os
import time
from pathlib import Path
from typing import Any, Iterable, List, Optional

import numpy as np
import pandas as pd


def _df_summary(df: pd.DataFrame) -> dict:
    numeric = df.select_dtypes(include=[np.number])
    summary: dict = {
        "rows": int(df.shape[0]),
        "cols": int(df.shape[1]),
        "nan_total": int(df.isna().sum().sum()),
    }
    if not numeric.empty:
        summary["numeric_min"] = float(numeric.min().min())
        summary["numeric_max"] = float(numeric.max().max())
        summary["numeric_mean"] = float(numeric.mean().mean())
    return summary


def _df_columns(df: pd.DataFrame, max_listed: int = 32) -> list:
    cols = []
    for c in list(df.columns)[:max_listed]:
        cols.append({"name": str(c), "dtype": str(df[c].dtype)})
    if df.shape[1] > max_listed:
        cols.append({"truncated": df.shape[1] - max_listed})
    return cols


def write_schema(
    step_dir: str,
    produced_by: str,
    artifacts: Iterable[dict],
    extra: Optional[dict] = None,
) -> str:
    """Write `<step_dir>/schema.json`.

    `artifacts` is an iterable of dicts. For each you can pass:
        - path (str)            : absolute artifact path (required)
        - format (str)          : file format hint (e.g. "parquet", "csv")
        - dataframe (pd.DataFrame): used to populate rows/cols/dtypes/summary
        - description (str)
    """
    out_path = os.path.join(step_dir, "schema.json")
    serialized: List[dict] = []

    for a in artifacts:
        entry: dict = {
            "path": str(a["path"]),
            "exists": os.path.exists(a["path"]),
            "size_bytes": os.path.getsize(a["path"]) if os.path.exists(a["path"]) else None,
            "format": a.get("format", Path(a["path"]).suffix.lstrip(".") or "unknown"),
        }
        if "description" in a:
            entry["description"] = a["description"]

        df = a.get("dataframe")
        if isinstance(df, pd.DataFrame):
            entry["shape"] = list(df.shape)
            entry["columns"] = _df_columns(df)
            entry["summary"] = _df_summary(df)
            if isinstance(df.index, pd.DatetimeIndex):
                entry["index"] = {"name": df.index.name or "timestamp",
                                  "dtype": "datetime64[ns]",
                                  "start": str(df.index.min()),
                                  "end": str(df.index.max())}
        serialized.append(entry)

    manifest = {
        "produced_by": produced_by,
        "produced_at": time.strftime("%Y-%m-%dT%H:%M:%SZ", time.gmtime()),
        "step_dir": step_dir,
        "artifacts": serialized,
    }
    if extra:
        manifest["extra"] = extra

    with open(out_path, "w") as f:
        json.dump(manifest, f, indent=2, default=str)
    return out_path
