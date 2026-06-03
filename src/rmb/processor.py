"""Core numerical transforms: parse MT/CT/PN, sleep conversion, frequencies."""

from __future__ import annotations

from typing import Dict, Tuple

import numpy as np
import pandas as pd

from .io import MONITOR_COL_TYPE, MONITOR_COL_DATA_START


class RMBDataProcessor:
    """Transformations on raw monitor frames and derived movement data."""

    def __init__(self, position_range: Tuple[int, int] = (1, 15)) -> None:
        self.position_range = position_range

    def split_record_types(self, raw: pd.DataFrame, monitor: str) -> Dict[str, pd.DataFrame]:
        """Split one monitor's raw rows into MT / CT / PN wide frames.

        Returns dict with keys 'mt', 'ct', 'pn'. Each frame has rows =
        timepoints and columns = 40 channels (numeric, NaN→0).
        """
        type_col = raw.iloc[:, MONITOR_COL_TYPE]
        out: Dict[str, pd.DataFrame] = {}

        for key, marker in [("mt", "MT"), ("ct", "CT"), ("pn", "Pn")]:
            mask = type_col == marker
            sub = raw.loc[mask].iloc[:, MONITOR_COL_DATA_START:].copy()
            sub = sub.apply(pd.to_numeric, errors="coerce").fillna(0)
            sub.columns = [f"{monitor}_Ch{i+1}" for i in range(sub.shape[1])]
            out[key] = sub.reset_index(drop=True)

        return out

    def timestamps_for(self, raw: pd.DataFrame, marker: str = "MT") -> pd.Series:
        """Return a datetime Series for rows where col[7] == marker."""
        mask = raw.iloc[:, MONITOR_COL_TYPE] == marker
        date = raw.loc[mask, 1].astype(str).str.strip()
        time = raw.loc[mask, 2].astype(str).str.strip()
        ts = pd.to_datetime(date + " " + time, format="%d %b %y %H:%M:%S", errors="coerce")
        return ts.reset_index(drop=True)

    @staticmethod
    def movement_to_awake(mt: pd.DataFrame, threshold: int = 5) -> pd.DataFrame:
        """Binary awake mask per channel.

        Runs of zeros shorter than `threshold` are interpreted as awake.
        Any nonzero movement is awake (=1).
        """
        def _col(series: pd.Series) -> pd.Series:
            result = series.copy()
            is_zero = (series == 0)
            groups = (is_zero != is_zero.shift()).cumsum()
            for gid in groups.unique():
                m = (groups == gid)
                if is_zero[m].iloc[0] and m.sum() < threshold:
                    result[m] = 1
            return result

        processed = mt.apply(_col)
        return (processed != 0).astype(np.int8)

    @staticmethod
    def flip_binary(df: pd.DataFrame) -> pd.DataFrame:
        return (1 - df).astype(np.int8)

    def frequency_distribution(self, data: pd.DataFrame) -> pd.DataFrame:
        """Counts of each integer position value per channel."""
        lo, hi = self.position_range
        out = {}
        for col in data.columns:
            vc = data[col].value_counts()
            out[col] = [int(vc.get(pos, 0)) for pos in range(lo, hi + 1)]
        return pd.DataFrame(out, index=list(range(lo, hi + 1)))
