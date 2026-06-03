"""Automatic light/dark day-window detection helpers for RMB data.

The raw monitor files include wall-clock timestamps but not an explicit protocol
start row. These helpers infer a day/night cycle from timestamps and activity,
then select complete LD cycles so downstream statistics are not diluted by
partial first/last days.
"""

from __future__ import annotations

from dataclasses import asdict, dataclass
from typing import Optional, Tuple

import numpy as np
import pandas as pd


@dataclass
class AnalysisWindow:
    """Detected complete-day analysis window in row coordinates."""

    selected_day_start: int
    selected_day_end: int
    start_index: int
    end_index: int
    n_rows_selected: int
    cycle_start_minute_of_day: int
    day_to_night_minute_of_day: int
    detection_method: str
    first_timestamp: Optional[str]
    last_timestamp: Optional[str]
    selected_start_timestamp: Optional[str]
    selected_end_timestamp: Optional[str]
    available_full_days: int

    def to_dict(self) -> dict:
        return asdict(self)


def _minute_of_day(ts: pd.Series) -> pd.Series:
    return ts.dt.hour * 60 + ts.dt.minute


def _circular_window_mean(values: np.ndarray, center: int, width: int, before: bool) -> float:
    if before:
        idx = (center - np.arange(1, width + 1)) % len(values)
    else:
        idx = (center + np.arange(width)) % len(values)
    return float(np.nanmean(values[idx]))


def detect_day_to_night_minute(
    timestamps: pd.Series,
    activity: pd.DataFrame,
    minutes_per_day: int = 1440,
    light_status: Optional[pd.Series] = None,
) -> Tuple[int, str]:
    """Infer the clock minute where day changes to night.

    Primary method: use the monitor light-status channel when present. In the
    RMB files observed here, 1→0 transitions mark day-to-night/lights-off and
    0→1 transitions mark night-to-day/lights-on. Fallback: aggregate population
    activity by clock minute over all days, smooth it, then choose the strongest
    sustained before/after transition. Final fallback: round the first timestamp
    to the nearest hour.
    """
    ts = pd.to_datetime(timestamps, errors="coerce")
    valid = ts.notna()

    if light_status is not None and valid.any():
        light = pd.to_numeric(light_status, errors="coerce").reset_index(drop=True)
        light_valid = valid.reset_index(drop=True) & light.notna()
        if light_valid.sum() > 1:
            ts_l = ts.reset_index(drop=True)[light_valid]
            light_l = light[light_valid]
            prev = light_l.shift()
            # Prefer 1→0 as day-to-night. If the sensor is inverted or uses other
            # numeric levels, fall back to the most frequent transition minute.
            transition = light_l.ne(prev) & prev.notna()
            day_to_night = transition & (prev > light_l)
            chosen = ts_l[day_to_night]
            method = "light_status_1_to_0_transition"
            if chosen.empty:
                chosen = ts_l[transition]
                method = "light_status_any_transition_fallback"
            if not chosen.empty:
                minutes = (chosen.dt.hour * 60 + chosen.dt.minute).astype(int)
                minute = int(minutes.mode().iloc[0]) % minutes_per_day
                return minute, method

    if valid.sum() < max(60, minutes_per_day // 4) or activity.empty:
        if valid.any():
            first = ts[valid].iloc[0]
            minute = int(round((first.hour * 60 + first.minute) / 60.0) * 60) % minutes_per_day
            return minute, "timestamp_rounded_start_fallback"
        return minutes_per_day // 2, "default_12h_fallback"

    pop = activity.loc[valid].apply(pd.to_numeric, errors="coerce").mean(axis=1)
    clock_minute = _minute_of_day(ts[valid])
    by_minute = (
        pd.DataFrame({"minute": clock_minute.values, "activity": pop.values})
        .groupby("minute")["activity"]
        .mean()
        .reindex(range(minutes_per_day))
        .interpolate(limit_direction="both")
        .bfill()
        .ffill()
    )

    if by_minute.isna().all() or float(by_minute.std(skipna=True)) == 0.0:
        first = ts[valid].iloc[0]
        minute = int(round((first.hour * 60 + first.minute) / 60.0) * 60) % minutes_per_day
        return minute, "timestamp_rounded_start_fallback"

    # Smooth and look for a sustained step change over 2-hour windows.
    smooth = by_minute.rolling(60, center=True, min_periods=1).mean().to_numpy(dtype=float)
    scores = np.zeros(minutes_per_day, dtype=float)
    for minute in range(minutes_per_day):
        before = _circular_window_mean(smooth, minute, width=120, before=True)
        after = _circular_window_mean(smooth, minute, width=120, before=False)
        scores[minute] = abs(after - before)

    best = int(np.nanargmax(scores))
    # Snap to nearest 15 min; LD transitions are typically scheduled rather than arbitrary.
    best = int(round(best / 15.0) * 15) % minutes_per_day
    return best, "activity_changepoint_by_clock_minute"


def select_full_day_window(
    timestamps: pd.Series,
    activity: pd.DataFrame,
    *,
    minutes_per_day: int = 1440,
    selected_day_start: int = 2,
    selected_day_end: int = 4,
    light_status: Optional[pd.Series] = None,
) -> AnalysisWindow:
    """Return row window for selected complete LD days.

    The detected day-to-night clock time is interpreted as lights-off. A full
    "day-to-night" cycle starts 12 hours earlier, so selected data start with
    day/light phase and continue into night/dark phase.
    """
    if selected_day_start < 1 or selected_day_end < selected_day_start:
        raise ValueError("selected_day_start/end must be a 1-based inclusive range")

    ts = pd.to_datetime(timestamps, errors="coerce").reset_index(drop=True)
    if ts.notna().sum() < minutes_per_day:
        n = len(ts)
        end = min(n, selected_day_end * minutes_per_day)
        start = min(n, max(0, (selected_day_start - 1) * minutes_per_day))
        return AnalysisWindow(
            selected_day_start, selected_day_end, start, end, end - start,
            0, minutes_per_day // 2, "row_count_fallback_no_timestamps",
            None, None, None, None, max(n // minutes_per_day, 0),
        )

    day_to_night, method = detect_day_to_night_minute(ts, activity, minutes_per_day, light_status=light_status)
    cycle_start = (day_to_night - minutes_per_day // 2) % minutes_per_day

    first = ts.dropna().iloc[0]
    last = ts.dropna().iloc[-1]
    clock = _minute_of_day(ts)
    is_cycle_boundary = (clock == cycle_start) & ts.notna()
    boundary_idx = np.flatnonzero(is_cycle_boundary.to_numpy())

    complete_starts = []
    for idx in boundary_idx:
        end_idx = idx + minutes_per_day
        if end_idx <= len(ts) and ts.iloc[end_idx - 1] - ts.iloc[idx] >= pd.Timedelta(minutes=minutes_per_day - 1):
            complete_starts.append(int(idx))

    # If exact minute is absent because data start/stop off-minute, choose the
    # first observed row after each clock boundary and require 24h of data.
    if not complete_starts:
        dates = pd.date_range(first.normalize() - pd.Timedelta(days=1), last.normalize() + pd.Timedelta(days=1), freq="D")
        target_times = [d + pd.Timedelta(minutes=int(cycle_start)) for d in dates]
        for target in target_times:
            candidates = np.flatnonzero((ts >= target).to_numpy())
            if len(candidates) == 0:
                continue
            idx = int(candidates[0])
            end_idx = idx + minutes_per_day
            if end_idx <= len(ts) and ts.iloc[end_idx - 1] - ts.iloc[idx] >= pd.Timedelta(minutes=minutes_per_day - 1):
                if not complete_starts or idx != complete_starts[-1]:
                    complete_starts.append(idx)
        method += "+nearest_boundary_rows"

    if len(complete_starts) < selected_day_end:
        # Last-resort row-count fallback, still skips partial first rows by using
        # only complete 1440-row chunks after the first available complete chunk.
        max_days = len(ts) // minutes_per_day
        start = min(len(ts), max(0, (selected_day_start - 1) * minutes_per_day))
        end = min(len(ts), selected_day_end * minutes_per_day, max_days * minutes_per_day)
        method += "+row_count_fallback_insufficient_full_boundaries"
        available = max_days
    else:
        start = complete_starts[selected_day_start - 1]
        end = complete_starts[selected_day_end - 1] + minutes_per_day
        available = len(complete_starts)

    start_ts = ts.iloc[start] if start < len(ts) else pd.NaT
    end_ts = ts.iloc[end - 1] if end > 0 and end - 1 < len(ts) else pd.NaT
    return AnalysisWindow(
        selected_day_start=selected_day_start,
        selected_day_end=selected_day_end,
        start_index=int(start),
        end_index=int(end),
        n_rows_selected=int(max(0, end - start)),
        cycle_start_minute_of_day=int(cycle_start),
        day_to_night_minute_of_day=int(day_to_night),
        detection_method=method,
        first_timestamp=str(first),
        last_timestamp=str(last),
        selected_start_timestamp=None if pd.isna(start_ts) else str(start_ts),
        selected_end_timestamp=None if pd.isna(end_ts) else str(end_ts),
        available_full_days=int(available),
    )
