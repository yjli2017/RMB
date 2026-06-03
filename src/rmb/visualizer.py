"""Matplotlib (PNG) and Plotly (HTML) plotting helpers.

The visual language is intentionally close to a Nature-style figure:
clean white background, thin axes, colorblind-safe Okabe-Ito colors,
high-DPI export, restrained typography, direct panel structure, and subtle
light/dark/day cues instead of heavy default grid styling.
"""

from __future__ import annotations

from pathlib import Path
from typing import Iterable, List, Optional, Tuple

import matplotlib
matplotlib.use("Agg")  # non-interactive backend; safe inside CLI/pipeline
import matplotlib.pyplot as plt
import numpy as np
import pandas as pd
import plotly.graph_objects as go


# Okabe-Ito / Nature-friendly colorblind-safe palette.
NATURE_PALETTE = {
    "black": "#000000",
    "orange": "#E69F00",
    "sky": "#56B4E9",
    "bluish_green": "#009E73",
    "yellow": "#F0E442",
    "blue": "#0072B2",
    "vermillion": "#D55E00",
    "purple": "#CC79A7",
    "grey": "#6E6E6E",
    "light_grey": "#EAEAEA",
    "dark_grey": "#333333",
}
NATURE_COLORS = [
    NATURE_PALETTE["blue"],
    NATURE_PALETTE["vermillion"],
    NATURE_PALETTE["bluish_green"],
    NATURE_PALETTE["purple"],
    NATURE_PALETTE["orange"],
    NATURE_PALETTE["sky"],
    NATURE_PALETTE["grey"],
]


def set_nature_rcparams() -> None:
    """Set conservative, publication-oriented matplotlib defaults."""
    plt.rcParams.update({
        "font.family": "DejaVu Sans",
        "font.size": 8.5,
        "axes.titlesize": 10,
        "axes.labelsize": 9,
        "xtick.labelsize": 7.5,
        "ytick.labelsize": 7.5,
        "legend.fontsize": 7.5,
        "figure.titlesize": 11,
        "axes.linewidth": 0.8,
        "xtick.major.width": 0.7,
        "ytick.major.width": 0.7,
        "xtick.major.size": 3.0,
        "ytick.major.size": 3.0,
        "savefig.dpi": 300,
        "savefig.transparent": False,
        "pdf.fonttype": 42,
        "ps.fonttype": 42,
    })


def apply_nature_style(ax, *, grid: str = "y"):
    """Apply clean Nature-like axis styling to one matplotlib Axes."""
    ax.set_facecolor("white")
    ax.spines["top"].set_visible(False)
    ax.spines["right"].set_visible(False)
    ax.spines["left"].set_color(NATURE_PALETTE["dark_grey"])
    ax.spines["bottom"].set_color(NATURE_PALETTE["dark_grey"])
    ax.tick_params(colors=NATURE_PALETTE["dark_grey"], direction="out", length=3, width=0.7)
    ax.title.set_fontweight("bold")
    ax.xaxis.label.set_color(NATURE_PALETTE["dark_grey"])
    ax.yaxis.label.set_color(NATURE_PALETTE["dark_grey"])
    if grid:
        ax.grid(True, axis=grid, color=NATURE_PALETTE["light_grey"], linewidth=0.65, alpha=0.9)
        ax.set_axisbelow(True)
    return ax


def add_day_boundaries(ax, boundaries: Iterable[int], *, color: str = "#BDBDBD", alpha: float = 0.75) -> None:
    """Draw subtle day separators."""
    for b in boundaries:
        ax.axvline(b, color=color, linestyle="--", linewidth=0.65, alpha=alpha, zorder=1)


def add_light_dark_bands(ax, n_points: int, minutes_per_day: int = 1440, *, lights_on: int = 720) -> None:
    """Add subtle alternating dark-phase background bands for circadian plots."""
    if n_points <= 0:
        return
    for start in range(0, n_points, minutes_per_day):
        dark_start = start + lights_on
        dark_end = min(start + minutes_per_day, n_points)
        if dark_start < n_points:
            ax.axvspan(dark_start, dark_end, color="#F3F3F3", alpha=0.8, lw=0, zorder=0)


def save_nature_figure(fig, path: str, *, dpi: int = 300) -> str:
    """Save a high-resolution, tightly cropped PNG."""
    Path(path).parent.mkdir(parents=True, exist_ok=True)
    try:
        fig.tight_layout(pad=0.45)
    except Exception:
        # Figures using constrained_layout or complex colorbar layouts may not need/allow tight_layout.
        pass
    fig.savefig(path, dpi=max(int(dpi), 300), bbox_inches="tight", facecolor="white", pad_inches=0.04)
    return path


def nature_plotly_layout(fig: go.Figure, title: str, *, width: int = 1000, height: int = 520, **kwargs) -> go.Figure:
    """Apply a clean matching style to Plotly HTML outputs."""
    fig.update_layout(
        template="plotly_white",
        title={"text": title, "x": 0.02, "xanchor": "left"},
        font={"family": "Arial, sans-serif", "size": 13, "color": NATURE_PALETTE["dark_grey"]},
        colorway=NATURE_COLORS,
        width=width,
        height=height,
        margin={"l": 70, "r": 30, "t": 65, "b": 60},
        legend={"orientation": "h", "yanchor": "bottom", "y": 1.02, "xanchor": "right", "x": 1},
        **kwargs,
    )
    fig.update_xaxes(showline=True, linewidth=1, linecolor=NATURE_PALETTE["dark_grey"], mirror=False)
    fig.update_yaxes(showline=True, linewidth=1, linecolor=NATURE_PALETTE["dark_grey"], mirror=False,
                     gridcolor=NATURE_PALETTE["light_grey"])
    return fig


set_nature_rcparams()


class RMBVisualizer:
    def __init__(self, minutes_per_day: int = 1440, dpi: int = 300) -> None:
        self.minutes_per_day = minutes_per_day
        self.dpi = max(int(dpi), 300)

    @staticmethod
    def _day_boundaries(n_rows: int, mins_per_day: int) -> List[int]:
        if n_rows < mins_per_day:
            return []
        n_days = n_rows // mins_per_day
        return [d * mins_per_day for d in range(1, n_days + 1)]

    def heatmap(self, data: pd.DataFrame, title: str, out_path_no_ext: str,
                cmap: str = "viridis") -> Tuple[str, str]:
        """Static PNG (matplotlib) + interactive HTML (plotly) heatmap."""
        png = f"{out_path_no_ext}.png"
        html = f"{out_path_no_ext}.html"
        mat = data.values
        boundaries = self._day_boundaries(mat.shape[0], self.minutes_per_day)
        x = np.arange(mat.shape[0])
        mean_v = np.nanmean(mat, axis=1)

        fig = plt.figure(figsize=(7.2, 5.0), constrained_layout=True)
        gs = fig.add_gridspec(2, 1, height_ratios=[4.2, 1.15], hspace=0.08)
        ax1 = fig.add_subplot(gs[0])
        ax2 = fig.add_subplot(gs[1], sharex=ax1)

        im = ax1.imshow(mat.T, aspect="auto", cmap=cmap, interpolation="nearest", rasterized=True)
        add_day_boundaries(ax1, boundaries, color="white", alpha=0.9)
        ax1.set_title(f"{title}  ({mat.shape[0]:,} timepoints × {mat.shape[1]:,} channels)", loc="left")
        ax1.set_ylabel("Channel")
        apply_nature_style(ax1, grid="")
        cb = fig.colorbar(im, ax=ax1, fraction=0.025, pad=0.015)
        cb.outline.set_visible(False)
        cb.ax.tick_params(length=2, width=0.6)
        cb.set_label("Value", rotation=270, labelpad=10)

        add_light_dark_bands(ax2, mat.shape[0], self.minutes_per_day)
        ax2.plot(x, mean_v, color=NATURE_PALETTE["blue"], linewidth=1.15)
        ax2.fill_between(x, mean_v, color=NATURE_PALETTE["sky"], alpha=0.25, lw=0)
        add_day_boundaries(ax2, boundaries)
        ax2.set_xlabel("Time (minutes)")
        ax2.set_ylabel("Mean")
        apply_nature_style(ax2)
        save_nature_figure(fig, png, dpi=self.dpi)
        plt.close(fig)

        plotly_fig = go.Figure(data=go.Heatmap(
            z=mat.T, colorscale=cmap.capitalize() if cmap.lower() != "coolwarm" else "RdBu",
            showscale=True, hovertemplate="t=%{x}<br>ch=%{y}<br>v=%{z}<extra></extra>",
        ))
        for b in boundaries:
            plotly_fig.add_vline(x=b, line_color="white", line_width=2, opacity=0.6)
        nature_plotly_layout(plotly_fig, f"{title} — {mat.shape[0]}×{mat.shape[1]}", width=1200, height=700)
        plotly_fig.update_xaxes(title="Time (minutes)")
        plotly_fig.update_yaxes(title="Channels")
        plotly_fig.write_html(html, include_plotlyjs="cdn")
        return png, html

    def population_trace(self, data: pd.DataFrame, title: str, out_path_no_ext: str,
                        ylabel: str = "Mean") -> Tuple[str, str]:
        """Line plot of population mean over time (PNG + HTML)."""
        png = f"{out_path_no_ext}.png"
        html = f"{out_path_no_ext}.html"
        mean_v = data.mean(axis=1).values
        x = np.arange(len(mean_v))
        boundaries = self._day_boundaries(len(mean_v), self.minutes_per_day)

        fig, ax = plt.subplots(figsize=(7.2, 2.6))
        add_light_dark_bands(ax, len(mean_v), self.minutes_per_day)
        ax.plot(x, mean_v, color=NATURE_PALETTE["blue"], linewidth=1.25)
        ax.fill_between(x, mean_v, alpha=0.22, color=NATURE_PALETTE["sky"], lw=0)
        add_day_boundaries(ax, boundaries)
        ax.set_title(title, loc="left")
        ax.set_xlabel("Time (minutes)")
        ax.set_ylabel(ylabel)
        apply_nature_style(ax)
        save_nature_figure(fig, png, dpi=self.dpi)
        plt.close(fig)

        pf = go.Figure(go.Scatter(x=x, y=mean_v, mode="lines", name=ylabel,
                                  line={"color": NATURE_PALETTE["blue"], "width": 2}))
        for b in boundaries:
            pf.add_vline(x=b, line_color=NATURE_PALETTE["grey"], line_dash="dash", opacity=0.45)
        nature_plotly_layout(pf, title, width=1200, height=420)
        pf.update_xaxes(title="Time (minutes)")
        pf.update_yaxes(title=ylabel)
        pf.write_html(html, include_plotlyjs="cdn")
        return png, html

    def frequency_barplot(self, freq: pd.DataFrame, title: str, out_path_no_ext: str
                          ) -> Tuple[str, str]:
        """Publication-style frequency summary (PNG) + interactive bar (HTML)."""
        png = f"{out_path_no_ext}.png"
        html = f"{out_path_no_ext}.html"
        totals = freq.sum(axis=0).replace(0, np.nan)
        prop = freq.div(totals, axis=1).fillna(0)
        avg = prop.mean(axis=1)
        sem = prop.sem(axis=1).fillna(0)

        fig = plt.figure(figsize=(7.2, 5.2), constrained_layout=True)
        gs = fig.add_gridspec(2, 2, height_ratios=[1.05, 1.0], width_ratios=[1.2, 1.0])
        ax = fig.add_subplot(gs[0, :])
        x = np.arange(len(avg))
        ax.bar(x, avg.values, color=NATURE_PALETTE["blue"], alpha=0.92, edgecolor="white", linewidth=0.5)
        ax.errorbar(x, avg.values, yerr=sem.values, fmt="none", ecolor=NATURE_PALETTE["black"], capsize=2, linewidth=0.75)
        ax.set_xticks(x); ax.set_xticklabels(avg.index.astype(str))
        ax.set_title(title, loc="left")
        ax.set_xlabel("Position")
        ax.set_ylabel("Mean proportion ± SEM")
        apply_nature_style(ax)

        ax2 = fig.add_subplot(gs[1, 0])
        im = ax2.imshow(prop.values, cmap="magma", aspect="auto", interpolation="nearest", rasterized=True)
        ax2.set_title("Per-channel distribution", loc="left")
        ax2.set_xlabel("Channel index"); ax2.set_ylabel("Position")
        apply_nature_style(ax2, grid="")
        cb = fig.colorbar(im, ax=ax2, fraction=0.045, pad=0.02)
        cb.outline.set_visible(False)

        ax3 = fig.add_subplot(gs[1, 1])
        ax3.hist(totals.dropna().values, bins=18, color=NATURE_PALETTE["vermillion"], alpha=0.9,
                 edgecolor="white", linewidth=0.5)
        ax3.set_title("Counts per channel", loc="left")
        ax3.set_xlabel("Total count"); ax3.set_ylabel("Channels")
        apply_nature_style(ax3)
        save_nature_figure(fig, png, dpi=self.dpi)
        plt.close(fig)

        pf = go.Figure(go.Bar(x=avg.index.astype(str), y=avg.values,
                              error_y={"type": "data", "array": sem.values},
                              marker={"color": NATURE_PALETTE["blue"]}))
        nature_plotly_layout(pf, title, width=900, height=500)
        pf.update_xaxes(title="Position")
        pf.update_yaxes(title="Mean proportion ± SEM")
        pf.write_html(html, include_plotlyjs="cdn")
        return png, html

    def grouped_bar(self, df: pd.DataFrame, title: str, out_path_no_ext: str,
                    ylabel: str = "Value") -> Tuple[str, str]:
        """Rows = categories, columns = group means; optional `<col>_std` columns."""
        png = f"{out_path_no_ext}.png"
        html = f"{out_path_no_ext}.html"
        mean_cols = [c for c in df.columns if not c.endswith("_std")]
        x = np.asarray(df.index, dtype=float) if np.all(pd.to_numeric(df.index, errors="coerce").notna()) else np.arange(len(df.index))

        fig, ax = plt.subplots(figsize=(7.4, 3.8))
        for i, col in enumerate(mean_cols):
            err = df[f"{col}_std"].values if f"{col}_std" in df.columns else None
            color = NATURE_COLORS[i % len(NATURE_COLORS)]
            ax.plot(x, df[col].values, marker="o", markersize=3.2, linewidth=1.25, color=color, label=col)
            if err is not None:
                lo = df[col].values - err
                hi = df[col].values + err
                ax.fill_between(x, lo, hi, color=color, alpha=0.13, linewidth=0)
        ax.set_xticks(x)
        ax.set_xticklabels(df.index.astype(str), rotation=0)
        ax.set_title(title, loc="left")
        ax.set_xlabel("Position")
        ax.set_ylabel(ylabel)
        ax.legend(frameon=False, ncol=min(3, max(1, len(mean_cols))), loc="upper center", bbox_to_anchor=(0.5, 1.18))
        apply_nature_style(ax)
        save_nature_figure(fig, png, dpi=self.dpi)
        plt.close(fig)

        pf = go.Figure()
        for i, col in enumerate(mean_cols):
            err = df[f"{col}_std"].values if f"{col}_std" in df.columns else None
            pf.add_scatter(name=col, x=df.index.astype(str), y=df[col].values, mode="lines+markers",
                           error_y={"type": "data", "array": err} if err is not None else None,
                           line={"color": NATURE_COLORS[i % len(NATURE_COLORS)], "width": 2})
        nature_plotly_layout(pf, title, width=950, height=520)
        pf.update_xaxes(title="Position")
        pf.update_yaxes(title=ylabel)
        pf.write_html(html, include_plotlyjs="cdn")
        return png, html
