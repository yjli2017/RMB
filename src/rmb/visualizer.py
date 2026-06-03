"""Matplotlib (PNG) and Plotly (HTML) plotting helpers.

The visual language follows Nature/NPG figure conventions:
- Helvetica/Arial typography sized for journal columns (7-8 pt body text)
- NPG-inspired primary palette (Nature Publishing Group / ggsci pal_npg)
- Okabe-Ito secondary palette retained for colorblind-safe overrides
- Thin spines, short inward ticks, sparing grid (y-axis only)
- Perceptually uniform sequential and diverging colormaps
- Panel-label helper (a, b, c) and slim colorbar styling
- Subtle light/dark/day cues for circadian context
- 600 DPI publication-grade PNG export, Type-42 fonts for PDF embedding
"""

from __future__ import annotations

from pathlib import Path
from typing import Iterable, List, Optional, Tuple

import matplotlib
matplotlib.use("Agg")  # non-interactive backend; safe inside CLI/pipeline
import matplotlib.pyplot as plt
from matplotlib import font_manager
from matplotlib.colors import LinearSegmentedColormap
import numpy as np
import pandas as pd
import plotly.graph_objects as go


def _pick_sans_font() -> str:
    """Return the first installed Helvetica-like sans-serif, else fall back."""
    installed = {f.name for f in font_manager.fontManager.ttflist}
    for candidate in (
        "Helvetica", "Arial", "Helvetica Neue", "Liberation Sans",
        "Nimbus Sans", "TeX Gyre Heros", "Roboto", "Source Sans Pro",
        "DejaVu Sans",
    ):
        if candidate in installed:
            return candidate
    return "DejaVu Sans"


_NATURE_SANS = _pick_sans_font()


# Primary palette — NPG / ggsci-inspired, the de-facto "Nature" colors.
NATURE_PALETTE = {
    "red": "#E64B35",
    "blue": "#4DBBD5",
    "teal": "#00A087",
    "navy": "#3C5488",
    "peach": "#F39B7F",
    "slate": "#8491B4",
    "mint": "#91D1C2",
    "scarlet": "#DC0000",
    "brown": "#7E6148",
    "sand": "#B09C85",
    # Neutral utility colors
    "black": "#1A1A1A",
    "dark_grey": "#2B2B2B",
    "grey": "#6E6E6E",
    "mid_grey": "#9A9A9A",
    "light_grey": "#E5E5E5",
    "panel_bg": "#FAFAFA",
    # Legacy keys kept so existing call sites remain valid.
    "orange": "#E64B35",
    "sky": "#4DBBD5",
    "bluish_green": "#00A087",
    "yellow": "#F39B7F",
    "vermillion": "#DC0000",
    "purple": "#8491B4",
}
NATURE_COLORS = [
    NATURE_PALETTE["red"],
    NATURE_PALETTE["blue"],
    NATURE_PALETTE["teal"],
    NATURE_PALETTE["navy"],
    NATURE_PALETTE["peach"],
    NATURE_PALETTE["slate"],
    NATURE_PALETTE["mint"],
    NATURE_PALETTE["brown"],
    NATURE_PALETTE["scarlet"],
    NATURE_PALETTE["sand"],
]

# Okabe-Ito colorblind-safe alternative (retained for accessibility-first plots).
OKABE_ITO = [
    "#0072B2", "#D55E00", "#009E73", "#CC79A7",
    "#E69F00", "#56B4E9", "#F0E442", "#000000",
]


def _make_cmap(name: str, stops: list[tuple[float, str]]) -> LinearSegmentedColormap:
    return LinearSegmentedColormap.from_list(name, stops, N=256)


# Custom NPG-derived colormaps (perceptual, publication-friendly).
NATURE_CMAPS = {
    # Sequential, warm — for activity / movement.
    "rmb_activity": _make_cmap("rmb_activity", [
        (0.00, "#FFF7F2"),
        (0.20, "#FBD3BA"),
        (0.45, "#F39B7F"),
        (0.70, "#E64B35"),
        (1.00, "#7A1500"),
    ]),
    # Sequential, cool — for position / structural metrics.
    "rmb_position": _make_cmap("rmb_position", [
        (0.00, "#F2F8FA"),
        (0.20, "#B9DDE7"),
        (0.45, "#4DBBD5"),
        (0.70, "#3C5488"),
        (1.00, "#0F1B33"),
    ]),
    # Sequential, single-hue teal — for sleep / inactive states.
    "rmb_sleep": _make_cmap("rmb_sleep", [
        (0.00, "#FFFFFF"),
        (0.30, "#C7E5DE"),
        (0.60, "#67B7A4"),
        (0.85, "#1F6F62"),
        (1.00, "#0A3A33"),
    ]),
    # Diverging, NPG blue ↔ NPG red, neutral midpoint — for awake-position contrast.
    "rmb_diverging": _make_cmap("rmb_diverging", [
        (0.00, "#3C5488"),
        (0.25, "#4DBBD5"),
        (0.50, "#F7F7F7"),
        (0.75, "#F39B7F"),
        (1.00, "#E64B35"),
    ]),
}

# Register so they can be referenced by name from any matplotlib call.
for _n, _cm in NATURE_CMAPS.items():
    try:
        matplotlib.colormaps.register(name=_n, cmap=_cm)
    except ValueError:
        # Already registered (e.g. on module reload).
        pass


def set_nature_rcparams() -> None:
    """Set conservative, publication-oriented matplotlib defaults."""
    plt.rcParams.update({
        "font.family": "sans-serif",
        "font.sans-serif": [_NATURE_SANS, "Liberation Sans", "DejaVu Sans"],
        "font.size": 8.0,
        "axes.titlesize": 9.5,
        "axes.titleweight": "bold",
        "axes.titlepad": 6.0,
        "axes.labelsize": 8.5,
        "axes.labelweight": "normal",
        "axes.labelpad": 3.0,
        "axes.edgecolor": NATURE_PALETTE["dark_grey"],
        "axes.linewidth": 0.9,
        "xtick.labelsize": 7.5,
        "ytick.labelsize": 7.5,
        "xtick.direction": "out",
        "ytick.direction": "out",
        "xtick.major.width": 0.8,
        "ytick.major.width": 0.8,
        "xtick.major.size": 2.8,
        "ytick.major.size": 2.8,
        "xtick.minor.width": 0.6,
        "ytick.minor.width": 0.6,
        "xtick.minor.size": 1.6,
        "ytick.minor.size": 1.6,
        "legend.fontsize": 7.5,
        "legend.frameon": False,
        "legend.handlelength": 1.4,
        "legend.handletextpad": 0.5,
        "legend.columnspacing": 1.2,
        "figure.titlesize": 11.0,
        "figure.titleweight": "bold",
        "figure.facecolor": "white",
        "savefig.dpi": 600,
        "savefig.facecolor": "white",
        "savefig.transparent": False,
        "savefig.bbox": "tight",
        "savefig.pad_inches": 0.04,
        "pdf.fonttype": 42,
        "ps.fonttype": 42,
        "svg.fonttype": "none",
        "image.cmap": "rmb_activity",
        "image.interpolation": "nearest",
    })


def apply_nature_style(ax, *, grid: str = "y"):
    """Apply clean Nature-like axis styling to one matplotlib Axes."""
    ax.set_facecolor("white")
    ax.spines["top"].set_visible(False)
    ax.spines["right"].set_visible(False)
    for side in ("left", "bottom"):
        ax.spines[side].set_color(NATURE_PALETTE["dark_grey"])
        ax.spines[side].set_linewidth(0.9)
    ax.tick_params(
        colors=NATURE_PALETTE["dark_grey"],
        direction="out",
        length=2.8,
        width=0.8,
        pad=2.0,
    )
    ax.title.set_color(NATURE_PALETTE["black"])
    ax.xaxis.label.set_color(NATURE_PALETTE["dark_grey"])
    ax.yaxis.label.set_color(NATURE_PALETTE["dark_grey"])
    if grid:
        ax.grid(
            True, axis=grid, color=NATURE_PALETTE["light_grey"],
            linewidth=0.55, alpha=0.85, zorder=0,
        )
        ax.set_axisbelow(True)
    return ax


def panel_label(ax, letter: str, *, x: float = -0.085, y: float = 1.06,
                fontsize: float = 11.5) -> None:
    """Add a bold lowercase panel letter (a, b, c, …) in the Nature style."""
    ax.text(
        x, y, letter, transform=ax.transAxes,
        fontsize=fontsize, fontweight="bold",
        color=NATURE_PALETTE["black"], ha="left", va="bottom",
    )


def add_day_boundaries(ax, boundaries: Iterable[int], *, color: str = "#BDBDBD",
                       alpha: float = 0.7) -> None:
    """Draw subtle day separators."""
    for b in boundaries:
        ax.axvline(b, color=color, linestyle=(0, (4, 2)), linewidth=0.6,
                   alpha=alpha, zorder=1)


def add_light_dark_bands(ax, n_points: int, minutes_per_day: int = 1440,
                         *, lights_on: int = 720, dark_color: str = "#ECECEC",
                         dark_alpha: float = 0.7) -> None:
    """Add subtle alternating dark-phase background bands for circadian plots."""
    if n_points <= 0:
        return
    for start in range(0, n_points, minutes_per_day):
        dark_start = start + lights_on
        dark_end = min(start + minutes_per_day, n_points)
        if dark_start < n_points:
            ax.axvspan(dark_start, dark_end, color=dark_color,
                       alpha=dark_alpha, lw=0, zorder=0)


def style_colorbar(cb, *, label: str = "", labelpad: float = 8.0) -> None:
    """Apply slim, frameless Nature colorbar styling."""
    cb.outline.set_visible(False)
    cb.ax.tick_params(
        length=2.0, width=0.7,
        colors=NATURE_PALETTE["dark_grey"], pad=2,
    )
    for spine in cb.ax.spines.values():
        spine.set_visible(False)
    if label:
        cb.set_label(label, rotation=270, labelpad=labelpad,
                     color=NATURE_PALETTE["dark_grey"], fontsize=7.5)


def save_nature_figure(fig, path: str, *, dpi: int = 600) -> str:
    """Save a high-resolution, tightly cropped PNG (publication quality)."""
    Path(path).parent.mkdir(parents=True, exist_ok=True)
    try:
        fig.tight_layout(pad=0.45)
    except Exception:
        # Figures using constrained_layout or complex colorbars may skip tight_layout.
        pass
    fig.savefig(
        path, dpi=max(int(dpi), 600),
        bbox_inches="tight", facecolor="white", pad_inches=0.04,
    )
    return path


def _plotly_colorway() -> list[str]:
    return list(NATURE_COLORS)


def nature_plotly_layout(fig: go.Figure, title: str, *,
                         width: int = 1000, height: int = 520, **kwargs) -> go.Figure:
    """Apply a clean matching style to Plotly HTML outputs."""
    fig.update_layout(
        template="plotly_white",
        title={
            "text": f"<b>{title}</b>", "x": 0.02, "xanchor": "left",
            "font": {"size": 15, "color": NATURE_PALETTE["black"]},
        },
        font={
            "family": "Helvetica, Arial, sans-serif", "size": 12,
            "color": NATURE_PALETTE["dark_grey"],
        },
        colorway=_plotly_colorway(),
        width=width,
        height=height,
        margin={"l": 70, "r": 30, "t": 70, "b": 60},
        paper_bgcolor="white",
        plot_bgcolor="white",
        legend={
            "orientation": "h", "yanchor": "bottom",
            "y": 1.02, "xanchor": "right", "x": 1,
            "bgcolor": "rgba(255,255,255,0)",
            "bordercolor": "rgba(0,0,0,0)",
            "font": {"size": 11},
        },
        **kwargs,
    )
    fig.update_xaxes(
        showline=True, linewidth=1.1, linecolor=NATURE_PALETTE["dark_grey"],
        mirror=False, ticks="outside", ticklen=4, tickwidth=1.0,
        tickcolor=NATURE_PALETTE["dark_grey"], gridcolor="rgba(0,0,0,0)",
        zeroline=False,
    )
    fig.update_yaxes(
        showline=True, linewidth=1.1, linecolor=NATURE_PALETTE["dark_grey"],
        mirror=False, ticks="outside", ticklen=4, tickwidth=1.0,
        tickcolor=NATURE_PALETTE["dark_grey"],
        gridcolor=NATURE_PALETTE["light_grey"], zeroline=False,
    )
    return fig


# Plotly colorscales matching the matplotlib NPG cmaps above.
PLOTLY_CMAPS = {
    "rmb_activity": [
        [0.00, "#FFF7F2"], [0.20, "#FBD3BA"], [0.45, "#F39B7F"],
        [0.70, "#E64B35"], [1.00, "#7A1500"],
    ],
    "rmb_position": [
        [0.00, "#F2F8FA"], [0.20, "#B9DDE7"], [0.45, "#4DBBD5"],
        [0.70, "#3C5488"], [1.00, "#0F1B33"],
    ],
    "rmb_sleep": [
        [0.00, "#FFFFFF"], [0.30, "#C7E5DE"], [0.60, "#67B7A4"],
        [0.85, "#1F6F62"], [1.00, "#0A3A33"],
    ],
    "rmb_diverging": [
        [0.00, "#3C5488"], [0.25, "#4DBBD5"], [0.50, "#F7F7F7"],
        [0.75, "#F39B7F"], [1.00, "#E64B35"],
    ],
}


def plotly_colorscale(name: str):
    """Resolve a matplotlib cmap name to a Plotly colorscale."""
    if name in PLOTLY_CMAPS:
        return PLOTLY_CMAPS[name]
    # Friendly aliases for legacy matplotlib names used elsewhere.
    aliases = {
        "plasma": "Plasma", "viridis": "Viridis", "magma": "Magma",
        "inferno": "Inferno", "cividis": "Cividis",
        "Reds": "Reds", "Blues": "Blues", "Greys": "Greys",
        "coolwarm": "RdBu", "RdBu_r": "RdBu",
    }
    return aliases.get(name, name.capitalize())


set_nature_rcparams()


class RMBVisualizer:
    """Reusable composite plots built from the styled primitives above."""

    def __init__(self, minutes_per_day: int = 1440, dpi: int = 600) -> None:
        self.minutes_per_day = minutes_per_day
        self.dpi = max(int(dpi), 600)

    @staticmethod
    def _day_boundaries(n_rows: int, mins_per_day: int) -> List[int]:
        if n_rows < mins_per_day:
            return []
        n_days = n_rows // mins_per_day
        return [d * mins_per_day for d in range(1, n_days + 1)]

    def heatmap(self, data: pd.DataFrame, title: str, out_path_no_ext: str,
                cmap: str = "rmb_activity") -> Tuple[str, str]:
        """Static PNG (matplotlib) + interactive HTML (plotly) heatmap.

        Layout: heatmap (a) on top, population mean trace (b) on bottom.
        """
        png = f"{out_path_no_ext}.png"
        html = f"{out_path_no_ext}.html"
        mat = data.values
        boundaries = self._day_boundaries(mat.shape[0], self.minutes_per_day)
        x = np.arange(mat.shape[0])
        mean_v = np.nanmean(mat, axis=1)

        fig = plt.figure(figsize=(7.2, 5.1), constrained_layout=True)
        gs = fig.add_gridspec(2, 1, height_ratios=[4.2, 1.15], hspace=0.05)
        ax1 = fig.add_subplot(gs[0])
        ax2 = fig.add_subplot(gs[1], sharex=ax1)

        im = ax1.imshow(mat.T, aspect="auto", cmap=cmap,
                        interpolation="nearest", rasterized=True)
        add_day_boundaries(ax1, boundaries, color="white", alpha=0.85)
        ax1.set_title(
            f"{title}  ({mat.shape[0]:,} timepoints × {mat.shape[1]:,} channels)",
            loc="left",
        )
        ax1.set_ylabel("Channel")
        apply_nature_style(ax1, grid="")
        # Hide the heatmap's x tick labels so they don't sit between the two
        # adjacent panels — the trace below shares the same x axis.
        ax1.tick_params(axis="x", labelbottom=False)
        cb = fig.colorbar(im, ax=ax1, fraction=0.022, pad=0.012, aspect=28)
        style_colorbar(cb, label="Value")

        add_light_dark_bands(ax2, mat.shape[0], self.minutes_per_day)
        ax2.plot(x, mean_v, color=NATURE_PALETTE["navy"], linewidth=1.25)
        ax2.fill_between(x, mean_v, color=NATURE_PALETTE["blue"],
                         alpha=0.22, lw=0)
        add_day_boundaries(ax2, boundaries)
        ax2.set_xlabel("Time (minutes)")
        ax2.set_ylabel("Population mean")
        apply_nature_style(ax2)
        save_nature_figure(fig, png, dpi=self.dpi)
        plt.close(fig)

        plotly_fig = go.Figure(data=go.Heatmap(
            z=mat.T, colorscale=plotly_colorscale(cmap),
            showscale=True,
            colorbar={"outlinewidth": 0, "thickness": 14, "len": 0.85,
                      "title": {"text": "Value", "side": "right"}},
            hovertemplate="t=%{x}<br>ch=%{y}<br>v=%{z}<extra></extra>",
        ))
        for b in boundaries:
            plotly_fig.add_vline(x=b, line_color="white",
                                 line_width=1.5, opacity=0.7)
        nature_plotly_layout(
            plotly_fig, f"{title} — {mat.shape[0]}×{mat.shape[1]}",
            width=1200, height=700,
        )
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
        sem_v = data.sem(axis=1).fillna(0).values if data.shape[1] > 1 else np.zeros_like(mean_v)
        x = np.arange(len(mean_v))
        boundaries = self._day_boundaries(len(mean_v), self.minutes_per_day)

        fig, ax = plt.subplots(figsize=(7.2, 2.7))
        add_light_dark_bands(ax, len(mean_v), self.minutes_per_day)
        ax.fill_between(x, mean_v - sem_v, mean_v + sem_v,
                        color=NATURE_PALETTE["blue"], alpha=0.18, lw=0)
        ax.plot(x, mean_v, color=NATURE_PALETTE["navy"], linewidth=1.3)
        add_day_boundaries(ax, boundaries)
        ax.set_title(title, loc="left")
        ax.set_xlabel("Time (minutes)")
        ax.set_ylabel(ylabel)
        apply_nature_style(ax)
        save_nature_figure(fig, png, dpi=self.dpi)
        plt.close(fig)

        pf = go.Figure()
        pf.add_scatter(
            x=np.concatenate([x, x[::-1]]),
            y=np.concatenate([mean_v + sem_v, (mean_v - sem_v)[::-1]]),
            fill="toself", fillcolor="rgba(77,187,213,0.18)",
            line={"color": "rgba(0,0,0,0)"}, hoverinfo="skip", showlegend=False,
        )
        pf.add_scatter(
            x=x, y=mean_v, mode="lines", name=ylabel,
            line={"color": NATURE_PALETTE["navy"], "width": 2.2},
        )
        for b in boundaries:
            pf.add_vline(x=b, line_color=NATURE_PALETTE["mid_grey"],
                         line_dash="dash", opacity=0.45)
        nature_plotly_layout(pf, title, width=1200, height=420)
        pf.update_xaxes(title="Time (minutes)")
        pf.update_yaxes(title=ylabel)
        pf.write_html(html, include_plotlyjs="cdn")
        return png, html

    def frequency_barplot(self, freq: pd.DataFrame, title: str,
                          out_path_no_ext: str) -> Tuple[str, str]:
        """Publication-style frequency summary (PNG) + interactive bar (HTML)."""
        png = f"{out_path_no_ext}.png"
        html = f"{out_path_no_ext}.html"
        totals = freq.sum(axis=0).replace(0, np.nan)
        prop = freq.div(totals, axis=1).fillna(0)
        avg = prop.mean(axis=1)
        sem = prop.sem(axis=1).fillna(0)

        fig = plt.figure(figsize=(7.2, 5.3), constrained_layout=True)
        gs = fig.add_gridspec(2, 2, height_ratios=[1.05, 1.0],
                              width_ratios=[1.2, 1.0])
        ax = fig.add_subplot(gs[0, :])
        x = np.arange(len(avg))
        ax.bar(
            x, avg.values, color=NATURE_PALETTE["blue"], alpha=0.9,
            edgecolor=NATURE_PALETTE["navy"], linewidth=0.6,
        )
        ax.errorbar(
            x, avg.values, yerr=sem.values, fmt="none",
            ecolor=NATURE_PALETTE["black"], capsize=2.5,
            capthick=0.8, elinewidth=0.9,
        )
        ax.set_xticks(x)
        ax.set_xticklabels(avg.index.astype(str))
        ax.set_title(title, loc="left")
        ax.set_xlabel("Position")
        ax.set_ylabel("Mean proportion ± SEM")
        apply_nature_style(ax)
        panel_label(ax, "a", y=1.02)

        ax2 = fig.add_subplot(gs[1, 0])
        im = ax2.imshow(prop.values, cmap="rmb_position",
                        aspect="auto", interpolation="nearest", rasterized=True)
        ax2.set_title("Per-channel distribution", loc="left")
        ax2.set_xlabel("Channel index")
        ax2.set_ylabel("Position")
        apply_nature_style(ax2, grid="")
        panel_label(ax2, "b")
        cb = fig.colorbar(im, ax=ax2, fraction=0.035, pad=0.015, aspect=22)
        style_colorbar(cb)

        ax3 = fig.add_subplot(gs[1, 1])
        ax3.hist(
            totals.dropna().values, bins=18,
            color=NATURE_PALETTE["red"], alpha=0.9,
            edgecolor="white", linewidth=0.6,
        )
        ax3.set_title("Counts per channel", loc="left")
        ax3.set_xlabel("Total count")
        ax3.set_ylabel("Channels")
        apply_nature_style(ax3)
        panel_label(ax3, "c")
        save_nature_figure(fig, png, dpi=self.dpi)
        plt.close(fig)

        pf = go.Figure(go.Bar(
            x=avg.index.astype(str), y=avg.values,
            error_y={"type": "data", "array": sem.values, "thickness": 1.2,
                     "color": NATURE_PALETTE["black"]},
            marker={"color": NATURE_PALETTE["blue"],
                    "line": {"color": NATURE_PALETTE["navy"], "width": 0.8}},
        ))
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
        if np.all(pd.to_numeric(df.index, errors="coerce").notna()):
            x = np.asarray(df.index, dtype=float)
        else:
            x = np.arange(len(df.index))

        fig, ax = plt.subplots(figsize=(7.4, 3.9))
        for i, col in enumerate(mean_cols):
            err = df[f"{col}_std"].values if f"{col}_std" in df.columns else None
            color = NATURE_COLORS[i % len(NATURE_COLORS)]
            ax.plot(
                x, df[col].values, marker="o", markersize=3.6,
                markeredgewidth=0.6, markeredgecolor="white",
                linewidth=1.35, color=color, label=col,
            )
            if err is not None:
                lo = df[col].values - err
                hi = df[col].values + err
                ax.fill_between(x, lo, hi, color=color, alpha=0.15, linewidth=0)
        ax.set_xticks(x)
        ax.set_xticklabels(df.index.astype(str), rotation=0)
        ax.set_title(title, loc="left")
        ax.set_xlabel("Position")
        ax.set_ylabel(ylabel)
        ax.legend(
            frameon=False, ncol=min(3, max(1, len(mean_cols))),
            loc="upper center", bbox_to_anchor=(0.5, 1.18),
        )
        apply_nature_style(ax)
        save_nature_figure(fig, png, dpi=self.dpi)
        plt.close(fig)

        pf = go.Figure()
        for i, col in enumerate(mean_cols):
            err = df[f"{col}_std"].values if f"{col}_std" in df.columns else None
            pf.add_scatter(
                name=col, x=df.index.astype(str), y=df[col].values,
                mode="lines+markers",
                error_y=({"type": "data", "array": err, "thickness": 1.1,
                          "color": NATURE_COLORS[i % len(NATURE_COLORS)]}
                         if err is not None else None),
                line={"color": NATURE_COLORS[i % len(NATURE_COLORS)], "width": 2.2},
                marker={"size": 7, "line": {"color": "white", "width": 1}},
            )
        nature_plotly_layout(pf, title, width=950, height=520)
        pf.update_xaxes(title="Position")
        pf.update_yaxes(title=ylabel)
        pf.write_html(html, include_plotlyjs="cdn")
        return png, html
