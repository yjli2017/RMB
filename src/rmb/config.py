"""Configuration for the RMB pipeline.

A run is fully described by an `RMBConfig` instance. Either build one
directly or load from YAML via `load_config(path)`.
"""

from __future__ import annotations

import os
import time
from dataclasses import dataclass, field, asdict
from pathlib import Path
from typing import Optional, Tuple

import yaml


@dataclass
class RMBConfig:
    data_dir: str
    metadata_dir: str
    output_dir: str
    run_id: str = field(default_factory=lambda: time.strftime("run_%Y%m%d_%H%M%S"))

    minutes_per_day: int = 1440
    position_range: Tuple[int, int] = (1, 15)
    sleep_threshold: int = 5
    plot_dpi: int = 150
    max_days_to_plot: int = 5

    def __post_init__(self) -> None:
        self.data_dir = str(Path(self.data_dir).expanduser().resolve())
        self.metadata_dir = str(Path(self.metadata_dir).expanduser().resolve())
        self.output_dir = str(Path(self.output_dir).expanduser().resolve())
        os.makedirs(self.run_dir, exist_ok=True)
        os.makedirs(self.metadata_dir, exist_ok=True)

    @property
    def run_dir(self) -> str:
        return str(Path(self.output_dir) / self.run_id)

    def step_dir(self, step_no: int, name: str) -> str:
        d = Path(self.run_dir) / f"{step_no:02d}_{name}"
        (d / "plots").mkdir(parents=True, exist_ok=True)
        return str(d)

    def to_dict(self) -> dict:
        return asdict(self)


def load_config(path: Optional[str] = None, **overrides) -> RMBConfig:
    """Load YAML config and apply CLI overrides.

    Overrides with value `None` are ignored so empty CLI flags don't clobber YAML.
    """
    base: dict = {}
    if path:
        with open(path, "r") as f:
            base = yaml.safe_load(f) or {}

    for k, v in overrides.items():
        if v is not None:
            base[k] = v

    if "position_range" in base and isinstance(base["position_range"], list):
        base["position_range"] = tuple(base["position_range"])

    return RMBConfig(**base)
