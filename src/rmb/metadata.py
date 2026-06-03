"""Metadata loading and (when absent) auto-generation."""

from __future__ import annotations

import glob
import os
from typing import Dict, List

import pandas as pd


class RMBMetadataManager:
    """Owns per-monitor metadata CSVs.

    Metadata columns (template): Fly, Lab, User, Date, Experiment_name,
    Monitor_type, Monitor_number, Channel, Group, Tube_type, Incubator,
    Temperature, Treatment, Sex, Other, Alive.
    """

    DEFAULT_CHANNELS = 32

    def __init__(self, metadata_dir: str) -> None:
        self.metadata_dir = metadata_dir
        os.makedirs(self.metadata_dir, exist_ok=True)

    def metadata_files(self) -> List[str]:
        return sorted(glob.glob(os.path.join(self.metadata_dir, "*_metadata.csv")))

    def exists(self) -> bool:
        return len(self.metadata_files()) > 0

    def load(self) -> Dict[str, pd.DataFrame]:
        out: Dict[str, pd.DataFrame] = {}
        for path in self.metadata_files():
            name = os.path.basename(path).replace("_metadata.csv", "")
            out[name] = pd.read_csv(path)
        return out

    def generate_sample(self, monitor_files: List[str], analysis_date: str = "20250101") -> Dict[str, pd.DataFrame]:
        """Write a placeholder metadata file per monitor (32 channels)."""
        out: Dict[str, pd.DataFrame] = {}
        for fp in monitor_files:
            name = os.path.splitext(os.path.basename(fp))[0]
            channels = list(range(1, self.DEFAULT_CHANNELS + 1))
            md = pd.DataFrame({
                "Monitor_number": [name] * len(channels),
                "Channel": channels,
                "Group": [f"Group_{(c-1)//16 + 1}" for c in channels],
                "Treatment": [f"Treatment_{(c-1)//8 + 1}" for c in channels],
                "Sex": ["Male" if c % 2 else "Female" for c in channels],
                "Genotype": [f"Genotype_{(c-1)//4 + 1}" for c in channels],
                "Alive": [1] * len(channels),
                "Lab": ["RMB_Lab"] * len(channels),
                "User": ["auto"] * len(channels),
                "Date": [analysis_date] * len(channels),
                "Temperature": [25] * len(channels),
                "Experiment_name": ["auto"] * len(channels),
                "Tube_type": ["Standard"] * len(channels),
                "Incubator": ["Inc_1"] * len(channels),
            })
            md["Fly"] = md.apply(
                lambda r: f"{r['Lab']}_{r['User']}_{r['Date']}_{r['Experiment_name']}_{r['Monitor_number']}_{r['Channel']}",
                axis=1,
            )
            md.to_csv(os.path.join(self.metadata_dir, f"{name}_metadata.csv"), index=False)
            out[name] = md
        return out

    @staticmethod
    def alive_counts(md: pd.DataFrame) -> Dict[str, int]:
        total = len(md)
        if "Alive" not in md.columns:
            return {"total": total, "alive": total, "dead": 0}
        alive_col = pd.to_numeric(md["Alive"], errors="coerce")
        if alive_col.isna().all():
            return {"total": total, "alive": total, "dead": 0}
        alive = int(alive_col.fillna(1).sum())
        return {"total": total, "alive": alive, "dead": total - alive}
