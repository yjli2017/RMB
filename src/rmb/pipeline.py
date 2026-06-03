"""End-to-end RMB pipeline runner.

Usage:
    python -m rmb.pipeline --config configs/default.yaml
    python -m rmb.pipeline --config configs/default.yaml \
                           --data-dir test/ --metadata-dir test/metadata/ \
                           --output-dir test/output/
"""

from __future__ import annotations

import argparse
import sys
import time
from pathlib import Path
from typing import Dict

from .config import load_config
from .steps import (
    step01_load,
    step02_metadata,
    step03_process,
    step04_sleep,
    step05_heatmaps,
    step06_frequency,
    step07_stats,
    step08_summary,
)


def _parse_args(argv=None) -> argparse.Namespace:
    p = argparse.ArgumentParser(description="Run the RMB end-to-end analysis pipeline.")
    p.add_argument("--config", required=False, help="Path to YAML config.")
    p.add_argument("--data-dir", dest="data_dir", default=None,
                   help="Override config.data_dir (where Monitor*.txt live).")
    p.add_argument("--metadata-dir", dest="metadata_dir", default=None,
                   help="Override config.metadata_dir (where *_metadata.csv live).")
    p.add_argument("--output-dir", dest="output_dir", default=None,
                   help="Override config.output_dir (per-run folders are created inside).")
    p.add_argument("--run-id", dest="run_id", default=None,
                   help="Override config.run_id (folder name under output_dir).")
    p.add_argument("--sleep-threshold", dest="sleep_threshold", type=int, default=None,
                   help="Override config.sleep_threshold.")
    return p.parse_args(argv)


def run(config) -> Dict[str, Dict]:
    """Run all 8 steps. Returns a dict step_name -> result dict."""
    print(f"[rmb] run_id={config.run_id}  output={config.run_dir}")
    results: Dict[str, Dict] = {}

    t0 = time.time()
    print("[rmb] Step 1/8: load")
    r1 = step01_load.run(config); results["01_load"] = r1

    print("[rmb] Step 2/8: metadata")
    r2 = step02_metadata.run(config, monitor_files=r1["monitor_files"]); results["02_metadata"] = r2

    print("[rmb] Step 3/8: process")
    r3 = step03_process.run(config, raw_path=r1["raw_path"]); results["03_process"] = r3

    print("[rmb] Step 4/8: sleep")
    r4 = step04_sleep.run(config, mt_path=r3["mt_path"], pn_path=r3["pn_path"]); results["04_sleep"] = r4

    print("[rmb] Step 5/8: heatmaps")
    r5 = step05_heatmaps.run(config, mt_path=r3["mt_path"], pn_path=r3["pn_path"],
                              sleep_path=r4["sleep_path"], pn_awake_path=r4["pn_awake_path"])
    results["05_heatmaps"] = r5

    print("[rmb] Step 6/8: frequency")
    r6 = step06_frequency.run(config, pn_awake_path=r4["pn_awake_path"],
                               metadata_path=r2["metadata_path"])
    results["06_frequency"] = r6

    print("[rmb] Step 7/8: stats")
    r7 = step07_stats.run(config, mt_path=r3["mt_path"], awake_path=r4["awake_path"],
                           sleep_path=r4["sleep_path"], metadata_path=r2["metadata_path"])
    results["07_stats"] = r7

    print("[rmb] Step 8/8: summary")
    r8 = step08_summary.run(config, step_results=results); results["08_summary"] = r8

    dt = time.time() - t0
    print(f"[rmb] complete in {dt:.1f}s. Review: file://{r8['index_html']}")
    return results


def main(argv=None) -> int:
    args = _parse_args(argv)
    config = load_config(
        args.config,
        data_dir=args.data_dir,
        metadata_dir=args.metadata_dir,
        output_dir=args.output_dir,
        run_id=args.run_id,
        sleep_threshold=args.sleep_threshold,
    )
    run(config)
    return 0


if __name__ == "__main__":
    sys.exit(main())
