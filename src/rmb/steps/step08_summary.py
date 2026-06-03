"""Step 8 — Final summary: per-run analysis_summary.json and a one-page index.html.

Input  : the dict returned by every prior step (step_results)
Output : analysis_summary.json — config snapshot, runtime, per-step artifacts
         plots/index.html      — single page linking every PNG/HTML in the run
         schema.json
"""

from __future__ import annotations

import json
import os
import time
from pathlib import Path
from typing import Dict, List

from ..config import RMBConfig
from ..manifest import write_schema


STEP_NO = 8
STEP_NAME = "summary"


def _rel(base: str, target: str) -> str:
    try:
        return os.path.relpath(target, base)
    except ValueError:
        return target


def run(config: RMBConfig, step_results: Dict[str, Dict]) -> Dict:
    step_dir = config.step_dir(STEP_NO, STEP_NAME)
    plots_dir = os.path.join(step_dir, "plots")

    summary_payload = {
        "produced_at": time.strftime("%Y-%m-%dT%H:%M:%SZ", time.gmtime()),
        "run_id": config.run_id,
        "config": config.to_dict(),
        "steps": {},
    }
    for step_name, info in step_results.items():
        summary_payload["steps"][step_name] = {
            k: (str(v) if isinstance(v, Path) else v)
            for k, v in info.items()
        }

    summary_path = os.path.join(step_dir, "analysis_summary.json")
    with open(summary_path, "w") as f:
        json.dump(summary_payload, f, indent=2, default=str)

    # Build run-wide index.html linking every PNG and HTML.
    run_dir = config.run_dir
    sections: List[str] = []
    for step_name, info in step_results.items():
        s_dir = info.get("step_dir")
        if not s_dir:
            continue
        section_plots = sorted(info.get("plots", []))
        if not section_plots:
            continue
        items = []
        for p in section_plots:
            rel = _rel(run_dir, p)
            if p.endswith(".html"):
                items.append(f'<li><a href="{rel}" target="_blank">{os.path.basename(p)} (interactive)</a></li>')
            elif p.endswith(".png"):
                items.append(f'<li><img src="{rel}" style="max-width:900px;border:1px solid #ccc"/><div><code>{rel}</code></div></li>')
        schema_rel = _rel(run_dir, info["schema"]) if "schema" in info else ""
        sections.append(
            f"<section><h2>{step_name}</h2>"
            f'<p>step_dir: <code>{_rel(run_dir, s_dir)}</code>'
            + (f' &middot; <a href="{schema_rel}">schema.json</a>' if schema_rel else "")
            + "</p><ul>" + "\n".join(items) + "</ul></section>"
        )

    index_html = os.path.join(run_dir, "index.html")
    with open(index_html, "w") as f:
        f.write(f"""<!doctype html>
<html><head><meta charset="utf-8"><title>RMB run {config.run_id}</title>
<style>body{{font-family:sans-serif;max-width:1000px;margin:2em auto;padding:0 1em;}}
h1{{border-bottom:1px solid #ccc;padding-bottom:.3em}}
h2{{margin-top:2em;color:#246}}
ul{{list-style:none;padding:0}} li{{margin:1em 0}}
code{{background:#f2f2f2;padding:1px 4px;border-radius:3px}}
</style></head>
<body>
<h1>RMB run <code>{config.run_id}</code></h1>
<p>Generated {summary_payload['produced_at']}.</p>
<p>Config: <code>data_dir={config.data_dir}</code>, <code>metadata_dir={config.metadata_dir}</code>,
<code>output_dir={config.output_dir}</code>, <code>sleep_threshold={config.sleep_threshold}</code>.</p>
{''.join(sections)}
</body></html>""")

    schema = write_schema(
        step_dir,
        produced_by=f"step{STEP_NO:02d}_{STEP_NAME}",
        artifacts=[
            {"path": summary_path, "format": "json",
             "description": "Full run summary: config + per-step artifact map"},
            {"path": index_html, "description": "One-page review index linking all per-step plots"},
        ],
    )

    return {
        "step_dir": step_dir,
        "summary_path": summary_path,
        "index_html": index_html,
        "schema": schema,
        "plots": [],
    }
