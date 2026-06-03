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
<style>
:root {{
  --fg:#1a1a1a; --muted:#6e6e6e; --line:#e5e5e5;
  --accent:#3C5488; --accent2:#E64B35; --bg:#fafafa;
}}
* {{ box-sizing:border-box; }}
body {{
  font-family:Helvetica,Arial,sans-serif; color:var(--fg);
  max-width:1080px; margin:2em auto; padding:0 1.4em; line-height:1.55;
  background:#ffffff;
}}
header {{
  border-bottom:2px solid var(--accent); padding-bottom:.6em; margin-bottom:1.4em;
}}
h1 {{ font-size:1.7rem; margin:0 0 .2em 0; letter-spacing:-0.01em; }}
h1 code {{ font-size:1.1rem; color:var(--accent); background:transparent; padding:0; }}
.meta {{ color:var(--muted); font-size:.85rem; }}
.meta code {{ font-family:Menlo,Consolas,monospace; background:var(--bg);
  padding:1px 5px; border-radius:3px; border:1px solid var(--line); }}
section {{
  margin-top:2.2em; padding-top:1.1em; border-top:1px solid var(--line);
}}
h2 {{
  margin:0 0 .4em 0; font-size:1.15rem; color:var(--accent);
  font-weight:600; letter-spacing:-0.005em;
}}
section > p {{ color:var(--muted); font-size:.85rem; margin:.2em 0 1em 0; }}
ul {{ list-style:none; padding:0; margin:0;
  display:grid; grid-template-columns:repeat(auto-fill, minmax(420px, 1fr));
  gap:1em; }}
li {{
  background:var(--bg); padding:.8em; border:1px solid var(--line);
  border-radius:6px; transition:border-color .15s ease;
}}
li:hover {{ border-color:var(--accent); }}
li img {{ max-width:100%; height:auto; border:1px solid var(--line);
  border-radius:4px; background:#fff; }}
li div code {{ display:block; font-size:.72rem; color:var(--muted);
  margin-top:.4em; word-break:break-all; }}
a {{ color:var(--accent2); text-decoration:none; }}
a:hover {{ text-decoration:underline; }}
code {{ font-family:Menlo,Consolas,monospace; background:var(--bg);
  padding:1px 5px; border-radius:3px; font-size:.85em; }}
</style></head>
<body>
<header>
  <h1>RMB run <code>{config.run_id}</code></h1>
  <p class="meta">Generated {summary_payload['produced_at']}</p>
  <p class="meta">
    <code>data_dir={config.data_dir}</code> &middot;
    <code>metadata_dir={config.metadata_dir}</code> &middot;
    <code>output_dir={config.output_dir}</code> &middot;
    <code>sleep_threshold={config.sleep_threshold}</code>
  </p>
</header>
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
