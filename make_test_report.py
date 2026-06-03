from pathlib import Path
import pandas as pd, zipfile
root=Path('/home/liy27/projects/RMB')
run=root/'test/output/test_20260603_031850'
meta=pd.read_parquet(run/'02_metadata/metadata.parquet')
counts=pd.read_csv(run/'01_load/per_monitor_counts.csv')
stats=pd.read_csv(run/'07_stats/stats_results.csv')
possible=['Phenotype','phenotype','group','Genotype','genotype','treatment','Treatment']
group_col=next((c for c in possible if c in meta.columns), None)
if group_col is None:
    for c in meta.columns:
        vals=set(map(str, meta[c].dropna().unique()))
        if {'Female_CS','Female_Caff','Female_Gaboxdol','Male_CS','Male_Caff','Male_Gaboxdol'} & vals:
            group_col=c; break
summary=[]
summary.append('RMB TEST DATA ANALYSIS REPORT')
summary.append('Run ID: test_20260603_031850')
summary.append('Generated: 2026-06-03T03:20:42Z')
summary.append('Status: PASSED - Python RMB pipeline completed all 8 steps without error in 57.8 seconds.')
summary.append('')
summary.append('Input data')
summary.append('- Data dir: /home/liy27/projects/RMB/test')
summary.append('- Monitors loaded: ' + ', '.join(sorted(counts.monitor.unique())))
summary.append('- Records per monitor/type: 8311 rows for MT, CT, and Pn on each of 6 monitors')
summary.append(f'- Metadata rows: {len(meta)} flies')
if group_col:
    summary.append(f'- Group column: {group_col}')
    vc=meta[group_col].value_counts().sort_index()
    for k,v in vc.items(): summary.append(f'  - {k}: {v}')
summary.append('')
summary.append('Pipeline steps completed')
for s in ['01_load','02_metadata','03_process','04_sleep','05_heatmaps','06_frequency','07_stats','08_summary']:
    summary.append(f'- {s}: completed')
summary.append('')
sig=stats[stats.p_value<0.05].copy()
summary.append('Statistical highlights, uncorrected p < 0.05')
summary.append(f'- Significant pairwise tests: {len(sig)} / {len(stats)}')
for metric in ['activity_mean','awake_proportion','sleep_proportion']:
    sub=sig[sig.metric==metric].sort_values('p_value').head(5)
    summary.append(f'- Top {metric} comparisons:')
    for _,r in sub.iterrows():
        summary.append(f'  - {r.group_a} vs {r.group_b}: p={r.p_value:.3g}; means {r.mean_a:.3g} vs {r.mean_b:.3g}')
summary.append('')
summary.append('Interpretation from test data')
summary.append('- Gaboxdol groups show much lower activity/awake proportion and higher sleep proportion than CS and caffeine groups.')
summary.append('- Female_Caff shows the highest mean activity among listed groups; Male_Gaboxdol shows the lowest.')
summary.append('- These are test-data results and p-values are currently uncorrected for multiple comparisons.')
summary.append('')
summary.append('Main output locations')
summary.append(f'- HTML summary: {run}/index.html')
summary.append(f'- Full output folder: {run}')
summary.append('- Key CSVs: 06_frequency/freq_by_group.csv, 07_stats/stats_results.csv, 07_stats/channel_stats.csv, 07_stats/circadian_hourly.csv')
summary.append('')
summary.append('Key figures included in the output folder')
for p in ['02_metadata/plots/group_breakdown.png','04_sleep/plots/population_sleep.png','05_heatmaps/plots/heatmap_pn_awake.png','06_frequency/plots/freq_by_group.png','07_stats/plots/group_compare.png','07_stats/plots/circadian.png']:
    summary.append(f'- {p}')
report=run/'RMB_test_report.txt'
report.write_text('\n'.join(summary)+'\n')
zip_path=root/'test/output/RMB_test_20260603_031850_report_bundle.zip'
with zipfile.ZipFile(zip_path,'w',zipfile.ZIP_DEFLATED) as z:
    for f in run.rglob('*'):
        if f.is_file():
            z.write(f, f.relative_to(run.parent))
print(report)
print(zip_path)
print('\n'.join(summary))
