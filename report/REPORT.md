# RMB Package Analysis Report

**Generated:** 2026-01-27  
**Refactored by:** Catbot 🐱 for Yongjun Li

---

## Executive Summary

I converted the RMB project from scattered scripts into a **proper R package** with:
- 7 commits on the `clawd` branch (all pushed to GitHub)
- Complete R package structure
- One-command pipeline (`runPipeline()`)
- Publication-quality visualizations
- testthat tests and GitHub Actions CI

### Test Data Results
- **Monitors loaded:** 6
- **Channels before QC:** 192
- **Channels after QC:** 184 (8 dead flies removed)
- **Recording duration:** 5.8 days (8311 minutes)
- **Mean total activity:** 18,279.9 beam crosses
- **Mean total sleep:** 5,649.5 minutes (68% of time)
- **Pipeline runtime:** 9.5 seconds

---

## What I Did: The Refactoring

### Before (Problems)
- 12+ scattered R files with duplicate functions
- `prepare_data()` defined in BOTH `bar_plot.R` AND `plotting_utilities.R`
- Magic numbers everywhere (1440, 15, 32)
- Fake S4 class using custom `@` operator hack
- Hardcoded paths (`/Users/yongjunli/...`)
- No tests, no documentation

### After (Solutions)

| Problem | Solution |
|---------|----------|
| Duplicate functions | Consolidated into single files |
| Magic numbers | `constants.R` with named values |
| Fake S4 class | Proper S4 `MonitorS4` class |
| Hardcoded paths | Auto-detection + configurable |
| No tests | testthat infrastructure |
| No CI | GitHub Actions workflows |

---

## Package Structure

```
RMB/
├── DESCRIPTION          # Package metadata
├── NAMESPACE            # Exported functions
├── LICENSE              # MIT license
├── R/
│   ├── constants.R      # MINUTES_PER_DAY, DAM_CHANNELS, etc.
│   ├── MonitorClass.R   # S4 class definition
│   ├── transforms.R     # activityToAwake, positionFrequency, etc.
│   ├── plotting.R       # plotTimeSeries, prepareBarPlotData, etc.
│   ├── plotting_advanced.R  # plotActivityRaster, plotSleepProfile
│   ├── themes.R         # theme_rmb, scale_fill_rmb
│   ├── checkpoints.R    # checkpoint, summarizeMonitor
│   ├── pipeline.R       # runPipeline, quickAnalysis
│   └── utils.R          # loadMonitorFiles, saveResults
├── tests/testthat/      # Unit tests
├── vignettes/           # Getting started guide
└── inst/extdata/        # Example data
```

---

## Pipeline Walkthrough

### Step 1: Load Data
Find all Monitor*.txt files and create MonitorS4 objects.

### Step 2: Process Data
Parse DAM format into assays:
- **mt**: Movement (beam crosses per minute)
- **pn**: Position (1-15 for multi-beam)
- **ct**: Cumulative activity

### Step 3: Merge Monitors
Combine multiple monitors into single object.

### Step 4: Quality Control
Remove dead flies (total activity < threshold).

### Step 5: Sleep Analysis
- **Sleep definition**: 5+ consecutive minutes without movement
- Creates `awake` (binary) and `sleep` (binary) assays
- Creates `pn_awake` (position only when awake)

### Step 6: Generate Outputs
- CSV exports (activity_summary, position_frequency)
- RDS files (processed monitor)

### Step 7: Generate Plots
- Activity raster (actogram)
- Sleep profile by group
- Group comparisons
- Distributions

---

## Generated Plots

### 1. Activity Raster (`01_activity_raster.png`)
Actogram showing daily activity patterns. Each row = 1 day.

### 2. Sleep Profile (`02_sleep_profile.png`)
Average sleep across 24h, grouped by phenotype.

### 3. Activity Comparison (`03_activity_comparison.png`)
Bar plot comparing total activity between groups.

### 4. Sleep Comparison (`04_sleep_comparison.png`)
Bar plot comparing total sleep between groups.

### 5. Activity Distribution (`05_activity_distribution.png`)
Histogram of total activity across all flies.

### 6. Sleep Distribution (`06_sleep_distribution.png`)
Histogram of total sleep across all flies.

### 7. Time Series (`07_timeseries.png`)
Raw activity time series for a single fly.

### 8. Position Heatmap (`08_position_heatmap.png`)
Where flies spend time in the tube (multi-beam).

---

## Checkpoints

Analysis state saved at 3 points:

1. **01_after_merge**: Right after combining monitors
2. **02_after_qc**: After removing dead flies
3. **03_after_sleep**: After sleep analysis

Each checkpoint includes:
- `summary.txt`: Text summary of data state
- `monitor.rds`: Full data object
- Diagnostic plots (heatmaps, distributions)

---

## Key Functions

### One-Command Analysis
```r
results <- runPipeline("path/to/data")
```

### Quick Exploratory
```r
monitor <- quickAnalysis("path/to/data")
```

### Manual Step-by-Step
```r
monitor <- createMonitor("Monitor36.txt")
monitor <- processMonitor(monitor)
monitor <- removeDeadFlies(monitor)
monitor@assays$sleep <- awakeToSleep(activityToAwake(monitor@assays$mt))
plotSleepProfile(monitor)
```

---

## Installation

```r
# From GitHub (clawd branch)
devtools::install_github("yjli2017/RMB", ref = "clawd")

# Load
library(RMB)

# Run analysis
results <- runPipeline("your/data/directory")
```

---

## Files in This Report

```
report/
├── REPORT.md                    # This file
├── activity_summary.csv         # Per-fly statistics
├── position_frequency.csv       # Position counts
├── monitor_processed.rds        # Full processed data
├── 01_activity_raster.png
├── 02_sleep_profile.png
├── 03_activity_comparison.png
├── 04_sleep_comparison.png
├── 05_activity_distribution.png
├── 06_sleep_distribution.png
├── 07_timeseries.png
├── 08_position_heatmap.png
├── plots/                       # Pipeline-generated plots
└── checkpoints/                 # Analysis checkpoints
    ├── 01_after_merge/
    ├── 02_after_qc/
    └── 03_after_sleep/
```

---

*Report generated by Catbot 🐱*
