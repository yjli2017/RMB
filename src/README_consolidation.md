# RMB Source Code Refactoring

## Changes Made (2026-01-27 by Catbot)

### New Files Created

1. **constants.R** - Centralized constants (no more magic numbers)
   - `MINUTES_PER_DAY`, `DAM_CHANNELS`, `DAM_POSITIONS`
   - `SLEEP_THRESHOLD_MINUTES`, default thresholds

2. **MonitorClass.R** - Proper S4 class definition
   - `MonitorS4` class with slots: assays, meta.data, time, monitor_name, file_path
   - `createMonitor()` - Create object from file
   - `processMonitor()` - Parse DAM data into assays
   - `mergeMonitors()` - Combine multiple monitors
   - `removeDeadFlies()` - Filter inactive channels

3. **transforms.R** - Consolidated data transformations
   - `activityToAwake()` - Activity to binary awake/sleep
   - `awakeToSleep()` - Flip awake to sleep binary
   - `positionFrequency()` - Count position occurrences
   - `positionProportion()` - Convert to proportions
   - `maskByAwake()` - Position data only when awake
   - `binTimeSeries()` - Aggregate into bins
   - `dailySummary()` - Split by day
   - `splitLightDark()` - Separate light/dark periods
   - `normalizeData()` - Z-score, min-max, proportion normalization
   - Legacy aliases: `convert_sequences`, `flip_binary`, `df_to_freq_dist`

4. **plotting.R** - Consolidated plotting functions
   - `prepareBarPlotData()` - Prepare data for ggplot
   - `calculateAvgProportion()` - Summary stats by group
   - `createBarPlot()` - Bar plot with error bars
   - `plotTimeSeries()` - Time series line plot
   - `plotBinned()` - 30-minute binned data plot
   - `plotLightSwitches()` - Visualize L/D switches
   - `plotPositionHeatmap()` - Position frequency heatmap

### Updated Files

1. **config/config.R** - Cleaner configuration
   - Auto-detect base directory
   - `load_rmb()` function to source all code
   - `load_required_packages()` for dependencies
   - Default parameters centralized

### New Entry Point

- **analysis.R** - Clean main analysis script
  - Uses new S4 class and functions
  - Well-commented sections
  - Generates all outputs (heatmaps, CSVs, plots)

### Legacy Files (can be removed)

These files are superseded by the new consolidated code:

- `bar_plot.R` → use `plotting.R`
- `plotting_utilities.R` → use `plotting.R`
- `data_transformations.R` → use `transforms.R`
- `convert_sequence.R` → use `transforms.R`
- `flip_binary.R` → use `transforms.R`
- `df_to_freq_dist.R` → use `transforms.R`
- `process_monitorS4_data.R` → use `MonitorClass.R`
- `mergeMonitorS4.R` → use `MonitorClass.R`
- `remove_dead_flies.R` → use `MonitorClass.R`

### Key Improvements

1. **No more magic numbers** - All constants in `constants.R`
2. **Proper S4 class** - Real S4 class instead of fake @ operator
3. **No duplicate code** - Single source of truth for each function
4. **Cleaner API** - Consistent function naming (camelCase)
5. **Better documentation** - Roxygen-style comments
6. **Backward compatibility** - Legacy function aliases maintained
