# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

RMB is an R-based analysis package for processing Drosophila (fruit fly) TriKinetics multi-beam (MB) and single-beam (SB) activity data. The package uses a custom MonitorS4 S4 class system similar to Seurat objects for organizing and analyzing behavioral data including activity, sleep, and positional tracking.

## Environment Setup

### Conda Environment (Primary)
```bash
# Create environment
conda env create -f environment.yml
conda activate rmb-analysis

# Install Bioconductor packages (if needed)
Rscript install_bioc_packages.R
```

### Alternative Setup Script
```bash
# Use the setup script
./setup_environment.sh
```

### Manual R Package Installation
Required packages: tidyverse, ggplot2, ComplexHeatmap (Bioconductor), circlize, lubridate, IRkernel, methods

## Core Architecture

### MonitorS4 Object System
The package centers around a custom S4 class with four main slots:
- `meta.data`: Metadata dataframe (one row per fly) with mortality and experimental information
- `assays`: List of data types (`mt` motor activity, `ct` continuous tracking, `pn` positional data, `raw_data`)
- `active.assay`: Currently selected assay
- `time`: Temporal data, light/dark cycle information, and processing metadata

### Directory Structure
- `R/`: Core R functions organized by purpose
  - `utilities.R`: Data processing pipeline functions
  - `analysis-functions.R`: Activity analysis and plotting
  - `visualization-functions.R`: QC and diagnostic plots
  - `loadRMB.R`: Data loading functions
  - `MonitorS4-class.R`: S4 class definition
- `inst/extdata/`: Sample data and metadata files
- `test_results/`: Processed RDS files and analysis outputs
- `inst/template/`: CSV templates for metadata

### Key Processing Pipeline Functions
- `process_monitor_complete()`: Complete processing pipeline for single monitor
- `clean_monitor_days()`: Extract specific full days (default: days 2-4)
- `detect_dead_flies_final_day()`: Mortality detection using 24-hour no-movement criterion
- `filter_to_alive_flies()`: Remove dead flies from analysis
- `save_cleaned_monitor()` / `load_cleaned_monitor()`: Data persistence

## Analysis Workflow

### New Metadata-First Pipeline
1. **Setup Environment**
   ```r
   library(tidyverse)
   library(ggplot2)
   library(lubridate)
   library(methods)
   source("./R/MonitorS4-class.R")
   source("./R/utilities.R")
   source("./R/metadata-management.R")
   source("./R/loadRMB.R")
   source("./R/analysis-functions.R")
   source("./R/visualization-functions.R")
   ```

2. **Metadata Generation and Management**
   ```r
   # Step 1: Generate metadata template
   generate_metadata("./inst/extdata/Monitor36.txt", output_dir = "./metadata/")
   
   # Step 2: (Optional) Update metadata for experimental groups
   update_metadata("./metadata/Monitor36_metadata.csv", 
                   updates = list(Group = "Male_Treatment", Sex = "Male"),
                   channels = 1:16)
   
   # Step 3: (Optional) Batch update multiple monitors
   assignments <- list(
     Monitor36 = list(channels = 1:32, updates = list(Group = "Male_Control", Sex = "Male")),
     Monitor37 = list(channels = 1:32, updates = list(Group = "Female_Control", Sex = "Female"))
   )
   batch_update_metadata("./metadata/", assignments)
   ```

3. **Data Loading and Processing**
   ```r
   # Load data with automatic processing
   monitor_obj <- loadRMB(
     data = "./inst/extdata/Monitor36.txt",
     metadata = "./metadata/Monitor36_metadata.csv",
     remove_dead = TRUE,
     activity_threshold = 1,
     consecutive_zeros = 12,
     analysis_days = c(2,3,4)  # Default 3-day analysis period
   )
   ```

4. **Quality Control Plots**
   ```r
   # Light schedule verification
   plot_light_schedule(monitor_obj, save_plot = TRUE, output_path = "./results/")
   
   # Binary activity QC (dead flies appear as white rows)
   plot_monitor_qc_binary(monitor_obj, save_plot = TRUE, output_path = "./results/")
   
   # Individual fly patterns
   plot_individual_flies(monitor_obj, max_flies = 16, save_plot = TRUE, output_path = "./results/")
   ```

5. **Activity Analysis**
   ```r
   # Group activity timecourse
   plot_group_activity_timecourse(
     monitor_obj, 
     group_by = "Group", 
     bin_minutes = 30, 
     save_plot = TRUE, 
     output_path = "./results/"
   )
   
   # Activity statistics
   activity_stats <- summarize_activity_by_group(monitor_obj, group_by = "Group")
   print(activity_stats)
   ```

6. **Multi-Monitor Analysis**
   ```r
   # Load processed monitors
   monitor_list <- list(
     Monitor36 = load_cleaned_monitor("./results/Monitor36_processed.rds"),
     Monitor37 = load_cleaned_monitor("./results/Monitor37_processed.rds"),
     # ... add more monitors
   )
   
   # Combined analysis plot
   combined_plot <- plot_combined_monitors_activity(
     monitor_list, 
     group_by = "Group", 
     save_plot = TRUE, 
     output_path = "./results/"
   )
   ```

### Metadata Management System
- **New Functions:** `generate_metadata()`, `update_metadata()`, `batch_update_metadata()`
- **Configuration Support:** YAML/JSON config files for experiment setup
- **Template Generation:** Automatic metadata templates with experimental group assignments
- **Location:** User-specified directory (default: `./metadata/`)
- **Required columns:** Fly, Lab, User, Date, Experiment_name, Monitor_type, Monitor_number, Channel, Group, Treatment, Sex
- **Auto-updated:** mortality_status, final_day_activity, total_experiment_activity, mortality_type, included_in_analysis

### Light Schedule Settings
- **Light ON:** 11:00 AM - 11:00 PM (12 hours)
- **Light OFF:** 11:00 PM - 11:00 AM (12 hours)
- **Verification:** Column 10 (Light) should be 1 during light phase, 0 during dark phase

### Data Processing Standards
- **Full Day Definition:** 1440 timepoints (24 hours × 60 minutes)
- **Analysis Period:** Days 2-4 (skip first day for acclimation, skip first full day)
- **Dead Fly Threshold:** 24 hours of no movement in final day
- **Time Binning:** 30-minute intervals for activity analysis
- **Channel Count:** Fixed 32 channels per monitor (columns 11-42 in raw data)
- **Circadian Time Format:** All time series plots use Day1, Day2, Day3 format with 6-hour tick intervals

## Testing Results (Comprehensive)

### Validated Functions
✅ **Metadata Management:** `generate_metadata()`, `update_metadata()`, `batch_update_metadata()` - YAML/JSON config support
✅ **Data Loading:** `loadRMB()` with `analysis_days` parameter - streamlined workflow
✅ **Data Cleaning:** `clean_monitor_days()` - properly identifies and selects full days
✅ **Mortality Detection:** `detect_dead_flies_final_day()` - 24-hour no-movement threshold
✅ **Data Filtering:** Automatic dead fly removal in `loadRMB()`
✅ **Data Persistence:** `save_cleaned_monitor()`, `load_cleaned_monitor()` - RDS format
✅ **QC Plotting:** `plot_light_schedule()`, `plot_monitor_qc_binary()` - visual QC with circadian time
✅ **Activity Analysis:** `plot_group_activity_timecourse()` - 30-min bins with Day1/Day2/Day3 format
✅ **Multi-Monitor:** `plot_combined_monitors_activity()` - combine multiple experiments with circadian time

### Test Data Results (171 total flies analyzed)
- **Monitor36 (Male_CS):** 30/32 flies alive (6.2% mortality)
- **Monitor37 (Female_CS):** 32/32 flies alive (0% mortality)
- **Monitor38 (Male_Gaboxdol):** 25/32 flies alive (21.9% mortality)
- **Monitor39 (Female_Gaboxdol):** 32/32 flies alive (0% mortality)
- **Monitor40 (Male_Caff):** 21/32 flies alive (34.4% mortality)
- **Monitor41 (Female_Caff):** 31/32 flies alive (3.1% mortality)

### Circadian Analysis Results
- **Control Groups:** Light/Dark activity ratio ~1.9 (normal circadian rhythm)
- **Gaboxdol Groups:** Reduced overall activity but maintained circadian rhythm (ratio >1)
- **Caffeine Groups:** Reversed circadian pattern (ratio <1) - higher activity during dark phase

## File Organization

### Input Files
- **Data:** `./inst/extdata/Monitor*.txt` (TriKinetics format, 42 columns)
- **Metadata:** `./inst/extdata/metadata/*_metadata.csv` (experimental design)

### Output Files
- **Processed Data:** `./test_results/*_processed.rds` (analysis-ready MonitorS4 objects)
- **QC Plots:** Light schedule, binary activity heatmaps, individual fly patterns
- **Analysis Plots:** Group activity timecourses, combined monitor visualizations
- **Final Results:** `final_real_group_activity_timecourse.png` (publication-ready)

## Error Handling & Troubleshooting

### Common Issues Fixed
1. **Parsing Error:** Channel count mismatch - fixed to always expect 32 channels
2. **Light Schedule:** Wrong column reference - corrected to column 10 (V10)
3. **Dead Fly Detection:** Wrong timepoint count - fixed to 1440 points (24 hours)
4. **Data Structure:** Metadata column mismatches during merging - handled with proper filtering

### Validation Steps
1. **Light Schedule:** Check column 10 values match expected 12:12 LD cycle
2. **Data Points:** Verify 1440 points per full day, 4320 total for 3-day analysis
3. **Channel Mapping:** Ensure metadata Channel column matches data Channel_* columns
4. **Mortality Detection:** Confirm 24-hour threshold applied to final day only

## Advanced Usage

### Custom Analysis Periods
```r
# Use different days (e.g., days 3-5)
monitor_obj <- clean_monitor_days(monitor_obj, full_days_to_include = c(3,4,5))
```

### Custom Dead Fly Threshold
```r
# Use 48-hour threshold instead of 24-hour
monitor_obj <- detect_dead_flies_final_day(monitor_obj, no_movement_threshold = 48)
```

### Export Analysis Data
```r
# Get activity summary statistics
stats <- summarize_activity_by_group(monitor_obj, group_by = "Group")
write.csv(stats, "./results/activity_summary.csv", row.names = FALSE)

# Generate mortality report
mortality_report <- generate_mortality_report(monitor_obj)
write.csv(mortality_report, "./results/mortality_report.csv", row.names = FALSE)
```

## Important Notes

### Data Requirements
- TriKinetics monitor files must have exactly 42 columns (10 metadata + 32 channels)
- Metadata CSV must include Channel column matching data channels
- Light schedule must be in column 10 of raw data
- Datetime parsing expects "d m y H'M'S" format

### Analysis Assumptions
- 1-minute data intervals
- 12:12 LD light cycle (11 AM - 11 PM light ON)
- 3-day analysis period standard (days 2-4)
- 24-hour no-movement criterion for death
- 30-minute time binning for group analysis

### Performance & Features
- Each processed monitor ~0.3-0.7 MB RDS file
- Processing time ~30 seconds per monitor
- Memory usage scales linearly with number of time points and flies
- Recommend processing monitors individually for large experiments
- **New Features:**
  - Metadata-first workflow with configuration file support
  - `analysis_days` parameter in `loadRMB()` (default: c(2,3,4))
  - Circadian time format (Day1, Day2, Day3) for all time series plots
  - 6-hour tick intervals for better readability
  - Automatic dead fly detection and removal
- we want to have loadRMB for quick analysis of everything, but we also want to have a scripts to run every step seperately
- use conda env to run our scripts
- there are 3 different measure types (MT, CT, Pn) per minutes,so in monitor S4 class they are seperat arrays, I don't know what you are doing here
- basically, our data should be first load, and create monitor object, and then combine monitors into one, then clean the data, then remove the dead flies. when clean the data and remove dead flies, all assays should be updated. Then all flies used for further analysis.