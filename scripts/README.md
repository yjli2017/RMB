# RMB Analysis Scripts

This directory contains step-by-step scripts for RMB data processing, allowing you to run each stage of the analysis pipeline independently or use the complete automated pipeline.

## Quick Start

### Option 1: Complete Automated Pipeline (Recommended)
Use the `loadRMB()` function for streamlined analysis:

```r
# In R console
library(tidyverse)
source("./R/utilities.R")
source("./R/loadRMB.R")

# Load and process data in one step
monitor_obj <- loadRMB(
  data = "./inst/extdata/Monitor36.txt",
  metadata = "./metadata/Monitor36_metadata.csv",
  analysis_days = c(2,3,4),
  remove_dead = TRUE
)
```

### Option 2: Complete Pipeline Script
Run the entire pipeline from command line:

```bash
Rscript scripts/run_complete_pipeline.R \
  ./inst/extdata/Monitor36.txt \
  ./metadata/Monitor36_metadata.csv \
  "2,3,4" 24 ./results/
```

### Option 3: Step-by-Step Processing
Run individual steps for custom analysis:

```bash
# Step 1: Load raw data
Rscript scripts/step1_load_raw_data.R \
  ./inst/extdata/Monitor36.txt \
  ./metadata/Monitor36_metadata.csv \
  ./results/

# Step 2: Clean to specific days
Rscript scripts/step2_clean_days.R \
  ./results/Monitor36_raw.rds \
  "2,3,4" ./results/

# Step 3: Detect dead flies
Rscript scripts/step3_detect_mortality.R \
  ./results/Monitor36_days_cleaned.rds \
  24 ./results/

# Step 4: Filter to alive flies
Rscript scripts/step4_filter_alive.R \
  ./results/Monitor36_with_mortality.rds \
  ./results/
```

## Script Details

### `step1_load_raw_data.R`
**Purpose:** Load and parse raw TriKinetics monitor data without any processing.

**Input:** 
- Monitor*.txt file (TriKinetics format)
- Metadata CSV file

**Output:** 
- `*_raw.rds` file containing unprocessed MonitorS4 object
- Data integrity report

**Use when:** You want to inspect raw data or perform custom processing steps.

```bash
Rscript step1_load_raw_data.R <data_file> <metadata_file> [output_dir]
```

### `step2_clean_days.R`
**Purpose:** Extract specific full days from the dataset for analysis.

**Input:** 
- `*_raw.rds` file from step 1
- Analysis days (comma-separated, default: "2,3,4")

**Output:** 
- `*_days_cleaned.rds` file with cleaned temporal data
- Day-by-day data point report

**Use when:** You want to analyze different time periods or verify day extraction.

```bash
Rscript step2_clean_days.R <input_rds> [analysis_days] [output_dir]
```

### `step3_detect_mortality.R`
**Purpose:** Identify dead flies using final day activity analysis.

**Input:** 
- `*_days_cleaned.rds` file from step 2
- No-movement threshold in hours (default: 24)

**Output:** 
- `*_with_mortality.rds` file with mortality information
- `*_mortality_report.csv` detailed mortality breakdown

**Use when:** You want to use different mortality thresholds or inspect mortality patterns.

```bash
Rscript step3_detect_mortality.R <input_rds> [threshold_hours] [output_dir]
```

### `step4_filter_alive.R`
**Purpose:** Remove dead flies from dataset, keeping only alive flies for analysis.

**Input:** 
- `*_with_mortality.rds` file from step 3

**Output:** 
- `*_processed.rds` final analysis-ready dataset

**Use when:** You want to inspect the filtering process or keep dead flies in the dataset.

```bash
Rscript step4_filter_alive.R <input_rds> [output_dir]
```

### `run_complete_pipeline.R`
**Purpose:** Execute the entire processing pipeline in one script.

**Input:** 
- Raw Monitor*.txt file
- Metadata CSV file
- All processing parameters

**Output:** 
- Final `*_processed.rds` file
- Mortality report CSV
- Processing summary CSV

**Use when:** You want automated processing with custom parameters.

```bash
Rscript run_complete_pipeline.R <data_file> <metadata_file> [analysis_days] [threshold_hours] [output_dir]
```

## File Naming Convention

The scripts use a consistent naming convention to track processing stages:

```
Monitor36.txt                    # Raw data input
Monitor36_raw.rds               # Step 1 output
Monitor36_days_cleaned.rds      # Step 2 output  
Monitor36_with_mortality.rds    # Step 3 output
Monitor36_processed.rds         # Step 4 output (final)
Monitor36_mortality_report.csv  # Mortality breakdown
Monitor36_processing_summary.csv # Pipeline summary
```

## Default Parameters

| Parameter | Default | Description |
|-----------|---------|-------------|
| Analysis Days | 2,3,4 | Full days to include (skip day 1 for acclimation) |
| Mortality Threshold | 24 hours | No-movement period to consider dead |
| Output Directory | ./results | Directory for processed files |

## Processing Overview

```
Raw Data (Monitor*.txt + metadata.csv)
    ↓ step1_load_raw_data.R
Raw MonitorS4 Object (*_raw.rds)
    ↓ step2_clean_days.R  
Cleaned Days (*_days_cleaned.rds)
    ↓ step3_detect_mortality.R
With Mortality Info (*_with_mortality.rds)
    ↓ step4_filter_alive.R
Final Dataset (*_processed.rds)
```

## Common Use Cases

### 1. Standard Analysis (Days 2-4, 24h threshold)
```bash
Rscript run_complete_pipeline.R data.txt metadata.csv
```

### 2. Extended Analysis Period (Days 3-5)
```bash
Rscript run_complete_pipeline.R data.txt metadata.csv "3,4,5"
```

### 3. Strict Mortality Threshold (48 hours)
```bash
Rscript run_complete_pipeline.R data.txt metadata.csv "2,3,4" 48
```

### 4. Custom Day Selection
```bash
# Process days 1,2,3 instead of 2,3,4
Rscript step2_clean_days.R raw_data.rds "1,2,3"
```

### 5. Keep Dead Flies in Analysis
```bash
# Skip step 4 - use *_with_mortality.rds directly
Rscript step3_detect_mortality.R cleaned_data.rds 24
# Analyze with mortality flags in metadata
```

## Output Verification

Each script provides verification output:

- **Data point counts:** Verify expected 1440 points per day
- **Channel counts:** Confirm channel numbers match metadata
- **File sizes:** Typical processed files are 0.3-0.7 MB
- **Mortality rates:** Check for reasonable mortality (typically <30%)

## Troubleshooting

### Common Issues

1. **"Not enough full days":** Your data may have partial days
   ```bash
   # Check what days are available
   Rscript step1_load_raw_data.R data.txt metadata.csv
   # Use available full days
   Rscript step2_clean_days.R raw.rds "1,2"
   ```

2. **"Metadata file not found":** Generate metadata first
   ```r
   source("./R/metadata-management.R")
   generate_metadata("./Monitor36.txt", output_dir = "./metadata/")
   ```

3. **"Channel count mismatch":** Metadata channels don't match data
   - Verify metadata has correct Channel column (1-32)
   - Check for duplicate or missing channel numbers

### Performance Notes

- Processing time: ~30 seconds per monitor
- Memory usage: Scales with number of time points × flies
- Disk space: ~0.5 MB per processed monitor
- Recommend processing monitors individually for large experiments

## Integration with Main Package

These scripts complement the main `loadRMB()` function:

- **Use `loadRMB()`** for standard analysis workflows
- **Use scripts** when you need:
  - Custom processing parameters
  - Step-by-step debugging
  - Batch processing multiple monitors
  - Integration with external pipelines
  - Quality control at each stage