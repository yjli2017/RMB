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

# Install Bioconductor packages (file currently empty - needs to be populated)
Rscript install_bioc_packages.R
```

### Alternative Setup Script
```bash
# Use the setup script
./setup_environment.sh
```

### Manual R Package Installation
Required packages: tidyverse, ggplot2, ComplexHeatmap (Bioconductor), circlize, lubridate, IRkernel

## Core Architecture

### MonitorS4 Object System
The package centers around a custom S4 class with four main slots:
- `meta.data`: Metadata dataframe (one row per fly)
- `assays`: List of data types (`mt` motor activity, `ct` continuous tracking, `pn` positional data, `raw_data`)
- `active.assay`: Currently selected assay
- `time`: Temporal data and light/dark cycle information

### Directory Structure
- `src/`: Core utility functions organized into modules
- `template/`: CSV templates for metadata
- `test/`: Sample data and test files with expected outputs
- Data organization: `./data/[experiment_name]/` with Monitor*.txt files and metadata/ subfolder

### Key Source Files
- `src/plotting_utilities.R`: Visualization functions
- `src/process_monitorS4_data.R`: Data processing workflows  
- `src/mergeMonitorS4.R`: Combining multiple monitor objects
- `src/remove_dead_flies.R`: Data filtering
- `src/*_utilities.R`: Various data transformation utilities

## Analysis Workflow

### Standard Pipeline
1. Organize Monitor*.txt files in experiment folders
2. Create/generate metadata CSV files using template
3. Create MonitorS4 objects: `create_monitorS4()`
4. Process assays: `process_monitorS4_data()`
5. Filter data: `remove_dead_flies()`
6. Analysis and visualization with plotting utilities
7. Export results as CSV/RDS files

### Metadata Management
- Location: `./data/[experiment_name]/metadata/[Monitor_name]_metadata.csv`
- Template: `./template/monitor_metadata_template.csv`
- Contains experimental conditions, genotypes, environmental data

## Testing

Use files in `./test/` directory:
- Monitor36.txt through Monitor41.txt (sample data)
- Pre-configured metadata files
- Expected output files for validation

## Common Commands

### R Environment Check
```r
# Verify required packages
required_packages <- c("tidyverse", "ggplot2", "ComplexHeatmap", "circlize")
missing_packages <- required_packages[!sapply(required_packages, requireNamespace, quietly = TRUE)]
if (length(missing_packages) == 0) cat("✓ All required packages installed\n")
```

### Basic Object Creation
```r
# Load utilities
source("./src/plotting_utilities.R")
source("./src/process_monitorS4_data.R")

# Create and process monitor object
monitor <- create_monitorS4("path/to/Monitor_file.txt")
monitor <- process_monitorS4_data(monitor)
monitor <- remove_dead_flies(monitor)
```

## Development Notes

- The codebase includes both R scripts and Jupyter notebooks (`.ipynb` files)
- Some setup files like `install_bioc_packages.R` are currently empty placeholders
- Output files are designed for GraphPad Prism, Excel, and other analysis tools
- The package supports multi-experiment integration through `mergeMonitorS4()`
- Both Python and R analysis workflows are supported (see notebooks)