# RMB: R Scripts for Analysis of Drosophila TriKinetics Multi-Beam (MB) and Single-Beam (SB) Data

Yongjun Li | 2025-07-09 | <yongjunli2017@gmail.com>

## Introduction

This R package provides comprehensive tools for analyzing Drosophila (fruit fly) TriKinetics (<https://www.trikinetics.com/>) data. The data is collected from the TriKinetics multi-beam (MB) or single-beam (SB) system, which is a high-throughput system to measure Drosophila activity. The package reads raw beam crossing data from text files, converts it into a structured S4 class object (similar to Seurat objects for single cell sequencing), and provides tools for analyzing activity, sleep, and positional changes of individual flies.

Key features:

- **Automated metadata generation** for multiple experiments
- **Modular S4 object system** for data organization and analysis
- **Comprehensive analysis workflows** for activity, sleep, and positional data, potentially for circadian analysis as well
- **Multi-experiment integration** capabilities
- **Visualization tools** for generating publication-ready plots or easy used in graphpad

## Installation

This is a pure R package with no external dependencies beyond R packages. Installation is simple:

1. Install R and Rstdio
2. Download the RMB package from GitHub and unzip it
3. Install required R packages (see Requirements section)
4. Configure the `config.R` file for your environment
5. You're ready to analyze your data!

**Note:** Some R programming experience is recommended for optimal usage.

## Requirements

Required R packages:

- `tidyverse` - Data manipulation and visualization
- `ggplot2` - Plotting (included in tidyverse)
- `tools` - File path utilities

## Quick Start

1. **Configure your environment**: Edit `config/config.R` to set paths and parameters
2. **Prepare your data**: Place experiment data in `./data/[experiment_name]/` folders
3. **Generate metadata**: Use `generate_metadata.R` or create manually from template
4. **Run analysis**: Execute your analysis scripts using the provided functions

## Project Structure

```text
RMB/
├── config/
│   └── config.R                 # Configuration settings
├── src/                         # Core utility functions
│   ├── data_transformations.R   # Data conversion utilities
│   ├── metadata.R               # Metadata management
│   ├── monitor_utilities.R      # MonitorS4 object functions
│   ├── plotting_utilities.R     # Visualization functions
│   └── out_pn.R                 # Output processing workflow
├── template/
│   └── monitor_metadata_template.csv  # Metadata template
├── test/                        # Test data and examples
├── generate_metadata.R          # Automated metadata generation
├── RMB.R                        # Main analysis script
├── RSB.R                        # Single-beam analysis script
└── README.md                    # This file
```

## Data Organization and Metadata

### Experiment Setup

- **Data Location**: Place experiment data in `./data/[experiment_name]/` folders
  - Use descriptive names with dates, e.g., `./data/20240222_aging`
  - Each experiment folder contains Monitor*.txt files and metadata

- **Metadata Management**: Each monitor requires a metadata.csv file
  - **Location**: `./data/[experiment_name]/metadata/[Monitor_name]_metadata.csv`
  - **Template**: Use `./template/monitor_metadata_template.csv` as a starting point
  - **Generation**: Use `generate_metadata.R` for automated creation or edit manually

### Metadata Structure

The metadata.csv file contains essential information for each fly:

- **Monitor Information**: Monitor number, date, user, lab
- **Experimental Details**: Experiment name, phenotype, treatment conditions
- **Environmental Conditions**: Temperature, humidity, light cycle
- **Fly Information**: Genotype, tube type, alive status

### Automated Metadata Generation

The `generate_metadata.R` script provides a streamlined approach to create metadata for multiple experiments:

```r
# Example configuration for automated metadata generation
experiments <- list(
  list(
    name = "experiment_name",
    data_dir = "./data/experiment_folder/",
    date = "YYYYMMDD",
    config_file = "path/to/config.txt",
    phenotype_assignments = list(
      list(monitor = 1, rows = 1:20, phenotype = "genotype1"),
      list(monitor = 1, rows = 21:32, phenotype = "genotype2")
    )
  )
)
```

## Core Functions and Utilities

### Data Transformation (`src/data_transformations.R`)

- `convert_sequences()`: Converts activity sequences for sleep analysis
- `flip_binary()`: Flips binary activity data (0↔1)
- `df_to_freq_dist()`: Converts dataframes to frequency distributions

### Monitor Object Management (`src/monitor_utilities.R`)

- `create_monitorS4()`: Creates MonitorS4 objects from raw data
- `process_monitorS4_data()`: Processes and organizes assay data
- `mergeMonitorS4()`: Combines multiple monitor objects
- `remove_dead_flies()`: Filters out inactive flies
- `select_time_range()`: Extracts specific time periods

### Visualization (`src/plotting_utilities.R`)

- `create_bar_plot()`: Generates activity bar plots with error bars
- `plot_time_series()`: Creates time series visualizations
- `plot_light_switches()`: Visualizes light/dark cycle transitions
- `plot_every_30()`: Generates 30-minute interval plots for sleep analysis

### Metadata Management (`src/metadata.R`)

- `load_monitor_files()`: Loads monitor data files
- `create_metadata_list()`: Initializes metadata structures
- `update_metadata()`: Updates metadata with experiment details
- `write_metadata()`: Outputs metadata to CSV files

## MonitorS4 Object System

The RMB package uses a custom **MonitorS4** S4 class object system, similar to Seurat objects for single cell sequencing. This provides a structured and scalable approach to organize and analyze multi-beam data.

### Object Structure

The MonitorS4 object contains four main slots:

- **`meta.data`**: A data.frame containing metadata for each fly
  - Phenotype information, experimental conditions, fly identifiers
  - One row per fly, columns for different metadata fields

- **`assays`**: A list of data.frames containing different data types
  - `mt`: Motor activity data (beam crossings)
  - `ct`: Continuous tracking data
  - `pn`: Positional data (sleep/wake states)
  - `raw_data`: Original unprocessed data

- **`active.assay`**: Currently selected assay for analysis
  - Allows switching between different data types

- **`time`**: Time information data.frame
  - Contains temporal data (first 10 columns of raw data)
  - Light/dark cycle information, timestamps

### Object Creation Workflow

```r
# Load required functions
source("./src/monitor_utilities.R")
source("./src/metadata.R")

# Create MonitorS4 object from raw data
monitor <- create_monitorS4("path/to/Monitor_file.txt")

# Load and attach metadata
metadata_list <- create_metadata_list(monitor_files)
metadata_list <- update_metadata(monitor_files, metadata_list, date, tube_type, user, experiment)

# Process the data into different assays
monitor <- process_monitorS4_data(monitor)

# Clean data by removing inactive flies
monitor <- remove_dead_flies(monitor)
```

## Analysis Workflow

### Standard Analysis Pipeline

1. **Data Preparation**
   - Organize raw Monitor*.txt files in experiment folders
   - Generate or prepare metadata.csv files
   - Configure experiment parameters

2. **Object Creation**
   - Create MonitorS4 objects from raw data
   - Attach metadata and process assays
   - Filter dead/inactive flies

3. **Data Analysis**
   - Convert activity data to sleep/wake states
   - Calculate frequency distributions
   - Generate time series data

4. **Visualization**
   - Create bar plots for phenotype comparisons
   - Generate time series plots
   - Plot light/dark cycle effects

5. **Output Generation**
   - Export processed data to CSV files
   - Save plots as high-resolution images
   - Store MonitorS4 objects for future analysis

### Example Analysis Session

```r
# 1. Load utilities
source("./src/monitor_utilities.R")
source("./src/data_transformations.R")
source("./src/plotting_utilities.R")

# 2. Process experiment data
data_dir <- "./data/20240222_aging"
monitor_files <- load_monitor_files(data_dir)
metadata_list <- create_metadata_list(monitor_files)

# 3. Create and process monitor objects
monitor <- create_monitorS4(monitor_files[1])
monitor <- process_monitorS4_data(monitor)
monitor <- remove_dead_flies(monitor)

# 4. Analyze and visualize
pn_freq <- df_to_freq_dist(monitor@assays$pn)
df_long <- prepare_data(monitor, pn_freq, data_dir)
results <- calculate_avg_proportion(df_long)
create_bar_plot(results$df_avg, results$df_cat1, "output_plot.png")
```

## Output and Results

### File Structure

After analysis, your experiment folder will have the following structure:

```txt
./data/experiment_name/
├── Monitor36.txt, Monitor37.txt, ...    # Raw data files
├── metadata/                            # Metadata directory
│   ├── Monitor36_metadata.csv          # Individual metadata files
│   ├── Monitor37_metadata.csv
│   └── ...
├── pn_awake_freq_all.csv               # Combined frequency data
├── pn_awake_freq_day1.csv              # Daily frequency data
├── pn_awake_freq_day2.csv, ...
├── pn_awake_freq_long.csv              # Long-format data
├── barplot_pn1_awake_all.png           # Combined bar plot
├── barplot_pn1_awake_day1.png          # Daily bar plots
├── barplot_pn1_awake_day2.png, ...
└── monitor.rds                         # Saved MonitorS4 object
```

### Output Types

#### Data Files (CSV)

- **Frequency distributions**: Activity counts binned by time periods
- **Long-format data**: Prepared for statistical analysis and plotting
- **Daily summaries**: Separate files for each experimental day
- **Combined datasets**: Aggregated results across all timepoints

#### Visualization Files (PNG/PDF)

- **Bar plots**: Mean activity with error bars, grouped by phenotype
- **Time series plots**: Activity patterns over time
- **Light cycle plots**: Showing light/dark transitions
- **Individual data points**: Overlaid on summary statistics

#### R Objects (RDS)

- **MonitorS4 objects**: Complete processed datasets
- **Combined monitors**: Merged data from multiple experiments
- **Analysis results**: Processed data ready for further analysis

### Data Integration

The RMB package supports combining data from multiple experiments:

```r
# Load multiple experiments
monitor_list <- list(
  readRDS("./data/exp1/monitor.rds"),
  readRDS("./data/exp2/monitor.rds")
)

# Merge into combined object
combined_monitor <- mergeMonitorS4(monitor_list)

# Save combined results
saveRDS(combined_monitor, "./data/combined_monitor.rds")
```

### Export for External Analysis

All output files are designed for easy import into external analysis tools:

- **GraphPad Prism**: CSV files can be directly imported
- **Excel**: Standard CSV format compatible with spreadsheet software
- **R/Statistical software**: RDS files preserve all object structure
- **Python**: CSV files easily readable with pandas

## Testing and Examples

The `./test/` folder contains sample data and examples:

- **Sample monitor files**: Monitor36.txt through Monitor41.txt
- **Example metadata**: Pre-configured metadata.csv files
- **Expected outputs**: Reference files for validation
- **Test scripts**: Example analysis workflows

To test the installation:

```r
# Set working directory to RMB folder
setwd("path/to/RMB")

# Source required functions
source("./src/monitor_utilities.R")
source("./src/data_transformations.R")
source("./src/plotting_utilities.R")

# Run test analysis
data_dir <- "./test"
# ... run analysis pipeline ...
```

## Recent Updates (2025-07-09)

### Function Consolidation

- Consolidated 12 individual function files into 3 organized utility files
- Improved code organization and maintainability
- Reduced file clutter while preserving all functionality

### Enhanced Metadata Generation

- Streamlined `generate_metadata.R` with configuration-based approach
- Added error handling and progress tracking
- Simplified adding new experiments

### Improved Documentation

- Updated README with comprehensive usage examples
- Added function reference documentation
- Included testing and validation instructions

## Development and Contributions

The RMB package is actively maintained and welcomes contributions:

### Current Status

- **Stable**: Core functionality for standard analysis workflows
- **Active Development**: Ongoing improvements and new features
- **Community-Driven**: Open to suggestions and contributions

### How to Contribute

1. **Bug Reports**: Submit issues with reproducible examples
2. **Feature Requests**: Suggest new functionality or improvements
3. **Code Contributions**: Submit pull requests with enhancements
4. **Documentation**: Help improve documentation and examples

### Roadmap

- Enhanced visualization options
- Additional statistical analysis functions
- Improved multi-experiment comparison tools
- Integration with other behavioral analysis packages

## Contact and Support

For questions, suggestions, or collaboration:

- **Email**: <yongjunli2017@gmail.com> | <yjli@sas.upenn.edu>
- **GitHub**: Submit issues and pull requests
- **Documentation**: Refer to function documentation and examples

I hope you find the RMB package useful for your Drosophila behavioral analysis needs. The package is designed to be flexible and extensible, allowing you to adapt it to your specific experimental requirements while maintaining robust data organization and analysis capabilities.
