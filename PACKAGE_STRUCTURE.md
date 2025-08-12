# RMB Package Structure

This document outlines the clean, organized structure of the RMB R package.

## Directory Structure

```
RMB/
├── DESCRIPTION              # Package metadata and dependencies
├── NAMESPACE                # Exported functions and imports
├── LICENSE                  # MIT license
├── README.md                # Package overview and quick start
├── CLAUDE.md                # Claude Code instructions for package
│
├── R/                       # Package source code
│   ├── MonitorS4-class.R    # S4 class definition and methods
│   ├── loadRMB.R            # Main data loading function
│   ├── utilities.R          # Helper and utility functions
│   ├── analysis-functions.R # Analysis functions (activity, sleep, circadian)
│   ├── visualization-functions.R # Plotting and visualization
│   └── report-generation.R # HTML report generation
│
├── man/                     # Generated documentation (roxygen2)
│   ├── *.Rd                 # Individual function documentation
│   └── ...
│
├── inst/                    # Installed files
│   ├── extdata/             # Example data files
│   │   ├── Monitor*.txt     # Sample TriKinetics monitor files
│   │   └── metadata/        # Corresponding metadata CSV files
│   ├── template/            # Templates for user data
│   │   └── monitor_metadata_template.csv
│   └── rmarkdown/           # Report templates
│       └── report_template.Rmd
│
├── vignettes/               # Package tutorials
│   └── RMB-package-tutorial.Rmd
│
├── install_package.R        # Easy installation script
├── quick_start.R            # Quick start example
├── install_deps.R           # Dependency installation
└── install_bioc_packages.R  # Bioconductor packages
```

## Package Components

### Core Functionality
- **loadRMB()**: Single function to load and process monitor data
- **generateReport()**: Automated HTML report generation
- **MonitorS4 class**: Organized data structure similar to Seurat objects

### Analysis Functions
- Activity pattern analysis
- Sleep analysis with customizable parameters  
- Circadian rhythm analysis
- Position tracking and movement analysis
- Comprehensive statistical summaries

### Visualization
- Interactive heatmaps using plotly
- Time series plots and bar charts
- Circadian rhythm visualizations
- Multi-panel dashboard plots

### Documentation
- Complete roxygen2 function documentation
- Package vignette with usage examples  
- README with installation and quick start

## Installation Options

### Option 1: Easy Install (Recommended)
```r
source("install_package.R")
```

### Option 2: Manual Install
```r
devtools::install_local(".")
```

### Option 3: Dependencies Only
```r
source("install_deps.R")
```

## Usage

### Basic Workflow
```r
library(RMB)

# Load data
data <- loadRMB(data = "path/to/data", metadata = "path/to/metadata")

# Generate report  
generateReport(data)
```

### Example with Package Data
```r
# Try with included example data
source("quick_start.R")
```

## File Organization Principles

1. **Standard R Package Layout**: Follows CRAN guidelines
2. **Clear Separation**: Code, data, documentation in appropriate directories
3. **Example Data**: Included in standard `inst/extdata/` location
4. **User-Friendly**: Simple installation and quick start scripts
5. **Professional**: Ready for distribution and sharing

## Development Files Removed

The following development and testing files have been cleaned up:
- Old `src/` directory (functionality moved to `R/`)
- Python environment files (`environment.yml`, `requirements.txt`)
- Development test scripts
- Python analysis outputs
- Old documentation files

This results in a clean, professional R package ready for distribution.