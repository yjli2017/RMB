# RMB Environment Setup Instructions

## Creating the Conda Environment

To create the RMB analysis environment, run the following commands:

```bash
# Create the conda environment
conda env create -f environment.yml

# Activate the environment
conda activate rmb-analysis

# Install Bioconductor packages
Rscript install_bioc_packages.R
```

## Alternative Setup (if conda fails)

If you encounter issues with the conda environment, you can set up the environment manually:

### 1. Install R (version 4.4 or higher)

Download from <https://cran.r-project.org/>

### 2. Install required R packages

```r
# Install CRAN packages
install.packages(c("tidyverse", "ggplot2", "dplyr", "tidyr", 
                   "lubridate", "circlize", "devtools"))

# Install BiocManager
install.packages("BiocManager")

# Install Bioconductor packages
BiocManager::install("ComplexHeatmap")

# Install IRkernel for Jupyter (if using notebooks)
install.packages("IRkernel")
IRkernel::installspec()
```

### 3. Install Jupyter (if using notebooks)

```bash
pip install jupyter
```

## Verifying the Installation

Run the following R code to verify all packages are installed:

```r
required_packages <- c("tidyverse", "ggplot2", "ComplexHeatmap", "circlize")
missing_packages <- required_packages[!sapply(required_packages, requireNamespace, quietly = TRUE)]

if (length(missing_packages) == 0) {
    cat("✓ All required packages are installed\n")
} else {
    cat("✗ Missing packages:", paste(missing_packages, collapse = ", "), "\n")
}
```

## Troubleshooting

### Common Issues

1. **BiocManager installation fails**: Try updating R to the latest version
2. **ComplexHeatmap conflicts**: Try installing with `BiocManager::install("ComplexHeatmap", force = TRUE)`
3. **Conda solver issues**: Try using mamba instead: `mamba env create -f environment.yml`

### Environment Variables

If you encounter issues with Bioconductor, you may need to set:

```bash
export R_BIOC_VERSION="3.18"  # Adjust based on your R version
```
