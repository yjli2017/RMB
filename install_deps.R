# Install required dependencies for RMB package

# Required packages for documentation and functionality
required_packages <- c(
  "roxygen2",
  "plotly", 
  "htmlwidgets",
  "DT",
  "viridis",
  "devtools",
  "knitr",
  "rmarkdown",
  "jsonlite",
  "ComplexHeatmap",
  "circlize"
)

# Install missing packages
for (pkg in required_packages) {
  if (!require(pkg, character.only = TRUE, quietly = TRUE)) {
    cat("Installing", pkg, "...\n")
    if (pkg %in% c("ComplexHeatmap", "circlize")) {
      # Install from Bioconductor
      if (!require("BiocManager", quietly = TRUE)) {
        install.packages("BiocManager")
      }
      BiocManager::install(pkg)
    } else {
      install.packages(pkg, repos = 'https://cran.r-project.org')
    }
  }
}

cat("All required packages installed!\n")