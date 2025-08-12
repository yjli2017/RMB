# Script to generate roxygen2 documentation

# Install and load roxygen2 if needed
if (!require(roxygen2, quietly = TRUE)) {
  install.packages('roxygen2', repos = 'https://cran.r-project.org')
}

library(roxygen2)

# Generate documentation
roxygenise()

cat("Documentation generation complete!\n")