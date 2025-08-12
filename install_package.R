# Install RMB Package
# Run this script to install the RMB package and all its dependencies

# Check for required packages and install if missing
required_packages <- c("devtools", "roxygen2")

for (pkg in required_packages) {
  if (!require(pkg, character.only = TRUE, quietly = TRUE)) {
    cat("Installing", pkg, "...\n")
    install.packages(pkg, repos = "https://cran.r-project.org")
  }
}

# Install package dependencies
cat("Installing package dependencies...\n")
source("install_deps.R")

# Install the RMB package
cat("Installing RMB package...\n")
devtools::install(dependencies = TRUE, upgrade = "never")

cat("✅ RMB package installation complete!\n")
cat("\nTo get started:\n")
cat("library(RMB)\n")
cat("data <- loadRMB(data = system.file('extdata', package = 'RMB'), \n")
cat("               metadata = file.path(system.file('extdata', package = 'RMB'), 'metadata'))\n")
cat("generateReport(data)\n")