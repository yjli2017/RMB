#!/usr/bin/env R

# Simple test runner for RMB metadata generation
# This script demonstrates how to test the metadata generation functionality

# Set working directory to RMB root (adjust if needed)
# setwd("/path/to/RMB")

cat("RMB Metadata Generation Test Runner\n")
cat("==================================\n\n")

# Check if we're in the right directory
if (!file.exists("./src/metadata.R")) {
  stop("Please run this script from the RMB root directory where src/metadata.R exists")
}

# Source the test script
tryCatch({
  source("./test/generate_metadata_test.R")
}, error = function(e) {
  cat("Error loading test script:", conditionMessage(e), "\n")
  stop("Failed to load test script")
})

# Run the tests
cat("Running automated tests...\n")
results <- tryCatch({
  run_metadata_tests()
}, error = function(e) {
  cat("Error running tests:", conditionMessage(e), "\n")
  return(NULL)
})

# Additional validation
if (!is.null(results) && !is.null(results[[1]])) {
  cat("\nRunning additional validation...\n")
  validate_metadata_structure(results[[1]])
  
  # Check if files were actually created
  test_metadata_dir <- "./test/metadata"
  if (dir.exists(test_metadata_dir)) {
    metadata_files <- list.files(test_metadata_dir, pattern = "_metadata.csv")
    cat("Created metadata files:\n")
    for (file in metadata_files) {
      cat("  -", file, "\n")
    }
  }
} else {
  cat("Tests failed or returned no results.\n")
}

cat("\nTest run completed!\n")
