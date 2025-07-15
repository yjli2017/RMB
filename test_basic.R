#!/usr/bin/env Rscript

# Simple test script to verify metadata generation
# Author: Updated for RMB workflow test

cat("RMB Test Script - Metadata Generation Test\n")
cat("==========================================\n\n")

# Set working directory
setwd("/scr1/users/liy27/20250709_RMB/dep/RMB")

# Test 1: Generate metadata using the new workflow
cat("Step 1: Testing metadata generation...\n")
tryCatch({
  source("./generate_metadata_test.R")
  cat("✅ Metadata generation completed successfully!\n\n")
}, error = function(e) {
  cat("❌ Error in metadata generation:", e$message, "\n")
  stop("Metadata generation failed")
})

# Test 2: Load basic functions and verify metadata files exist
cat("Step 2: Verifying metadata files...\n")
tryCatch({
  source("./src/metadata.R")
  
  # Check if metadata files were created
  metadata_dir <- "./test/metadata"
  if (dir.exists(metadata_dir)) {
    metadata_files <- list.files(metadata_dir, pattern = "_metadata.csv")
    cat("Found", length(metadata_files), "metadata files:\n")
    for (file in metadata_files) {
      cat("  ✓", file, "\n")
    }
    
    # Check content of first metadata file
    if (length(metadata_files) > 0) {
      first_file <- file.path(metadata_dir, metadata_files[1])
      test_metadata <- read.csv(first_file)
      cat("✅ Sample metadata structure:\n")
      cat("  Columns:", paste(colnames(test_metadata), collapse = ", "), "\n")
      cat("  Rows:", nrow(test_metadata), "\n")
      cat("  Sample Groups:", paste(unique(test_metadata$Group), collapse = ", "), "\n")
      cat("  Sample Treatments:", paste(unique(test_metadata$Treatment), collapse = ", "), "\n")
      if ("Sex" %in% colnames(test_metadata)) {
        cat("  Sample Sex:", paste(unique(test_metadata$Sex), collapse = ", "), "\n")
      }
    }
  } else {
    stop("Metadata directory not found")
  }
  
  cat("\n✅ Metadata verification completed successfully!\n\n")
}, error = function(e) {
  cat("❌ Error in metadata verification:", e$message, "\n")
  stop("Metadata verification failed")
})

# Test 3: Load monitor files and basic processing
cat("Step 3: Testing monitor file loading...\n")
tryCatch({
  # Load monitor files
  monitor_list <- load_monitor_files("./test/")
  cat("✅ Found", length(monitor_list), "monitor files\n")
  
  # Test file names
  for (i in 1:length(monitor_list)) {
    file_name <- basename(monitor_list[i])
    cat("  ✓", file_name, "\n")
  }
  
  cat("\n✅ Monitor file loading completed successfully!\n\n")
}, error = function(e) {
  cat("❌ Error in monitor file loading:", e$message, "\n")
  stop("Monitor file loading failed")
})

cat("🎉 All tests completed successfully! 🎉\n")
cat("The RMB workflow is ready to use.\n")
cat("\nTo run the full analysis with visualizations, ensure the following packages are installed:\n")
cat("- tidyverse\n")
cat("- lubridate\n")
cat("- ComplexHeatmap\n")
cat("- circlize\n")
cat("- ggplot2\n")
cat("- rstudioapi\n")
cat("\nThen run: Rscript RMB_test.R\n")
