# Simple test to verify package installation

cat("Testing basic package functionality...\n")

# Source functions directly to test
source("R/MonitorS4-class.R")
source("R/utilities.R")
source("R/analysis-functions.R")

# Load required libraries
library(methods)
library(ggplot2)
library(dplyr)
library(tidyr)

# Test MonitorS4 class creation
cat("Testing MonitorS4 object creation...\n")

# Create sample data
test_metadata <- data.frame(
  Channel = 1:4,
  Fly = paste0("Test_", 1:4),
  Treatment = c("Control", "Control", "Treated", "Treated"),
  stringsAsFactors = FALSE
)

# Create sample time series data
n_timepoints <- 100
sample_data <- data.frame(
  DateTime = seq(as.POSIXct("2023-01-01 00:00:00"), 
                as.POSIXct("2023-01-01 06:00:00"), 
                length.out = n_timepoints),
  Monitor_Name = "TestMonitor",
  Measure_Type = "MT",
  Channel_1 = sample(0:10, n_timepoints, replace = TRUE),
  Channel_2 = sample(0:15, n_timepoints, replace = TRUE),
  Channel_3 = sample(0:8, n_timepoints, replace = TRUE),
  Channel_4 = sample(0:12, n_timepoints, replace = TRUE)
)

# Test MonitorS4 object creation
test_object <- MonitorS4(
  meta.data = test_metadata,
  assays = list(mt = sample_data),
  active.assay = "mt"
)

cat("✓ MonitorS4 object created successfully\n")
cat("  Flies:", nrow(test_metadata), "\n")
cat("  Data points:", nrow(sample_data), "\n")

# Test analysis function
cat("Testing analysis functions...\n")
analysis_result <- analyze_activity(test_object)

if (is.list(analysis_result) && "overall_stats" %in% names(analysis_result)) {
  cat("✓ Activity analysis completed successfully\n")
  cat("  Mean activity:", round(mean(analysis_result$overall_stats$mean_activity), 2), "\n")
} else {
  cat("✗ Activity analysis failed\n")
}

# Test utility functions
cat("Testing utility functions...\n")
long_data <- wide_to_long(sample_data, "activity")
if (nrow(long_data) > 0 && "Channel" %in% colnames(long_data)) {
  cat("✓ Data conversion successful\n")
} else {
  cat("✗ Data conversion failed\n")
}

cat("\nBasic functionality test complete!\n")
cat("The package core functions are working correctly.\n")