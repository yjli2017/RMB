#!/usr/bin/env Rscript
#' Step 4: Filter Monitor Data to Alive Flies Only
#'
#' @description 
#' This script removes dead flies from the dataset, keeping only alive flies
#' for downstream analysis. Dead fly channels are removed from all assays.
#'
#' @usage
#' Rscript step4_filter_alive.R <input_rds> [output_dir]
#'
#' @examples
#' Rscript step4_filter_alive.R ./results/Monitor36_with_mortality.rds ./results/
#'

# Load required libraries
suppressPackageStartupMessages({
  library(tidyverse)
  library(lubridate)
  library(methods)
})

# Source required functions
source("./R/MonitorS4-class.R")
source("./R/utilities.R")

# Parse command line arguments
args <- commandArgs(trailingOnly = TRUE)

if (length(args) < 1) {
  cat("Usage: Rscript step4_filter_alive.R <input_rds> [output_dir]\n")
  cat("Example: Rscript step4_filter_alive.R ./results/Monitor36_with_mortality.rds ./results/\n")
  quit(status = 1)
}

input_rds <- args[1]
output_dir <- if (length(args) >= 2) args[2] else "./results"

# Validate inputs
if (!file.exists(input_rds)) {
  stop("Input RDS file does not exist: ", input_rds)
}

# Create output directory if it doesn't exist
if (!dir.exists(output_dir)) {
  dir.create(output_dir, recursive = TRUE)
  cat("Created output directory:", output_dir, "\n")
}

cat("=== Step 4: Filter Monitor Data to Alive Flies Only ===\n")
cat("Input file:", input_rds, "\n")
cat("Output directory:", output_dir, "\n\n")

# Step 1: Load MonitorS4 object with mortality information
cat("Loading MonitorS4 object with mortality information...\n")
monitor_obj <- readRDS(input_rds)

if (!is(monitor_obj, "MonitorS4")) {
  stop("Input file does not contain a MonitorS4 object")
}

# Verify mortality information exists
if (!"mortality_status" %in% colnames(monitor_obj@meta.data)) {
  stop("No mortality information found. Run step3_detect_mortality.R first.")
}

# Step 2: Analyze current mortality status
cat("Analyzing current mortality status...\n")
metadata <- monitor_obj@meta.data
mortality_analysis <- monitor_obj@time$mortality_analysis

total_flies <- nrow(metadata)
alive_flies <- sum(metadata$mortality_status == "alive")
dead_flies <- sum(metadata$mortality_status == "dead")

cat("Current status:\n")
cat("- Total flies:", total_flies, "\n")
cat("- Alive flies:", alive_flies, "\n")
cat("- Dead flies:", dead_flies, "\n")
cat("- Mortality rate:", round(dead_flies / total_flies * 100, 1), "%\n")

if (dead_flies == 0) {
  cat("\nNo dead flies detected. Saving copy of original object...\n")
  # Still process to add filtering metadata
} else {
  cat("\nProceeding to remove", dead_flies, "dead flies...\n")
}

# Step 3: Filter to alive flies only
cat("Filtering data to alive flies...\n")
monitor_obj <- filter_to_alive_flies(monitor_obj)

# Update processing metadata
monitor_obj@time$processing_step <- "alive_filtered"
monitor_obj@time$filtering_date <- Sys.time()
monitor_obj@time$filtering_params <- list(
  original_flies = total_flies,
  alive_flies = alive_flies,
  dead_flies_removed = dead_flies
)

# Step 4: Verify filtering results
cat("\nVerifying filtering results...\n")
filtered_metadata <- monitor_obj@meta.data
mt_data <- monitor_obj@assays[["mt"]]

# Count remaining channels in data
channel_cols <- grep("Channel_", colnames(mt_data))
remaining_channels <- length(channel_cols)

# Count alive flies in metadata
alive_in_metadata <- sum(filtered_metadata$included_in_analysis)

cat("Verification:\n")
cat("- Expected alive flies:", alive_flies, "\n")
cat("- Data channels remaining:", remaining_channels, "\n")
cat("- Metadata alive flags:", alive_in_metadata, "\n")
cat("- Filtering success:", if (remaining_channels == alive_flies) "✓ PASS" else "✗ FAIL", "\n")

# Step 5: Generate filtered data summary
if ("Group" %in% colnames(filtered_metadata)) {
  cat("\n=== Alive Flies by Group ===\n")
  groups <- unique(filtered_metadata$Group)
  groups <- groups[!is.na(groups)]
  
  for (group in groups) {
    group_data <- filtered_metadata[filtered_metadata$Group == group & !is.na(filtered_metadata$Group), ]
    alive_in_group <- sum(group_data$included_in_analysis)
    total_in_group <- nrow(group_data)
    
    cat("Group:", group, "\n")
    cat("  Total flies:", total_in_group, "\n")
    cat("  Alive flies for analysis:", alive_in_group, "\n")
    cat("  Survival rate:", round(alive_in_group / total_in_group * 100, 1), "%\n\n")
  }
}

# Step 6: Save filtered object
input_basename <- tools::file_path_sans_ext(basename(input_rds))
monitor_name <- gsub("_(raw|days_cleaned|with_mortality)$", "", input_basename)
output_file <- file.path(output_dir, paste0(monitor_name, "_processed.rds"))

# Use the existing save function
save_cleaned_monitor(monitor_obj, output_file, overwrite = TRUE)

# Step 7: Generate final analysis summary
cat("\n=== Final Analysis Dataset Summary ===\n")
cat("Monitor name:", monitor_name, "\n")
cat("Processing complete: Raw → Days cleaned → Mortality detected → Alive filtered\n")

if (!is.null(monitor_obj@time$analysis_period)) {
  analysis_period <- monitor_obj@time$analysis_period
  cat("Analysis period:", analysis_period$days_analyzed, "days\n")
  cat("Date range:", paste(analysis_period$date_range, collapse = " to "), "\n")
  cat("Data points per fly:", analysis_period$total_points, "\n")
}

cat("Flies ready for analysis:", remaining_channels, "\n")
cat("Total data points:", remaining_channels * nrow(mt_data), "\n")

# Calculate data reduction
original_data_points <- total_flies * nrow(mt_data)
final_data_points <- remaining_channels * nrow(mt_data)
reduction_percent <- round((1 - final_data_points / original_data_points) * 100, 1)

cat("Data reduction:", reduction_percent, "% (removed dead flies)\n")

cat("\n=== Step 4 Complete ===\n")
cat("Dataset ready for analysis:\n")
cat("- File:", output_file, "\n")
cat("- Load with: load_cleaned_monitor('", output_file, "')\n")
cat("- Or use analysis functions directly on the object\n")