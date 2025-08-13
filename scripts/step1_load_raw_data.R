#!/usr/bin/env Rscript
#' Step 1: Load Raw Data
#'
#' @description 
#' This script loads raw TriKinetics monitor data and metadata without any processing.
#' Use this when you want to inspect raw data or perform custom processing steps.
#'
#' @usage
#' Rscript step1_load_raw_data.R <data_file> <metadata_file> [output_dir]
#'
#' @examples
#' Rscript step1_load_raw_data.R ./inst/extdata/Monitor36.txt ./metadata/Monitor36_metadata.csv ./results/
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
source("./R/loadRMB.R")

# Parse command line arguments
args <- commandArgs(trailingOnly = TRUE)

if (length(args) < 2) {
  cat("Usage: Rscript step1_load_raw_data.R <data_file> <metadata_file> [output_dir]\n")
  cat("Example: Rscript step1_load_raw_data.R ./inst/extdata/Monitor36.txt ./metadata/Monitor36_metadata.csv ./results/\n")
  quit(status = 1)
}

data_file <- args[1]
metadata_file <- args[2]
output_dir <- if (length(args) >= 3) args[3] else "./results"

# Validate inputs
if (!file.exists(data_file)) {
  stop("Data file does not exist: ", data_file)
}

if (!file.exists(metadata_file)) {
  stop("Metadata file does not exist: ", metadata_file)
}

# Create output directory if it doesn't exist
if (!dir.exists(output_dir)) {
  dir.create(output_dir, recursive = TRUE)
  cat("Created output directory:", output_dir, "\n")
}

cat("=== Step 1: Load Raw Data ===\n")
cat("Data file:", data_file, "\n")
cat("Metadata file:", metadata_file, "\n")
cat("Output directory:", output_dir, "\n\n")

# Step 1: Parse monitor data file
cat("Parsing monitor data file...\n")
raw_data <- parse_monitor_file(data_file)
cat("Raw data dimensions:", nrow(raw_data), "rows x", ncol(raw_data), "columns\n")

# Step 2: Process into assay types
cat("Processing into assay types...\n")
assays <- process_monitor_assays(raw_data)
cat("Available assays:", paste(names(assays), collapse = ", "), "\n")

# Step 3: Load metadata
cat("Loading metadata...\n")
metadata_df <- readr::read_csv(metadata_file, show_col_types = FALSE)
cat("Metadata dimensions:", nrow(metadata_df), "rows x", ncol(metadata_df), "columns\n")

# Step 4: Create time information
cat("Creating time information...\n")
time_info <- list(
  start_time = min(raw_data$DateTime, na.rm = TRUE),
  end_time = max(raw_data$DateTime, na.rm = TRUE),
  duration_hours = as.numeric(difftime(max(raw_data$DateTime, na.rm = TRUE), 
                                     min(raw_data$DateTime, na.rm = TRUE), 
                                     units = "hours")),
  time_points = unique(raw_data$DateTime),
  light_cycle = unique(raw_data$Light)
)

cat("Time range:", format(time_info$start_time), "to", format(time_info$end_time), "\n")
cat("Duration:", round(time_info$duration_hours, 1), "hours\n")
cat("Total time points:", length(time_info$time_points), "\n")

# Step 5: Create MonitorS4 object
cat("Creating MonitorS4 object...\n")
monitor_obj <- MonitorS4(
  meta.data = metadata_df,
  assays = assays,
  active.assay = "mt",
  time = time_info
)

# Add processing metadata
monitor_obj@time$processing_step <- "raw_data_loaded"
monitor_obj@time$processing_date <- Sys.time()

# Step 6: Save raw object
monitor_name <- tools::file_path_sans_ext(basename(data_file))
output_file <- file.path(output_dir, paste0(monitor_name, "_raw.rds"))

saveRDS(monitor_obj, file = output_file)
file_size <- round(file.size(output_file) / (1024^2), 2)

cat("\nRaw MonitorS4 object saved to:", output_file, "\n")
cat("File size:", file_size, "MB\n")

# Step 7: Print summary
cat("\n=== Raw Data Summary ===\n")
cat("Monitor name:", monitor_name, "\n")
cat("Total flies:", nrow(metadata_df), "\n")
cat("Available assays:", paste(names(assays), collapse = ", "), "\n")
cat("MT data points:", nrow(assays$mt), "\n")
if (!is.null(assays$ct) && nrow(assays$ct) > 0) {
  cat("CT data points:", nrow(assays$ct), "\n")
}
if (!is.null(assays$pn) && nrow(assays$pn) > 0) {
  cat("PN data points:", nrow(assays$pn), "\n")
}

# Check for data integrity
dates <- as.Date(assays$mt$DateTime)
unique_dates <- sort(unique(dates))
cat("\nData span:", length(unique_dates), "days\n")
for (i in seq_along(unique_dates)) {
  date <- unique_dates[i]
  points_in_day <- sum(dates == date)
  status <- if (points_in_day == 1440) "(Full Day)" else "(Partial)"
  cat("Day", i, ":", as.character(date), "- Points:", points_in_day, status, "\n")
}

cat("\n=== Step 1 Complete ===\n")
cat("Next steps:\n")
cat("- Run step2_clean_days.R to extract specific analysis days\n")
cat("- Or use loadRMB() for complete automated processing\n")