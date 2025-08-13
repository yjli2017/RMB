#!/usr/bin/env Rscript
#' Step 2: Clean Monitor Data to Specific Days
#'
#' @description 
#' This script cleans monitor data to extract specific full days for analysis.
#' Typically uses days 2-4 (skipping first day for acclimation).
#'
#' @usage
#' Rscript step2_clean_days.R <input_rds> [analysis_days] [output_dir]
#'
#' @examples
#' Rscript step2_clean_days.R ./results/Monitor36_raw.rds "2,3,4" ./results/
#' Rscript step2_clean_days.R ./results/Monitor36_raw.rds "3,4,5" ./results/
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
  cat("Usage: Rscript step2_clean_days.R <input_rds> [analysis_days] [output_dir]\n")
  cat("Example: Rscript step2_clean_days.R ./results/Monitor36_raw.rds \"2,3,4\" ./results/\n")
  cat("Default analysis_days: 2,3,4\n")
  quit(status = 1)
}

input_rds <- args[1]
analysis_days_str <- if (length(args) >= 2) args[2] else "2,3,4"
output_dir <- if (length(args) >= 3) args[3] else "./results"

# Parse analysis days
analysis_days <- as.numeric(strsplit(analysis_days_str, ",")[[1]])

# Validate inputs
if (!file.exists(input_rds)) {
  stop("Input RDS file does not exist: ", input_rds)
}

if (any(is.na(analysis_days)) || length(analysis_days) == 0) {
  stop("Invalid analysis days format. Use comma-separated numbers like '2,3,4'")
}

# Create output directory if it doesn't exist
if (!dir.exists(output_dir)) {
  dir.create(output_dir, recursive = TRUE)
  cat("Created output directory:", output_dir, "\n")
}

cat("=== Step 2: Clean Monitor Data to Specific Days ===\n")
cat("Input file:", input_rds, "\n")
cat("Analysis days:", paste(analysis_days, collapse = ", "), "\n")
cat("Output directory:", output_dir, "\n\n")

# Step 1: Load raw MonitorS4 object
cat("Loading raw MonitorS4 object...\n")
monitor_obj <- readRDS(input_rds)

if (!is(monitor_obj, "MonitorS4")) {
  stop("Input file does not contain a MonitorS4 object")
}

# Verify this is a raw object
if (is.null(monitor_obj@time$processing_step) || monitor_obj@time$processing_step != "raw_data_loaded") {
  warning("Input object may not be a raw data object. Processing anyway...")
}

# Step 2: Analyze current data structure
cat("Analyzing current data structure...\n")
mt_data <- monitor_obj@assays[["mt"]]
dates <- as.Date(mt_data$DateTime)
unique_dates <- sort(unique(dates))

cat("Current data span:", length(unique_dates), "days\n")
full_days <- c()
full_day_counter <- 0

for (i in seq_along(unique_dates)) {
  date <- unique_dates[i]
  points_in_day <- sum(dates == date)
  is_full_day <- points_in_day == 1440
  
  if (is_full_day) {
    full_day_counter <- full_day_counter + 1
    full_days <- c(full_days, i)
    status <- paste("(Full Day", full_day_counter, ")")
  } else {
    status <- "(Partial)"
  }
  
  cat("Day", i, ":", as.character(date), "- Points:", points_in_day, status, "\n")
}

# Step 3: Validate analysis days are available
cat("\nValidating analysis days...\n")
if (length(full_days) < max(analysis_days)) {
  stop("Not enough full days available. Found ", length(full_days), " but need ", max(analysis_days))
}

if (any(analysis_days > length(full_days))) {
  stop("Requested analysis day(s) ", paste(analysis_days[analysis_days > length(full_days)], collapse = ", "), 
       " not available. Only ", length(full_days), " full days found.")
}

# Step 4: Clean data to specified days
cat("Cleaning data to analysis days:", paste(analysis_days, collapse = ", "), "\n")
monitor_obj <- clean_monitor_days(monitor_obj, full_days_to_include = analysis_days)

# Update processing metadata
monitor_obj@time$processing_step <- "days_cleaned"
monitor_obj@time$cleaning_date <- Sys.time()
monitor_obj@time$cleaning_params <- list(
  analysis_days = analysis_days,
  original_days = length(unique_dates),
  full_days_available = length(full_days)
)

# Step 5: Save cleaned object
input_basename <- tools::file_path_sans_ext(basename(input_rds))
monitor_name <- gsub("_raw$", "", input_basename)
output_file <- file.path(output_dir, paste0(monitor_name, "_days_cleaned.rds"))

saveRDS(monitor_obj, file = output_file)
file_size <- round(file.size(output_file) / (1024^2), 2)

cat("\nCleaned MonitorS4 object saved to:", output_file, "\n")
cat("File size:", file_size, "MB\n")

# Step 6: Print summary
analysis_info <- monitor_obj@time$analysis_period
cleaned_mt_data <- monitor_obj@assays[["mt"]]

cat("\n=== Day Cleaning Summary ===\n")
cat("Monitor name:", monitor_name, "\n")
cat("Analysis period:", analysis_info$days_analyzed, "days\n")
cat("Date range:", paste(analysis_info$date_range, collapse = " to "), "\n")
cat("Total data points:", analysis_info$total_points, "\n")
cat("Expected points (", analysis_info$days_analyzed, " × 1440):", analysis_info$days_analyzed * 1440, "\n")
cat("Data integrity check:", if (analysis_info$total_points == analysis_info$days_analyzed * 1440) "✓ PASS" else "✗ FAIL", "\n")

# Channel verification
channel_cols <- grep("Channel_", colnames(cleaned_mt_data))
cat("Active channels:", length(channel_cols), "\n")
cat("Metadata flies:", nrow(monitor_obj@meta.data), "\n")

cat("\n=== Step 2 Complete ===\n")
cat("Next steps:\n")
cat("- Run step3_detect_mortality.R to identify dead flies\n")
cat("- Or continue with analysis on cleaned data\n")