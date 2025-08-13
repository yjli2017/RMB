#!/usr/bin/env Rscript
#' Step 3: Detect Dead Flies Using Final Day Analysis
#'
#' @description 
#' This script analyzes the final day of the experiment to identify dead flies
#' using a no-movement threshold (default: 24 hours of no activity).
#'
#' @usage
#' Rscript step3_detect_mortality.R <input_rds> [threshold_hours] [output_dir]
#'
#' @examples
#' Rscript step3_detect_mortality.R ./results/Monitor36_days_cleaned.rds 24 ./results/
#' Rscript step3_detect_mortality.R ./results/Monitor36_days_cleaned.rds 48 ./results/
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
  cat("Usage: Rscript step3_detect_mortality.R <input_rds> [threshold_hours] [output_dir]\n")
  cat("Example: Rscript step3_detect_mortality.R ./results/Monitor36_days_cleaned.rds 24 ./results/\n")
  cat("Default threshold_hours: 24\n")
  quit(status = 1)
}

input_rds <- args[1]
threshold_hours <- if (length(args) >= 2) as.numeric(args[2]) else 24
output_dir <- if (length(args) >= 3) args[3] else "./results"

# Validate inputs
if (!file.exists(input_rds)) {
  stop("Input RDS file does not exist: ", input_rds)
}

if (is.na(threshold_hours) || threshold_hours <= 0) {
  stop("Invalid threshold hours. Must be a positive number.")
}

# Create output directory if it doesn't exist
if (!dir.exists(output_dir)) {
  dir.create(output_dir, recursive = TRUE)
  cat("Created output directory:", output_dir, "\n")
}

cat("=== Step 3: Detect Dead Flies Using Final Day Analysis ===\n")
cat("Input file:", input_rds, "\n")
cat("No-movement threshold:", threshold_hours, "hours\n")
cat("Output directory:", output_dir, "\n\n")

# Step 1: Load cleaned MonitorS4 object
cat("Loading cleaned MonitorS4 object...\n")
monitor_obj <- readRDS(input_rds)

if (!is(monitor_obj, "MonitorS4")) {
  stop("Input file does not contain a MonitorS4 object")
}

# Verify this is a cleaned object
if (is.null(monitor_obj@time$processing_step) || 
    !monitor_obj@time$processing_step %in% c("days_cleaned", "raw_data_loaded")) {
  warning("Input object processing step unclear. Processing anyway...")
}

# Step 2: Analyze current data structure
cat("Analyzing current data structure...\n")
mt_data <- monitor_obj@assays[["mt"]]
analysis_period <- monitor_obj@time$analysis_period

if (!is.null(analysis_period)) {
  cat("Analysis period:", analysis_period$days_analyzed, "days\n")
  cat("Date range:", paste(analysis_period$date_range, collapse = " to "), "\n")
  cat("Total data points:", analysis_period$total_points, "\n")
} else {
  cat("No analysis period information found (raw data)\n")
  dates <- as.Date(mt_data$DateTime)
  unique_dates <- sort(unique(dates))
  cat("Data span:", length(unique_dates), "days\n")
  cat("Date range:", as.character(min(unique_dates)), "to", as.character(max(unique_dates)), "\n")
  cat("Total data points:", nrow(mt_data), "\n")
}

cat("Total flies in metadata:", nrow(monitor_obj@meta.data), "\n")

# Step 3: Detect dead flies
cat("\nRunning mortality detection...\n")
monitor_obj <- detect_dead_flies_final_day(monitor_obj, no_movement_threshold = threshold_hours)

# Update processing metadata
monitor_obj@time$processing_step <- "mortality_detected"
monitor_obj@time$mortality_detection_date <- Sys.time()
monitor_obj@time$mortality_params <- list(
  threshold_hours = threshold_hours
)

# Step 4: Generate detailed mortality report
cat("\nGenerating detailed mortality report...\n")
metadata <- monitor_obj@meta.data
mortality_analysis <- monitor_obj@time$mortality_analysis

# Create detailed report by group if available
if ("Group" %in% colnames(metadata)) {
  cat("\n=== Mortality by Group ===\n")
  groups <- unique(metadata$Group)
  groups <- groups[!is.na(groups)]
  
  for (group in groups) {
    group_data <- metadata[metadata$Group == group & !is.na(metadata$Group), ]
    total <- nrow(group_data)
    alive <- sum(group_data$mortality_status == "alive")
    dead <- sum(group_data$mortality_status == "dead")
    rate <- round(dead / total * 100, 1)
    
    cat("Group:", group, "\n")
    cat("  Total flies:", total, "\n")
    cat("  Alive flies:", alive, "\n")
    cat("  Dead flies:", dead, "\n")
    cat("  Mortality rate:", rate, "%\n\n")
  }
}

# Create mortality types breakdown
cat("=== Mortality Types ===\n")
mortality_types <- table(metadata$mortality_type)
for (type in names(mortality_types)) {
  cat(type, ":", mortality_types[[type]], "flies\n")
}

# Step 5: Save object with mortality information
input_basename <- tools::file_path_sans_ext(basename(input_rds))
monitor_name <- gsub("_(raw|days_cleaned)$", "", input_basename)
output_file <- file.path(output_dir, paste0(monitor_name, "_with_mortality.rds"))

saveRDS(monitor_obj, file = output_file)
file_size <- round(file.size(output_file) / (1024^2), 2)

cat("\nMonitorS4 object with mortality information saved to:", output_file, "\n")
cat("File size:", file_size, "MB\n")

# Step 6: Generate and save mortality report CSV
mortality_summary <- data.frame(
  Fly = metadata$Fly,
  Channel = metadata$Channel,
  Group = if ("Group" %in% colnames(metadata)) metadata$Group else "Unknown",
  Sex = if ("Sex" %in% colnames(metadata)) metadata$Sex else "Unknown",
  Mortality_Status = metadata$mortality_status,
  Mortality_Type = metadata$mortality_type,
  Final_Day_Activity = metadata$final_day_activity,
  Total_Experiment_Activity = metadata$total_experiment_activity,
  stringsAsFactors = FALSE
)

mortality_report_file <- file.path(output_dir, paste0(monitor_name, "_mortality_report.csv"))
write.csv(mortality_summary, mortality_report_file, row.names = FALSE)

cat("Mortality report saved to:", mortality_report_file, "\n")

# Step 7: Print final summary
cat("\n=== Mortality Detection Summary ===\n")
cat("Monitor name:", monitor_name, "\n")
cat("Detection threshold:", threshold_hours, "hours of no movement\n")
cat("Final day analyzed:", as.character(mortality_analysis$final_day), "\n")
cat("Total flies analyzed:", mortality_analysis$total_flies, "\n")
cat("Dead flies detected:", mortality_analysis$dead_flies, "\n")
cat("Overall mortality rate:", mortality_analysis$mortality_rate, "%\n")
cat("- Completely inactive:", mortality_analysis$completely_inactive, "flies\n")
cat("- Final day inactive:", mortality_analysis$final_day_inactive, "flies\n")

cat("\n=== Step 3 Complete ===\n")
cat("Next steps:\n")
cat("- Run step4_filter_alive.R to remove dead flies for analysis\n")
cat("- Or use the data as-is with mortality flags in metadata\n")