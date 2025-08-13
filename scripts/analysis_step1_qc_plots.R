#!/usr/bin/env Rscript
#' Analysis Step 1: Quality Control Plots
#'
#' @description 
#' Generate quality control plots to verify data integrity and identify issues.
#' Creates light schedule verification, binary activity heatmap, and individual fly patterns.
#'
#' @usage
#' Rscript analysis_step1_qc_plots.R <processed_rds> [output_dir]
#'
#' @examples
#' Rscript analysis_step1_qc_plots.R ./results/Monitor36_processed.rds ./analysis/
#'

# Load required libraries
suppressPackageStartupMessages({
  library(tidyverse)
  library(ggplot2)
  library(lubridate)
  library(methods)
})

# Source required functions
source("./R/MonitorS4-class.R")
source("./R/visualization-functions.R")

# Parse command line arguments
args <- commandArgs(trailingOnly = TRUE)

if (length(args) < 1) {
  cat("Usage: Rscript analysis_step1_qc_plots.R <processed_rds> [output_dir]\n")
  cat("Example: Rscript analysis_step1_qc_plots.R ./results/Monitor36_processed.rds ./analysis/\n")
  quit(status = 1)
}

processed_rds <- args[1]
output_dir <- if (length(args) >= 2) args[2] else "./analysis"

# Validate inputs
if (!file.exists(processed_rds)) {
  stop("Processed RDS file does not exist: ", processed_rds)
}

# Create output directory if it doesn't exist
if (!dir.exists(output_dir)) {
  dir.create(output_dir, recursive = TRUE)
  cat("Created output directory:", output_dir, "\n")
}

cat("=== Analysis Step 1: Quality Control Plots ===\n")
cat("Input file:", processed_rds, "\n")
cat("Output directory:", output_dir, "\n\n")

# Load processed MonitorS4 object
cat("Loading processed MonitorS4 object...\n")
monitor_obj <- readRDS(processed_rds)

if (!is(monitor_obj, "MonitorS4")) {
  stop("Input file does not contain a MonitorS4 object")
}

# Get monitor name for file naming
input_basename <- tools::file_path_sans_ext(basename(processed_rds))
monitor_name <- gsub("_processed$", "", input_basename)

# Print summary information
cat("Monitor information:\n")
cat("- Monitor name:", monitor_name, "\n")
cat("- Total flies in metadata:", nrow(monitor_obj@meta.data), "\n")

if ("included_in_analysis" %in% colnames(monitor_obj@meta.data)) {
  alive_flies <- sum(monitor_obj@meta.data$included_in_analysis)
  cat("- Flies included in analysis:", alive_flies, "\n")
  mortality_rate <- round((nrow(monitor_obj@meta.data) - alive_flies) / nrow(monitor_obj@meta.data) * 100, 1)
  cat("- Mortality rate:", mortality_rate, "%\n")
}

if (!is.null(monitor_obj@time$analysis_period)) {
  analysis_period <- monitor_obj@time$analysis_period
  cat("- Analysis period:", analysis_period$days_analyzed, "days\n")
  cat("- Date range:", paste(analysis_period$date_range, collapse = " to "), "\n")
  cat("- Data points:", analysis_period$total_points, "\n")
}

cat("\n")

# QC Plot 1: Light Schedule Verification
cat("Generating QC Plot 1: Light Schedule Verification...\n")
tryCatch({
  light_plot <- plot_light_schedule(monitor_obj, save_plot = TRUE, output_path = output_dir)
  cat("✓ Light schedule plot generated\n")
}, error = function(e) {
  cat("✗ Failed to generate light schedule plot:", e$message, "\n")
})

# QC Plot 2: Binary Activity Heatmap
cat("Generating QC Plot 2: Binary Activity Heatmap...\n")
tryCatch({
  binary_plot <- plot_monitor_qc_binary(monitor_obj, save_plot = TRUE, output_path = output_dir)
  cat("✓ Binary activity heatmap generated\n")
}, error = function(e) {
  cat("✗ Failed to generate binary activity plot:", e$message, "\n")
})

# QC Plot 3: Individual Fly Patterns (sample of flies)
cat("Generating QC Plot 3: Individual Fly Patterns...\n")
tryCatch({
  individual_plot <- plot_individual_flies(monitor_obj, max_flies = 16, 
                                          save_plot = TRUE, output_path = output_dir)
  cat("✓ Individual fly patterns generated (16 flies)\n")
}, error = function(e) {
  cat("✗ Failed to generate individual fly plots:", e$message, "\n")
})

# QC Plot 4: Mortality Summary (if mortality analysis was done)
if ("mortality_status" %in% colnames(monitor_obj@meta.data)) {
  cat("Generating QC Plot 4: Mortality Summary...\n")
  tryCatch({
    mortality_plot <- plot_mortality_summary(monitor_obj, save_plot = TRUE, output_path = output_dir)
    cat("✓ Mortality summary plot generated\n")
  }, error = function(e) {
    cat("✗ Failed to generate mortality summary plot:", e$message, "\n")
  })
}

# Generate QC Summary Report
cat("Generating QC summary report...\n")

# Data integrity checks
mt_data <- monitor_obj@assays[["mt"]]
channel_cols <- grep("Channel_", colnames(mt_data))
metadata <- monitor_obj@meta.data

qc_report <- data.frame(
  Check = c(
    "Total time points",
    "Expected time points (3 days × 1440)",
    "Data integrity",
    "Total flies in metadata", 
    "Active data channels",
    "Channel-metadata match",
    "Light schedule range",
    "Missing values in data"
  ),
  Result = c(
    nrow(mt_data),
    ifelse(!is.null(monitor_obj@time$analysis_period), 
           monitor_obj@time$analysis_period$days_analyzed * 1440, "Unknown"),
    ifelse(nrow(mt_data) == ifelse(!is.null(monitor_obj@time$analysis_period), 
                                   monitor_obj@time$analysis_period$total_points, nrow(mt_data)), 
           "✓ PASS", "✗ FAIL"),
    nrow(metadata),
    length(channel_cols),
    ifelse(length(channel_cols) == sum(metadata$included_in_analysis, na.rm = TRUE), "✓ PASS", "✗ FAIL"),
    paste(range(mt_data$Light, na.rm = TRUE), collapse = " to "),
    sum(is.na(mt_data[, channel_cols]))
  ),
  Status = c(
    "Info", "Info", 
    ifelse(nrow(mt_data) == ifelse(!is.null(monitor_obj@time$analysis_period), 
                                   monitor_obj@time$analysis_period$total_points, nrow(mt_data)), 
           "PASS", "FAIL"),
    "Info", "Info",
    ifelse(length(channel_cols) == sum(metadata$included_in_analysis, na.rm = TRUE), "PASS", "FAIL"),
    ifelse(all(range(mt_data$Light, na.rm = TRUE) == c(0, 1)), "PASS", "WARNING"),
    ifelse(sum(is.na(mt_data[, channel_cols])) == 0, "PASS", "WARNING")
  ),
  stringsAsFactors = FALSE
)

# Add mortality information if available
if ("mortality_status" %in% colnames(metadata)) {
  mortality_info <- monitor_obj@time$mortality_analysis
  mortality_rows <- data.frame(
    Check = c("Total flies", "Dead flies", "Alive flies", "Mortality rate"),
    Result = c(
      mortality_info$total_flies,
      mortality_info$dead_flies,
      mortality_info$total_flies - mortality_info$dead_flies,
      paste0(mortality_info$mortality_rate, "%")
    ),
    Status = c(
      "Info", "Info", "Info",
      ifelse(mortality_info$mortality_rate < 30, "PASS", "WARNING")
    ),
    stringsAsFactors = FALSE
  )
  qc_report <- rbind(qc_report, mortality_rows)
}

# Save QC report
qc_report_file <- file.path(output_dir, paste0(monitor_name, "_qc_report.csv"))
write.csv(qc_report, qc_report_file, row.names = FALSE)

cat("QC report saved to:", qc_report_file, "\n")

# Print QC summary to console
cat("\n=== QC Summary ===\n")
for (i in 1:nrow(qc_report)) {
  status_symbol <- switch(qc_report$Status[i],
                         "PASS" = "✓",
                         "FAIL" = "✗", 
                         "WARNING" = "⚠",
                         "INFO" = "ℹ",
                         "ℹ")
  cat(sprintf("%-30s: %s %s\n", qc_report$Check[i], qc_report$Result[i], status_symbol))
}

# List generated files
cat("\n=== Generated Files ===\n")
output_files <- list.files(output_dir, pattern = paste0("(", monitor_name, "|light_schedule|mortality)"), full.names = FALSE)
for (file in output_files) {
  file_path <- file.path(output_dir, file)
  file_size <- round(file.size(file_path) / 1024, 1)  # KB
  cat("- ", file, " (", file_size, " KB)\n", sep = "")
}

cat("\n=== QC Analysis Complete ===\n")
cat("Next steps:\n")
cat("- Review QC plots for data quality issues\n")
cat("- Check for dead flies in binary activity plot (white rows)\n") 
cat("- Verify light schedule matches experimental design\n")
cat("- Proceed to analysis_step2_activity_analysis.R for activity analysis\n")