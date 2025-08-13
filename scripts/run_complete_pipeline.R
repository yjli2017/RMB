#!/usr/bin/env Rscript
#' Complete RMB Processing Pipeline
#'
#' @description 
#' This script runs the complete RMB processing pipeline from raw data to 
#' analysis-ready dataset. It combines all individual steps into one script.
#'
#' @usage
#' Rscript run_complete_pipeline.R <data_file> <metadata_file> [analysis_days] [threshold_hours] [output_dir]
#'
#' @examples
#' Rscript run_complete_pipeline.R ./inst/extdata/Monitor36.txt ./metadata/Monitor36_metadata.csv "2,3,4" 24 ./results/
#' Rscript run_complete_pipeline.R ./inst/extdata/Monitor36.txt ./metadata/Monitor36_metadata.csv "3,4,5" 48 ./results/
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
  cat("Usage: Rscript run_complete_pipeline.R <data_file> <metadata_file> [analysis_days] [threshold_hours] [output_dir]\n")
  cat("Example: Rscript run_complete_pipeline.R ./inst/extdata/Monitor36.txt ./metadata/Monitor36_metadata.csv \"2,3,4\" 24 ./results/\n")
  cat("Defaults: analysis_days=2,3,4, threshold_hours=24, output_dir=./results\n")
  quit(status = 1)
}

data_file <- args[1]
metadata_file <- args[2]
analysis_days_str <- if (length(args) >= 3) args[3] else "2,3,4"
threshold_hours <- if (length(args) >= 4) as.numeric(args[4]) else 24
output_dir <- if (length(args) >= 5) args[5] else "./results"

# Parse analysis days
analysis_days <- as.numeric(strsplit(analysis_days_str, ",")[[1]])

# Validate inputs
if (!file.exists(data_file)) {
  stop("Data file does not exist: ", data_file)
}

if (!file.exists(metadata_file)) {
  stop("Metadata file does not exist: ", metadata_file)
}

if (any(is.na(analysis_days)) || length(analysis_days) == 0) {
  stop("Invalid analysis days format. Use comma-separated numbers like '2,3,4'")
}

if (is.na(threshold_hours) || threshold_hours <= 0) {
  stop("Invalid threshold hours. Must be a positive number.")
}

# Create output directory if it doesn't exist
if (!dir.exists(output_dir)) {
  dir.create(output_dir, recursive = TRUE)
  cat("Created output directory:", output_dir, "\n")
}

cat("=== RMB Complete Processing Pipeline ===\n")
cat("Data file:", data_file, "\n")
cat("Metadata file:", metadata_file, "\n")
cat("Analysis days:", paste(analysis_days, collapse = ", "), "\n")
cat("Mortality threshold:", threshold_hours, "hours\n")
cat("Output directory:", output_dir, "\n\n")

# Get monitor name for file naming
monitor_name <- tools::file_path_sans_ext(basename(data_file))

# STEP 1: Load raw data
cat("=== STEP 1: Loading Raw Data ===\n")
start_time <- Sys.time()

raw_data <- parse_monitor_file(data_file)
assays <- process_monitor_assays(raw_data)
metadata_df <- readr::read_csv(metadata_file, show_col_types = FALSE)

time_info <- list(
  start_time = min(raw_data$DateTime, na.rm = TRUE),
  end_time = max(raw_data$DateTime, na.rm = TRUE),
  duration_hours = as.numeric(difftime(max(raw_data$DateTime, na.rm = TRUE), 
                                     min(raw_data$DateTime, na.rm = TRUE), 
                                     units = "hours")),
  time_points = unique(raw_data$DateTime),
  light_cycle = unique(raw_data$Light)
)

monitor_obj <- MonitorS4(
  meta.data = metadata_df,
  assays = assays,
  active.assay = "mt",
  time = time_info
)

step1_time <- Sys.time()
cat("✓ Step 1 completed in", round(as.numeric(step1_time - start_time), 1), "seconds\n")
cat("  Raw data:", nrow(raw_data), "time points,", nrow(metadata_df), "flies\n\n")

# STEP 2: Clean to analysis days
cat("=== STEP 2: Cleaning to Analysis Days ===\n")
monitor_obj <- clean_monitor_days(monitor_obj, full_days_to_include = analysis_days)

step2_time <- Sys.time()
cat("✓ Step 2 completed in", round(as.numeric(step2_time - step1_time), 1), "seconds\n")
analysis_period <- monitor_obj@time$analysis_period
cat("  Cleaned to", analysis_period$days_analyzed, "days,", analysis_period$total_points, "time points\n\n")

# STEP 3: Detect mortality
cat("=== STEP 3: Detecting Dead Flies ===\n")
monitor_obj <- detect_dead_flies_final_day(monitor_obj, no_movement_threshold = threshold_hours)

step3_time <- Sys.time()
cat("✓ Step 3 completed in", round(as.numeric(step3_time - step2_time), 1), "seconds\n")
mortality_analysis <- monitor_obj@time$mortality_analysis
cat("  Mortality rate:", mortality_analysis$mortality_rate, "% (", mortality_analysis$dead_flies, "/", mortality_analysis$total_flies, "flies)\n\n")

# STEP 4: Filter to alive flies
cat("=== STEP 4: Filtering to Alive Flies ===\n")
monitor_obj <- filter_to_alive_flies(monitor_obj)

step4_time <- Sys.time()
cat("✓ Step 4 completed in", round(as.numeric(step4_time - step3_time), 1), "seconds\n")

# Get final statistics
final_metadata <- monitor_obj@meta.data
alive_flies <- sum(final_metadata$included_in_analysis)
mt_data <- monitor_obj@assays[["mt"]]
channel_cols <- grep("Channel_", colnames(mt_data))

cat("  Final dataset:", alive_flies, "alive flies,", length(channel_cols), "data channels\n\n")

# STEP 5: Save final processed object
cat("=== STEP 5: Saving Final Dataset ===\n")
output_file <- file.path(output_dir, paste0(monitor_name, "_processed.rds"))

# Add final processing metadata
monitor_obj@time$pipeline_complete <- Sys.time()
monitor_obj@time$total_processing_time <- round(as.numeric(step4_time - start_time), 1)
monitor_obj@time$pipeline_params <- list(
  analysis_days = analysis_days,
  threshold_hours = threshold_hours,
  remove_dead = TRUE
)

save_cleaned_monitor(monitor_obj, output_file, overwrite = TRUE)

step5_time <- Sys.time()
cat("✓ Step 5 completed in", round(as.numeric(step5_time - step4_time), 1), "seconds\n\n")

# STEP 6: Generate summary reports
cat("=== STEP 6: Generating Summary Reports ===\n")

# Mortality report
mortality_summary <- data.frame(
  Fly = final_metadata$Fly,
  Channel = final_metadata$Channel,
  Group = if ("Group" %in% colnames(final_metadata)) final_metadata$Group else "Unknown",
  Sex = if ("Sex" %in% colnames(final_metadata)) final_metadata$Sex else "Unknown",
  Mortality_Status = final_metadata$mortality_status,
  Mortality_Type = final_metadata$mortality_type,
  Final_Day_Activity = final_metadata$final_day_activity,
  Total_Experiment_Activity = final_metadata$total_experiment_activity,
  Included_In_Analysis = final_metadata$included_in_analysis,
  stringsAsFactors = FALSE
)

mortality_report_file <- file.path(output_dir, paste0(monitor_name, "_mortality_report.csv"))
write.csv(mortality_summary, mortality_report_file, row.names = FALSE)

# Processing summary
processing_summary <- data.frame(
  Parameter = c("Monitor_Name", "Processing_Date", "Total_Processing_Time_Seconds", 
                "Analysis_Days", "Mortality_Threshold_Hours", "Original_Flies", 
                "Dead_Flies", "Alive_Flies", "Mortality_Rate_Percent", 
                "Final_Data_Points", "Date_Range_Start", "Date_Range_End"),
  Value = c(monitor_name, as.character(Sys.time()), 
            round(as.numeric(step5_time - start_time), 1),
            paste(analysis_days, collapse = ","), threshold_hours, 
            mortality_analysis$total_flies, mortality_analysis$dead_flies, 
            alive_flies, mortality_analysis$mortality_rate,
            nrow(mt_data) * length(channel_cols),
            as.character(analysis_period$date_range[1]),
            as.character(analysis_period$date_range[2])),
  stringsAsFactors = FALSE
)

processing_summary_file <- file.path(output_dir, paste0(monitor_name, "_processing_summary.csv"))
write.csv(processing_summary, processing_summary_file, row.names = FALSE)

step6_time <- Sys.time()
cat("✓ Step 6 completed in", round(as.numeric(step6_time - step5_time), 1), "seconds\n")
cat("  Mortality report:", mortality_report_file, "\n")
cat("  Processing summary:", processing_summary_file, "\n\n")

# FINAL SUMMARY
total_time <- Sys.time()
cat("=== PIPELINE COMPLETE ===\n")
cat("Total processing time:", round(as.numeric(total_time - start_time), 1), "seconds\n")
cat("Monitor:", monitor_name, "\n")
cat("Final dataset: ", output_file, "\n")
cat("Summary:\n")
cat("  - Processed", mortality_analysis$total_flies, "flies →", alive_flies, "alive flies for analysis\n")
cat("  - Data period:", paste(analysis_period$date_range, collapse = " to "), "\n")
cat("  - Analysis days:", paste(analysis_days, collapse = ", "), "(", analysis_period$days_analyzed, "days)\n")
cat("  - Mortality rate:", mortality_analysis$mortality_rate, "%\n")
cat("  - Final dataset size:", round(file.size(output_file) / (1024^2), 2), "MB\n")

# Group-wise summary if available
if ("Group" %in% colnames(final_metadata)) {
  cat("\nSummary by Group:\n")
  groups <- unique(final_metadata$Group)
  groups <- groups[!is.na(groups)]
  
  for (group in groups) {
    group_data <- final_metadata[final_metadata$Group == group & !is.na(final_metadata$Group), ]
    total_in_group <- nrow(group_data)
    alive_in_group <- sum(group_data$included_in_analysis)
    survival_rate <- round(alive_in_group / total_in_group * 100, 1)
    
    cat("  ", group, ":", alive_in_group, "/", total_in_group, "flies (", survival_rate, "% survival)\n")
  }
}

cat("\nNext steps:\n")
cat("- Load data: load_cleaned_monitor('", output_file, "')\n")
cat("- Run QC plots: plot_monitor_qc_binary(), plot_light_schedule()\n")
cat("- Analyze activity: plot_group_activity_timecourse()\n")