# RMB Package - Complete Analysis Workflow Example
# 
# This script demonstrates the complete tested pipeline for processing
# TriKinetics monitor data using the RMB package.
#
# Author: RMB Package Development Team
# Last Updated: 2025-08-13

# Setup Environment ----
cat("=== RMB Analysis Workflow ===\n")
cat("Loading required libraries...\n")

# Load required libraries
library(tidyverse)
library(ggplot2)
library(lubridate)
library(methods)

# Source RMB functions
source("./R/MonitorS4-class.R")
source("./R/utilities.R")
source("./R/metadata-management.R")
source("./R/loadRMB.R")
source("./R/analysis-functions.R")
source("./R/visualization-functions.R")

cat("✓ Libraries and functions loaded\n\n")

# Create output directory
output_dir <- "./results"
if (!dir.exists(output_dir)) {
  dir.create(output_dir, recursive = TRUE)
}

# Single Monitor Processing ----
cat("=== Single Monitor Processing Example ===\n")

# Step 1: Generate/verify metadata exists
metadata_file <- "./inst/extdata/metadata/Monitor36_metadata.csv"
if (!file.exists(metadata_file)) {
  generate_metadata("./inst/extdata/Monitor36.txt", output_dir = "./inst/extdata/metadata/")
}

# Step 2: Load data using new loadRMB workflow
monitor36 <- loadRMB(
  data = "./inst/extdata/Monitor36.txt",
  metadata = metadata_file,
  remove_dead = TRUE,
  activity_threshold = 1,
  consecutive_zeros = 12,
  analysis_days = c(2,3,4)  # Use days 2-4 for analysis
)

# Note: Dead fly removal is handled automatically in loadRMB with remove_dead = TRUE

cat("\n=== Quality Control Plots ===\n")

# Generate QC plots
plot_light_schedule(monitor36, save_plot = TRUE, output_path = output_dir)
plot_monitor_qc_binary(monitor36, save_plot = TRUE, output_path = output_dir)
plot_individual_flies(monitor36, max_flies = 16, save_plot = TRUE, output_path = output_dir)

cat("\n=== Activity Analysis ===\n")

# Generate activity plots
activity_plot <- plot_group_activity_timecourse(
  monitor36, 
  group_by = "Group", 
  bin_minutes = 30, 
  save_plot = TRUE, 
  output_path = output_dir
)

# Get activity statistics
activity_stats <- summarize_activity_by_group(monitor36, group_by = "Group")
print("Activity Statistics for Monitor36:")
print(activity_stats)

# Multi-Monitor Analysis ----
cat("\n=== Multi-Monitor Analysis Example ===\n")

# List of all test monitors
monitor_files <- c(
  "./inst/extdata/Monitor36.txt",
  "./inst/extdata/Monitor37.txt", 
  "./inst/extdata/Monitor38.txt",
  "./inst/extdata/Monitor39.txt",
  "./inst/extdata/Monitor40.txt",
  "./inst/extdata/Monitor41.txt"
)

metadata_files <- c(
  "./inst/extdata/metadata/Monitor36_metadata.csv",
  "./inst/extdata/metadata/Monitor37_metadata.csv",
  "./inst/extdata/metadata/Monitor38_metadata.csv",
  "./inst/extdata/metadata/Monitor39_metadata.csv",
  "./inst/extdata/metadata/Monitor40_metadata.csv",
  "./inst/extdata/metadata/Monitor41_metadata.csv"
)

# Process all monitors using new workflow
processed_monitors <- list()
monitor_names <- paste0("Monitor", 36:41)

for (i in seq_along(monitor_files)) {
  cat("Processing", monitor_names[i], "...\n")
  
  # Ensure metadata exists
  if (!file.exists(metadata_files[i])) {
    generate_metadata(monitor_files[i], output_dir = dirname(metadata_files[i]))
  }
  
  # Process monitor using loadRMB
  monitor_obj <- loadRMB(
    data = monitor_files[i],
    metadata = metadata_files[i],
    remove_dead = TRUE,
    activity_threshold = 1,
    consecutive_zeros = 12,
    analysis_days = c(2,3,4)
  )
  
  # Store in list
  processed_monitors[[monitor_names[i]]] <- monitor_obj
}

cat("\n=== Combined Analysis ===\n")

# Generate combined activity plot
combined_plot <- plot_combined_monitors_activity(
  processed_monitors, 
  group_by = "Group", 
  bin_minutes = 30,
  save_plot = TRUE, 
  output_path = output_dir
)

# Get combined statistics
combined_stats <- summarize_activity_by_group(processed_monitors, group_by = "Group")
print("Combined Activity Statistics:")
print(combined_stats)

# Export results
write.csv(combined_stats, file.path(output_dir, "combined_activity_statistics.csv"), row.names = FALSE)

# Generate Mortality Report ----
cat("\n=== Mortality Analysis ===\n")

mortality_summary <- data.frame()

for (i in seq_along(processed_monitors)) {
  monitor_name <- names(processed_monitors)[i]
  monitor_obj <- processed_monitors[[i]]
  
  # Generate mortality report
  mortality_report <- generate_mortality_report(monitor_obj)
  mortality_report$Monitor <- monitor_name
  
  mortality_summary <- rbind(mortality_summary, mortality_report)
}

print("Mortality Summary Across All Monitors:")
print(mortality_summary)

# Export mortality report
write.csv(mortality_summary, file.path(output_dir, "mortality_summary.csv"), row.names = FALSE)

# Summary Statistics ----
cat("\n=== Experiment Summary ===\n")

total_flies <- sum(mortality_summary$Total_Flies)
total_alive <- sum(mortality_summary$Alive_Flies)
total_dead <- sum(mortality_summary$Dead_Flies)
overall_mortality <- round(total_dead / total_flies * 100, 1)

cat("Total flies analyzed:", total_flies, "\n")
cat("Alive flies:", total_alive, "\n")
cat("Dead flies:", total_dead, "\n")
cat("Overall mortality rate:", overall_mortality, "%\n")

# Groups analyzed
groups_analyzed <- unique(combined_stats$Group)
cat("Experimental groups:", paste(groups_analyzed, collapse = ", "), "\n")

# Light/Dark ratios
light_dark_ratios <- combined_stats$light_dark_ratio
names(light_dark_ratios) <- combined_stats$Group
cat("\nCircadian Analysis (Light/Dark Activity Ratios):\n")
for (group in names(light_dark_ratios)) {
  ratio <- light_dark_ratios[group]
  status <- ifelse(ratio > 1, "Normal circadian", 
                  ifelse(ratio < 1, "Reversed circadian", "Arrhythmic"))
  cat("-", group, ":", ratio, "(", status, ")\n")
}

cat("\n=== Analysis Complete ===\n")
cat("Results saved to:", output_dir, "\n")
cat("Key outputs:\n")
cat("- Individual monitor RDS files: *_processed.rds\n")
cat("- QC plots: light_schedule_verification.png, *_qc_binary.png\n")
cat("- Activity plots: combined_monitors_activity_Group_30min.png\n")
cat("- Statistics: combined_activity_statistics.csv, mortality_summary.csv\n")
cat("\nWorkflow completed successfully!\n")