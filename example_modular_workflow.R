# Example: RMB Modular Workflow
# This script demonstrates the new Seurat-like modular approach for RMB analysis

# Load required libraries
library(methods)
library(ggplot2)
library(dplyr)
library(readr)
library(lubridate)

# Source RMB functions
source("R/MonitorS4-class.R")
source("R/utilities.R")
source("R/loadRMB.R") 
source("R/analysis-functions.R")
source("R/visualization-functions.R")

# Step 1: Load raw data only (no processing)
cat("📂 Loading raw data...\n")
data <- loadRMB(data = "./inst/extdata", 
                metadata = "./inst/extdata/metadata")

# Step 2: Process data step by step
cat("🔧 Processing data...\n")

# Clean to analysis days (typically days 2-4)
data <- CleanDays(data, days = c(2,3,4))

# Detect dead flies using 24-hour no-movement criterion  
data <- DetectDeadFlies(data, threshold_hours = 24)

# Filter to include only alive flies in analysis
data <- FilterAliveFlies(data)

# Step 3: Analyze data
cat("🔬 Analyzing data...\n")

# Activity analysis by treatment group
activity_stats <- AnalyzeActivity(data, group_by = "Group", bin_minutes = 30)
print(activity_stats)

# Step 4: Generate plots
cat("📊 Generating plots...\n")

# Quality control plots
PlotLightSchedule(data)                    # Verify light cycle
PlotQCBinary(data)                        # Dead flies appear as white rows

# Analysis plots  
PlotActivityTimecourse(data, group_by = "Group")  # Group activity over time
PlotIndividualFlies(data, max_flies = 8)          # Individual fly patterns

# Step 5: Export results
cat("💾 Exporting results...\n")
write.csv(activity_stats, "activity_summary.csv", row.names = FALSE)
write.csv(getMeta(data), "metadata_with_analysis.csv", row.names = FALSE)

cat("✅ Analysis complete!\n")
cat("Workflow: loadRMB() → CleanDays() → DetectDeadFlies() → FilterAliveFlies() → AnalyzeActivity() → Plot functions\n")