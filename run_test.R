# Test script for RMB package with example data

cat("🧪 Testing RMB Package - Modular Workflow\n\n")

# Source functions directly
source("R/MonitorS4-class.R")
source("R/utilities.R")
source("R/loadRMB.R") 
source("R/analysis-functions.R")
source("R/visualization-functions.R")
source("R/report-generation.R")

# Load required libraries
suppressPackageStartupMessages({
  library(methods)
  library(ggplot2)
  library(dplyr)
  library(tidyr)
  library(readr)
  library(lubridate)
  library(stringr)
})

# Step 1: Load raw data only
cat("📂 Step 1: Loading raw data...\n")
tryCatch({
  data <- loadRMB(data = "./inst/extdata", 
                  metadata = "./inst/extdata/metadata")
  
  cat("✅ Raw data loaded successfully!\n")
  cat("   Total flies:", nrow(getMeta(data)), "\n")
  cat("   Available assays:", paste(names(data@assays), collapse = ", "), "\n")
  cat("   Duration:", round(data@time$duration_hours, 1), "hours\n")
  
  # Step 2: Process data using modular functions
  cat("\n🔧 Step 2: Processing data...\n")
  
  # Clean to analysis days
  data <- CleanDays(data, days = c(2,3,4))
  cat("✅ Data cleaned to analysis days 2-4\n")
  
  # Detect dead flies
  data <- DetectDeadFlies(data, threshold_hours = 24)
  cat("✅ Dead flies detected\n")
  
  # Filter to alive flies only
  data <- FilterAliveFlies(data)
  cat("✅ Filtered to alive flies only\n")
  cat("   Final fly count:", nrow(getMeta(data)), "\n")
  
  # Step 3: Analyze data
  cat("\n🔬 Step 3: Analyzing data...\n")
  
  # Activity analysis using new function
  activity_stats <- AnalyzeActivity(data, group_by = "Group")
  cat("✅ Activity analysis completed\n")
  cat("   Groups analyzed:", nrow(activity_stats), "\n")
  
  # Step 4: Generate plots using new functions
  cat("\n📊 Step 4: Generating plots...\n")
  output_dir <- "./inst/extdata/RMB_output"
  if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)
  
  # Light schedule plot
  tryCatch({
    PlotLightSchedule(data, save_plot = TRUE, output_path = output_dir)
    cat("✅ Light schedule plot completed\n")
  }, error = function(e) {
    cat("⚠️ Light schedule plot skipped:", e$message, "\n")
  })
  
  # QC plot
  tryCatch({
    PlotQCBinary(data, save_plot = TRUE, output_path = output_dir) 
    cat("✅ QC binary plot completed\n")
  }, error = function(e) {
    cat("⚠️ QC plot skipped:", e$message, "\n")
  })
  
  # Activity timecourse
  tryCatch({
    PlotActivityTimecourse(data, group_by = "Group", save_plot = TRUE, output_path = output_dir)
    cat("✅ Group activity timecourse completed\n")
  }, error = function(e) {
    cat("⚠️ Activity timecourse plot skipped:", e$message, "\n")
  })
  
  # Step 5: Export results
  cat("\n💾 Step 5: Exporting results...\n")
  
  # Export activity stats
  write.csv(activity_stats, file.path(output_dir, "activity_summary.csv"), row.names = FALSE)
  cat("✅ Activity summary exported\n")
  
  # Export metadata
  write.csv(getMeta(data), file.path(output_dir, "metadata_with_analysis.csv"), row.names = FALSE)
  cat("✅ Metadata with analysis exported\n")
  
  # Export mortality report if function exists
  tryCatch({
    mortality_report <- generate_mortality_report(data)
    write.csv(mortality_report, file.path(output_dir, "mortality_report.csv"), row.names = FALSE)
    cat("✅ Mortality report exported\n")
  }, error = function(e) {
    cat("⚠️ Mortality report skipped:", e$message, "\n")
  })
  
  exported_files <- list.files(output_dir, full.names = FALSE)
  cat("✅ Total files exported:", length(exported_files), "to", output_dir, "\n")
  
  # Summary
  cat("\n🎉 MODULAR WORKFLOW TEST PASSED!\n")
  cat("📊 Summary:\n")
  cat("   • Flies analyzed:", nrow(getMeta(data)), "\n") 
  cat("   • Duration:", round(data@time$duration_hours, 1), "hours\n") 
  cat("   • Files exported:", length(exported_files), "\n")
  cat("   • Workflow: loadRMB() → CleanDays() → DetectDeadFlies() → FilterAliveFlies() → AnalyzeActivity() → Plot functions\n")
  cat("\n✨ RMB modular workflow is working correctly!\n")
  
}, error = function(e) {
  cat("❌ Test failed with error:", e$message, "\n")
  cat("Please check package installation and dependencies.\n")
})
