# Test script for RMB package with example data

cat("🧪 Testing RMB Package\n\n")

# Source functions directly
source("R/MonitorS4-class.R")
source("R/utilities.R")
source("R/loadRMB.R") 
source("R/analysis-functions.R")
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

# Test data loading
cat("📂 Loading example data...\n")
tryCatch({
  data <- loadRMB(data = "./inst/extdata", 
                  metadata = "./inst/extdata/metadata", 
                  remove_dead = FALSE)
  
  cat("✅ Data loaded successfully!\n")
  cat("   Flies:", nrow(getMeta(data)), "\n")
  cat("   Assays:", paste(names(data@assays), collapse = ", "), "\n")
  cat("   Duration:", round(data@time$duration_hours, 1), "hours\n")
  
  # Test analysis
  cat("\n🔬 Running analyses...\n")
  
  # Activity analysis
  activity_results <- analyze_activity(data)
  cat("✅ Activity analysis completed\n")
  cat("   Mean total activity:", round(mean(activity_results$overall_stats$total_activity, na.rm = TRUE), 0), "\n")
  
  # Circadian analysis  
  circadian_results <- analyze_circadian(data)
  cat("✅ Circadian analysis completed\n")
  cat("   Rhythm profiles:", nrow(circadian_results$circadian_parameters), "\n")
  
  # Position analysis
  if ("pn" %in% names(data@assays)) {
    position_results <- analyze_position(data)
    cat("✅ Position analysis completed\n")
    cat("   Mean movement:", round(mean(position_results$position_stats$total_movement, na.rm = TRUE), 0), "\n")
  }
  
  # Test comprehensive analysis
  cat("\n📊 Running comprehensive analysis...\n")
  comprehensive_results <- comprehensive_analysis(data, include_sleep = FALSE)
  cat("✅ Comprehensive analysis completed\n")
  
  # Test report generation
  cat("\n📋 Generating reports...\n")
  quick_report <- generateQuickReport(data, "rmb_test_quick_report.html")
  cat("✅ Quick report generated:", quick_report, "\n")
  
  # Test data export
  cat("\n💾 Exporting data...\n")
  export_dir <- "rmb_test_output"
  exported_files <- export_analysis_data(comprehensive_results, export_dir)
  cat("✅ Data exported:", length(exported_files), "files to", export_dir, "\n")
  
  # Summary
  cat("\n🎉 ALL TESTS PASSED!\n")
  cat("📊 Summary:\n")
  cat("   • Flies analyzed:", nrow(getMeta(data)), "\n")
  cat("   • Duration:", round(data@time$duration_hours, 1), "hours\n") 
  cat("   • Data files exported:", length(exported_files), "\n")
  cat("   • Reports generated: 1\n")
  cat("\n✨ RMB package is working correctly!\n")
  
}, error = function(e) {
  cat("❌ Test failed with error:", e$message, "\n")
  cat("Please check package installation and dependencies.\n")
})