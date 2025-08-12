# RMB Package Quick Start Example
# This script demonstrates the basic usage of the RMB package

# Load the RMB package
library(RMB)

# Get example data paths
data_path <- system.file("extdata", package = "RMB")
metadata_path <- file.path(data_path, "metadata")

cat("Example data location:", data_path, "\n")
cat("Metadata location:", metadata_path, "\n")

# Load and process the data
cat("Loading data...\n")
data <- loadRMB(data = data_path, metadata = metadata_path)

# Display basic information
cat("\n=== Data Summary ===\n")
print(data)

# Generate a quick report
cat("\nGenerating quick report...\n")
generateQuickReport(data, "rmb_quick_example_report.html")

# Generate comprehensive report
cat("Generating comprehensive report...\n")
generateReport(data, 
               output_file = "rmb_comprehensive_example_report.html",
               experiment_name = "RMB Example Analysis",
               author = "RMB User")

cat("\n✅ Analysis complete!\n")
cat("📄 Reports generated:\n")
cat("  - rmb_quick_example_report.html\n")
cat("  - rmb_comprehensive_example_report.html\n")

# Optional: Run individual analyses
cat("\n=== Optional: Individual Analysis Examples ===\n")

# Activity analysis
activity_results <- analyze_activity(data)
cat("Activity analysis completed -", nrow(activity_results$overall_stats), "flies analyzed\n")

# Circadian analysis
circadian_results <- analyze_circadian(data)
cat("Circadian analysis completed -", nrow(circadian_results$circadian_parameters), "rhythm profiles generated\n")

# Export data
export_dir <- "rmb_example_output"
exported_files <- export_analysis_data(list(activity = activity_results, circadian = circadian_results), 
                                      output_dir = export_dir)
cat("Data exported to", export_dir, "- ", length(exported_files), "files created\n")