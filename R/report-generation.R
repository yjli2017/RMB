#' Report Generation Functions for RMB Package
#'
#' @description Functions for generating HTML reports from analysis results

#' Generate comprehensive HTML report
#'
#' @param object MonitorS4 object with processed data
#' @param analysis_results Results from comprehensive_analysis() (optional, will run if not provided)
#' @param output_file Output HTML file name (default: "rmb_analysis_report.html")
#' @param output_dir Output directory (default: current directory)
#' @param author Author name for report (default: "RMB Package User")
#' @param experiment_name Experiment name for report (default: "Drosophila Activity Analysis")
#' @param open_report Logical, whether to open report in browser after generation (default: TRUE)
#' @param export_data Logical, whether to export data as CSV files (default: TRUE)
#' @return Path to generated HTML report
#' @export
#' @examples
#' \dontrun{
#' # Basic report generation
#' data <- loadRMB(data = "./test/", metadata = "./test/metadata/")
#' generateReport(data)
#' 
#' # Custom report with pre-computed analysis
#' analysis <- comprehensive_analysis(data)
#' generateReport(data, analysis, output_file = "my_experiment_report.html")
#' }
generateReport <- function(object,
                          analysis_results = NULL,
                          output_file = "rmb_analysis_report.html",
                          output_dir = ".",
                          author = "RMB Package User",
                          experiment_name = "Drosophila Activity Analysis",
                          open_report = TRUE,
                          export_data = TRUE) {
  
  if (!is(object, "MonitorS4")) {
    stop("Object must be of class MonitorS4")
  }
  
  # Create output directory if it doesn't exist
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
  }
  
  # Full output path
  output_path <- file.path(output_dir, output_file)
  
  cat("Generating RMB analysis report...\n")
  
  # Run comprehensive analysis if not provided
  if (is.null(analysis_results)) {
    cat("Running comprehensive analysis...\n")
    analysis_results <- comprehensive_analysis(object)
  }
  
  # Validate analysis results
  if (!is.list(analysis_results)) {
    stop("analysis_results must be a list from comprehensive_analysis()")
  }
  
  # Export data files if requested
  if (export_data) {
    cat("Exporting data files...\n")
    export_analysis_data(analysis_results, output_dir)
  }
  
  # Get template path
  template_path <- system.file("rmarkdown", "report_template.Rmd", package = "RMB")
  
  if (!file.exists(template_path)) {
    # If package template not available, create a temporary one
    template_path <- create_temp_template()
  }
  
  cat("Rendering HTML report...\n")
  
  # Render the report
  tryCatch({
    rmarkdown::render(
      input = template_path,
      output_file = basename(output_path),
      output_dir = dirname(output_path),
      params = list(
        analysis_results = analysis_results,
        object = object,
        author = author,
        experiment_name = experiment_name
      ),
      quiet = TRUE
    )
    
    cat("Report generated successfully: ", output_path, "\n")
    
    # Open report in browser if requested
    if (open_report && interactive()) {
      cat("Opening report in browser...\n")
      utils::browseURL(output_path)
    }
    
    return(output_path)
    
  }, error = function(e) {
    stop("Failed to generate report: ", e$message)
  })
}

#' Export analysis data to CSV files
#'
#' @param analysis_results Results from comprehensive_analysis()
#' @param output_dir Output directory for CSV files
#' @return Character vector of exported file paths
#' @export
export_analysis_data <- function(analysis_results, output_dir = ".") {
  
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
  }
  
  exported_files <- character(0)
  
  # Export activity data
  if ("activity" %in% names(analysis_results)) {
    activity_data <- analysis_results$activity
    
    if ("overall_stats" %in% names(activity_data)) {
      stats_file <- file.path(output_dir, "activity_statistics.csv")
      readr::write_csv(activity_data$overall_stats, stats_file)
      exported_files <- c(exported_files, stats_file)
    }
    
    if ("daily_totals" %in% names(activity_data)) {
      daily_file <- file.path(output_dir, "daily_activity.csv")
      readr::write_csv(activity_data$daily_totals, daily_file)
      exported_files <- c(exported_files, daily_file)
    }
    
    if ("hourly_patterns" %in% names(activity_data)) {
      hourly_file <- file.path(output_dir, "hourly_patterns.csv")
      readr::write_csv(activity_data$hourly_patterns, hourly_file)
      exported_files <- c(exported_files, hourly_file)
    }
  }
  
  # Export circadian data
  if ("circadian" %in% names(analysis_results)) {
    circadian_data <- analysis_results$circadian
    
    if ("circadian_parameters" %in% names(circadian_data)) {
      circadian_file <- file.path(output_dir, "circadian_parameters.csv")
      readr::write_csv(circadian_data$circadian_parameters, circadian_file)
      exported_files <- c(exported_files, circadian_file)
    }
    
    if ("hourly_averages" %in% names(circadian_data)) {
      hourly_avg_file <- file.path(output_dir, "circadian_hourly_averages.csv")
      readr::write_csv(circadian_data$hourly_averages, hourly_avg_file)
      exported_files <- c(exported_files, hourly_avg_file)
    }
  }
  
  # Export position data
  if ("position" %in% names(analysis_results)) {
    position_data <- analysis_results$position
    
    if ("position_stats" %in% names(position_data)) {
      pos_stats_file <- file.path(output_dir, "position_statistics.csv")
      readr::write_csv(position_data$position_stats, pos_stats_file)
      exported_files <- c(exported_files, pos_stats_file)
    }
    
    if ("position_preferences" %in% names(position_data)) {
      pos_pref_file <- file.path(output_dir, "position_preferences.csv")
      readr::write_csv(position_data$position_preferences, pos_pref_file)
      exported_files <- c(exported_files, pos_pref_file)
    }
  }
  
  # Export sleep data if available
  if ("sleep" %in% names(analysis_results)) {
    sleep_data <- analysis_results$sleep
    
    if ("overall_stats" %in% names(sleep_data)) {
      sleep_file <- file.path(output_dir, "sleep_statistics.csv")
      readr::write_csv(sleep_data$overall_stats, sleep_file)
      exported_files <- c(exported_files, sleep_file)
    }
  }
  
  # Export summary information
  if ("summary" %in% names(analysis_results)) {
    summary_file <- file.path(output_dir, "analysis_summary.json")
    jsonlite::write_json(analysis_results$summary, summary_file, pretty = TRUE)
    exported_files <- c(exported_files, summary_file)
  }
  
  if (length(exported_files) > 0) {
    cat("Exported", length(exported_files), "data files to", output_dir, "\n")
    for (file in exported_files) {
      cat("  -", basename(file), "\n")
    }
  }
  
  return(exported_files)
}

#' Create temporary report template (fallback)
#'
#' @return Path to temporary template file
create_temp_template <- function() {
  temp_template <- tempfile(fileext = ".Rmd")
  
  # Basic template content
  template_content <- '---
title: "RMB Analysis Report"
output: html_document
params:
  analysis_results: NULL
  object: NULL
  author: "RMB User"
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = FALSE, warning = FALSE, message = FALSE)
library(knitr)
library(DT)
```

# RMB Analysis Report

## Summary

```{r}
if (!is.null(params$analysis_results) && "summary" %in% names(params$analysis_results)) {
  summary_info <- params$analysis_results$summary
  cat("Number of flies:", summary_info$total_flies, "\\n")
  cat("Available assays:", paste(summary_info$available_assays, collapse = ", "), "\\n")
  cat("Duration:", round(summary_info$duration_hours, 2), "hours\\n")
}
```

## Activity Statistics

```{r}
if (!is.null(params$analysis_results) && "activity" %in% names(params$analysis_results)) {
  activity_stats <- params$analysis_results$activity$overall_stats
  DT::datatable(activity_stats, options = list(pageLength = 10))
}
```

---
*Report generated by RMB package*
'
  
  writeLines(template_content, temp_template)
  return(temp_template)
}

#' Generate quick summary report
#'
#' @param object MonitorS4 object
#' @param output_file Output file name (default: "rmb_quick_summary.html")
#' @return Path to generated report
#' @export
generateQuickReport <- function(object, output_file = "rmb_quick_summary.html") {
  
  if (!is(object, "MonitorS4")) {
    stop("Object must be of class MonitorS4")
  }
  
  # Run basic analysis
  activity_analysis <- analyze_activity(object)
  
  # Create simple HTML report
  html_content <- sprintf('
<!DOCTYPE html>
<html>
<head>
    <title>RMB Quick Summary Report</title>
    <style>
        body { font-family: Arial, sans-serif; margin: 40px; }
        table { border-collapse: collapse; width: 100%%; }
        th, td { border: 1px solid #ddd; padding: 8px; text-align: left; }
        th { background-color: #f2f2f2; }
        .summary { background-color: #f9f9f9; padding: 20px; margin: 20px 0; }
    </style>
</head>
<body>
    <h1>RMB Quick Summary Report</h1>
    <p><strong>Generated:</strong> %s</p>
    
    <div class="summary">
        <h2>Experiment Summary</h2>
        <p><strong>Number of flies:</strong> %d</p>
        <p><strong>Available assays:</strong> %s</p>
        <p><strong>Duration:</strong> %.2f hours</p>
        <p><strong>Time range:</strong> %s to %s</p>
    </div>
    
    <h2>Activity Statistics Summary</h2>
    <p>Mean total activity: %.2f</p>
    <p>Range: %.2f - %.2f</p>
    <p>Standard deviation: %.2f</p>
    
    <hr>
    <p><em>Generated by RMB package</em></p>
</body>
</html>',
    Sys.time(),
    nrow(object@meta.data),
    paste(names(object@assays), collapse = ", "),
    object@time$duration_hours,
    format(object@time$start_time),
    format(object@time$end_time),
    mean(activity_analysis$overall_stats$total_activity, na.rm = TRUE),
    min(activity_analysis$overall_stats$total_activity, na.rm = TRUE),
    max(activity_analysis$overall_stats$total_activity, na.rm = TRUE),
    sd(activity_analysis$overall_stats$total_activity, na.rm = TRUE)
  )
  
  writeLines(html_content, output_file)
  
  cat("Quick summary report generated:", output_file, "\n")
  
  if (interactive()) {
    utils::browseURL(output_file)
  }
  
  return(output_file)
}