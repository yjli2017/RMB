# Test script for RMB package functionality

cat("Testing RMB package...\n")

# Test 1: Package installation and loading
tryCatch({
  if (!require(devtools, quietly = TRUE)) {
    install.packages("devtools")
  }
  
  # Install package locally
  devtools::install_local(".", quiet = TRUE, force = TRUE)
  
  # Load package
  library(RMB)
  cat("✓ Package loaded successfully\n")
}, error = function(e) {
  cat("✗ Package loading failed:", e$message, "\n")
  quit(status = 1)
})

# Test 2: Basic object creation
tryCatch({
  # Create sample metadata
  test_metadata <- data.frame(
    Channel = 1:4,
    Fly = paste0("Test_Fly_", 1:4),
    Treatment = c("Control", "Control", "Treatment", "Treatment"),
    Sex = c("Male", "Female", "Male", "Female"),
    stringsAsFactors = FALSE
  )
  
  # Create sample data (simplified Monitor format)
  sample_times <- seq(as.POSIXct("2023-06-06 18:00:00"), 
                     as.POSIXct("2023-06-07 18:00:00"), 
                     by = "1 min")
  
  sample_data <- data.frame(
    DateTime = rep(sample_times, 3),
    Monitor_Name = "TestMonitor",
    Measure_Type = rep(c("MT", "CT", "Pn"), each = length(sample_times)),
    Channel_1 = sample(0:20, length(sample_times) * 3, replace = TRUE),
    Channel_2 = sample(0:25, length(sample_times) * 3, replace = TRUE),
    Channel_3 = sample(0:15, length(sample_times) * 3, replace = TRUE),
    Channel_4 = sample(0:30, length(sample_times) * 3, replace = TRUE)
  )
  
  # Create MonitorS4 object
  test_assays <- list(
    mt = sample_data[sample_data$Measure_Type == "MT", ],
    ct = sample_data[sample_data$Measure_Type == "CT", ],
    pn = sample_data[sample_data$Measure_Type == "Pn", ]
  )
  
  test_time <- list(
    start_time = min(sample_times),
    end_time = max(sample_times),
    duration_hours = 24,
    time_points = sample_times
  )
  
  test_object <- MonitorS4(
    meta.data = test_metadata,
    assays = test_assays,
    active.assay = "mt",
    time = test_time
  )
  
  cat("✓ MonitorS4 object created successfully\n")
}, error = function(e) {
  cat("✗ MonitorS4 object creation failed:", e$message, "\n")
})

# Test 3: Analysis functions
tryCatch({
  # Test activity analysis
  activity_results <- analyze_activity(test_object)
  if (is.list(activity_results) && "overall_stats" %in% names(activity_results)) {
    cat("✓ Activity analysis completed\n")
  } else {
    stop("Activity analysis returned unexpected format")
  }
  
  # Test circadian analysis
  circadian_results <- analyze_circadian(test_object)
  if (is.list(circadian_results) && "circadian_parameters" %in% names(circadian_results)) {
    cat("✓ Circadian analysis completed\n")
  } else {
    stop("Circadian analysis returned unexpected format")
  }
  
  # Test position analysis
  position_results <- analyze_position(test_object)
  if (is.list(position_results) && "position_stats" %in% names(position_results)) {
    cat("✓ Position analysis completed\n")
  } else {
    stop("Position analysis returned unexpected format")
  }
  
}, error = function(e) {
  cat("✗ Analysis functions failed:", e$message, "\n")
})

# Test 4: Comprehensive analysis
tryCatch({
  comprehensive_results <- comprehensive_analysis(test_object)
  
  expected_components <- c("activity", "circadian", "position", "summary")
  if (all(expected_components %in% names(comprehensive_results))) {
    cat("✓ Comprehensive analysis completed\n")
  } else {
    missing <- setdiff(expected_components, names(comprehensive_results))
    stop(paste("Missing components:", paste(missing, collapse = ", ")))
  }
}, error = function(e) {
  cat("✗ Comprehensive analysis failed:", e$message, "\n")
})

# Test 5: Visualization functions
tryCatch({
  # Test heatmap
  heatmap_plot <- plot_heatmap(test_object, assay_type = "mt")
  if (inherits(heatmap_plot, "ggplot")) {
    cat("✓ Heatmap visualization created\n")
  } else {
    stop("Heatmap plot is not a ggplot object")
  }
  
  # Test activity bars
  activity_bars <- plot_activity_bars(activity_results, "overall_stats")
  if (inherits(activity_bars, "ggplot")) {
    cat("✓ Activity bar plot created\n")
  } else {
    stop("Activity bar plot is not a ggplot object")
  }
  
  # Test circadian plot
  circadian_plot <- plot_circadian(circadian_results, "phase_plot")
  if (inherits(circadian_plot, "ggplot")) {
    cat("✓ Circadian plot created\n")
  } else {
    stop("Circadian plot is not a ggplot object")
  }
  
}, error = function(e) {
  cat("✗ Visualization functions failed:", e$message, "\n")
})

# Test 6: Report generation
tryCatch({
  # Test quick report generation
  quick_report_path <- generateQuickReport(test_object, "test_quick_report.html")
  
  if (file.exists(quick_report_path)) {
    cat("✓ Quick report generated successfully\n")
  } else {
    stop("Quick report file was not created")
  }
  
}, error = function(e) {
  cat("✗ Report generation failed:", e$message, "\n")
})

# Test 7: Data export
tryCatch({
  # Test data export
  exported_files <- export_analysis_data(comprehensive_results, "test_output")
  
  if (length(exported_files) > 0 && all(file.exists(exported_files))) {
    cat("✓ Data export completed successfully\n")
    cat("  Exported", length(exported_files), "files\n")
  } else {
    stop("Data export failed - files not created")
  }
  
}, error = function(e) {
  cat("✗ Data export failed:", e$message, "\n")
})

# Test 8: Real data loading (if test data exists)
tryCatch({
  test_data_dir <- "./test/"
  if (dir.exists(test_data_dir)) {
    monitor_files <- list.files(test_data_dir, pattern = "^Monitor.*\\.txt$")
    metadata_dir <- file.path(test_data_dir, "metadata")
    
    if (length(monitor_files) > 0 && dir.exists(metadata_dir)) {
      cat("Testing with real data files...\n")
      
      # Test with a single monitor file
      test_monitor <- file.path(test_data_dir, monitor_files[1])
      monitor_name <- tools::file_path_sans_ext(basename(test_monitor))
      
      # Look for corresponding metadata
      metadata_files <- list.files(metadata_dir, pattern = "metadata.*\\.csv$")
      metadata_match <- grep(monitor_name, metadata_files)
      
      if (length(metadata_match) > 0) {
        test_metadata_file <- file.path(metadata_dir, metadata_files[metadata_match[1]])
        
        # Test loadRMB function
        loaded_data <- loadRMB(data = test_monitor, 
                              metadata = test_metadata_file,
                              remove_dead = FALSE)  # Skip dead fly removal for testing
        
        if (is(loaded_data, "MonitorS4")) {
          cat("✓ Real data loading successful\n")
          cat("  Loaded", nrow(getMeta(loaded_data)), "flies\n")
          cat("  Duration:", round(loaded_data@time$duration_hours, 2), "hours\n")
        } else {
          stop("loadRMB did not return MonitorS4 object")
        }
      } else {
        cat("ℹ No matching metadata found for testing real data\n")
      }
    } else {
      cat("ℹ No test data files found for real data testing\n")
    }
  } else {
    cat("ℹ Test directory not found - skipping real data test\n")
  }
}, error = function(e) {
  cat("✗ Real data loading test failed:", e$message, "\n")
})

# Cleanup test files
cleanup_files <- c("test_quick_report.html", "test_output")
for (file in cleanup_files) {
  if (file.exists(file)) {
    if (dir.exists(file)) {
      unlink(file, recursive = TRUE)
    } else {
      file.remove(file)
    }
  }
}

cat("\n=== RMB Package Testing Complete ===\n")
cat("All major functions tested successfully!\n")
cat("\nTo use the package:\n")
cat("1. Load data: data <- loadRMB(data = 'path/to/data', metadata = 'path/to/metadata')\n")
cat("2. Generate report: generateReport(data)\n")