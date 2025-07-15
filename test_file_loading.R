#!/usr/bin/env Rscript

# Test script to verify the updated RMB_test.R works up to the metadata loading

cat("Testing RMB_test.R - Metadata and File Loading\n")
cat("==============================================\n\n")

# Set working directory
setwd("/scr1/users/liy27/20250709_RMB/dep/RMB")

# Test the first part of RMB_test.R
tryCatch({
  # Load the config and modify here as needed
  base_dir <- "/scr1/users/liy27/20250709_RMB/dep/RMB"
  setwd(base_dir)
  data_dir <- "./test/"
  date <- "20250709"
  
  # Source only essential scripts
  source("./src/metadata.R")
  source("./src/metadata_workflow.R")
  
  # Generate metadata
  cat("Generating metadata files...\n")
  source("./generate_metadata_test.R")
  
  # Load monitor files
  monitor_files <- load_monitor_files(data_dir)
  cat("Found", length(monitor_files), "monitor files:\n")
  for (i in 1:length(monitor_files)) {
    cat("  ", i, ":", basename(monitor_files[i]), "\n")
  }
  
  # Create placeholder monitor objects
  create_monitorS4 <- function(file_path) {
    monitor_obj <- list(
      file_path = file_path,
      monitor_name = tools::file_path_sans_ext(basename(file_path)),
      data = NULL,
      meta.data = NULL
    )
    class(monitor_obj) <- "monitorS4"
    return(monitor_obj)
  }
  
  # Create monitor list
  monitor_list <- list()
  for (i in 1:length(monitor_files)) {
    monitor_list[[i]] <- create_monitorS4(monitor_files[i])
  }
  
  cat("Created monitor_list with", length(monitor_list), "elements\n")
  
  # Load metadata files
  metadata_files <- list.files(path = file.path(data_dir, "metadata/"),
                              pattern = "metadata", full.names = TRUE)
  
  if (length(metadata_files) > 0) {
    cat("Found", length(metadata_files), "metadata files:\n")
    for (file in metadata_files) {
      cat("  -", basename(file), "\n")
    }
    
    for (i in 1:min(length(metadata_files), length(monitor_list))) {
      metadata <- read.csv(metadata_files[i])
      monitor_list[[i]]$meta.data <- metadata
      cat("Loaded metadata for monitor", i, "- dimensions:", dim(metadata), "\n")
    }
    
    if (length(monitor_list) > 0 && !is.null(monitor_list[[1]]$meta.data)) {
      cat("Sample metadata columns:", paste(colnames(monitor_list[[1]]$meta.data), collapse = ", "), "\n")
      cat("Sample metadata preview:\n")
      print(head(monitor_list[[1]]$meta.data[, c("Monitor_number", "Channel", "Group", "Treatment", "Sex")], 3))
    }
  } else {
    cat("Warning: No metadata files found\n")
  }
  
  cat("\n✅ File loading test completed successfully!\n")
  cat("Monitor list length:", length(monitor_list), "\n")
  cat("Metadata files found:", length(metadata_files), "\n")
  
}, error = function(e) {
  cat("❌ Error in test:", e$message, "\n")
  cat("Call stack:\n")
  print(sys.calls())
})
