#' @title Utility Functions
#' @description Helper functions for RMB package
#' @name utils

#' Load Monitor Files
#'
#' Find and list all Monitor*.txt files in a directory
#'
#' @param data_dir Directory to search
#' @param pattern File pattern (default "Monitor.*\\.txt$")
#' @param exclude_LC Exclude MonitorLC files (default TRUE)
#'
#' @return Character vector of file paths
#' @export
loadMonitorFiles <- function(data_dir, pattern = "Monitor.*\\.txt$", exclude_LC = TRUE) {
  files <- list.files(data_dir, pattern = pattern, full.names = TRUE)
  
  if (exclude_LC) {
    lc_pattern <- "MonitorLC"
    files <- files[!grepl(lc_pattern, files)]
  }
  
  if (length(files) == 0) {
    warning("No monitor files found in: ", data_dir)
  }
  
  return(files)
}

#' Load Metadata
#'
#' Load metadata CSV files for monitors
#'
#' @param metadata_dir Directory containing metadata files
#' @param monitor_names Optional vector of monitor names to match
#'
#' @return List of data frames
#' @export
loadMetadata <- function(metadata_dir, monitor_names = NULL) {
  
  if (!dir.exists(metadata_dir)) {
    warning("Metadata directory not found: ", metadata_dir)
    return(list())
  }
  
  files <- list.files(metadata_dir, pattern = "metadata\\.csv$", full.names = TRUE)
  
  if (length(files) == 0) {
    warning("No metadata files found in: ", metadata_dir)
    return(list())
  }
  
  metadata_list <- lapply(files, function(f) {
    read.csv(f, stringsAsFactors = FALSE)
  })
  
  names(metadata_list) <- gsub("_metadata\\.csv$", "", basename(files))
  
  return(metadata_list)
}

#' Save Results
#'
#' Save analysis results to files
#'
#' @param monitor A MonitorS4 object
#' @param output_dir Output directory
#' @param prefix File name prefix
#' @param formats Formats to save ("rds", "csv", "both") (default "both")
#'
#' @return Invisibly returns list of saved file paths
#' @export
saveResults <- function(monitor, output_dir = ".", prefix = "rmb_results",
                        formats = "both") {
  
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  saved_files <- list()
  
  # Save RDS
  if (formats %in% c("rds", "both")) {
    rds_path <- file.path(output_dir, paste0(prefix, ".rds"))
    saveRDS(monitor, rds_path)
    saved_files$rds <- rds_path
    message("Saved: ", rds_path)
  }
  
  # Save CSVs
  if (formats %in% c("csv", "both")) {
    # Activity data
    if (!is.null(monitor@assays$mt)) {
      mt_path <- file.path(output_dir, paste0(prefix, "_activity.csv"))
      write.csv(monitor@assays$mt, mt_path)
      saved_files$activity <- mt_path
    }
    
    # Sleep data
    if (!is.null(monitor@assays$sleep)) {
      sleep_path <- file.path(output_dir, paste0(prefix, "_sleep.csv"))
      write.csv(monitor@assays$sleep, sleep_path)
      saved_files$sleep <- sleep_path
    }
    
    # Summary stats
    if (!is.null(monitor@assays$mt)) {
      summary_df <- data.frame(
        fly = colnames(monitor@assays$mt),
        total_activity = colSums(monitor@assays$mt, na.rm = TRUE)
      )
      
      if (!is.null(monitor@assays$sleep)) {
        summary_df$total_sleep <- colSums(monitor@assays$sleep, na.rm = TRUE)
        summary_df$percent_sleep <- summary_df$total_sleep / nrow(monitor@assays$sleep) * 100
      }
      
      if (nrow(monitor@meta.data) > 0) {
        for (col in colnames(monitor@meta.data)) {
          if (length(monitor@meta.data[[col]]) == nrow(summary_df)) {
            summary_df[[col]] <- monitor@meta.data[[col]]
          }
        }
      }
      
      summary_path <- file.path(output_dir, paste0(prefix, "_summary.csv"))
      write.csv(summary_df, summary_path, row.names = FALSE)
      saved_files$summary <- summary_path
      message("Saved: ", summary_path)
    }
  }
  
  invisible(saved_files)
}

#' Calculate Zeitgeber Time
#'
#' Convert raw time index to Zeitgeber time (ZT)
#'
#' @param time_idx Time index (1-based)
#' @param lights_on Hour when lights turn on (default 0)
#' @param minutes_per_day Minutes per day (default 1440)
#'
#' @return ZT hour (0-24)
#' @export
calculateZT <- function(time_idx, lights_on = 0, minutes_per_day = 1440) {
  minutes_of_day <- (time_idx - 1) %% minutes_per_day
  zt <- (minutes_of_day / 60 + lights_on) %% 24
  return(zt)
}

#' Get Day Number
#'
#' Calculate which day a time index falls on
#'
#' @param time_idx Time index (1-based)
#' @param minutes_per_day Minutes per day (default 1440)
#'
#' @return Day number (1-based)
#' @export
getDay <- function(time_idx, minutes_per_day = 1440) {
  day <- ((time_idx - 1) %/% minutes_per_day) + 1
  return(day)
}

#' Print Package Info
#'
#' Print information about the RMB package
#'
#' @export
rmbInfo <- function() {
  cat("\n")
  cat("╔══════════════════════════════════════════════════════════╗\n")
  cat("║                                                          ║\n")
  cat("║    RMB: R Multi-Beam Activity Analysis                   ║\n")
  cat("║                                                          ║\n")
  cat("╚══════════════════════════════════════════════════════════╝\n\n")
  
  cat("Version: 1.0.0\n")
  cat("Author: Yongjun Li\n")
  cat("GitHub: https://github.com/yjli2017/RMB\n\n")
  
  cat("Quick Start:\n")
  cat("  results <- runPipeline('path/to/data')\n")
  cat("  monitor <- quickAnalysis('path/to/data')\n\n")
  
  cat("Documentation:\n")
  cat("  vignette('getting-started', package = 'RMB')\n")
  cat("  ?runPipeline\n")
  cat("  ?MonitorS4\n\n")
}

#' Check Data Quality
#'
#' Perform quality checks on a MonitorS4 object
#'
#' @param monitor A MonitorS4 object
#' @param verbose Print results (default TRUE)
#'
#' @return List of quality metrics
#' @export
checkQuality <- function(monitor, verbose = TRUE) {
  
  results <- list()
  
  if (verbose) {
    cat("\n=== Data Quality Check ===\n\n")
  }
  
  # Check activity data
  if (!is.null(monitor@assays$mt)) {
    mt <- monitor@assays$mt
    
    results$n_timepoints <- nrow(mt)
    results$n_channels <- ncol(mt)
    results$n_days <- nrow(mt) / 1440
    
    # Check for dead flies (total activity < threshold)
    total_activity <- colSums(mt, na.rm = TRUE)
    results$dead_flies <- sum(total_activity < 10)
    results$mean_activity <- mean(total_activity)
    
    # Check for missing values
    results$na_count <- sum(is.na(mt))
    results$na_percent <- results$na_count / (nrow(mt) * ncol(mt)) * 100
    
    if (verbose) {
      cat("Activity Data:\n")
      cat("  Timepoints:", results$n_timepoints, sprintf("(%.1f days)\n", results$n_days))
      cat("  Channels:", results$n_channels, "\n")
      cat("  Likely dead flies:", results$dead_flies, "\n")
      cat("  Mean total activity:", round(results$mean_activity, 1), "\n")
      cat("  Missing values:", results$na_count, sprintf("(%.2f%%)\n", results$na_percent))
    }
  }
  
  # Check metadata
  if (nrow(monitor@meta.data) > 0) {
    results$has_metadata <- TRUE
    results$metadata_cols <- colnames(monitor@meta.data)
    
    if (verbose) {
      cat("\nMetadata:\n")
      cat("  Columns:", paste(results$metadata_cols, collapse = ", "), "\n")
    }
  } else {
    results$has_metadata <- FALSE
    if (verbose) cat("\nMetadata: None\n")
  }
  
  # Overall quality
  results$quality_score <- 100
  if (results$dead_flies > 0) results$quality_score <- results$quality_score - results$dead_flies
  if (results$na_percent > 0) results$quality_score <- results$quality_score - results$na_percent
  results$quality_score <- max(0, results$quality_score)
  
  if (verbose) {
    cat("\nQuality Score:", round(results$quality_score, 1), "/ 100\n\n")
  }
  
  invisible(results)
}
