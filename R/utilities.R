#' Utility Functions for RMB Package
#'
#' @description Core utility functions for processing TriKinetics monitor data
#' This file contains the complete tested pipeline for RMB data analysis.

#' Parse TriKinetics monitor data file
#'
#' @param file_path Path to Monitor*.txt file
#' @return A data.frame with parsed monitor data
#' @export
parse_monitor_file <- function(file_path) {
  if (!file.exists(file_path)) {
    stop("File does not exist: ", file_path)
  }
  
  # Read the raw data
  raw_data <- readr::read_delim(file_path, delim = "\t", col_names = FALSE, show_col_types = FALSE)
  
  # Extract monitor number from filename
  monitor_name <- tools::file_path_sans_ext(basename(file_path))
  
  # The first 10 columns are metadata, followed by 32 channel data columns
  # Format: Index, Date, Time, Status1, Status2, Monitor, Light, Measure_Type, + 2 more metadata, then 32 channels
  
  # Create column names - there are exactly 32 fly channels (columns 11-42)
  n_channels <- 32  # Fixed: always 32 channels for TriKinetics monitors
  channel_cols <- paste0("Channel_", 1:n_channels)
  
  # First 10 columns are metadata, then 32 channel columns
  metadata_cols <- c("Index", "Date", "Time", "Status1", "Status2", 
                     "Monitor", "Light", "Measure_Type", "Meta9", "Meta10")
  colnames(raw_data) <- c(metadata_cols, channel_cols)
  
  # Parse datetime
  raw_data$DateTime <- lubridate::dmy_hms(paste(raw_data$Date, raw_data$Time))
  
  # Add monitor name
  raw_data$Monitor_Name <- monitor_name
  
  return(raw_data)
}

#' Process monitor data into different assay types
#'
#' @param raw_data Parsed monitor data from parse_monitor_file
#' @return A list with different assay types (mt, ct, pn)
#' @export
process_monitor_assays <- function(raw_data) {
  # Split by measure type
  mt_data <- raw_data[raw_data$Measure_Type == "MT", ]  # Motor activity
  ct_data <- raw_data[raw_data$Measure_Type == "CT", ]  # Continuous tracking
  pn_data <- raw_data[raw_data$Measure_Type == "PN", ]  # Position data
  
  # Return list of assays
  assays <- list(
    mt = mt_data,
    ct = ct_data,
    pn = pn_data,
    raw_data = raw_data
  )
  
  return(assays)
}

#' Clean monitor data to specific analysis days
#'
#' @param object MonitorS4 object
#' @param days Vector of full day numbers to include in analysis (default: c(2,3,4))
#' @return MonitorS4 object with data filtered to specified days
#' @export
#' @examples
#' \dontrun{
#' data <- loadRMB("./monitors/", "./metadata/")
#' data <- CleanDays(data, days = c(2,3,4))
#' }
CleanDays <- function(object, days = c(2,3,4)) {
  return(clean_monitor_days(object, full_days_to_include = days))
}

#' Internal function: Clean monitor data to specific full days
#'
#' @param object MonitorS4 object
#' @param full_days_to_include Vector of full day numbers to include (default: c(2,3,4))
#' @return MonitorS4 object with cleaned data
clean_monitor_days <- function(object, full_days_to_include = c(2,3,4)) {
  
  if (!is(object, "MonitorS4")) {
    stop("Object must be of class MonitorS4")
  }
  
  if (!"mt" %in% names(object@assays)) {
    stop("MT assay not found in object")
  }
  
  mt_data <- object@assays[["mt"]]
  datetime_col <- mt_data$DateTime
  
  # Create date groupings
  dates <- as.Date(datetime_col)
  unique_dates <- sort(unique(dates))
  
  cat("Found", length(unique_dates), "unique days in data:\n")
  
  # Analyze each day to determine if it's full or partial
  full_days <- c()
  full_day_counter <- 0
  
  for (i in seq_along(unique_dates)) {
    date <- unique_dates[i]
    points_in_day <- sum(dates == date)
    
    # A full day should have 1440 points (24 hours * 60 minutes) for 1-minute data
    is_full_day <- points_in_day == 1440
    
    if (is_full_day) {
      full_day_counter <- full_day_counter + 1
      full_days <- c(full_days, full_day_counter)  # Store full day number
      status <- paste("(Full)")
    } else {
      status <- "(Partial)"
    }
    
    cat("Day", i, ":", as.character(date), "- Points:", points_in_day, status, "\n")
  }
  
  cat("\nFull days identified:", length(full_days), "\n")
  
  # Select the requested full days
  if (length(full_days) < max(full_days_to_include)) {
    stop("Not enough full days available. Found ", length(full_days), " but need ", max(full_days_to_include))
  }
  
  # Map full day numbers to actual date indices
  full_day_date_map <- data.frame(
    full_day_num = full_days,
    date_index = which(sapply(seq_along(unique_dates), function(i) {
      date <- unique_dates[i]
      points_in_day <- sum(dates == date)
      points_in_day == 1440
    }))
  )
  
  selected_date_indices <- full_day_date_map$date_index[full_day_date_map$full_day_num %in% full_days_to_include]
  selected_dates <- unique_dates[selected_date_indices]
  
  cat("Selecting full days:", paste(full_days_to_include, collapse = ", "), "\n")
  cat("Date range:", as.character(min(selected_dates)), "to", as.character(max(selected_dates)), "\n")
  
  # Filter data to selected days
  keep_indices <- dates %in% selected_dates
  expected_points <- length(full_days_to_include) * 1440
  actual_points <- sum(keep_indices)
  
  cat("Expected total points:", expected_points, "\n")
  cat("Actual total points:", actual_points, "\n")
  
  if (actual_points != expected_points) {
    warning("Point count mismatch. Expected: ", expected_points, ", Actual: ", actual_points)
  }
  
  # Filter all assays to the selected timepoints (MT, CT, PN share same structure)
  for (assay_name in c("mt", "ct", "pn")) {
    if (assay_name %in% names(object@assays) && !is.null(object@assays[[assay_name]])) {
      object@assays[[assay_name]] <- object@assays[[assay_name]][keep_indices, ]
    }
  }
  
  # Remove raw_data assay as we don't need it after processing
  if ("raw_data" %in% names(object@assays)) {
    object@assays[["raw_data"]] <- NULL
  }
  
  # Update time information
  object@time$time_points <- datetime_col[keep_indices]
  object@time$analysis_period <- list(
    full_days_included = full_days_to_include,
    date_range = c(min(selected_dates), max(selected_dates)),
    total_points = actual_points,
    days_analyzed = length(full_days_to_include)
  )
  
  cat("\nData successfully cleaned to", length(full_days_to_include), "full days\n")
  
  return(object)
}

#' Detect dead flies based on activity patterns
#'
#' @param object MonitorS4 object (preferably after CleanDays)
#' @param threshold_hours Hours of no movement to consider dead (default: 24)
#' @return MonitorS4 object with mortality information added to metadata
#' @export
#' @examples
#' \dontrun{
#' data <- loadRMB("./monitors/", "./metadata/")
#' data <- CleanDays(data, days = c(2,3,4))
#' data <- DetectDeadFlies(data, threshold_hours = 24)
#' }
DetectDeadFlies <- function(object, threshold_hours = 24) {
  return(detect_dead_flies_final_day(object, no_movement_threshold = threshold_hours))
}

#' Internal function: Detect dead flies using final day activity
#'
#' @param object MonitorS4 object with cleaned data
#' @param no_movement_threshold Hours of no movement to consider dead (default: 24)
#' @return MonitorS4 object with mortality information in metadata
detect_dead_flies_final_day <- function(object, no_movement_threshold = 24) {
  
  if (!is(object, "MonitorS4")) {
    stop("Object must be of class MonitorS4")
  }
  
  mt_data <- object@assays[["mt"]]
  metadata <- object@meta.data
  
  # Get the final day of the analysis period
  datetime_col <- mt_data$DateTime
  dates <- as.Date(datetime_col)
  unique_dates <- sort(unique(dates))
  final_date <- max(unique_dates)
  
  cat("Detecting dead flies using final day:", as.character(final_date), "\n")
  
  # Get final day data
  final_day_indices <- dates == final_date
  final_day_points <- sum(final_day_indices)
  
  cat("Final day data points:", final_day_points, "\n")
  
  # Calculate threshold: number of timepoints for no_movement_threshold hours
  # 1 minute intervals = 60 points per hour
  threshold_points <- no_movement_threshold * 60
  
  cat("Checking last", no_movement_threshold, "hours of final day\n")
  
  # Get the last threshold_points from final day
  if (final_day_points >= threshold_points) {
    final_period_start <- final_day_points - threshold_points + 1
    final_period_indices <- which(final_day_indices)
    check_indices <- final_period_indices[final_period_start:final_day_points]
  } else {
    # If final day has fewer points than threshold, use all final day points
    check_indices <- which(final_day_indices)
  }
  
  final_period_data <- mt_data[check_indices, ]
  start_time <- min(final_period_data$DateTime)
  end_time <- max(final_period_data$DateTime)
  
  cat("Time range:", as.character(start_time), "to", as.character(end_time), "\n\n")
  
  # Initialize mortality tracking
  metadata$mortality_status <- "alive"
  metadata$final_day_activity <- 0
  metadata$total_experiment_activity <- 0
  metadata$mortality_type <- "alive"
  
  # Analyze each channel
  dead_count <- 0
  completely_inactive_count <- 0
  final_day_inactive_count <- 0
  
  channel_cols <- grep("Channel_", colnames(final_period_data))
  
  for (i in seq_along(channel_cols)) {
    channel_name <- colnames(final_period_data)[channel_cols[i]]
    # Extract channel identifier (e.g., "Channel_1_Monitor36" -> "1_Monitor36")
    channel_id <- gsub("Channel_", "", channel_name)
    
    # Find corresponding metadata row
    meta_row <- which(metadata$Channel == channel_id)
    
    if (length(meta_row) == 1) {
      # Calculate activities
      final_period_activity <- sum(final_period_data[[channel_name]], na.rm = TRUE)
      
      # Get total experiment activity for this channel
      total_activity <- sum(mt_data[[channel_name]], na.rm = TRUE)
      
      # Store in metadata
      metadata$final_day_activity[meta_row] <- final_period_activity
      metadata$total_experiment_activity[meta_row] <- total_activity
      
      # Determine mortality status
      if (final_period_activity == 0) {
        dead_count <- dead_count + 1
        metadata$mortality_status[meta_row] <- "dead"
        
        if (total_activity == 0) {
          completely_inactive_count <- completely_inactive_count + 1
          metadata$mortality_type[meta_row] <- "completely_inactive"
        } else {
          final_day_inactive_count <- final_day_inactive_count + 1
          metadata$mortality_type[meta_row] <- "final_day_inactive"
        }
      }
    }
  }
  
  # Update metadata in object
  object@meta.data <- metadata
  
  # Store mortality analysis in time slot
  object@time$mortality_analysis <- list(
    analysis_date = Sys.time(),
    final_day = final_date,
    threshold_hours = no_movement_threshold,
    total_flies = nrow(metadata),
    dead_flies = dead_count,
    completely_inactive = completely_inactive_count,
    final_day_inactive = final_day_inactive_count,
    mortality_rate = round(dead_count / nrow(metadata) * 100, 1)
  )
  
  cat("Dead fly analysis results:\n")
  cat("Total flies:", nrow(metadata), "\n")
  cat("Dead in final", no_movement_threshold, "hours:", dead_count, "\n")
  cat("  - Completely inactive entire experiment:", completely_inactive_count, "\n")
  cat("  - Active earlier but inactive final", no_movement_threshold, "hours:", final_day_inactive_count, "\n")
  cat("Active in final", no_movement_threshold, "hours:", nrow(metadata) - dead_count, "\n\n")
  
  cat("Metadata updated with mortality information\n")
  cat("Mortality rate:", round(dead_count / nrow(metadata) * 100, 1), "%\n")
  
  return(object)
}

#' Filter to alive flies only
#'
#' @param object MonitorS4 object (must have mortality information from DetectDeadFlies)
#' @return MonitorS4 object with only alive flies included in analysis
#' @export
#' @examples
#' \dontrun{
#' data <- loadRMB("./monitors/", "./metadata/")
#' data <- CleanDays(data, days = c(2,3,4))
#' data <- DetectDeadFlies(data)
#' data <- FilterAliveFlies(data)
#' }
FilterAliveFlies <- function(object) {
  return(filter_to_alive_flies(object))
}

#' Internal function: Filter to alive flies
filter_to_alive_flies <- function(object) {
  
  if (!is(object, "MonitorS4")) {
    stop("Object must be of class MonitorS4")
  }
  
  metadata <- object@meta.data
  
  if (!"mortality_status" %in% colnames(metadata)) {
    stop("No mortality information found. Run detect_dead_flies_final_day() first.")
  }
  
  # Identify alive flies
  alive_flies <- metadata$mortality_status == "alive"
  alive_channels <- metadata$Channel[alive_flies]
  dead_channels <- metadata$Channel[!alive_flies]
  
  cat("Filtering to alive flies only:\n")
  cat("- Total flies:", nrow(metadata), "\n")
  cat("- Alive flies:", sum(alive_flies), "\n")
  cat("- Dead flies to remove:", sum(!alive_flies), "\n")
  
  if (sum(!alive_flies) > 0) {
    cat("- Dead fly channels:", paste(dead_channels, collapse = ", "), "\n")
  }
  
  # Filter each assay
  for (assay_name in names(object@assays)) {
    assay_data <- object@assays[[assay_name]]
    
    if (!is.null(assay_data) && is.data.frame(assay_data)) {
      channel_cols <- grep("Channel_", colnames(assay_data))
      
      if (length(channel_cols) > 0) {
        # Remove dead fly channels
        cols_to_remove <- c()
        
        for (channel_col in channel_cols) {
          channel_name <- colnames(assay_data)[channel_col]
          channel_num <- as.numeric(gsub("Channel_", "", channel_name))
          
          if (channel_num %in% dead_channels) {
            cols_to_remove <- c(cols_to_remove, channel_col)
          }
        }
        
        if (length(cols_to_remove) > 0) {
          object@assays[[assay_name]] <- assay_data[, -cols_to_remove]
          cat("- Removed", length(cols_to_remove), "dead fly channels from", assay_name, "assay\n")
        }
      }
    }
  }
  
  # Update metadata - mark which flies are included in analysis
  metadata$included_in_analysis <- alive_flies
  object@meta.data <- metadata
  
  cat("- Preserved all metadata with 'included_in_analysis' flag\n")
  
  # Verification
  mt_data <- object@assays[["mt"]]
  if (!is.null(mt_data)) {
    remaining_channels <- ncol(mt_data) - 10  # Subtract metadata columns
    expected_channels <- sum(alive_flies)
    data_points <- nrow(mt_data)
    
    cat("\nFiltering verification:\n")
    cat("- Expected alive channels:", expected_channels, "\n")
    cat("- Actual channels in MT assay:", remaining_channels, "\n")
    cat("- Data points per channel:", data_points, "\n")
    cat("- Total data points for analysis:", remaining_channels * data_points, "\n")
    
    if (remaining_channels == expected_channels) {
      cat("✓ Filtering successful - ready for analysis with alive flies only\n")
    } else {
      warning("Channel count mismatch after filtering")
    }
  }
  
  return(object)
}

#' Save cleaned MonitorS4 object to RDS file
#'
#' @param object MonitorS4 object to save
#' @param output_path Path for output RDS file
#' @param overwrite Whether to overwrite existing file (default: FALSE)
#' @return NULL (saves file)
#' @export
save_cleaned_monitor <- function(object, output_path, overwrite = FALSE) {
  
  if (!is(object, "MonitorS4")) {
    stop("Object must be of class MonitorS4")
  }
  
  if (file.exists(output_path) && !overwrite) {
    stop("File already exists. Set overwrite=TRUE to replace it.")
  }
  
  # Add save timestamp
  object@time$saved_timestamp <- Sys.time()
  
  # Save the object
  saveRDS(object, file = output_path)
  
  # Calculate file size
  file_size <- file.size(output_path)
  size_mb <- round(file_size / (1024^2), 2)
  
  cat("Cleaned MonitorS4 object saved to:", output_path, "\n")
  cat("File size:", size_mb, "MB\n")
  
  # Print summary
  if (!is.null(object@time$mortality_analysis)) {
    mortality_info <- object@time$mortality_analysis
    monitor_info <- object@meta.data$Monitor_number[1]
    
    if (!is.null(monitor_info)) {
      if (!is.na(monitor_info)) {
        monitor_name <- paste0("Monitor", monitor_info)
      } else {
        monitor_name <- "Unknown"
      }
    } else {
      monitor_name <- "Unknown"
    }
    
    analysis_info <- object@time$analysis_period
    
    cat("\nSaved object summary:\n")
    cat("- Monitor:", monitor_name, "\n")
    cat("- Total flies:", mortality_info$total_flies, "\n")
    cat("- Alive flies:", mortality_info$total_flies - mortality_info$dead_flies, "\n")
    cat("- Dead flies:", mortality_info$dead_flies, "\n")
    cat("- Data points:", nrow(object@assays$mt), "\n")
    cat("- Analysis period:", analysis_info$days_analyzed, "days\n")
    cat("- Date range:", paste(analysis_info$date_range, collapse = " to "), "\n")
  }
}

#' Load cleaned MonitorS4 object from RDS file
#'
#' @param file_path Path to RDS file
#' @return MonitorS4 object
#' @export
load_cleaned_monitor <- function(file_path) {
  
  if (!file.exists(file_path)) {
    stop("File does not exist: ", file_path)
  }
  
  object <- readRDS(file_path)
  
  if (!is(object, "MonitorS4")) {
    stop("File does not contain a MonitorS4 object")
  }
  
  # Calculate file size
  file_size <- file.size(file_path)
  size_mb <- round(file_size / (1024^2), 2)
  
  cat("Loaded MonitorS4 object from:", file_path, "\n")
  cat("File size:", size_mb, "MB\n")
  
  # Print summary if available
  if (!is.null(object@time$mortality_analysis)) {
    mortality_info <- object@time$mortality_analysis
    monitor_info <- object@meta.data$Monitor_number[1]
    
    if (!is.null(monitor_info)) {
      if (!is.na(monitor_info)) {
        monitor_name <- paste0("Monitor", monitor_info)
      } else {
        monitor_name <- "Unknown"
      }
    } else {
      monitor_name <- "Unknown"
    }
    
    analysis_info <- object@time$analysis_period
    
    cat("\nLoaded object summary:\n")
    cat("- Monitor:", monitor_name, "\n")
    cat("- Total flies:", mortality_info$total_flies, "\n")
    cat("- Alive flies:", mortality_info$total_flies - mortality_info$dead_flies, "\n")
    cat("- Dead flies:", mortality_info$dead_flies, "\n")
    cat("- Data points:", nrow(object@assays$mt), "\n")
    cat("- Analysis period:", analysis_info$days_analyzed, "days\n")
    cat("- Date range:", paste(analysis_info$date_range, collapse = " to "), "\n")
    cat("- Processed on:", as.character(object@time$saved_timestamp), "\n")
  }
  
  return(object)
}

#' Generate mortality report for final analysis
#'
#' @param object MonitorS4 object with mortality analysis
#' @return Data frame with mortality summary
#' @export
generate_mortality_report <- function(object) {
  
  if (!is(object, "MonitorS4")) {
    stop("Object must be of class MonitorS4")
  }
  
  if (!"mortality_status" %in% colnames(object@meta.data)) {
    stop("No mortality information found. Run detect_dead_flies_final_day() first.")
  }
  
  metadata <- object@meta.data
  mortality_analysis <- object@time$mortality_analysis
  
  # Create summary by group if available
  if (is.null(group_by) || !group_by %in% colnames(metadata)) {
    # Overall summary
    summary_data <- data.frame(
      Group = "All",
      Total_Flies = nrow(metadata),
      Alive_Flies = sum(metadata$mortality_status == "alive"),
      Dead_Flies = sum(metadata$mortality_status == "dead"),
      Mortality_Rate_Percent = round(sum(metadata$mortality_status == "dead") / nrow(metadata) * 100, 1),
      stringsAsFactors = FALSE
    )
  } else {
    # Group-wise summary
    groups <- unique(metadata[[group_by]])
    groups <- groups[!is.na(groups)]
    
    summary_list <- list()
    for (grp in groups) {
      group_data <- metadata[metadata[[group_by]] == grp & !is.na(metadata[[group_by]]), ]
      
      summary_list[[grp]] <- data.frame(
        Group = grp,
        Total_Flies = nrow(group_data),
        Alive_Flies = sum(group_data$mortality_status == "alive"),
        Dead_Flies = sum(group_data$mortality_status == "dead"),
        Mortality_Rate_Percent = round(sum(group_data$mortality_status == "dead") / nrow(group_data) * 100, 1),
        stringsAsFactors = FALSE
      )
    }
    
    summary_data <- do.call(rbind, summary_list)
    rownames(summary_data) <- NULL
  }
  
  # Add analysis metadata
  if (!is.null(mortality_analysis)) {
    attr(summary_data, "analysis_info") <- list(
      analysis_date = mortality_analysis$analysis_date,
      threshold_hours = mortality_analysis$threshold_hours,
      final_day = mortality_analysis$final_day
    )
  }
  
  return(summary_data)
}

# Note: process_monitor_complete() function deprecated in favor of modular workflow.
# 
# New recommended approach:
# data <- loadRMB(data_file, metadata_file)
# data <- CleanDays(data, days = c(2,3,4))
# data <- DetectDeadFlies(data, threshold_hours = 24)
# data <- FilterAliveFlies(data)
# activity_stats <- AnalyzeActivity(data, group_by = "Group")
# PlotActivityTimecourse(data, group_by = "Group")

#' Match data files with metadata files
#'
#' @param data_path Directory containing Monitor*.txt files
#' @param metadata_path Directory containing metadata CSV files
#' @return A data.frame mapping monitor files to metadata files
#' @export
match_files <- function(data_path, metadata_path) {
  # Find all Monitor*.txt files
  data_files <- list.files(data_path, pattern = "^Monitor.*\\.txt$", full.names = TRUE)
  
  if (length(data_files) == 0) {
    stop("No Monitor*.txt files found in: ", data_path)
  }
  
  # Extract monitor names
  monitor_names <- tools::file_path_sans_ext(basename(data_files))
  
  # Find corresponding metadata files
  if (dir.exists(metadata_path)) {
    metadata_files <- list.files(metadata_path, pattern = ".*metadata.*\\.csv$", full.names = TRUE)
    
    matched_metadata <- character(length(monitor_names))
    for (i in seq_along(monitor_names)) {
      monitor_name <- monitor_names[i]
      metadata_match <- grep(monitor_name, metadata_files, value = TRUE)
      if (length(metadata_match) > 0) {
        matched_metadata[i] <- metadata_match[1]
      } else {
        matched_metadata[i] <- NA_character_
      }
    }
  } else {
    matched_metadata <- rep(NA_character_, length(monitor_names))
  }
  
  # Create mapping data frame
  file_mapping <- data.frame(
    monitor_file = data_files,
    monitor_name = monitor_names,
    metadata_file = matched_metadata,
    stringsAsFactors = FALSE
  )
  
  return(file_mapping)
}

# Note: remove_dead_flies() function deprecated in favor of modular workflow:
# Use DetectDeadFlies() followed by FilterAliveFlies() instead