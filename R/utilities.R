#' Utility Functions for RMB Package
#'
#' @description Various utility functions for data processing and manipulation

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
  pn_data <- raw_data[raw_data$Measure_Type == "Pn", ]  # Position data
  
  assays <- list(
    mt = mt_data,
    ct = ct_data,
    pn = pn_data,
    raw_data = raw_data
  )
  
  return(assays)
}

#' Generate metadata template based on monitor data
#'
#' @param raw_data Parsed monitor data
#' @param monitor_name Name of the monitor
#' @return A data.frame with metadata template
#' @export
generate_metadata_template <- function(raw_data, monitor_name) {
  # Get number of channels with data
  channel_cols <- grep("^Channel_", colnames(raw_data), value = TRUE)
  n_channels <- length(channel_cols)
  
  # Create metadata template
  template <- data.frame(
    Fly = paste0(monitor_name, "_", 1:n_channels),
    Lab = NA_character_,
    User = NA_character_,
    Date = NA_character_,
    Experiment_name = NA_character_,
    Monitor_type = "MB",  # Default to multi-beam
    Monitor_number = monitor_name,
    Channel = 1:n_channels,
    Group = NA_character_,
    Tube_type = "MB",
    Incubator = NA_character_,
    Temperature = NA_real_,
    Treatment = NA_character_,
    Sex = NA_character_,
    Other = NA_character_,
    Alive = NA
  )
  
  return(template)
}

#' Match monitor files with metadata files
#'
#' @param data_path Path to directory containing Monitor files
#' @param metadata_path Path to directory containing metadata files
#' @return A data.frame matching monitor files to metadata files
#' @export
match_files <- function(data_path, metadata_path) {
  # Find monitor files
  monitor_files <- list.files(data_path, pattern = "^Monitor.*\\.txt$", full.names = TRUE)
  if (length(monitor_files) == 0) {
    stop("No Monitor*.txt files found in: ", data_path)
  }
  
  # Find metadata files
  metadata_files <- list.files(metadata_path, pattern = ".*metadata.*\\.csv$", full.names = TRUE)
  
  # Extract monitor names
  monitor_names <- tools::file_path_sans_ext(basename(monitor_files))
  
  # Try to match metadata files
  matched_metadata <- character(length(monitor_files))
  
  for (i in seq_along(monitor_files)) {
    monitor_name <- monitor_names[i]
    
    # Look for exact match first
    exact_match <- grep(paste0(monitor_name, "_metadata"), metadata_files, value = TRUE)
    if (length(exact_match) > 0) {
      matched_metadata[i] <- exact_match[1]
    } else {
      # Look for partial match
      partial_match <- grep(monitor_name, metadata_files, value = TRUE)
      if (length(partial_match) > 0) {
        matched_metadata[i] <- partial_match[1]
      } else {
        matched_metadata[i] <- NA_character_
      }
    }
  }
  
  file_mapping <- data.frame(
    monitor_file = monitor_files,
    monitor_name = monitor_names,
    metadata_file = matched_metadata,
    stringsAsFactors = FALSE
  )
  
  return(file_mapping)
}

#' Remove dead flies from MonitorS4 object
#'
#' @param object MonitorS4 object
#' @param activity_threshold Minimum activity threshold to consider alive
#' @param consecutive_zeros Maximum consecutive zeros allowed
#' @return MonitorS4 object with dead flies removed
#' @export
remove_dead_flies <- function(object, activity_threshold = 1, consecutive_zeros = 12) {
  if (!is(object, "MonitorS4")) {
    stop("Object must be of class MonitorS4")
  }
  
  # Get motor activity data
  if (!"mt" %in% names(object@assays)) {
    warning("No motor activity (mt) data found, skipping dead fly removal")
    return(object)
  }
  
  mt_data <- object@assays$mt
  channel_cols <- grep("^Channel_", colnames(mt_data), value = TRUE)
  
  alive_channels <- logical(length(channel_cols))
  names(alive_channels) <- channel_cols
  
  for (i in seq_along(channel_cols)) {
    channel_col <- channel_cols[i]
    channel_data <- mt_data[[channel_col]]
    
    # Calculate total activity
    total_activity <- sum(channel_data, na.rm = TRUE)
    
    # Check for consecutive zeros
    rle_result <- rle(channel_data == 0)
    max_consecutive_zeros <- max(rle_result$lengths[rle_result$values], na.rm = TRUE)
    
    # Mark as alive if meets criteria
    alive_channels[i] <- (total_activity >= activity_threshold && 
                         max_consecutive_zeros <= consecutive_zeros)
  }
  
  alive_indices <- which(alive_channels)
  
  if (length(alive_indices) == 0) {
    warning("All flies marked as dead, returning original object")
    return(object)
  }
  
  # Filter metadata
  object@meta.data <- object@meta.data[alive_indices, , drop = FALSE]
  
  # Filter assay data
  for (assay_name in names(object@assays)) {
    assay_data <- object@assays[[assay_name]]
    alive_channel_cols <- channel_cols[alive_indices]
    
    # Keep metadata columns and alive channels
    meta_cols <- setdiff(colnames(assay_data), channel_cols)
    object@assays[[assay_name]] <- assay_data[, c(meta_cols, alive_channel_cols), drop = FALSE]
  }
  
  cat("Removed", length(channel_cols) - length(alive_indices), "dead flies\n")
  cat("Remaining", length(alive_indices), "live flies\n")
  
  return(object)
}

#' Convert wide format data to long format for analysis
#'
#' @param data Data frame in wide format
#' @param value_name Name for the value column
#' @return Data frame in long format
#' @export
wide_to_long <- function(data, value_name = "value") {
  channel_cols <- grep("^Channel_", colnames(data), value = TRUE)
  meta_cols <- setdiff(colnames(data), channel_cols)
  
  # Reshape to long format
  long_data <- tidyr::pivot_longer(
    data, 
    cols = all_of(channel_cols),
    names_to = "Channel",
    values_to = value_name,
    names_prefix = "Channel_"
  )
  
  # Convert Channel to numeric
  long_data$Channel <- as.numeric(long_data$Channel)
  
  return(long_data)
}

#' Clean monitor data to include only specific full days
#'
#' @description Extracts specific full days from monitor data, typically excluding 
#' the first full day as flies acclimate to tubes. Automatically identifies full vs 
#' partial days, then selects specified full days. Each full day should have exactly 
#' 1,440 data points (24 hours × 60 minutes).
#'
#' @param object MonitorS4 object with raw data
#' @param full_days_to_include Vector of full day numbers to include (default: c(2,3,4) = 2nd, 3rd, 4th full days)
#' @param points_per_day Expected data points per day (default: 1440 for 1-minute intervals)
#' @return MonitorS4 object with cleaned data
#' @export
clean_monitor_days <- function(object, full_days_to_include = c(2,3,4), points_per_day = 1440) {
  
  if (!is(object, "MonitorS4")) {
    stop("Object must be of class MonitorS4")
  }
  
  # Get MT data (motor activity)
  mt_data <- object@assays$mt
  if (is.null(mt_data)) {
    stop("MT assay not found in object")
  }
  
  # Identify day boundaries based on DateTime
  mt_data$Day <- as.Date(mt_data$DateTime)
  unique_days <- sort(unique(mt_data$Day))
  
  # Identify full vs partial days
  cat("Found", length(unique_days), "unique days in data:\n")
  full_days <- c()
  full_day_dates <- c()
  
  for (i in seq_along(unique_days)) {
    day_data <- mt_data[mt_data$Day == unique_days[i], ]
    points <- nrow(day_data)
    is_full <- points >= points_per_day * 0.95  # Allow 5% tolerance
    
    cat("Day", i, ":", as.character(unique_days[i]), "- Points:", points)
    if (is_full) {
      full_days <- c(full_days, length(full_days) + 1)
      full_day_dates <- c(full_day_dates, as.character(unique_days[i]))
      cat(" (Full Day", length(full_days), ")\n")
    } else {
      cat(" (Partial)\n")
    }
  }
  
  cat("\nFull days identified:", length(full_days), "\n")
  
  # Extract specified full days
  if (max(full_days_to_include) > length(full_day_dates)) {
    stop("Requested full day ", max(full_days_to_include), " but only ", length(full_day_dates), " full days available")
  }
  
  selected_day_strings <- full_day_dates[full_days_to_include]
  selected_days <- as.Date(selected_day_strings)
  cat("Selecting full days:", paste(full_days_to_include, collapse = ", "), "\n")
  cat("Date range:", as.character(min(selected_days)), "to", as.character(max(selected_days)), "\n")
  
  # Filter data for selected days
  cleaned_mt <- mt_data[mt_data$Day %in% selected_days, ]
  
  # Verify we have expected number of points
  expected_points <- length(full_days_to_include) * points_per_day
  actual_points <- nrow(cleaned_mt)
  
  cat("Expected total points:", expected_points, "\n")
  cat("Actual total points:", actual_points, "\n")
  
  if (actual_points != expected_points) {
    warning("Point count mismatch. Expected: ", expected_points, ", Got: ", actual_points)
  }
  
  # Update the object with cleaned data
  object@assays$mt <- cleaned_mt
  
  # Also clean other assays if they exist
  for (assay_name in names(object@assays)) {
    if (assay_name != "mt" && !is.null(object@assays[[assay_name]])) {
      assay_data <- object@assays[[assay_name]]
      if ("DateTime" %in% colnames(assay_data)) {
        assay_data$Day <- as.Date(assay_data$DateTime)
        object@assays[[assay_name]] <- assay_data[assay_data$Day %in% selected_days, ]
      }
    }
  }
  
  # Update time information
  object@time$analysis_period <- list(
    start_date = min(selected_days),
    end_date = max(selected_days),
    full_days_included = full_days_to_include,
    total_days = length(full_days_to_include),
    points_per_day = points_per_day,
    total_points = nrow(cleaned_mt),
    selected_dates = selected_days
  )
  
  cat("\nData successfully cleaned to", length(full_days_to_include), "full days\n")
  
  return(object)
}

#' Detect dead flies using last day of cleaned data
#'
#' @description Identifies dead flies based on activity in the final day of the 
#' cleaned dataset. Updates metadata with mortality information.
#'
#' @param object MonitorS4 object with cleaned data (should have analysis_period info)
#' @param no_movement_threshold Hours of no movement to consider dead (default: 24)
#' @return MonitorS4 object with updated metadata including mortality status
#' @export
detect_dead_flies_final_day <- function(object, no_movement_threshold = 24) {
  
  if (!is(object, "MonitorS4")) {
    stop("Object must be of class MonitorS4")
  }
  
  # Get MT data
  mt_data <- object@assays$mt
  if (is.null(mt_data)) {
    stop("MT assay not found in object")
  }
  
  # Check if we have analysis period info
  if (is.null(object@time$analysis_period)) {
    stop("No analysis period found. Please run clean_monitor_days() first.")
  }
  
  # Get the last day of the analysis period
  last_date <- max(object@time$analysis_period$selected_dates)
  cat("Detecting dead flies using final day:", as.character(last_date), "\n")
  
  # Extract data from the final day
  final_day_data <- mt_data[as.Date(mt_data$DateTime) == last_date, ]
  cat("Final day data points:", nrow(final_day_data), "\n")
  
  # Get channel columns
  channel_cols <- grep("^Channel_", colnames(final_day_data), value = TRUE)
  
  # Calculate timepoints to check (hours to minutes)
  timepoints_to_check <- no_movement_threshold * 60
  total_timepoints <- nrow(final_day_data)
  
  if (timepoints_to_check > total_timepoints) {
    warning("Requested ", no_movement_threshold, " hours but only ", 
            round(total_timepoints/60, 1), " hours available in final day")
    timepoints_to_check <- total_timepoints
  }
  
  # Check activity in the specified period (last N hours of final day)
  start_idx <- max(1, total_timepoints - timepoints_to_check + 1)
  period_data <- final_day_data[start_idx:total_timepoints, ]
  
  cat("Checking last", round(timepoints_to_check/60, 1), "hours of final day\n")
  cat("Time range:", as.character(min(period_data$DateTime)), "to", 
      as.character(max(period_data$DateTime)), "\n")
  
  # Calculate activity for each channel in the checking period
  fly_activity <- sapply(channel_cols, function(col) {
    sum(period_data[[col]], na.rm = TRUE)
  })
  
  # Identify dead flies (no movement in checking period)
  dead_flies <- names(fly_activity)[fly_activity == 0]
  alive_flies <- names(fly_activity)[fly_activity > 0]
  
  # Also check total experiment activity to distinguish truly dead vs just inactive
  total_activity <- sapply(channel_cols, function(col) {
    sum(mt_data[[col]], na.rm = TRUE)
  })
  truly_dead_flies <- names(total_activity)[total_activity == 0]
  inactive_flies <- setdiff(dead_flies, truly_dead_flies)
  
  cat("\nDead fly analysis results:\n")
  cat("Total flies:", length(channel_cols), "\n")
  cat("Dead in final", no_movement_threshold, "hours:", length(dead_flies), "\n")
  cat("  - Completely inactive entire experiment:", length(truly_dead_flies), "\n")
  cat("  - Active earlier but inactive final", no_movement_threshold, "hours:", length(inactive_flies), "\n")
  cat("Active in final", no_movement_threshold, "hours:", length(alive_flies), "\n")
  
  # Update metadata with mortality information
  metadata <- object@meta.data
  
  # Add mortality columns if they don't exist
  if (!"mortality_status" %in% colnames(metadata)) {
    metadata$mortality_status <- "alive"
  }
  if (!"final_day_activity" %in% colnames(metadata)) {
    metadata$final_day_activity <- NA
  }
  if (!"total_experiment_activity" %in% colnames(metadata)) {
    metadata$total_experiment_activity <- NA
  }
  if (!"mortality_type" %in% colnames(metadata)) {
    metadata$mortality_type <- "alive"
  }
  
  # Update mortality information for each channel
  for (i in 1:nrow(metadata)) {
    channel_name <- paste0("Channel_", metadata$Channel[i])
    
    if (channel_name %in% channel_cols) {
      # Update activity values
      metadata$final_day_activity[i] <- fly_activity[channel_name]
      metadata$total_experiment_activity[i] <- total_activity[channel_name]
      
      # Update mortality status
      if (channel_name %in% truly_dead_flies) {
        metadata$mortality_status[i] <- "dead"
        metadata$mortality_type[i] <- "never_active"
      } else if (channel_name %in% inactive_flies) {
        metadata$mortality_status[i] <- "dead"
        metadata$mortality_type[i] <- paste0("inactive_final_", no_movement_threshold, "h")
      } else {
        metadata$mortality_status[i] <- "alive"
        metadata$mortality_type[i] <- "active"
      }
    }
  }
  
  # Update the object
  object@meta.data <- metadata
  
  # Add mortality summary to time info
  object@time$mortality_analysis <- list(
    detection_date = last_date,
    no_movement_threshold_hours = no_movement_threshold,
    total_flies = length(channel_cols),
    dead_flies = length(dead_flies),
    truly_dead_flies = length(truly_dead_flies),
    inactive_final_period = length(inactive_flies),
    alive_flies = length(alive_flies),
    mortality_rate = round(length(dead_flies) / length(channel_cols) * 100, 1)
  )
  
  cat("\nMetadata updated with mortality information\n")
  cat("Mortality rate:", round(length(dead_flies) / length(channel_cols) * 100, 1), "%\n")
  
  return(object)
}

#' Save cleaned MonitorS4 object
#'
#' @description Saves a processed MonitorS4 object to RDS format for future use.
#' Includes all cleaned data, metadata with mortality information, and analysis parameters.
#'
#' @param object MonitorS4 object to save
#' @param output_path Path where to save the RDS file (default: uses monitor name)
#' @param overwrite Logical, whether to overwrite existing file (default: FALSE)
#' @return Path to saved file
#' @export
save_cleaned_monitor <- function(object, output_path = NULL, overwrite = FALSE) {
  
  if (!is(object, "MonitorS4")) {
    stop("Object must be of class MonitorS4")
  }
  
  # Generate default filename if not provided
  if (is.null(output_path)) {
    monitor_name <- unique(object@meta.data$Monitor_number)[1]
    if (is.na(monitor_name)) {
      monitor_name <- "Monitor_Unknown"
    }
    output_path <- paste0("cleaned_", monitor_name, ".rds")
  }
  
  # Check if file exists
  if (file.exists(output_path) && !overwrite) {
    stop("File already exists: ", output_path, ". Use overwrite = TRUE to replace it.")
  }
  
  # Add processing timestamp
  object@time$processing_info <- list(
    processed_date = Sys.time(),
    r_version = R.version.string,
    package_version = "RMB_dev",
    processing_steps = c("data_loading", "day_cleaning", "dead_fly_detection")
  )
  
  # Save the object
  saveRDS(object, file = output_path)
  
  cat("Cleaned MonitorS4 object saved to:", output_path, "\n")
  cat("File size:", round(file.info(output_path)$size / 1024^2, 2), "MB\n")
  
  # Print summary of what was saved
  cat("\nSaved object summary:\n")
  cat("- Monitor:", unique(object@meta.data$Monitor_number)[1], "\n")
  cat("- Total flies:", nrow(object@meta.data), "\n")
  cat("- Alive flies:", sum(object@meta.data$mortality_status == "alive"), "\n")
  cat("- Dead flies:", sum(object@meta.data$mortality_status == "dead"), "\n")
  cat("- Data points:", nrow(object@assays$mt), "\n")
  cat("- Analysis period:", object@time$analysis_period$total_days, "days\n")
  cat("- Date range:", as.character(object@time$analysis_period$start_date), 
      "to", as.character(object@time$analysis_period$end_date), "\n")
  
  return(output_path)
}

#' Load cleaned MonitorS4 object
#'
#' @description Loads a previously saved MonitorS4 object from RDS format.
#'
#' @param file_path Path to the RDS file
#' @param verbose Logical, whether to print loading information (default: TRUE)
#' @return MonitorS4 object
#' @export
load_cleaned_monitor <- function(file_path, verbose = TRUE) {
  
  if (!file.exists(file_path)) {
    stop("File does not exist: ", file_path)
  }
  
  # Load the object
  object <- readRDS(file_path)
  
  if (!is(object, "MonitorS4")) {
    stop("File does not contain a MonitorS4 object")
  }
  
  if (verbose) {
    cat("Loaded MonitorS4 object from:", file_path, "\n")
    cat("File size:", round(file.info(file_path)$size / 1024^2, 2), "MB\n")
    
    # Print summary
    cat("\nLoaded object summary:\n")
    cat("- Monitor:", unique(object@meta.data$Monitor_number)[1], "\n")
    cat("- Total flies:", nrow(object@meta.data), "\n")
    cat("- Alive flies:", sum(object@meta.data$mortality_status == "alive"), "\n")
    cat("- Dead flies:", sum(object@meta.data$mortality_status == "dead"), "\n")
    cat("- Data points:", nrow(object@assays$mt), "\n")
    cat("- Analysis period:", object@time$analysis_period$total_days, "days\n")
    cat("- Date range:", as.character(object@time$analysis_period$start_date), 
        "to", as.character(object@time$analysis_period$end_date), "\n")
    
    if (!is.null(object@time$processing_info)) {
      cat("- Processed on:", as.character(object@time$processing_info$processed_date), "\n")
    }
  }
  
  return(object)
}

#' Complete processing pipeline for monitor data
#'
#' @description Runs the complete data processing pipeline: load, clean, detect dead flies, and save.
#' This is a convenience function that combines all processing steps.
#'
#' @param data_file Path to Monitor*.txt file
#' @param metadata_file Path to corresponding metadata CSV file
#' @param output_dir Directory to save cleaned RDS file (default: current directory)
#' @param full_days_to_include Vector of full day numbers to include (default: c(2,3,4))
#' @param no_movement_threshold Hours of no movement to consider dead (default: 24)
#' @param save_rds Logical, whether to save cleaned object as RDS (default: TRUE)
#' @return MonitorS4 object with all processing completed
#' @export
process_monitor_complete <- function(data_file, 
                                   metadata_file,
                                   output_dir = ".",
                                   full_days_to_include = c(2,3,4),
                                   no_movement_threshold = 24,
                                   save_rds = TRUE) {
  
  cat("=== RMB Complete Processing Pipeline ===\n")
  cat("Data file:", data_file, "\n")
  cat("Metadata file:", metadata_file, "\n")
  cat("Processing settings:\n")
  cat("- Full days to include:", paste(full_days_to_include, collapse = ", "), "\n")
  cat("- Dead fly threshold:", no_movement_threshold, "hours\n\n")
  
  # Step 1: Load raw data
  cat("Step 1: Loading raw data...\n")
  result <- loadRMB(data = data_file, metadata = metadata_file)
  
  # Step 2: Clean to specified days
  cat("\nStep 2: Cleaning data to specified days...\n")
  cleaned_result <- clean_monitor_days(result, full_days_to_include = full_days_to_include)
  
  # Step 3: Detect dead flies
  cat("\nStep 3: Detecting dead flies using final day...\n")
  final_result <- detect_dead_flies_final_day(cleaned_result, no_movement_threshold = no_movement_threshold)
  
  # Step 4: Save if requested
  if (save_rds) {
    cat("\nStep 4: Saving cleaned object...\n")
    monitor_name <- tools::file_path_sans_ext(basename(data_file))
    output_path <- file.path(output_dir, paste0("cleaned_", monitor_name, ".rds"))
    save_cleaned_monitor(final_result, output_path = output_path, overwrite = TRUE)
  }
  
  cat("\n=== Processing Complete ===\n")
  
  return(final_result)
}

#' Generate mortality report table for final reports
#'
#' @description Creates a formatted mortality report table suitable for inclusion 
#' in final analysis reports. Summarizes alive/dead flies with detailed statistics.
#'
#' @param object MonitorS4 object with mortality analysis completed
#' @param group_by Optional metadata column to group results by (e.g., "Sex", "Treatment")
#' @param export_csv Logical, whether to export as CSV file (default: FALSE)
#' @param output_file Path for CSV export if export_csv = TRUE
#' @return Data frame with mortality report table
#' @export
generate_mortality_report <- function(object, group_by = NULL, export_csv = FALSE, output_file = NULL) {
  
  if (!is(object, "MonitorS4")) {
    stop("Object must be of class MonitorS4")
  }
  
  if (!"mortality_status" %in% colnames(object@meta.data)) {
    stop("No mortality analysis found. Please run detect_dead_flies_final_day() first.")
  }
  
  metadata <- object@meta.data
  
  # Basic mortality summary
  if (is.null(group_by) || !group_by %in% colnames(metadata)) {
    # Overall summary
    report <- data.frame(
      Group = "All Flies",
      Total_Flies = nrow(metadata),
      Alive_Flies = sum(metadata$mortality_status == "alive"),
      Dead_Flies = sum(metadata$mortality_status == "dead"),
      Mortality_Rate_Percent = round(sum(metadata$mortality_status == "dead") / nrow(metadata) * 100, 1),
      Never_Active = sum(metadata$mortality_type == "never_active", na.rm = TRUE),
      Inactive_Final_Period = sum(grepl("inactive_final", metadata$mortality_type), na.rm = TRUE),
      stringsAsFactors = FALSE
    )
  } else {
    # Grouped summary
    groups <- unique(metadata[[group_by]])
    groups <- groups[!is.na(groups)]
    
    report_list <- lapply(groups, function(grp) {
      group_data <- metadata[metadata[[group_by]] == grp & !is.na(metadata[[group_by]]), ]
      
      data.frame(
        Group = as.character(grp),
        Total_Flies = nrow(group_data),
        Alive_Flies = sum(group_data$mortality_status == "alive"),
        Dead_Flies = sum(group_data$mortality_status == "dead"),
        Mortality_Rate_Percent = round(sum(group_data$mortality_status == "dead") / nrow(group_data) * 100, 1),
        Never_Active = sum(group_data$mortality_type == "never_active", na.rm = TRUE),
        Inactive_Final_Period = sum(grepl("inactive_final", group_data$mortality_type), na.rm = TRUE),
        stringsAsFactors = FALSE
      )
    })
    
    report <- do.call(rbind, report_list)
    
    # Add overall summary row
    overall <- data.frame(
      Group = "OVERALL",
      Total_Flies = nrow(metadata),
      Alive_Flies = sum(metadata$mortality_status == "alive"),
      Dead_Flies = sum(metadata$mortality_status == "dead"),
      Mortality_Rate_Percent = round(sum(metadata$mortality_status == "dead") / nrow(metadata) * 100, 1),
      Never_Active = sum(metadata$mortality_type == "never_active", na.rm = TRUE),
      Inactive_Final_Period = sum(grepl("inactive_final", metadata$mortality_type), na.rm = TRUE),
      stringsAsFactors = FALSE
    )
    
    report <- rbind(report, overall)
  }
  
  # Add analysis details as attributes
  if (!is.null(object@time$mortality_analysis)) {
    attr(report, "analysis_info") <- list(
      detection_date = object@time$mortality_analysis$detection_date,
      threshold_hours = object@time$mortality_analysis$no_movement_threshold_hours,
      analysis_period = paste(object@time$analysis_period$total_days, "days"),
      date_range = paste(object@time$analysis_period$start_date, "to", object@time$analysis_period$end_date)
    )
  }
  
  # Add monitor information
  monitor_name <- unique(object@meta.data$Monitor_number)[1]
  attr(report, "monitor_info") <- list(
    monitor = monitor_name,
    experiment = unique(object@meta.data$Experiment_name)[1]
  )
  
  # Export to CSV if requested
  if (export_csv) {
    if (is.null(output_file)) {
      output_file <- paste0("mortality_report_", monitor_name, ".csv")
    }
    
    # Create header with analysis info
    header_lines <- c(
      paste("# Mortality Report -", monitor_name),
      paste("# Generated:", Sys.time()),
      paste("# Analysis Period:", attr(report, "analysis_info")$analysis_period),
      paste("# Date Range:", attr(report, "analysis_info")$date_range),
      paste("# Dead Fly Threshold:", attr(report, "analysis_info")$threshold_hours, "hours"),
      "#",
      "# Column Definitions:",
      "# Total_Flies: Total number of flies in group",
      "# Alive_Flies: Flies active in final analysis period", 
      "# Dead_Flies: Flies with no movement in final period",
      "# Mortality_Rate_Percent: Percentage of dead flies",
      "# Never_Active: Flies with zero activity throughout experiment",
      "# Inactive_Final_Period: Flies active earlier but inactive in final period",
      "#"
    )
    
    # Write header and data
    writeLines(header_lines, output_file)
    suppressWarnings(write.table(report, output_file, append = TRUE, sep = ",", 
                                row.names = FALSE, quote = FALSE))
    
    cat("Mortality report exported to:", output_file, "\n")
  }
  
  return(report)
}

#' Print formatted mortality report
#'
#' @description Prints a nicely formatted mortality report for console display or reports
#'
#' @param mortality_report Data frame from generate_mortality_report()
#' @return Invisible, prints formatted table
#' @export
print_mortality_report <- function(mortality_report) {
  
  # Get analysis info from attributes
  analysis_info <- attr(mortality_report, "analysis_info")
  monitor_info <- attr(mortality_report, "monitor_info")
  
  cat("\n")
  cat("===============================================\n")
  cat("           MORTALITY ANALYSIS REPORT          \n")
  cat("===============================================\n")
  
  if (!is.null(monitor_info)) {
    cat("Monitor:", monitor_info$monitor, "\n")
    if (!is.na(monitor_info$experiment)) {
      cat("Experiment:", monitor_info$experiment, "\n")
    }
  }
  
  if (!is.null(analysis_info)) {
    cat("Analysis Period:", analysis_info$analysis_period, "\n")
    cat("Date Range:", analysis_info$date_range, "\n")
    cat("Dead Fly Threshold:", analysis_info$threshold_hours, "hours no movement\n")
  }
  
  cat("Generated:", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n")
  cat("-----------------------------------------------\n\n")
  
  # Print the table
  print(mortality_report, row.names = FALSE)
  
  cat("\n")
  cat("LEGEND:\n")
  cat("- Never_Active: Flies with zero activity throughout entire experiment\n")
  cat("- Inactive_Final_Period: Flies that were active earlier but showed no\n")
  cat("  movement in the final analysis period\n")
  cat("===============================================\n\n")
  
  invisible(mortality_report)
}

#' Generate sample metadata summary table for reports
#'
#' @description Creates a comprehensive metadata summary table for the beginning 
#' of analysis reports. Shows experimental design, sample sizes, and key parameters.
#'
#' @param object MonitorS4 object with metadata
#' @param group_columns Vector of metadata columns to summarize (default: auto-detect)
#' @param export_csv Logical, whether to export as CSV file (default: FALSE)
#' @param output_file Path for CSV export if export_csv = TRUE
#' @return List containing experimental summary and sample size tables
#' @export
generate_metadata_summary <- function(object, group_columns = NULL, export_csv = FALSE, output_file = NULL) {
  
  if (!is(object, "MonitorS4")) {
    stop("Object must be of class MonitorS4")
  }
  
  metadata <- object@meta.data
  
  # Auto-detect informative columns if not specified
  if (is.null(group_columns)) {
    # Look for common experimental design columns
    potential_cols <- c("Sex", "Genotype", "Treatment", "Group", "Condition", 
                       "Strain", "Drug", "Dose", "Temperature", "Incubator")
    group_columns <- intersect(potential_cols, colnames(metadata))
    
    # Remove columns with too many unique values (likely not grouping variables)
    group_columns <- group_columns[sapply(group_columns, function(col) {
      length(unique(metadata[[col]])) <= nrow(metadata) * 0.5
    })]
  }
  
  # 1. Experimental Overview Table
  monitor_name <- unique(metadata$Monitor_number)[1]
  experiment_name <- unique(metadata$Experiment_name)[1]
  
  # Get analysis period info if available
  analysis_info <- ""
  if (!is.null(object@time$analysis_period)) {
    analysis_info <- paste(object@time$analysis_period$total_days, "days:",
                          object@time$analysis_period$start_date, "to", 
                          object@time$analysis_period$end_date)
  }
  
  # Get mortality info if available
  mortality_info <- ""
  if (!is.null(object@time$mortality_analysis)) {
    mortality_info <- paste(object@time$mortality_analysis$no_movement_threshold_hours, 
                           "hours no movement threshold")
  }
  
  experimental_overview <- data.frame(
    Parameter = c("Monitor", "Experiment", "Total Samples", "Analysis Period", 
                 "Mortality Detection", "Data Processing Date", "Temperature", "Lab/User"),
    Value = c(
      ifelse(is.na(monitor_name), "Unknown", monitor_name),
      ifelse(is.na(experiment_name), "Not specified", experiment_name),
      nrow(metadata),
      ifelse(analysis_info == "", "Not specified", analysis_info),
      ifelse(mortality_info == "", "Not performed", mortality_info),
      ifelse(!is.null(object@time$processing_info), 
             as.character(object@time$processing_info$processed_date), "Unknown"),
      ifelse("Temperature" %in% colnames(metadata), 
             paste(unique(metadata$Temperature), "°C", collapse = ", "), "Not specified"),
      ifelse("Lab" %in% colnames(metadata) && "User" %in% colnames(metadata),
             paste(unique(metadata$Lab), "/", unique(metadata$User), collapse = ", "), 
             "Not specified")
    ),
    stringsAsFactors = FALSE
  )
  
  # 2. Sample Size Summary by Groups
  sample_summary_list <- list()
  
  if (length(group_columns) > 0) {
    for (col in group_columns) {
      # Skip columns with all NA or single value
      col_values <- metadata[[col]]
      unique_values <- unique(col_values[!is.na(col_values)])
      
      if (length(unique_values) > 1) {
        summary_table <- table(col_values, useNA = "ifany")
        summary_df <- data.frame(
          Group = names(summary_table),
          Sample_Size = as.numeric(summary_table),
          Percentage = round(as.numeric(summary_table) / nrow(metadata) * 100, 1),
          stringsAsFactors = FALSE
        )
        summary_df$Group[is.na(summary_df$Group)] <- "Not specified"
        sample_summary_list[[col]] <- summary_df
      }
    }
  }
  
  # 3. Combined Sample Design Table (if multiple grouping variables)
  combined_summary <- NULL
  if (length(group_columns) >= 2) {
    # Create crosstab for first two grouping variables
    col1 <- group_columns[1]
    col2 <- group_columns[2]
    
    if (!is.null(sample_summary_list[[col1]]) && !is.null(sample_summary_list[[col2]])) {
      crosstab <- table(metadata[[col1]], metadata[[col2]], useNA = "ifany")
      combined_summary <- as.data.frame.matrix(crosstab)
      combined_summary <- cbind(Group = rownames(combined_summary), combined_summary)
      rownames(combined_summary) <- NULL
      
      # Add totals
      combined_summary$Total <- rowSums(combined_summary[, -1, drop = FALSE])
      
      attr(combined_summary, "variables") <- c(col1, col2)
    }
  }
  
  # 4. Data Quality Summary
  quality_summary <- data.frame(
    Metric = c("Channels with Data", "Empty Channels", "Data Completeness", 
              "Alive Flies", "Dead Flies"),
    Value = c(
      sum(!is.na(metadata$Channel)),
      sum(is.na(metadata$Channel)),
      paste(round(sum(!is.na(metadata$Channel)) / nrow(metadata) * 100, 1), "%"),
      ifelse("mortality_status" %in% colnames(metadata),
             sum(metadata$mortality_status == "alive", na.rm = TRUE), "Not analyzed"),
      ifelse("mortality_status" %in% colnames(metadata),
             sum(metadata$mortality_status == "dead", na.rm = TRUE), "Not analyzed")
    ),
    stringsAsFactors = FALSE
  )
  
  # Compile results
  results <- list(
    experimental_overview = experimental_overview,
    sample_summaries = sample_summary_list,
    combined_design = combined_summary,
    data_quality = quality_summary
  )
  
  # Add metadata for export
  attr(results, "monitor_info") <- list(
    monitor = monitor_name,
    experiment = experiment_name,
    total_samples = nrow(metadata)
  )
  
  # Export to CSV if requested
  if (export_csv) {
    if (is.null(output_file)) {
      output_file <- paste0("metadata_summary_", monitor_name, ".csv")
    }
    
    # Create comprehensive CSV with all tables
    header_lines <- c(
      paste("# Metadata Summary Report -", monitor_name),
      paste("# Generated:", Sys.time()),
      paste("# Total Samples:", nrow(metadata)),
      "#"
    )
    
    writeLines(header_lines, output_file)
    
    # Write experimental overview
    write.table("# EXPERIMENTAL OVERVIEW", output_file, append = TRUE, 
                col.names = FALSE, row.names = FALSE, quote = FALSE)
    suppressWarnings(write.table(experimental_overview, output_file, append = TRUE, 
                                sep = ",", row.names = FALSE, quote = FALSE))
    
    # Write sample summaries
    for (col_name in names(sample_summary_list)) {
      write.table(paste("#", toupper(col_name), "SUMMARY"), output_file, append = TRUE,
                  col.names = FALSE, row.names = FALSE, quote = FALSE)
      suppressWarnings(write.table(sample_summary_list[[col_name]], output_file, 
                                  append = TRUE, sep = ",", row.names = FALSE, quote = FALSE))
    }
    
    # Write data quality
    write.table("# DATA QUALITY", output_file, append = TRUE,
                col.names = FALSE, row.names = FALSE, quote = FALSE)
    suppressWarnings(write.table(quality_summary, output_file, append = TRUE,
                                sep = ",", row.names = FALSE, quote = FALSE))
    
    cat("Metadata summary exported to:", output_file, "\n")
  }
  
  return(results)
}

#' Print formatted metadata summary
#'
#' @description Prints a nicely formatted metadata summary for console display or reports
#'
#' @param metadata_summary List from generate_metadata_summary()
#' @return Invisible, prints formatted tables
#' @export
print_metadata_summary <- function(metadata_summary) {
  
  monitor_info <- attr(metadata_summary, "monitor_info")
  
  cat("\n")
  cat("===============================================\n")
  cat("         EXPERIMENTAL METADATA SUMMARY        \n")
  cat("===============================================\n\n")
  
  # Experimental Overview
  cat("EXPERIMENTAL OVERVIEW:\n")
  cat("-----------------------------------------------\n")
  print(metadata_summary$experimental_overview, row.names = FALSE)
  
  # Sample Size Summaries
  if (length(metadata_summary$sample_summaries) > 0) {
    cat("\n\nSAMPLE SIZE SUMMARIES:\n")
    cat("-----------------------------------------------\n")
    
    for (group_name in names(metadata_summary$sample_summaries)) {
      cat("\n", toupper(group_name), "Distribution:\n")
      print(metadata_summary$sample_summaries[[group_name]], row.names = FALSE)
    }
  }
  
  # Combined Design
  if (!is.null(metadata_summary$combined_design)) {
    vars <- attr(metadata_summary$combined_design, "variables")
    cat("\n\nCOMBINED EXPERIMENTAL DESIGN (", vars[1], " x ", vars[2], "):\n")
    cat("-----------------------------------------------\n")
    print(metadata_summary$combined_design, row.names = FALSE)
  }
  
  # Data Quality
  cat("\n\nDATA QUALITY SUMMARY:\n")
  cat("-----------------------------------------------\n")
  print(metadata_summary$data_quality, row.names = FALSE)
  
  cat("\n===============================================\n\n")
  
  invisible(metadata_summary)
}

#' Filter dataset to alive flies only for analysis
#'
#' @description Removes dead flies from all assays while preserving their information 
#' in metadata. Creates a clean dataset with only alive flies for analysis.
#'
#' @param object MonitorS4 object with mortality analysis completed
#' @param preserve_dead_metadata Logical, keep dead fly metadata for reference (default: TRUE)
#' @return MonitorS4 object with only alive flies in assays
#' @export
filter_to_alive_flies <- function(object, preserve_dead_metadata = TRUE) {
  
  if (!is(object, "MonitorS4")) {
    stop("Object must be of class MonitorS4")
  }
  
  if (!"mortality_status" %in% colnames(object@meta.data)) {
    stop("No mortality analysis found. Please run detect_dead_flies_final_day() first.")
  }
  
  # Identify alive and dead flies
  metadata <- object@meta.data
  alive_flies <- metadata[metadata$mortality_status == "alive", ]
  dead_flies <- metadata[metadata$mortality_status == "dead", ]
  
  cat("Filtering to alive flies only:\n")
  cat("- Total flies:", nrow(metadata), "\n")
  cat("- Alive flies:", nrow(alive_flies), "\n") 
  cat("- Dead flies to remove:", nrow(dead_flies), "\n")
  
  if (nrow(dead_flies) > 0) {
    cat("- Dead fly channels:", paste(dead_flies$Channel, collapse = ", "), "\n")
  }
  
  # Get channels to keep (alive flies only)
  alive_channels <- paste0("Channel_", alive_flies$Channel)
  dead_channels <- paste0("Channel_", dead_flies$Channel)
  
  # Filter each assay to remove dead fly channels
  for (assay_name in names(object@assays)) {
    assay_data <- object@assays[[assay_name]]
    
    if (!is.null(assay_data) && is.data.frame(assay_data)) {
      # Get channel columns in this assay
      channel_cols <- grep("^Channel_", colnames(assay_data), value = TRUE)
      channels_to_remove <- intersect(dead_channels, channel_cols)
      
      if (length(channels_to_remove) > 0) {
        # Remove dead fly columns
        cols_to_keep <- setdiff(colnames(assay_data), channels_to_remove)
        object@assays[[assay_name]] <- assay_data[, cols_to_keep, drop = FALSE]
        
        cat("- Removed", length(channels_to_remove), "dead fly channels from", assay_name, "assay\n")
      }
    }
  }
  
  # Update metadata
  if (preserve_dead_metadata) {
    # Keep all metadata but mark dataset as filtered
    object@meta.data$included_in_analysis <- ifelse(metadata$mortality_status == "alive", TRUE, FALSE)
    cat("- Preserved all metadata with 'included_in_analysis' flag\n")
  } else {
    # Remove dead fly metadata entirely
    object@meta.data <- alive_flies
    cat("- Removed dead fly metadata\n")
  }
  
  # Update analysis info
  if (!is.null(object@time$mortality_analysis)) {
    object@time$mortality_analysis$dataset_filtered_to_alive <- TRUE
    object@time$mortality_analysis$alive_flies_in_analysis <- nrow(alive_flies)
    object@time$mortality_analysis$excluded_dead_flies <- nrow(dead_flies)
  }
  
  # Verify filtering
  remaining_mt_channels <- grep("^Channel_", colnames(object@assays$mt), value = TRUE)
  expected_channels <- length(alive_channels)
  actual_channels <- length(remaining_mt_channels)
  
  cat("\nFiltering verification:\n")
  cat("- Expected alive channels:", expected_channels, "\n")
  cat("- Actual channels in MT assay:", actual_channels, "\n")
  cat("- Data points per channel:", nrow(object@assays$mt), "\n")
  cat("- Total data points for analysis:", nrow(object@assays$mt) * actual_channels, "\n")
  
  if (expected_channels == actual_channels) {
    cat("✓ Filtering successful - ready for analysis with alive flies only\n")
  } else {
    warning("Channel count mismatch after filtering")
  }
  
  return(object)
}

#' Process all monitors and combine into single dataset
#'
#' @description Processes multiple monitors through the complete pipeline and combines
#' them into a single analysis-ready dataset with all alive flies.
#'
#' @param data_dir Directory containing Monitor*.txt files
#' @param metadata_dir Directory containing metadata CSV files
#' @param full_days_to_include Vector of full day numbers to include (default: c(2,3,4))
#' @param no_movement_threshold Hours of no movement to consider dead (default: 24)
#' @param save_combined Logical, whether to save combined dataset (default: TRUE)
#' @param output_path Path for saving combined RDS file
#' @return Combined MonitorS4 object with all monitors
#' @export
process_all_monitors_combined <- function(data_dir = "./inst/extdata",
                                        metadata_dir = "./inst/extdata/metadata", 
                                        full_days_to_include = c(2,3,4),
                                        no_movement_threshold = 24,
                                        save_combined = TRUE,
                                        output_path = "combined_all_monitors_analysis_ready.rds") {
  
  # Find all monitor files
  monitor_files <- list.files(data_dir, pattern = "^Monitor.*\.txt$", full.names = TRUE)
  
  if (length(monitor_files) == 0) {
    stop("No Monitor*.txt files found in: ", data_dir)
  }
  
  cat("=== Processing All Monitors Combined ===
")
  cat("Found", length(monitor_files), "monitor files
")
  
  processed_monitors <- list()
  
  # Process each monitor individually
  for (file in monitor_files) {
    monitor_name <- tools::file_path_sans_ext(basename(file))
    cat("
--- Processing", monitor_name, "---
")
    
    # Find corresponding metadata file
    metadata_file <- file.path(metadata_dir, paste0(monitor_name, "_metadata.csv"))
    
    if (!file.exists(metadata_file)) {
      warning("Metadata file not found for ", monitor_name, ", skipping...")
      next
    }
    
    # Process this monitor through complete pipeline
    monitor_obj <- process_monitor_complete(
      data_file = file,
      metadata_file = metadata_file,
      output_dir = tempdir(),  # Use temp directory to avoid clutter
      full_days_to_include = full_days_to_include,
      no_movement_threshold = no_movement_threshold,
      save_rds = FALSE  # Don't save individual monitors
    )
    
    # Filter to alive flies only
    monitor_obj <- filter_to_alive_flies(monitor_obj)
    
    processed_monitors[[monitor_name]] <- monitor_obj
  }
  
  if (length(processed_monitors) == 0) {
    stop("No monitors were successfully processed")
  }
  
  cat("
=== Combining All Monitors ===
")
  cat("Successfully processed monitors:", length(processed_monitors), "
")
  
  # Combine all monitors using merge_monitors function
  combined_object <- merge_monitors(processed_monitors)
  
  # Summary statistics
  total_flies <- nrow(combined_object@meta.data)
  alive_flies <- sum(combined_object@meta.data$included_in_analysis)
  unique_groups <- unique(combined_object@meta.data$Group)
  
  cat("
=== Combined Dataset Summary ===
")
  cat("Total monitors combined:", length(processed_monitors), "
")
  cat("Total flies:", total_flies, "
")
  cat("Alive flies (for analysis):", alive_flies, "
")
  cat("Unique groups found:", length(unique_groups), "
")
  cat("Groups:", paste(unique_groups, collapse = ", "), "
")
  
  # Save combined dataset
  if (save_combined) {
    cat("
=== Saving Combined Dataset ===
")
    save_cleaned_monitor(combined_object, output_path = output_path, overwrite = TRUE)
  }
  
  cat("
=== All Monitors Processing Complete ===
")
  
  return(combined_object)
}
