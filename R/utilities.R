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
  
  # The first few columns are metadata, followed by channel data
  # Typical format: Index, Date, Time, Status1, Status2, Monitor, Light, Measure_Type, Channel_data...
  
  # Create column names
  n_channels <- ncol(raw_data) - 8  # Subtract metadata columns
  channel_cols <- paste0("Channel_", 1:n_channels)
  
  colnames(raw_data) <- c("Index", "Date", "Time", "Status1", "Status2", 
                         "Monitor", "Light", "Measure_Type", channel_cols)
  
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