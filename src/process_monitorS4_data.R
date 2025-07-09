# MonitorS4 Data Processing Functions
# This file contains functions for processing monitor data

# Function to create a MonitorS4 object from a file path
create_monitorS4 <- function(file_path) {
  cat("Creating MonitorS4 object for:", basename(file_path), "\n")
  
  # Read the monitor data file
  if (file.exists(file_path)) {
    # Read the raw data - assuming it's tab-delimited
    raw_data <- read.table(file_path, header = FALSE, sep = "\t", fill = TRUE, stringsAsFactors = FALSE)
    
    # Get monitor name from filename
    monitor_name <- tools::file_path_sans_ext(basename(file_path))
    
    # Create the monitor object structure
    monitor_obj <- list(
      file_path = file_path,
      monitor_name = monitor_name,
      raw_data = raw_data,
      assays = list(),
      meta.data = NULL
    )
    
    class(monitor_obj) <- "monitorS4"
    return(monitor_obj)
  } else {
    stop("File not found:", file_path)
  }
}

# Function to process MonitorS4 data
process_monitorS4_data <- function(monitor_obj) {
  cat("Processing data for:", monitor_obj$monitor_name, "\n")
  
  if (is.null(monitor_obj$raw_data)) {
    warning("No raw data found for", monitor_obj$monitor_name)
    return(monitor_obj)
  }
  
  # Extract mt (movement) and pn (position) data from raw data
  # Assuming the structure based on typical DAM monitor files
  raw_data <- monitor_obj$raw_data
  
  # Skip header rows and extract data columns
  # Typically columns 3-34 are channel data (32 channels)
  if (ncol(raw_data) >= 34) {
    # Extract movement data (odd columns) and position data (even columns)
    mt_columns <- seq(3, 34, by = 2)  # Movement columns (3, 5, 7, ...)
    pn_columns <- seq(4, 34, by = 2)  # Position columns (4, 6, 8, ...)
    
    # Create mt (movement) data
    mt_data <- raw_data[, mt_columns, drop = FALSE]
    colnames(mt_data) <- paste0("Channel_", 1:ncol(mt_data))
    
    # Create pn (position) data  
    pn_data <- raw_data[, pn_columns, drop = FALSE]
    colnames(pn_data) <- paste0("Channel_", 1:ncol(pn_data))
    
    # Store in assays
    monitor_obj$assays$mt <- mt_data
    monitor_obj$assays$pn <- pn_data
    
    cat("  Extracted", nrow(mt_data), "rows and", ncol(mt_data), "channels\n")
  } else {
    warning("Unexpected data format for", monitor_obj$monitor_name)
  }
  
  return(monitor_obj)
}

# Function to select time range (placeholder)
select_time_range <- function(monitor_list, start_time = NULL, end_time = NULL) {
  cat("Selecting time range for", length(monitor_list), "monitors\n")
  
  # For now, just return the full data
  # In a real implementation, you would filter by time
  for (i in 1:length(monitor_list)) {
    if (!is.null(monitor_list[[i]]$assays$mt)) {
      cat("  Monitor", i, "has", nrow(monitor_list[[i]]$assays$mt), "time points\n")
    }
  }
  
  return(monitor_list)
}

# Function to merge multiple MonitorS4 objects
mergeMonitorS4 <- function(monitor_list) {
  cat("Merging", length(monitor_list), "monitors\n")
  
  # Initialize merged object
  merged_monitor <- list(
    assays = list(),
    meta.data = NULL
  )
  class(merged_monitor) <- "mergedMonitorS4"
  
  # Combine mt data from all monitors
  mt_list <- list()
  pn_list <- list()
  metadata_list <- list()
  
  for (i in 1:length(monitor_list)) {
    monitor <- monitor_list[[i]]
    
    if (!is.null(monitor$assays$mt)) {
      mt_list[[i]] <- monitor$assays$mt
    }
    if (!is.null(monitor$assays$pn)) {
      pn_list[[i]] <- monitor$assays$pn
    }
    if (!is.null(monitor$meta.data)) {
      metadata_list[[i]] <- monitor$meta.data
    }
  }
  
  # Combine data horizontally (by columns)
  if (length(mt_list) > 0) {
    merged_monitor$assays$mt <- do.call(cbind, mt_list)
    cat("  Merged mt data:", dim(merged_monitor$assays$mt), "\n")
  }
  
  if (length(pn_list) > 0) {
    merged_monitor$assays$pn <- do.call(cbind, pn_list)
    cat("  Merged pn data:", dim(merged_monitor$assays$pn), "\n")
  }
  
  if (length(metadata_list) > 0) {
    merged_monitor$meta.data <- do.call(rbind, metadata_list)
    cat("  Merged metadata:", dim(merged_monitor$meta.data), "\n")
  }
  
  return(merged_monitor)
}

# Function to remove dead flies
remove_dead_flies <- function(monitor) {
  cat("Removing dead flies\n")
  
  # Simple implementation: remove channels with no movement
  if (!is.null(monitor$assays$mt)) {
    mt_data <- monitor$assays$mt
    
    # Calculate total movement per channel
    channel_totals <- colSums(mt_data, na.rm = TRUE)
    alive_channels <- channel_totals > 0
    
    cat("  Channels with movement:", sum(alive_channels), "out of", length(alive_channels), "\n")
    
    # Keep only alive channels
    monitor$assays$mt <- mt_data[, alive_channels, drop = FALSE]
    
    if (!is.null(monitor$assays$pn)) {
      monitor$assays$pn <- monitor$assays$pn[, alive_channels, drop = FALSE]
    }
    
    if (!is.null(monitor$meta.data)) {
      monitor$meta.data <- monitor$meta.data[alive_channels, , drop = FALSE]
    }
  }
  
  return(monitor)
}
