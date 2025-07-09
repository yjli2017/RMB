# Dead Fly Removal Functions
# This file contains functions for removing dead flies from the analysis

# Function to remove dead flies based on movement data
remove_dead_flies <- function(monitor, min_movement_threshold = 10) {
  cat("Removing dead flies (movement threshold:", min_movement_threshold, ")\n")
  
  if (is.null(monitor$assays$mt)) {
    warning("No movement data found for dead fly removal")
    return(monitor)
  }
  
  mt_data <- monitor$assays$mt
  
  # Calculate total movement per channel
  channel_totals <- colSums(mt_data, na.rm = TRUE)
  alive_channels <- channel_totals >= min_movement_threshold
  
  dead_count <- sum(!alive_channels)
  alive_count <- sum(alive_channels)
  
  cat("  Total channels:", length(alive_channels), "\n")
  cat("  Alive channels:", alive_count, "\n") 
  cat("  Dead channels:", dead_count, "\n")
  
  if (alive_count == 0) {
    warning("No alive channels found - check your threshold")
    return(monitor)
  }
  
  # Filter data to keep only alive channels
  monitor$assays$mt <- mt_data[, alive_channels, drop = FALSE]
  
  if (!is.null(monitor$assays$pn)) {
    monitor$assays$pn <- monitor$assays$pn[, alive_channels, drop = FALSE]
  }
  
  # Filter other assays if they exist
  if (!is.null(monitor$assays$awake_mt)) {
    monitor$assays$awake_mt <- monitor$assays$awake_mt[, alive_channels, drop = FALSE]
  }
  
  if (!is.null(monitor$assays$sleep_mt)) {
    monitor$assays$sleep_mt <- monitor$assays$sleep_mt[, alive_channels, drop = FALSE]
  }
  
  if (!is.null(monitor$assays$pn_awake)) {
    monitor$assays$pn_awake <- monitor$assays$pn_awake[, alive_channels, drop = FALSE]
  }
  
  # Filter metadata
  if (!is.null(monitor$meta.data)) {
    original_metadata <- monitor$meta.data
    
    # Make sure we have the right number of metadata rows
    if (nrow(original_metadata) == length(alive_channels)) {
      monitor$meta.data <- original_metadata[alive_channels, , drop = FALSE]
      cat("  Filtered metadata to", nrow(monitor$meta.data), "rows\n")
    } else {
      warning("Metadata rows don't match channel count - keeping original metadata")
    }
  }
  
  return(monitor)
}
