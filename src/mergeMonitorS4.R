# MonitorS4 Merging Functions
# This file contains functions for merging multiple monitor objects

# Function to merge multiple MonitorS4 objects
mergeMonitorS4 <- function(monitor_list) {
  cat("Merging", length(monitor_list), "monitors into single object\n")
  
  # Initialize merged object
  merged_monitor <- list(
    assays = list(),
    meta.data = NULL
  )
  
  # Use S4-like structure with @ access
  class(merged_monitor) <- c("mergedMonitorS4", "list")
  
  # Collect data from all monitors
  mt_list <- list()
  pn_list <- list()
  metadata_list <- list()
  
  for (i in 1:length(monitor_list)) {
    monitor <- monitor_list[[i]]
    
    if (!is.null(monitor$assays$mt)) {
      # Rename columns to include monitor identifier
      mt_data <- monitor$assays$mt
      # Ensure data is numeric
      mt_data[] <- lapply(mt_data, function(x) as.numeric(as.character(x)))
      colnames(mt_data) <- paste0(monitor$monitor_name, "_", colnames(mt_data))
      mt_list[[i]] <- mt_data
    }
    
    if (!is.null(monitor$assays$pn)) {
      pn_data <- monitor$assays$pn
      # Ensure data is numeric
      pn_data[] <- lapply(pn_data, function(x) as.numeric(as.character(x)))
      colnames(pn_data) <- paste0(monitor$monitor_name, "_", colnames(pn_data))
      pn_list[[i]] <- pn_data
    }
    
    if (!is.null(monitor$meta.data)) {
      metadata_list[[i]] <- monitor$meta.data
    }
  }
  
  # Combine data horizontally (by columns)
  if (length(mt_list) > 0) {
    merged_monitor$assays$mt <- do.call(cbind, mt_list)
    cat("  Merged mt data dimensions:", dim(merged_monitor$assays$mt), "\n")
  }
  
  if (length(pn_list) > 0) {
    merged_monitor$assays$pn <- do.call(cbind, pn_list)
    cat("  Merged pn data dimensions:", dim(merged_monitor$assays$pn), "\n")
  }
  
  if (length(metadata_list) > 0) {
    merged_monitor$meta.data <- do.call(rbind, metadata_list)
    cat("  Merged metadata dimensions:", dim(merged_monitor$meta.data), "\n")
    
    # Show sample of groups
    if ("Group" %in% colnames(merged_monitor$meta.data)) {
      groups <- unique(merged_monitor$meta.data$Group)
      cat("  Groups found:", paste(groups, collapse = ", "), "\n")
    }
  }
  
  return(merged_monitor)
}

# Method to access assays (simulate S4 @ access)
`$.mergedMonitorS4` <- function(x, name) {
  if (name == "assays") {
    return(x$assays)
  } else if (name == "meta.data") {
    return(x$meta.data)
  } else {
    return(x[[name]])
  }
}

# Method to access nested assays (simulate S4 @ access)
`@` <- function(x, name) {
  if (class(x)[1] == "mergedMonitorS4") {
    if (name == "assays") {
      return(x$assays)
    } else if (name == "meta.data") {
      return(x$meta.data)
    }
  }
  stop("@ operator not supported for this object type")
}
