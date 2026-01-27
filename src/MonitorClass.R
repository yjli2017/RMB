# MonitorS4 Class Definition
# Proper S4 class for Drosophila activity monitor data

#' MonitorS4 Class
#' 
#' An S4 class for storing and analyzing Drosophila TriKinetics monitor data.
#' Inspired by Seurat object design patterns.
#' 
#' @slot assays List of data.frames containing different data types (mt, pn, ct, etc.)
#' @slot meta.data Data.frame containing metadata for each fly/channel
#' @slot time Data.frame containing temporal information
#' @slot monitor_name Character string identifying the monitor
#' @slot file_path Character string with path to source file
#' @slot active.assay Character string indicating the currently active assay
#' 
#' @exportClass MonitorS4
setClass("MonitorS4",
  slots = list(
    assays = "list",
    meta.data = "data.frame",
    time = "data.frame",
    monitor_name = "character",
    file_path = "character",
    active.assay = "character"
  ),
  prototype = list(
    assays = list(),
    meta.data = data.frame(),
    time = data.frame(),
    monitor_name = character(0),
    file_path = character(0),
    active.assay = "mt"
  )
)

#' Create a MonitorS4 object from a file path
#' 
#' @param file_path Path to the DAM monitor text file
#' @param verbose Print progress messages (default TRUE)
#' @return A MonitorS4 object
#' @export
createMonitor <- function(file_path, verbose = TRUE) {
  if (!file.exists(file_path)) {
    stop("File not found: ", file_path)
  }
  
  monitor_name <- tools::file_path_sans_ext(basename(file_path))
  if (verbose) cat("Creating MonitorS4 object for:", monitor_name, "\n")
  
  # Read raw data
  raw_data <- read.table(file_path, header = FALSE, sep = "\t", 
                         fill = TRUE, stringsAsFactors = FALSE)
  
  # Create the object
  monitor <- new("MonitorS4",
    monitor_name = monitor_name,
    file_path = file_path,
    active.assay = "mt"
  )
  
  # Store raw data temporarily for processing
  monitor@assays$raw <- raw_data
  
  return(monitor)
}

#' Process MonitorS4 data into assays
#' 
#' Parses raw DAM data into separate assays (mt, pn, ct)
#' 
#' @param monitor A MonitorS4 object
#' @param verbose Print progress messages (default TRUE)
#' @return The MonitorS4 object with populated assays
#' @export
processMonitor <- function(monitor, verbose = TRUE) {
  source_constants()
  
  if (verbose) cat("Processing data for:", monitor@monitor_name, "\n")
  
  raw_data <- monitor@assays$raw
  if (is.null(raw_data) || nrow(raw_data) == 0) {
    warning("No raw data found for ", monitor@monitor_name)
    return(monitor)
  }
  
  # Check data format
  if (ncol(raw_data) < DAM_HEADER_COLS + DAM_CHANNELS) {
    warning("Unexpected data format for ", monitor@monitor_name)
    return(monitor)
  }
  
  # Parse by data type
  type_col <- DAM_HEADER_COLS  # Column 8 contains the type
  
  mt_rows <- which(raw_data[, type_col] == DAM_TYPE_MOVEMENT)
  pn_rows <- which(raw_data[, type_col] == DAM_TYPE_POSITION)
  ct_rows <- which(raw_data[, type_col] == DAM_TYPE_CUMULATIVE)
  
  data_cols <- (DAM_HEADER_COLS + 1):min(DAM_HEADER_COLS + DAM_CHANNELS, ncol(raw_data))
  
  # Extract movement data (MT)
  if (length(mt_rows) > 0) {
    mt_data <- raw_data[mt_rows, data_cols]
    mt_data[] <- lapply(mt_data, function(x) as.numeric(as.character(x)))
    colnames(mt_data) <- paste0("Ch", seq_len(ncol(mt_data)))
    monitor@assays$mt <- mt_data
    if (verbose) cat("  MT data:", nrow(mt_data), "rows x", ncol(mt_data), "channels\n")
  }
  
  # Extract position data (Pn)
  if (length(pn_rows) > 0) {
    pn_data <- raw_data[pn_rows, data_cols]
    pn_data[] <- lapply(pn_data, function(x) as.numeric(as.character(x)))
    colnames(pn_data) <- paste0("Ch", seq_len(ncol(pn_data)))
    monitor@assays$pn <- pn_data
    if (verbose) cat("  Pn data:", nrow(pn_data), "rows x", ncol(pn_data), "channels\n")
  }
  
  # Extract cumulative data (CT)
  if (length(ct_rows) > 0) {
    ct_data <- raw_data[ct_rows, data_cols]
    ct_data[] <- lapply(ct_data, function(x) as.numeric(as.character(x)))
    colnames(ct_data) <- paste0("Ch", seq_len(ncol(ct_data)))
    monitor@assays$ct <- ct_data
    if (verbose) cat("  CT data:", nrow(ct_data), "rows x", ncol(ct_data), "channels\n")
  }
  
  # Extract time information from MT rows
  if (length(mt_rows) > 0) {
    time_data <- raw_data[mt_rows, 1:DAM_HEADER_COLS]
    colnames(time_data) <- c("RowID", "Date", "Time", "V4", "V5", "Monitor", "V7", "Type")
    monitor@time <- time_data
  }
  
  # Remove raw data to save memory
  monitor@assays$raw <- NULL
  
  return(monitor)
}

#' Merge multiple MonitorS4 objects
#' 
#' @param monitor_list List of MonitorS4 objects
#' @param verbose Print progress messages (default TRUE)
#' @return A merged MonitorS4 object
#' @export
mergeMonitors <- function(monitor_list, verbose = TRUE) {
  if (verbose) cat("Merging", length(monitor_list), "monitors\n")
  
  # Initialize merged object
  merged <- new("MonitorS4",
    monitor_name = "merged",
    active.assay = "mt"
  )
  
  # Collect assays
  assay_names <- unique(unlist(lapply(monitor_list, function(m) names(m@assays))))
  assay_names <- assay_names[!is.na(assay_names) & assay_names != "raw"]
  
  for (assay_name in assay_names) {
    data_list <- lapply(seq_along(monitor_list), function(i) {
      m <- monitor_list[[i]]
      if (!is.null(m@assays[[assay_name]])) {
        d <- m@assays[[assay_name]]
        colnames(d) <- paste0(m@monitor_name, "_", colnames(d))
        return(d)
      }
      return(NULL)
    })
    data_list <- data_list[!sapply(data_list, is.null)]
    
    if (length(data_list) > 0) {
      merged@assays[[assay_name]] <- do.call(cbind, data_list)
      if (verbose) cat("  Merged", assay_name, ":", dim(merged@assays[[assay_name]]), "\n")
    }
  }
  
  # Merge metadata
  meta_list <- lapply(monitor_list, function(m) {
    if (nrow(m@meta.data) > 0) return(m@meta.data)
    return(NULL)
  })
  meta_list <- meta_list[!sapply(meta_list, is.null)]
  
  if (length(meta_list) > 0) {
    merged@meta.data <- do.call(rbind, meta_list)
    if (verbose) cat("  Merged metadata:", nrow(merged@meta.data), "rows\n")
  }
  
  return(merged)
}

#' Remove dead/inactive flies from a MonitorS4 object
#' 
#' @param monitor A MonitorS4 object
#' @param min_movement Minimum total movement to be considered alive (default 10)
#' @param verbose Print progress messages (default TRUE)
#' @return The MonitorS4 object with dead flies removed
#' @export
removeDeadFlies <- function(monitor, min_movement = 10, verbose = TRUE) {
  if (verbose) cat("Removing dead flies (threshold:", min_movement, ")\n")
  
  if (is.null(monitor@assays$mt)) {
    warning("No movement data found")
    return(monitor)
  }
  
  mt_data <- monitor@assays$mt
  channel_totals <- colSums(mt_data, na.rm = TRUE)
  alive <- channel_totals >= min_movement
  
  n_alive <- sum(alive)
  n_dead <- sum(!alive)
  
  if (verbose) {
    cat("  Total:", length(alive), "| Alive:", n_alive, "| Dead:", n_dead, "\n")
  }
  
  if (n_alive == 0) {
    warning("No alive channels found - check threshold")
    return(monitor)
  }
  
  # Filter all assays
  for (assay_name in names(monitor@assays)) {
    if (!is.null(monitor@assays[[assay_name]])) {
      monitor@assays[[assay_name]] <- monitor@assays[[assay_name]][, alive, drop = FALSE]
    }
  }
  
  # Filter metadata
  if (nrow(monitor@meta.data) == length(alive)) {
    monitor@meta.data <- monitor@meta.data[alive, , drop = FALSE]
  }
  
  return(monitor)
}

# Helper to source constants if not already loaded
source_constants <- function() {
  if (!exists("DAM_CHANNELS")) {
    # Try to source constants from the same directory
    script_dir <- dirname(sys.frame(1)$ofile)
    if (!is.null(script_dir)) {
      constants_file <- file.path(script_dir, "constants.R")
      if (file.exists(constants_file)) {
        source(constants_file)
      }
    }
  }
}

# Print method for MonitorS4
setMethod("show", "MonitorS4", function(object) {
  cat("MonitorS4 object:", object@monitor_name, "\n")
  cat("  Assays:", paste(names(object@assays), collapse = ", "), "\n")
  if (!is.null(object@assays$mt)) {
    cat("  Dimensions:", nrow(object@assays$mt), "timepoints x", 
        ncol(object@assays$mt), "channels\n")
  }
  if (nrow(object@meta.data) > 0) {
    cat("  Metadata:", nrow(object@meta.data), "rows\n")
  }
})
