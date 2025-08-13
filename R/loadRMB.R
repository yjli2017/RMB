#' Load RMB Data
#'
#' @description 
#' Load TriKinetics monitor data and metadata, creating a raw MonitorS4 object.
#' This function only loads data without any processing. Use separate functions
#' for data cleaning, filtering, and analysis.
#'
#' @param data Path to Monitor*.txt files or directory containing them
#' @param metadata Path to metadata CSV file(s) or directory containing them
#' @return A MonitorS4 object with raw data (MT, CT, PN assays)
#' @export
#' @examples
#' \dontrun{
#' # Step 1: Generate metadata template
#' generate_metadata("./Monitor36.txt", output_dir = "./metadata/")
#' 
#' # Step 2: Edit the generated metadata file as needed
#' # ./metadata/Monitor36_metadata.csv
#' 
#' # Step 3: Load raw data
#' data <- loadRMB(data = "./Monitor36.txt", 
#'                metadata = "./metadata/Monitor36_metadata.csv")
#' 
#' # Step 4: Process data using separate functions
#' data <- CleanDays(data, days = c(2,3,4))
#' data <- DetectDeadFlies(data)
#' data <- FilterAliveFlies(data)
#' 
#' # Step 5: Analyze and plot
#' activity_stats <- AnalyzeActivity(data, group_by = "Group")
#' PlotActivityTimecourse(data, group_by = "Group")
#' }
loadRMB <- function(data, metadata) {
  
  # Validate inputs
  if (missing(data) || missing(metadata)) {
    stop("Both 'data' and 'metadata' arguments must be provided")
  }
  
  # Handle data path
  if (dir.exists(data)) {
    data_path <- data
    file_mapping <- match_files(data_path, metadata)
  } else if (file.exists(data)) {
    # Single file provided
    data_path <- dirname(data)
    monitor_name <- tools::file_path_sans_ext(basename(data))
    
    # Try to find corresponding metadata
    if (dir.exists(metadata)) {
      metadata_files <- list.files(metadata, pattern = ".*metadata.*\\.csv$", full.names = TRUE)
      metadata_match <- grep(monitor_name, metadata_files, value = TRUE)
      if (length(metadata_match) > 0) {
        metadata_file <- metadata_match[1]
      } else {
        metadata_file <- NA_character_
      }
    } else if (file.exists(metadata)) {
      metadata_file <- metadata
    } else {
      metadata_file <- NA_character_
    }
    
    file_mapping <- data.frame(
      monitor_file = data,
      monitor_name = monitor_name,
      metadata_file = metadata_file,
      stringsAsFactors = FALSE
    )
  } else {
    stop("Data path does not exist: ", data)
  }
  
  # Check if metadata directory/file exists
  if (!dir.exists(metadata) && !file.exists(metadata)) {
    stop("Metadata path does not exist: ", metadata)
  }
  
  cat("Found", nrow(file_mapping), "monitor file(s) to process\n")
  
  # Process each monitor file
  all_monitors <- list()
  
  for (i in seq_len(nrow(file_mapping))) {
    monitor_file <- file_mapping$monitor_file[i]
    monitor_name <- file_mapping$monitor_name[i]
    metadata_file <- file_mapping$metadata_file[i]
    
    cat("Processing", monitor_name, "...\n")
    
    # Parse monitor data
    tryCatch({
      raw_data <- parse_monitor_file(monitor_file)
      assays <- process_monitor_assays(raw_data)
      
      # Handle metadata
      if (is.na(metadata_file) || !file.exists(metadata_file)) {
        stop("Metadata file not found for ", monitor_name, ". Please generate metadata first using generate_metadata()")
      } else {
        cat("  Loading metadata from", basename(metadata_file), "\n")
        metadata_df <- readr::read_csv(metadata_file, show_col_types = FALSE)
      }
      
      # Create time information
      time_info <- list(
        start_time = min(raw_data$DateTime, na.rm = TRUE),
        end_time = max(raw_data$DateTime, na.rm = TRUE),
        duration_hours = as.numeric(difftime(max(raw_data$DateTime, na.rm = TRUE), 
                                           min(raw_data$DateTime, na.rm = TRUE), 
                                           units = "hours")),
        time_points = unique(raw_data$DateTime),
        light_cycle = unique(raw_data$Light)
      )
      
      # Create MonitorS4 object
      monitor_obj <- MonitorS4(
        meta.data = metadata_df,
        assays = assays,
        active.assay = "mt",
        time = time_info
      )
      
      all_monitors[[monitor_name]] <- monitor_obj
      cat("  Successfully processed", monitor_name, "\n")
      
    }, error = function(e) {
      warning("Failed to process ", monitor_name, ": ", e$message)
    })
  }
  
  if (length(all_monitors) == 0) {
    stop("Failed to process any monitor files")
  }
  
  # Merge multiple monitors if necessary
  if (length(all_monitors) == 1) {
    final_object <- all_monitors[[1]]
  } else {
    cat("Merging", length(all_monitors), "monitor objects...\n")
    final_object <- merge_monitors(all_monitors)
  }
  
  # Add loading metadata
  final_object@time$loading_date <- Sys.time()
  final_object@time$loading_params <- list(
    data_source = if(dir.exists(data)) data else dirname(data),
    metadata_source = if(dir.exists(metadata)) metadata else dirname(metadata),
    monitors_loaded = length(all_monitors)
  )
  
  cat("Data loading complete!\n")
  cat("Total flies:", nrow(final_object@meta.data), "\n")
  cat("Available assays:", paste(names(final_object@assays), collapse = ", "), "\n")
  cat("Time range:", format(final_object@time$start_time), "to", format(final_object@time$end_time), "\n")
  cat("Use CleanDays(), DetectDeadFlies(), and FilterAliveFlies() for data processing.\n")
  
  return(final_object)
}

#' Merge multiple MonitorS4 objects
#'
#' @param monitor_list List of MonitorS4 objects to merge
#' @return Single merged MonitorS4 object
#' @export
merge_monitors <- function(monitor_list) {
  if (length(monitor_list) == 1) {
    return(monitor_list[[1]])
  }
  
  # Merge metadata - combine all monitor metadata with updated channel names
  all_metadata <- do.call(rbind, lapply(names(monitor_list), function(monitor_name) {
    metadata <- monitor_list[[monitor_name]]@meta.data
    # Update channel references to include monitor name
    metadata$Channel <- paste0(metadata$Channel, "_", monitor_name)
    return(metadata)
  }))
  
  # Merge assays
  assay_names <- unique(unlist(lapply(monitor_list, function(x) names(x@assays))))
  merged_assays <- list()
  
  for (assay in assay_names) {
    assay_list <- lapply(monitor_list, function(x) {
      if (assay %in% names(x@assays)) {
        return(x@assays[[assay]])
      } else {
        return(NULL)
      }
    })
    assay_list <- assay_list[!sapply(assay_list, is.null)]
    
    if (length(assay_list) > 0) {
      # For merging monitors: same timepoints (rows), but combine fly channels (columns)
      # Take metadata columns from first monitor, combine channel columns from all
      
      base_data <- assay_list[[1]]
      metadata_cols <- c("Index", "Date", "Time", "Status1", "Status2", 
                        "Monitor", "Light", "Measure_Type", "Meta9", "Meta10", 
                        "DateTime", "Monitor_Name")
      
      # Start with metadata from first monitor
      merged_data <- base_data[, metadata_cols[metadata_cols %in% colnames(base_data)]]
      
      # Add channel columns from each monitor with unique names
      for (i in seq_along(assay_list)) {
        monitor_data <- assay_list[[i]]
        monitor_name <- unique(monitor_data$Monitor_Name)[1]
        channel_cols <- grep("Channel_", colnames(monitor_data), value = TRUE)
        
        # Rename channels to include monitor name
        renamed_channels <- monitor_data[, channel_cols]
        colnames(renamed_channels) <- paste0(channel_cols, "_", monitor_name)
        
        # Add to merged data
        merged_data <- cbind(merged_data, renamed_channels)
      }
      
      merged_assays[[assay]] <- merged_data
    }
  }
  
  # Merge time information
  all_times <- lapply(monitor_list, function(x) x@time)
  merged_time <- list(
    start_time = min(sapply(all_times, function(x) x$start_time), na.rm = TRUE),
    end_time = max(sapply(all_times, function(x) x$end_time), na.rm = TRUE),
    duration_hours = max(sapply(all_times, function(x) x$duration_hours), na.rm = TRUE),
    time_points = sort(unique(unlist(lapply(all_times, function(x) x$time_points)))),
    light_cycle = unique(unlist(lapply(all_times, function(x) x$light_cycle)))
  )
  
  # Create merged object
  merged_object <- MonitorS4(
    meta.data = all_metadata,
    assays = merged_assays,
    active.assay = names(merged_assays)[1],
    time = merged_time
  )
  
  return(merged_object)
}