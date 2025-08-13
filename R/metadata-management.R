#' Metadata Management Functions for RMB Package
#'
#' @description Functions for generating, updating, and managing metadata files
#' before loading data with loadRMB()

#' Generate metadata template from monitor file
#'
#' @param monitor_file Path to Monitor*.txt file
#' @param config_file Path to experiment configuration file (YAML or JSON, optional)
#' @param output_dir Directory to save metadata file (default: "./metadata/")
#' @param experiment_info List with experiment information (alternative to config_file)
#' @return Path to generated metadata file
#' @export
generate_metadata <- function(monitor_file, 
                             config_file = NULL,
                             output_dir = "./metadata/",
                             experiment_info = NULL) {
  
  if (!file.exists(monitor_file)) {
    stop("Monitor file does not exist: ", monitor_file)
  }
  
  # Create output directory if it doesn't exist
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
  }
  
  # Get monitor name
  monitor_name <- tools::file_path_sans_ext(basename(monitor_file))
  output_file <- file.path(output_dir, paste0(monitor_name, "_metadata.csv"))
  
  # Load experiment info from config file or parameter
  if (!is.null(config_file) && file.exists(config_file)) {
    experiment_info <- load_config_file(config_file)
  } else if (is.null(experiment_info)) {
    # Use default template
    experiment_info <- get_default_experiment_info()
  }
  
  # Read monitor file to get channel count
  raw_data <- readr::read_delim(monitor_file, delim = "\t", col_names = FALSE, show_col_types = FALSE)
  n_channels <- 32  # Standard TriKinetics setup
  
  # Extract monitor number from filename
  monitor_number <- gsub("Monitor", "", monitor_name)
  if (!grepl("^[0-9]+$", monitor_number)) {
    monitor_number <- "1"  # Default if can't extract
  }
  
  # Generate metadata template
  metadata_template <- data.frame(
    Fly = paste0(monitor_name, "_", sprintf("%02d", 1:n_channels)),
    Lab = experiment_info$lab,
    User = experiment_info$user,
    Date = experiment_info$date,
    Experiment_name = experiment_info$experiment_name,
    Monitor_type = experiment_info$monitor_type %||% "TriKinetics",
    Monitor_number = as.numeric(monitor_number),
    Channel = 1:n_channels,
    Group = experiment_info$default_group %||% "Unknown",
    Tube_type = experiment_info$tube_type %||% "Standard",
    Incubator = experiment_info$incubator %||% "Main",
    Temperature = experiment_info$temperature %||% 25,
    Treatment = experiment_info$default_treatment %||% "Control",
    Sex = experiment_info$default_sex %||% "Unknown",
    Other = "",
    Alive = TRUE,
    stringsAsFactors = FALSE
  )
  
  # Apply group assignments if provided in config
  if (!is.null(experiment_info$groups)) {
    metadata_template <- apply_group_assignments(metadata_template, experiment_info$groups)
  }
  
  # Save metadata file
  write.csv(metadata_template, output_file, row.names = FALSE)
  
  cat("Metadata template generated:", output_file, "\n")
  cat("Please review and update the metadata before running loadRMB()\n")
  
  if (!is.null(experiment_info$groups)) {
    cat("\nGroup assignments applied:\n")
    for (group in experiment_info$groups) {
      cat("-", group$name, ":", group$description %||% "", "\n")
    }
  }
  
  cat("\nNext steps:\n")
  cat("1. Review and edit:", output_file, "\n")
  cat("2. Update Group, Treatment, Sex columns as needed\n")
  cat("3. Run: loadRMB(data = '", monitor_file, "', metadata = '", output_file, "')\n")
  
  return(output_file)
}

#' Update existing metadata file
#'
#' @param metadata_file Path to existing metadata CSV file
#' @param updates Named list of updates to apply
#' @param channels Vector of channel numbers to update (default: all)
#' @return Updated metadata data.frame
#' @export
#' @examples
#' \dontrun{
#' # Update specific channels
#' update_metadata("./metadata/Monitor36_metadata.csv",
#'                 updates = list(Group = "Male_Treatment", Sex = "Male"),
#'                 channels = 1:16)
#'                 
#' # Update by condition
#' update_metadata("./metadata/Monitor36_metadata.csv",
#'                 updates = list(Treatment = "Caffeine_5mM"),
#'                 channels = c(1:8, 17:24))
#' }
update_metadata <- function(metadata_file, updates, channels = NULL) {
  
  if (!file.exists(metadata_file)) {
    stop("Metadata file does not exist: ", metadata_file)
  }
  
  # Read existing metadata
  metadata <- read.csv(metadata_file, stringsAsFactors = FALSE)
  
  # Determine which rows to update
  if (is.null(channels)) {
    rows_to_update <- 1:nrow(metadata)
  } else {
    rows_to_update <- which(metadata$Channel %in% channels)
  }
  
  if (length(rows_to_update) == 0) {
    warning("No matching channels found for update")
    return(metadata)
  }
  
  # Apply updates
  for (col_name in names(updates)) {
    if (col_name %in% colnames(metadata)) {
      metadata[rows_to_update, col_name] <- updates[[col_name]]
    } else {
      warning("Column '", col_name, "' not found in metadata")
    }
  }
  
  # Save updated metadata
  write.csv(metadata, metadata_file, row.names = FALSE)
  
  cat("Updated", length(rows_to_update), "rows in", basename(metadata_file), "\n")
  cat("Columns updated:", paste(names(updates), collapse = ", "), "\n")
  
  return(metadata)
}

#' Batch update metadata for multiple monitors
#'
#' @param metadata_dir Directory containing metadata files
#' @param group_assignments List defining group assignments by monitor and channels
#' @return List of updated metadata files
#' @export
#' @examples
#' \dontrun{
#' # Define group assignments
#' assignments <- list(
#'   Monitor36 = list(
#'     channels = 1:32, 
#'     updates = list(Group = "Male_Control", Sex = "Male", Treatment = "Vehicle")
#'   ),
#'   Monitor37 = list(
#'     channels = 1:32,
#'     updates = list(Group = "Female_Control", Sex = "Female", Treatment = "Vehicle")
#'   )
#' )
#' 
#' batch_update_metadata("./metadata/", assignments)
#' }
batch_update_metadata <- function(metadata_dir, group_assignments) {
  
  if (!dir.exists(metadata_dir)) {
    stop("Metadata directory does not exist: ", metadata_dir)
  }
  
  updated_files <- list()
  
  for (monitor_name in names(group_assignments)) {
    metadata_file <- file.path(metadata_dir, paste0(monitor_name, "_metadata.csv"))
    
    if (file.exists(metadata_file)) {
      assignment <- group_assignments[[monitor_name]]
      
      updated_metadata <- update_metadata(
        metadata_file = metadata_file,
        updates = assignment$updates,
        channels = assignment$channels
      )
      
      updated_files[[monitor_name]] <- updated_metadata
      
    } else {
      warning("Metadata file not found: ", metadata_file)
    }
  }
  
  cat("\nBatch update completed for", length(updated_files), "monitors\n")
  return(updated_files)
}

#' Load experiment configuration from file
#'
#' @param config_file Path to YAML or JSON configuration file
#' @return List with experiment configuration
#' @export
load_config_file <- function(config_file) {
  
  if (!file.exists(config_file)) {
    stop("Configuration file does not exist: ", config_file)
  }
  
  file_ext <- tools::file_ext(config_file)
  
  if (file_ext %in% c("yml", "yaml")) {
    if (requireNamespace("yaml", quietly = TRUE)) {
      config <- yaml::read_yaml(config_file)
    } else {
      stop("yaml package required for YAML config files. Install with: install.packages('yaml')")
    }
  } else if (file_ext == "json") {
    if (requireNamespace("jsonlite", quietly = TRUE)) {
      config <- jsonlite::fromJSON(config_file)
    } else {
      stop("jsonlite package required for JSON config files. Install with: install.packages('jsonlite')")
    }
  } else {
    stop("Unsupported config file format. Use .yml, .yaml, or .json")
  }
  
  return(config)
}

#' Create example configuration file
#'
#' @param output_path Path for output configuration file
#' @param format Format: "yaml" or "json" (default: "yaml")
#' @return Path to created configuration file
#' @export
create_config_template <- function(output_path = "./experiment_config.yaml", format = "yaml") {
  
  # Example configuration
  config <- list(
    experiment = list(
      name = "Example_Circadian_Experiment",
      lab = "Your_Lab_Name",
      user = "Your_Name",
      date = format(Sys.Date(), "%Y-%m-%d"),
      monitor_type = "TriKinetics",
      tube_type = "Standard",
      incubator = "Main",
      temperature = 25
    ),
    defaults = list(
      group = "Control",
      treatment = "Vehicle", 
      sex = "Unknown"
    ),
    groups = list(
      list(
        name = "Male_Control",
        description = "Male flies, vehicle treatment",
        treatment = "Vehicle",
        sex = "Male",
        channels = 1:16
      ),
      list(
        name = "Female_Control", 
        description = "Female flies, vehicle treatment",
        treatment = "Vehicle",
        sex = "Female",
        channels = 17:32
      )
    )
  )
  
  if (format == "yaml") {
    if (requireNamespace("yaml", quietly = TRUE)) {
      yaml::write_yaml(config, output_path)
    } else {
      stop("yaml package required. Install with: install.packages('yaml')")
    }
  } else if (format == "json") {
    if (requireNamespace("jsonlite", quietly = TRUE)) {
      jsonlite::write_json(config, output_path, pretty = TRUE)
    } else {
      stop("jsonlite package required. Install with: install.packages('jsonlite')")
    }
  }
  
  cat("Configuration template created:", output_path, "\n")
  cat("Please edit this file with your experiment details before generating metadata\n")
  
  return(output_path)
}

# Helper functions ----

#' Get default experiment information
#' @return List with default experiment info
get_default_experiment_info <- function() {
  list(
    experiment_name = "RMB_Experiment",
    lab = "Your_Lab",
    user = "User",
    date = format(Sys.Date(), "%Y-%m-%d"),
    monitor_type = "TriKinetics",
    tube_type = "Standard",
    incubator = "Main",
    temperature = 25,
    default_group = "Control",
    default_treatment = "Vehicle",
    default_sex = "Unknown"
  )
}

#' Apply group assignments to metadata template
#' @param metadata_template Data.frame with metadata template
#' @param groups List of group definitions
#' @return Updated metadata template
apply_group_assignments <- function(metadata_template, groups) {
  
  for (group in groups) {
    if (!is.null(group$channels) && length(group$channels) > 0) {
      # Find matching rows
      rows_to_update <- which(metadata_template$Channel %in% group$channels)
      
      if (length(rows_to_update) > 0) {
        # Apply group information
        metadata_template$Group[rows_to_update] <- group$name
        
        if (!is.null(group$treatment)) {
          metadata_template$Treatment[rows_to_update] <- group$treatment
        }
        if (!is.null(group$sex)) {
          metadata_template$Sex[rows_to_update] <- group$sex
        }
      }
    }
  }
  
  return(metadata_template)
}

#' Null-coalescing operator
#' @param x First value
#' @param y Default value if x is NULL
#' @return x if not NULL, otherwise y
`%||%` <- function(x, y) {
  if (is.null(x)) y else x
}