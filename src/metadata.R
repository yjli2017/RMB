# Function to load monitor files
load_monitor_files <- function(data_dir) {
  monitor_list <- list.files(path = data_dir, pattern = "Monitor", full.names = TRUE)
  monitorLC_path <- list.files(path = data_dir, pattern = "MonitorLC", full.names = TRUE)
  monitor_list <- setdiff(monitor_list, monitorLC_path)
  return(monitor_list)
}

# Function to create metadata list
create_metadata_list <- function(monitor_list) {
  metadata <- read.csv("./template/monitor_metadata_template.csv")
  metadata_list <- replicate(length(monitor_list), metadata, simplify = FALSE)
  return(metadata_list)
}

# Function to update metadata
update_metadata <- function(monitor_list, metadata_list, Date, tube_type, User, Experiment) {
  for (i in 1:length(monitor_list)) {
    metadata_list[[i]]$Monitor_number <- tools::file_path_sans_ext(basename(monitor_list[i]))  
    metadata_list[[i]]$Date <- Date
    metadata_list[[i]]$Tube_type <- tube_type
    metadata_list[[i]]$User <- User
    metadata_list[[i]]$Temperature <- 25
    metadata_list[[i]]$Experiment_name <- Experiment
    metadata_list[[i]]$Fly <- paste(metadata_list[[i]]$Lab,
                                    metadata_list[[i]]$User,
                                    metadata_list[[i]]$Date,
                                    metadata_list[[i]]$Experiment_name,
                                    metadata_list[[i]]$Monitor_number,
                                    metadata_list[[i]]$Channel,
                                    sep = "_")
  }
  return(metadata_list)
}

# Function to write metadata
write_metadata <- function(data_dir, monitor_list, metadata_list) {
  # Create the metadata directory if it doesn't exist
  dir_path <- file.path(data_dir, "metadata")
  if (!dir.exists(dir_path)) {
    dir.create(dir_path, recursive = TRUE)
  }
  
  # Write each metadata to a CSV file
  for (i in 1:length(monitor_list)) {
    write.csv(metadata_list[[i]], file = paste0(dir_path, "/", tools::file_path_sans_ext(basename(monitor_list[i])), "_metadata.csv"), row.names = FALSE)
  }
}

# Legacy function for backward compatibility (uses global variables)
# This function is kept for compatibility with existing scripts that use output_metadata.R
output_metadata_legacy <- function() {
  # This assumes dir_path, monitor_list, and metadata_list are in global environment
  if (!exists("dir_path") || !exists("monitor_list") || !exists("metadata_list")) {
    stop("Required global variables not found. Please ensure dir_path, monitor_list, and metadata_list are defined.")
  }
  
  if (!dir.exists(dir_path)) {
    dir.create(dir_path, recursive = TRUE)
  }
  
  for (i in 1:length(monitor_list)) {
    write.csv(metadata_list[[i]], file = paste0(dir_path, "/", tools::file_path_sans_ext(basename(monitor_list[i])), "_metadata.csv"), row.names = FALSE)
  }
}

################################################################################
# Metadata Generation Workflow Functions
# This file contains all functions needed for metadata generation workflow

# Function to read experiment configuration from CSV
read_experiment_config <- function(config_file) {
  if (!file.exists(config_file)) {
    stop(paste("Configuration file not found:", config_file))
  }
  
  config <- read.csv(config_file, stringsAsFactors = FALSE)
  
  # Validate required columns
  required_cols <- c("Monitor", "Start_channel", "End_channel", "Phenotype")
  missing_cols <- setdiff(required_cols, colnames(config))
  
  if (length(missing_cols) > 0) {
    stop(paste("Missing required columns in config file:", paste(missing_cols, collapse = ", ")))
  }
  
  return(config)
}

# Function to convert config to phenotype assignments
config_to_phenotype_assignments <- function(config) {
  assignments <- list()
  
  for (i in 1:nrow(config)) {
    monitor_num <- as.numeric(gsub("Monitor", "", config$Monitor[i]))
    start_ch <- config$Start_channel[i]
    end_ch <- config$End_channel[i]
    phenotype <- config$Phenotype[i]
    
    assignments[[i]] <- list(
      monitor = monitor_num,
      rows = start_ch:end_ch,
      phenotype = phenotype
    )
  }
  
  return(assignments)
}

# Function to create phenotype assignments from hardcoded input
# Accepts either a data.frame or a list of assignments
create_phenotype_assignments <- function(assignments_input) {
  if (is.data.frame(assignments_input)) {
    # Convert data.frame to list format
    assignments <- list()
    
    for (i in 1:nrow(assignments_input)) {
      monitor_num <- as.numeric(gsub("Monitor", "", assignments_input$Monitor[i]))
      start_ch <- assignments_input$Start_channel[i]
      end_ch <- assignments_input$End_channel[i]
      phenotype <- assignments_input$Phenotype[i]
      
      assignments[[i]] <- list(
        monitor = monitor_num,
        rows = start_ch:end_ch,
        phenotype = phenotype
      )
    }
    
    return(assignments)
  } else if (is.list(assignments_input)) {
    # Validate list format
    required_fields <- c("monitor", "rows", "phenotype")
    
    for (i in seq_along(assignments_input)) {
      assignment <- assignments_input[[i]]
      missing_fields <- setdiff(required_fields, names(assignment))
      
      if (length(missing_fields) > 0) {
        stop(paste("Assignment", i, "missing required fields:", paste(missing_fields, collapse = ", ")))
      }
    }
    
    return(assignments_input)
  } else {
    stop("assignments_input must be either a data.frame or a list")
  }
}

# Main workflow function to process an experiment
process_experiment <- function(experiment_name, data_dir, date, config_file = NULL, 
                             phenotype_assignments = NULL, user = "liy27", tube_type = "Normal", verbose = TRUE) {
  
  if (verbose) {
    cat("Processing experiment:", experiment_name, "\n")
    cat("Data directory:", data_dir, "\n")
    cat("Date:", date, "\n")
  }
  
  # Source required functions
  source("./src/metadata.R")
  
  # Load monitor files
  monitor_list <- load_monitor_files(data_dir)
  if (verbose) {
    cat("Found", length(monitor_list), "monitor files\n")
  }
  
  # Create metadata list
  metadata_list <- create_metadata_list(monitor_list)
  
  # Update metadata with experiment details
  metadata_list <- update_metadata(monitor_list, metadata_list, date, tube_type, user, experiment_name)
  
  # Apply phenotype assignments from either CSV file or hardcoded input
  assignments <- NULL
  
  if (!is.null(config_file)) {
    # CSV-based configuration
    if (verbose) cat("Reading configuration from:", config_file, "\n")
    
    config <- read_experiment_config(config_file)
    assignments <- config_to_phenotype_assignments(config)
    
  } else if (!is.null(phenotype_assignments)) {
    # Hardcoded configuration
    if (verbose) cat("Using hardcoded phenotype assignments\n")
    
    assignments <- create_phenotype_assignments(phenotype_assignments)
  }
  
  # Process assignments if available
  if (!is.null(assignments)) {
    treatments <- extract_treatments(assignments)
    
    if (verbose) {
      cat("Detected treatments:", paste(treatments, collapse = ", "), "\n")
      cat("Applying phenotype assignments...\n")
    }
    
    # Apply phenotype assignments
    for (assignment in assignments) {
      monitor_idx <- assignment$monitor - 35  # Convert Monitor36 -> index 1
      rows <- assignment$rows
      phenotype <- assignment$phenotype
      
      if (monitor_idx > 0 && monitor_idx <= length(metadata_list)) {
        metadata_list[[monitor_idx]][rows, ]$Phenotype <- phenotype
        
        # Extract and assign treatment
        treatment <- sub("^(Male|Female)\\s+", "", phenotype)
        metadata_list[[monitor_idx]][rows, ]$Treatment <- treatment
        
        if (verbose) {
          cat("  Monitor", assignment$monitor, "channels", min(rows), "-", max(rows), 
              "->", phenotype, "(Treatment:", treatment, ")\n")
        }
      } else {
        warning(paste("Monitor", assignment$monitor, "not found for experiment", experiment_name))
      }
    }
  }
  
  # Write metadata files
  write_metadata(data_dir, monitor_list, metadata_list)
  
  if (verbose) {
    cat("Metadata generation completed for:", experiment_name, "\n")
    cat("Files written to:", file.path(data_dir, "metadata"), "\n\n")
  }
  
  return(metadata_list)
}

# Function to process multiple experiments from a list
process_experiments <- function(experiments, verbose = TRUE) {
  if (verbose) {
    cat("Starting metadata generation for", length(experiments), "experiments...\n\n")
  }
  
  results <- list()
  
  for (i in seq_along(experiments)) {
    exp <- experiments[[i]]
    
    results[[i]] <- tryCatch({
      process_experiment(
        experiment_name = exp$name,
        data_dir = exp$data_dir,
        date = exp$date,
        config_file = exp$config_file,
        phenotype_assignments = exp$phenotype_assignments,
        user = ifelse(is.null(exp$user), "liy27", exp$user),
        tube_type = ifelse(is.null(exp$tube_type), "Normal", exp$tube_type),
        verbose = verbose
      )
    }, error = function(e) {
      cat("ERROR processing experiment", exp$name, ":", conditionMessage(e), "\n")
      return(NULL)
    })
  }
  
  # Summary
  if (verbose) {
    successful <- sum(sapply(results, function(x) !is.null(x)))
    cat("Completed! Successfully processed", successful, "out of", length(experiments), "experiments.\n")
  }
  
  return(results)
}
