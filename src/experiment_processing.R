# Metadata Generation Workflow Utilities
# Functions for processing experiments and generating metadata

# Function to process a single experiment
process_experiment <- function(experiment_name, data_dir, date, config_file_path = NULL, 
                              config_print_rows = NULL, config_print_cols = NULL, 
                              phenotype_assignments = NULL,
                              user = "liy27", experiment_prefix = "", tube_type = "Normal") {
  
  cat("Processing experiment:", experiment_name, "\n")
  
  # Set global variables for the experiment
  data_dir <<- data_dir
  date <<- date
  
  # Read config file if provided
  if (!is.null(config_file_path) && file.exists(config_file_path)) {
    config_file <- read.delim(config_file_path, header = FALSE, sep = " ")
    
    if (!is.null(config_print_rows) && !is.null(config_print_cols)) {
      cat("Config file preview:\n")
      print(config_file[config_print_rows, config_print_cols])
    }
  }
  
  # Load monitor files and create metadata
  monitor_list <- load_monitor_files(data_dir)
  metadata_list <- create_metadata_list(monitor_list)
  
  # Update metadata with experiment details
  metadata_list <- update_metadata(monitor_list, metadata_list, date, tube_type, user, experiment_name)
  
  # Apply phenotype assignments if provided
  if (!is.null(phenotype_assignments)) {
    for (assignment in phenotype_assignments) {
      monitor_idx <- assignment$monitor
      rows <- assignment$rows
      phenotype <- assignment$phenotype
      
      if (monitor_idx <= length(metadata_list)) {
        metadata_list[[monitor_idx]][rows, ]$Phenotype <- phenotype
      } else {
        warning(paste("Monitor", monitor_idx, "not found for experiment", experiment_name))
      }
    }
  }
  
  # Create output directory path
  dir_path <<- file.path(data_dir, "metadata")
  
  # Write metadata files
  write_metadata(data_dir, monitor_list, metadata_list)
  
  cat("Completed experiment:", experiment_name, "\n\n")
  
  return(metadata_list)
}

# Function to process multiple experiments
process_experiments <- function(experiments) {
  cat("Starting metadata generation for all experiments...\n")
  cat("Total experiments to process:", length(experiments), "\n\n")
  
  results <- list()
  
  for (i in seq_along(experiments)) {
    exp <- experiments[[i]]
    
    results[[i]] <- tryCatch({
      process_experiment(
        experiment_name = exp$name,
        data_dir = exp$data_dir,
        date = exp$date,
        config_file_path = exp$config_file,
        config_print_rows = exp$config_print_rows,
        config_print_cols = exp$config_print_cols,
        phenotype_assignments = exp$phenotype_assignments,
        user = if(is.null(exp$user)) "liy27" else exp$user,
        tube_type = if(is.null(exp$tube_type)) "Normal" else exp$tube_type
      )
    }, error = function(e) {
      cat("Error processing experiment", exp$name, ":", conditionMessage(e), "\n")
      return(NULL)
    })
  }
  
  cat("Metadata generation completed!\n")
  return(results)
}

# Function to validate experiment configuration
validate_experiment_config <- function(experiments) {
  cat("Validating experiment configurations...\n")
  
  required_fields <- c("name", "data_dir", "date")
  
  for (i in seq_along(experiments)) {
    exp <- experiments[[i]]
    missing_fields <- setdiff(required_fields, names(exp))
    
    if (length(missing_fields) > 0) {
      warning(paste("Experiment", i, "missing required fields:", paste(missing_fields, collapse = ", ")))
    }
    
    # Check if data directory exists
    if (!is.null(exp$data_dir) && !dir.exists(exp$data_dir)) {
      warning(paste("Data directory does not exist for experiment", exp$name, ":", exp$data_dir))
    }
    
    # Check if config file exists (if specified)
    if (!is.null(exp$config_file) && !file.exists(exp$config_file)) {
      warning(paste("Config file does not exist for experiment", exp$name, ":", exp$config_file))
    }
  }
  
  cat("Configuration validation completed.\n")
}
