#' Generate Metadata for TriKinetics Experiments
#'
#' @description 
#' Automated metadata generation for TriKinetics monitor experiments with 
#' common experimental designs (all channels same vs. split design)
#'
#' @param experiments List of experiment configurations
#' @return List of generated metadata files
#' @export
generate_experiment_metadata <- function(experiments) {
  
  if (!is.list(experiments)) {
    stop("experiments must be a list of experiment configurations")
  }
  
  generated_files <- character(0)
  
  for (exp in experiments) {
    cat("Processing experiment:", exp$name, "\n")
    
    # Validate experiment configuration
    required_fields <- c("name", "data_dir", "date", "assignments")
    missing_fields <- setdiff(required_fields, names(exp))
    if (length(missing_fields) > 0) {
      warning("Skipping experiment ", exp$name, ": missing fields: ", paste(missing_fields, collapse = ", "))
      next
    }
    
    # Find monitor files in the directory
    monitor_files <- list.files(exp$data_dir, pattern = "^Monitor.*\\.txt$", full.names = FALSE)
    if (length(monitor_files) == 0) {
      warning("No Monitor*.txt files found in ", exp$data_dir)
      next
    }
    
    # Create metadata directory if it doesn't exist
    metadata_dir <- file.path(exp$data_dir, "metadata")
    if (!dir.exists(metadata_dir)) {
      dir.create(metadata_dir, recursive = TRUE)
    }
    
    # Process each monitor file
    for (monitor_file in monitor_files) {
      monitor_name <- tools::file_path_sans_ext(monitor_file)
      
      # Check if this monitor has specific assignments
      monitor_assignments <- NULL
      for (assignment in exp$assignments) {
        if (assignment$monitor == monitor_name || 
            assignment$monitor == as.numeric(gsub("Monitor", "", monitor_name))) {
          monitor_assignments <- assignment
          break
        }
      }
      
      if (is.null(monitor_assignments)) {
        cat("  No assignments found for", monitor_name, "- using default\n")
        monitor_assignments <- list(
          channels = 1:32,
          genotype = "Unknown",
          treatment = "Control",
          sex = "Unknown"
        )
      }
      
      # Generate metadata for this monitor
      metadata <- generate_monitor_metadata(
        monitor_name = monitor_name,
        experiment_config = exp,
        assignments = monitor_assignments
      )
      
      # Write metadata file
      metadata_file <- file.path(metadata_dir, paste0(monitor_name, "_metadata.csv"))
      readr::write_csv(metadata, metadata_file)
      generated_files <- c(generated_files, metadata_file)
      
      cat("  Generated:", basename(metadata_file), "\n")
    }
  }
  
  cat("Metadata generation complete!\n")
  cat("Generated", length(generated_files), "files\n")
  
  return(generated_files)
}

#' Generate metadata for a single monitor
#'
#' @param monitor_name Name of the monitor (e.g., "Monitor36")
#' @param experiment_config Experiment configuration
#' @param assignments Channel assignments for this monitor
#' @return Data frame with metadata
generate_monitor_metadata <- function(monitor_name, experiment_config, assignments) {
  
  # Create base metadata structure for 32 channels
  metadata <- data.frame(
    Fly = paste0(experiment_config$user, "_", experiment_config$date, "_", 
                experiment_config$name, "_", monitor_name, "_", 1:32),
    Lab = experiment_config$lab %||% "Lab",
    User = experiment_config$user %||% "User", 
    Date = experiment_config$date,
    Experiment_name = experiment_config$name,
    Monitor_type = experiment_config$monitor_type %||% "MB",
    Monitor_number = monitor_name,
    Channel = 1:32,
    Group = NA_character_,
    Tube_type = experiment_config$tube_type %||% "MB",
    Incubator = experiment_config$incubator %||% "Incubator1",
    Temperature = experiment_config$temperature %||% 25,
    Treatment = "Control",  # Default, will be overridden
    Sex = "Unknown",        # Default, will be overridden
    Other = experiment_config$other %||% "Wcs",
    Alive = NA,
    stringsAsFactors = FALSE
  )
  
  # Apply assignments based on type
  if ("design" %in% names(assignments)) {
    metadata <- apply_experimental_design(metadata, assignments)
  } else {
    # Legacy single assignment format
    channels <- assignments$channels %||% 1:32
    metadata$Group[channels] <- assignments$genotype %||% "Unknown"
    metadata$Treatment[channels] <- assignments$treatment %||% "Control"
    metadata$Sex[channels] <- assignments$sex %||% "Unknown"
  }
  
  return(metadata)
}

#' Apply experimental design to metadata
#'
#' @param metadata Base metadata data frame
#' @param assignments Assignment configuration with design
#' @return Updated metadata data frame
apply_experimental_design <- function(metadata, assignments) {
  
  design_type <- assignments$design
  
  if (design_type == "all_same") {
    # All 32 channels have the same condition
    metadata$Group <- assignments$genotype %||% "Unknown"
    metadata$Treatment <- assignments$treatment %||% "Control" 
    metadata$Sex <- assignments$sex %||% "Unknown"
    
  } else if (design_type == "split_half") {
    # Split design: channels 1-16 vs 17-32
    
    # First half (channels 1-16)
    first_half <- 1:16
    metadata$Group[first_half] <- assignments$first_half$genotype %||% "Genotype1"
    metadata$Treatment[first_half] <- assignments$first_half$treatment %||% "Control"
    metadata$Sex[first_half] <- assignments$first_half$sex %||% "Unknown"
    
    # Second half (channels 17-32)
    second_half <- 17:32
    metadata$Group[second_half] <- assignments$second_half$genotype %||% "Genotype2"
    metadata$Treatment[second_half] <- assignments$second_half$treatment %||% "Treatment"
    metadata$Sex[second_half] <- assignments$second_half$sex %||% "Unknown"
    
  } else if (design_type == "custom") {
    # Custom channel assignments
    for (group in assignments$groups) {
      channels <- group$channels
      metadata$Group[channels] <- group$genotype %||% "Unknown"
      metadata$Treatment[channels] <- group$treatment %||% "Control"
      metadata$Sex[channels] <- group$sex %||% "Unknown"
    }
  }
  
  return(metadata)
}

#' Helper operator for default values
`%||%` <- function(x, y) if (is.null(x)) y else x

#' Create example experiment configurations
#'
#' @param template_type Type of template ("all_same", "split_half", "custom")
#' @return Example experiment configuration
#' @export
create_experiment_template <- function(template_type = "split_half") {
  
  base_config <- list(
    name = "my_experiment",
    data_dir = "./data/my_experiment/",
    date = format(Sys.Date(), "%Y%m%d"),
    lab = "MyLab",
    user = "researcher",
    monitor_type = "MB",
    tube_type = "MB", 
    incubator = "Incubator1",
    temperature = 25,
    other = "Wcs"
  )
  
  if (template_type == "all_same") {
    base_config$assignments <- list(
      list(
        monitor = "Monitor36",  # Can be monitor name or number
        design = "all_same",
        genotype = "w1118",
        treatment = "Control",
        sex = "Mixed"
      )
    )
    
  } else if (template_type == "split_half") {
    base_config$assignments <- list(
      list(
        monitor = "Monitor36",
        design = "split_half",
        first_half = list(
          genotype = "Control_CS",
          treatment = "Control", 
          sex = "Male"
        ),
        second_half = list(
          genotype = "Mutant_line",
          treatment = "Control",
          sex = "Male"
        )
      )
    )
    
  } else if (template_type == "custom") {
    base_config$assignments <- list(
      list(
        monitor = "Monitor36",
        design = "custom", 
        groups = list(
          list(channels = 1:8, genotype = "WT_Male", treatment = "Control", sex = "Male"),
          list(channels = 9:16, genotype = "WT_Female", treatment = "Control", sex = "Female"),
          list(channels = 17:24, genotype = "Mutant_Male", treatment = "Drug", sex = "Male"),
          list(channels = 25:32, genotype = "Mutant_Female", treatment = "Drug", sex = "Female")
        )
      )
    )
  }
  
  return(base_config)
}

#' Generate metadata from Monitor file analysis
#'
#' @description
#' Analyzes actual Monitor*.txt files to understand data structure and 
#' generates appropriate metadata templates
#' 
#' @param monitor_file Path to Monitor*.txt file
#' @return List with file info and suggested metadata structure
#' @export
analyze_monitor_file <- function(monitor_file) {
  
  if (!file.exists(monitor_file)) {
    stop("Monitor file does not exist: ", monitor_file)
  }
  
  # Read first few lines to understand structure
  raw_lines <- readLines(monitor_file, n = 20)
  
  # Try to parse the data
  tryCatch({
    raw_data <- readr::read_delim(monitor_file, delim = "\t", 
                                 col_names = FALSE, show_col_types = FALSE, n_max = 100)
    
    # Analyze structure
    n_cols <- ncol(raw_data)
    n_data_cols <- n_cols - 8  # Subtract metadata columns
    
    # Check for MT, CT, Pn data types
    if ("X8" %in% colnames(raw_data)) {
      measure_types <- unique(raw_data$X8)
    } else {
      measure_types <- c("Unknown")
    }
    
    # Extract time range if possible
    if (n_cols >= 3) {
      tryCatch({
        datetime_col <- paste(raw_data[[2]], raw_data[[3]])
        parsed_times <- lubridate::dmy_hms(datetime_col)
        time_range <- range(parsed_times, na.rm = TRUE)
      }, error = function(e) {
        time_range <- c(NA, NA)
      })
    } else {
      time_range <- c(NA, NA)
    }
    
    analysis <- list(
      file_path = monitor_file,
      monitor_name = tools::file_path_sans_ext(basename(monitor_file)),
      total_columns = n_cols,
      data_columns = n_data_cols,
      expected_channels = min(32, n_data_cols),  # Usually 32, but could be less
      measure_types = measure_types,
      time_range = time_range,
      total_rows = nrow(raw_data),
      suggestions = list()
    )
    
    # Add suggestions
    if (n_data_cols == 32) {
      analysis$suggestions <- c(analysis$suggestions,
        "✓ Standard 32-channel monitor detected",
        "• Use 'split_half' design for 1-16 vs 17-32 comparison", 
        "• Use 'all_same' design if all channels have same condition"
      )
    } else {
      analysis$suggestions <- c(analysis$suggestions,
        paste("⚠ Unusual channel count:", n_data_cols, "channels detected"),
        "• Check monitor configuration"
      )
    }
    
    if ("MT" %in% measure_types && "CT" %in% measure_types && "Pn" %in% measure_types) {
      analysis$suggestions <- c(analysis$suggestions,
        "✓ All data types detected (MT, CT, Pn)",
        "• Full analysis pipeline available"
      )
    }
    
    return(analysis)
    
  }, error = function(e) {
    warning("Could not parse monitor file: ", e$message)
    return(list(
      file_path = monitor_file,
      monitor_name = tools::file_path_sans_ext(basename(monitor_file)),
      error = e$message,
      suggestions = c("⚠ File parsing failed - check file format")
    ))
  })
}