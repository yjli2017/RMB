# Metadata Generation Workflow Functions
# This file contains all the workflow functions for generating metadata

# Function to parse phenotype information from sample config
parse_phenotype_info <- function(phenotype_string) {
  # Parse phenotype string like "Male CS" or "Female Gaboxdol"
  parts <- strsplit(phenotype_string, " ")[[1]]
  
  if (length(parts) >= 2) {
    sex <- parts[1]
    treatment <- paste(parts[2:length(parts)], collapse = " ")
    
    # Map treatments to standard names
    if (treatment == "CS") {
      treatment <- "Control"
    } else if (treatment == "Gaboxdol") {
      treatment <- "Gaboxdol"
    } else if (treatment == "Caff") {
      treatment <- "Caffeine"
    }
    
    return(list(
      sex = sex,
      treatment = treatment,
      phenotype = phenotype_string
    ))
  } else {
    return(list(
      sex = "Unknown",
      treatment = "Unknown", 
      phenotype = phenotype_string
    ))
  }
}

# Function to process hardcoded phenotype assignments
process_hardcoded_assignments <- function(sample_config) {
  assignments <- list()
  
  for (i in 1:nrow(sample_config)) {
    monitor_num <- as.numeric(gsub("Monitor", "", sample_config$Monitor[i]))
    start_ch <- sample_config$Start_channel[i]
    end_ch <- sample_config$End_channel[i]
    phenotype <- sample_config$Phenotype[i]
    
    # Parse phenotype information
    phenotype_info <- parse_phenotype_info(phenotype)
    
    assignments[[i]] <- list(
      monitor = monitor_num,
      start_channel = start_ch,
      end_channel = end_ch,
      phenotype = phenotype_info$phenotype,
      treatment = phenotype_info$treatment,
      sex = phenotype_info$sex
    )
  }
  
  return(assignments)
}

# Function to process CSV file assignments
process_csv_assignments <- function(csv_file) {
  if (is.character(csv_file)) {
    config_data <- read.csv(csv_file, stringsAsFactors = FALSE)
  } else {
    config_data <- csv_file  # Already a data frame
  }
  
  assignments <- list()
  
  for (i in 1:nrow(config_data)) {
    monitor_num <- as.numeric(gsub("Monitor", "", config_data$Monitor_number[i]))
    
    assignment <- list(
      monitor = monitor_num,
      start_channel = config_data$Start_channel[i],
      end_channel = config_data$End_channel[i],
      group = config_data$Group[i],
      treatment = config_data$Treatment[i],
      genotype = config_data$Genotype[i]
    )
    
    # Add optional fields if they exist in the CSV
    if ("Temperature" %in% colnames(config_data)) {
      assignment$temperature <- config_data$Temperature[i]
    }
    if ("Incubator" %in% colnames(config_data)) {
      assignment$incubator <- config_data$Incubator[i]
    }
    if ("Monitor_type" %in% colnames(config_data)) {
      assignment$monitor_type <- config_data$Monitor_type[i]
    }
    if ("Sex" %in% colnames(config_data)) {
      assignment$sex <- config_data$Sex[i]
    }
    
    assignments[[i]] <- assignment
  }
  
  return(assignments)
}

# Main function to process a single experiment
process_experiment <- function(experiment_config, verbose = FALSE) {
  if (verbose) {
    cat("Processing experiment:", experiment_config$name, "\n")
    cat("Data directory:", experiment_config$data_dir, "\n")
  }
  
  # Load monitor files
  monitor_list <- load_monitor_files(experiment_config$data_dir)
  
  if (length(monitor_list) == 0) {
    cat("⚠️  No monitor files found in", experiment_config$data_dir, "\n")
    return(NULL)
  }
  
  if (verbose) {
    cat("Found", length(monitor_list), "monitor files\n")
  }
  
  # Create metadata list from template
  metadata_list <- create_metadata_list(monitor_list)
  
  # Update metadata with basic experiment info
  metadata_list <- update_metadata(
    monitor_list = monitor_list,
    metadata_list = metadata_list,
    Date = experiment_config$date,
    tube_type = experiment_config$tube_type,
    User = experiment_config$user,
    Experiment = experiment_config$name
  )
  
  # Process phenotype assignments
  if (!is.null(experiment_config$hardcoded_assignments)) {
    assignments <- process_hardcoded_assignments(experiment_config$hardcoded_assignments)
  } else if (!is.null(experiment_config$config_file)) {
    assignments <- process_csv_assignments(experiment_config$config_file)
  } else {
    cat("⚠️  No phenotype assignments provided\n")
    return(metadata_list)
  }
  
  # Apply phenotype assignments to metadata
  metadata_list <- apply_phenotype_assignments(metadata_list, assignments, verbose)
  
  # Write metadata files
  write_metadata(experiment_config$data_dir, monitor_list, metadata_list)
  
  if (verbose) {
    cat("✓ Metadata generation completed for", experiment_config$name, "\n")
  }
  
  return(metadata_list)
}

# Function to apply phenotype assignments to metadata
apply_phenotype_assignments <- function(metadata_list, assignments, verbose = FALSE) {
  
  for (assignment in assignments) {
    monitor_num <- assignment$monitor
    
    # Find the corresponding metadata entry
    for (i in seq_along(metadata_list)) {
      current_monitor <- as.numeric(gsub("Monitor", "", metadata_list[[i]]$Monitor_number[1]))
      
      if (current_monitor == monitor_num) {
        # Apply assignments to the specified channel range
        start_ch <- assignment$start_channel
        end_ch <- assignment$end_channel
        
        # Get the channel range for this monitor
        channel_range <- start_ch:end_ch
        
        # Apply group to all channels in range
        for (ch in channel_range) {
          if (ch <= nrow(metadata_list[[i]])) {
            metadata_list[[i]]$Group[ch] <- assignment$group
            metadata_list[[i]]$Treatment[ch] <- assignment$treatment
            
            # Add other fields if available
            if (!is.null(assignment$genotype)) {
              metadata_list[[i]]$Other[ch] <- assignment$genotype
            }
            if (!is.null(assignment$temperature)) {
              metadata_list[[i]]$Temperature[ch] <- assignment$temperature
            }
            if (!is.null(assignment$incubator)) {
              metadata_list[[i]]$Incubator[ch] <- assignment$incubator
            }
            if (!is.null(assignment$monitor_type)) {
              metadata_list[[i]]$Monitor_type[ch] <- assignment$monitor_type
            }
            if (!is.null(assignment$sex)) {
              metadata_list[[i]]$Sex[ch] <- assignment$sex
            }
          }
        }
        
        if (verbose) {
          cat("  ✓ Applied", assignment$group, "to Monitor", monitor_num, 
              "channels", start_ch, "-", end_ch, "\n")
        }
        break
      }
    }
  }
  
  return(metadata_list)
}

# Function to process multiple experiments
process_experiments <- function(experiments, verbose = FALSE) {
  results <- list()
  
  for (i in seq_along(experiments)) {
    experiment <- experiments[[i]]
    
    if (verbose) {
      cat("\n", paste(rep("=", 50), collapse = ""), "\n")
      cat("Processing experiment", i, "of", length(experiments), "\n")
      cat(paste(rep("=", 50), collapse = ""), "\n")
    }
    
    tryCatch({
      result <- process_experiment(experiment, verbose)
      results[[i]] <- result
      
      if (verbose && !is.null(result)) {
        cat("✅ Experiment", experiment$name, "completed successfully\n")
      }
    }, error = function(e) {
      cat("❌ Error processing experiment", experiment$name, ":", e$message, "\n")
      results[[i]] <- NULL
    })
  }
  
  return(results)
}