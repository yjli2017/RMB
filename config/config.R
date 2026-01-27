# RMB Configuration
# User-editable configuration settings

################################################################################
# Path Configuration
# Set base_dir to your RMB installation directory
# Set data_dir to where your experiment data is stored
################################################################################

# Auto-detect base directory (works if you source from within the project)
if (exists(".rmb_base_dir")) {
  base_dir <- .rmb_base_dir
} else if (interactive() && requireNamespace("rstudioapi", quietly = TRUE)) {
  # Try to get from RStudio if running interactively
  tryCatch({
    base_dir <- dirname(dirname(rstudioapi::getActiveDocumentContext()$path))
  }, error = function(e) {
    base_dir <- getwd()
  })
} else {
  # Default to current working directory
  base_dir <- getwd()
}

# Override with your path if auto-detection doesn't work
# base_dir <- "/path/to/your/RMB"

# Default data directory (can be overridden per-analysis)
data_dir <- file.path(base_dir, "test")

################################################################################
# Lab/User Information
# Used for metadata generation
################################################################################

LAB <- "Sehgal"
USER <- "yjli"

################################################################################
# Default Experimental Parameters
################################################################################

DEFAULT_TUBE_TYPE <- "Normal"
DEFAULT_INCUBATOR <- "MB"  
DEFAULT_TEMPERATURE <- 25

################################################################################
# Analysis Parameters
################################################################################

# Sleep definition: minimum consecutive minutes without movement
SLEEP_THRESHOLD <- 5

# Dead fly detection: minimum total movement to be considered alive
MIN_MOVEMENT_THRESHOLD <- 10

# Minutes per day (for DAM monitors recording every minute)
MINUTES_PER_DAY <- 1440

################################################################################
# Package Loading
# Core packages required for analysis
################################################################################

load_required_packages <- function(verbose = FALSE) {
  packages <- c("tidyverse", "ggplot2", "dplyr", "tidyr")
  
  for (pkg in packages) {
    if (!require(pkg, character.only = TRUE, quietly = !verbose)) {
      if (verbose) cat("Installing", pkg, "...\n")
      install.packages(pkg, quiet = TRUE)
      library(pkg, character.only = TRUE)
    }
  }
  
  # Bioconductor packages (optional, for heatmaps)
  bioc_packages <- c("ComplexHeatmap")
  for (pkg in bioc_packages) {
    if (!require(pkg, character.only = TRUE, quietly = TRUE)) {
      if (verbose) cat("Note:", pkg, "not installed. Install with BiocManager for heatmaps.\n")
    }
  }
}

################################################################################
# Helper Functions
################################################################################

#' Set the RMB base directory
#' @param path Path to RMB directory
#' @export
set_rmb_dir <- function(path) {
  if (!dir.exists(path)) {
    stop("Directory does not exist: ", path)
  }
  assign(".rmb_base_dir", path, envir = .GlobalEnv)
  setwd(path)
  cat("RMB directory set to:", path, "\n")
}

#' Source all RMB functions
#' @param base_dir Path to RMB directory (default: auto-detect)
#' @param verbose Print loading messages
#' @export
load_rmb <- function(base_dir = NULL, verbose = TRUE) {
  
  if (is.null(base_dir)) {
    if (exists(".rmb_base_dir", envir = .GlobalEnv)) {
      base_dir <- get(".rmb_base_dir", envir = .GlobalEnv)
    } else {
      base_dir <- getwd()
    }
  }
  
  src_dir <- file.path(base_dir, "src")
  
  if (!dir.exists(src_dir)) {
    stop("src directory not found in: ", base_dir)
  }
  
  # Load in order (constants first, then classes, then utilities)
  load_order <- c(
    "constants.R",
    "MonitorClass.R", 
    "transforms.R",
    "plotting.R",
    "metadata.R"
  )
  
  for (file in load_order) {
    file_path <- file.path(src_dir, file)
    if (file.exists(file_path)) {
      if (verbose) cat("Loading:", file, "\n")
      source(file_path)
    }
  }
  
  # Load any additional R files not in the load order
  all_files <- list.files(src_dir, pattern = "\\.R$", full.names = TRUE)
  loaded <- file.path(src_dir, load_order)
  
  for (file_path in setdiff(all_files, loaded)) {
    file_name <- basename(file_path)
    # Skip legacy/redundant files
    if (file_name %in% c("bar_plot.R", "plotting_utilities.R", 
                         "data_transformations.R", "convert_sequence.R",
                         "flip_binary.R", "df_to_freq_dist.R",
                         "process_monitorS4_data.R", "mergeMonitorS4.R",
                         "remove_dead_flies.R")) {
      if (verbose) cat("Skipping legacy file:", file_name, "\n")
      next
    }
    if (verbose) cat("Loading:", file_name, "\n")
    source(file_path)
  }
  
  if (verbose) cat("RMB loaded successfully from:", base_dir, "\n")
}
