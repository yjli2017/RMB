#' @title RMB Analysis Pipeline
#' @description Complete analysis pipelines for RMB data
#' @name pipeline

#' Run Complete Analysis Pipeline
#'
#' Runs a full analysis pipeline from raw data to final outputs
#'
#' @param data_dir Directory containing Monitor*.txt files
#' @param output_dir Directory for outputs (default: data_dir/output)
#' @param experiment_name Name of the experiment
#' @param min_movement Minimum total movement to keep fly (default 10)
#' @param sleep_threshold Minutes without movement to count as sleep (default 5)
#' @param make_plots Generate all plots (default TRUE)
#' @param make_checkpoints Save checkpoints at each step (default TRUE)
#' @param verbose Print progress messages (default TRUE)
#'
#' @return A list containing the processed monitor and summary statistics
#' @export
runPipeline <- function(data_dir,
                        output_dir = NULL,
                        experiment_name = "RMB_Analysis",
                        min_movement = 10,
                        sleep_threshold = 5,
                        make_plots = TRUE,
                        make_checkpoints = TRUE,
                        verbose = TRUE) {

  start_time <- Sys.time()

  if (is.null(output_dir)) {
    output_dir <- file.path(data_dir, "output")
  }
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

  # Initialize results
  results <- list(
    experiment_name = experiment_name,
    data_dir = data_dir,
    output_dir = output_dir,
    start_time = start_time
  )

  # ═══════════════════════════════════════════════════════════════════════════
  # STEP 1: Load Data
  # ═══════════════════════════════════════════════════════════════════════════
  if (verbose) {
    cat("\n")
    cat(strrep("═", 60), "\n")
    cat("STEP 1: Loading Data\n")
    cat(strrep("═", 60), "\n\n")
  }

  monitor_files <- list.files(data_dir, pattern = "Monitor.*\\.txt$", full.names = TRUE)

  if (length(monitor_files) == 0) {
    stop("No Monitor files found in: ", data_dir)
  }

  if (verbose) cat("Found", length(monitor_files), "monitor files\n")

  # Load and process each monitor
  monitor_list <- lapply(monitor_files, function(f) {
    if (verbose) cat("  Loading:", basename(f), "\n")
    m <- createMonitor(f, verbose = FALSE)
    m <- processMonitor(m, verbose = FALSE)
    return(m)
  })

  results$n_monitors <- length(monitor_list)

  # ═══════════════════════════════════════════════════════════════════════════
  # STEP 2: Load Metadata
  # ═══════════════════════════════════════════════════════════════════════════
  if (verbose) {
    cat("\n")
    cat(strrep("═", 60), "\n")
    cat("STEP 2: Loading Metadata\n")
    cat(strrep("═", 60), "\n\n")
  }

  metadata_dir <- file.path(data_dir, "metadata")
  if (dir.exists(metadata_dir)) {
    metadata_files <- list.files(metadata_dir, pattern = "metadata\\.csv$", full.names = TRUE)

    for (i in seq_along(metadata_files)) {
      if (i <= length(monitor_list)) {
        monitor_list[[i]]@meta.data <- read.csv(metadata_files[i], stringsAsFactors = FALSE)
        if (verbose) cat("  Loaded metadata for:", monitor_list[[i]]@monitor_name, "\n")
      }
    }
  } else {
    if (verbose) cat("  No metadata directory found\n")
  }

  # ═══════════════════════════════════════════════════════════════════════════
  # STEP 3: Merge Monitors
  # ═══════════════════════════════════════════════════════════════════════════
  if (verbose) {
    cat("\n")
    cat(strrep("═", 60), "\n")
    cat("STEP 3: Merging Monitors\n")
    cat(strrep("═", 60), "\n\n")
  }

  monitor <- mergeMonitors(monitor_list, verbose = verbose)

  # Checkpoint: after merge
  if (make_checkpoints) {
    checkpoint(monitor, "01_after_merge", output_dir, save_plots = FALSE, verbose = verbose)
  }

  # ═══════════════════════════════════════════════════════════════════════════
  # STEP 4: Quality Control - Remove Dead Flies
  # ═══════════════════════════════════════════════════════════════════════════
  if (verbose) {
    cat("\n")
    cat(strrep("═", 60), "\n")
    cat("STEP 4: Quality Control\n")
    cat(strrep("═", 60), "\n\n")
  }

  n_before <- ncol(monitor@assays$mt)
  monitor <- removeDeadFlies(monitor, min_movement = min_movement, verbose = verbose)
  n_after <- ncol(monitor@assays$mt)

  results$n_channels_before_qc <- n_before
  results$n_channels_after_qc <- n_after
  results$n_removed <- n_before - n_after

  # Checkpoint: after QC
  if (make_checkpoints) {
    checkpoint(monitor, "02_after_qc", output_dir, verbose = verbose)
  }

  # ═══════════════════════════════════════════════════════════════════════════
  # STEP 5: Sleep Analysis
  # ═══════════════════════════════════════════════════════════════════════════
  if (verbose) {
    cat("\n")
    cat(strrep("═", 60), "\n")
    cat("STEP 5: Sleep Analysis\n")
    cat(strrep("═", 60), "\n\n")
  }

  # Convert activity to awake/sleep
  if (verbose) cat("Calculating awake/sleep states...\n")
  monitor@assays$awake <- activityToAwake(monitor@assays$mt, threshold = sleep_threshold)
  monitor@assays$sleep <- awakeToSleep(monitor@assays$awake)

  # Position while awake
  if (!is.null(monitor@assays$pn)) {
    if (verbose) cat("Masking position by awake status...\n")
    monitor@assays$pn_awake <- maskByAwake(monitor@assays$pn, monitor@assays$awake)
  }

  # Calculate sleep statistics
  total_sleep <- colSums(monitor@assays$sleep, na.rm = TRUE)
  results$mean_total_sleep <- mean(total_sleep)
  results$sd_total_sleep <- sd(total_sleep)

  if (verbose) {
    cat("Sleep analysis complete\n")
    cat("  Mean sleep:", round(results$mean_total_sleep, 1), "minutes\n")
  }

  # Checkpoint: after sleep analysis
  if (make_checkpoints) {
    checkpoint(monitor, "03_after_sleep", output_dir, verbose = verbose)
  }

  # ═══════════════════════════════════════════════════════════════════════════
  # STEP 6: Generate Outputs
  # ═══════════════════════════════════════════════════════════════════════════
  if (verbose) {
    cat("\n")
    cat(strrep("═", 60), "\n")
    cat("STEP 6: Generating Outputs\n")
    cat(strrep("═", 60), "\n\n")
  }

  # Save processed data
  saveRDS(monitor, file.path(output_dir, "monitor_processed.rds"))
  if (verbose) cat("Saved: monitor_processed.rds\n")

  # Export CSV files
  if (verbose) cat("Exporting CSV files...\n")

  # Activity summary by fly
  activity_summary <- data.frame(
    fly = colnames(monitor@assays$mt),
    total_activity = colSums(monitor@assays$mt, na.rm = TRUE),
    total_sleep = colSums(monitor@assays$sleep, na.rm = TRUE),
    percent_sleep = colSums(monitor@assays$sleep, na.rm = TRUE) / nrow(monitor@assays$sleep) * 100
  )

  if (nrow(monitor@meta.data) > 0 && "Phenotype" %in% colnames(monitor@meta.data)) {
    activity_summary$phenotype <- monitor@meta.data$Phenotype
  }

  write.csv(activity_summary, file.path(output_dir, "activity_summary.csv"), row.names = FALSE)
  if (verbose) cat("  Saved: activity_summary.csv\n")

  # Position frequency
  if (!is.null(monitor@assays$pn_awake)) {
    pos_freq <- positionFrequency(monitor@assays$pn_awake)
    write.csv(pos_freq, file.path(output_dir, "position_frequency.csv"))
    if (verbose) cat("  Saved: position_frequency.csv\n")
  }

  # ═══════════════════════════════════════════════════════════════════════════
  # STEP 7: Generate Plots
  # ═══════════════════════════════════════════════════════════════════════════
  if (make_plots) {
    if (verbose) {
      cat("\n")
      cat(strrep("═", 60), "\n")
      cat("STEP 7: Generating Plots\n")
      cat(strrep("═", 60), "\n\n")
    }

    plots_dir <- file.path(output_dir, "plots")
    dir.create(plots_dir, recursive = TRUE, showWarnings = FALSE)

    # Activity raster
    if (verbose) cat("Creating activity raster plot...\n")
    p <- plotActivityRaster(monitor, channel = 1, title = paste(experiment_name, "- Activity Raster"))
    ggplot2::ggsave(file.path(plots_dir, "activity_raster.png"), p, width = 10, height = 8, dpi = 300)

    # Sleep profile
    if (verbose) cat("Creating sleep profile plot...\n")
    p <- plotSleepProfile(monitor, title = paste(experiment_name, "- Sleep Profile"))
    ggplot2::ggsave(file.path(plots_dir, "sleep_profile.png"), p, width = 10, height = 6, dpi = 300)

    # Group comparisons
    if (nrow(monitor@meta.data) > 0 && "Phenotype" %in% colnames(monitor@meta.data)) {
      if (verbose) cat("Creating group comparison plots...\n")

      p <- plotGroupComparison(monitor, metric = "total_activity",
                               title = paste(experiment_name, "- Total Activity"))
      ggplot2::ggsave(file.path(plots_dir, "total_activity_comparison.png"), p, width = 8, height = 6, dpi = 300)

      p <- plotGroupComparison(monitor, metric = "total_sleep",
                               title = paste(experiment_name, "- Total Sleep"))
      ggplot2::ggsave(file.path(plots_dir, "total_sleep_comparison.png"), p, width = 8, height = 6, dpi = 300)
    }

    # Summary figure
    if (requireNamespace("patchwork", quietly = TRUE)) {
      if (verbose) cat("Creating summary figure...\n")
      plotSummaryFigure(monitor, file.path(plots_dir, "summary_figure.png"))
    }

    if (verbose) cat("All plots saved to:", plots_dir, "\n")
  }

  # ═══════════════════════════════════════════════════════════════════════════
  # COMPLETE
  # ═══════════════════════════════════════════════════════════════════════════
  end_time <- Sys.time()
  elapsed <- difftime(end_time, start_time, units = "secs")

  results$end_time <- end_time
  results$elapsed_seconds <- as.numeric(elapsed)

  if (verbose) {
    cat("\n")
    cat(strrep("═", 60), "\n")
    cat("PIPELINE COMPLETE\n")
    cat(strrep("═", 60), "\n\n")
    cat("Time elapsed:", round(elapsed, 1), "seconds\n")
    cat("Output directory:", output_dir, "\n\n")
  }

  results$monitor <- monitor

  return(results)
}

#' Quick Analysis
#'
#' Simplified pipeline for quick exploratory analysis
#'
#' @param data_dir Directory containing Monitor*.txt files
#' @param verbose Print progress (default TRUE)
#'
#' @return A processed MonitorS4 object
#' @export
quickAnalysis <- function(data_dir, verbose = TRUE) {

  # Load
  monitor_files <- list.files(data_dir, pattern = "Monitor.*\\.txt$", full.names = TRUE)
  if (verbose) cat("Loading", length(monitor_files), "monitors...\n")

  monitor_list <- lapply(monitor_files, function(f) {
    m <- createMonitor(f, verbose = FALSE)
    m <- processMonitor(m, verbose = FALSE)
    return(m)
  })

  # Merge
  if (verbose) cat("Merging...\n")
  monitor <- mergeMonitors(monitor_list, verbose = FALSE)

  # QC
  if (verbose) cat("Removing dead flies...\n")
  monitor <- removeDeadFlies(monitor, verbose = FALSE)

  # Sleep
  if (verbose) cat("Calculating sleep...\n")
  monitor@assays$awake <- activityToAwake(monitor@assays$mt)
  monitor@assays$sleep <- awakeToSleep(monitor@assays$awake)

  if (verbose) cat("Done!\n")

  # Print summary
  summarizeMonitor(monitor)

  return(monitor)
}
