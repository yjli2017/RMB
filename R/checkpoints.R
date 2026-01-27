#' @title Checkpoint and Summary Functions
#' @description Functions for creating analysis checkpoints and summaries
#' @name checkpoints

#' Create Analysis Checkpoint
#'
#' Saves the current state of the analysis with a summary and optional plots
#'
#' @param monitor A MonitorS4 object
#' @param name Checkpoint name (e.g., "after_filtering", "sleep_analysis")
#' @param output_dir Directory to save checkpoint files
#' @param save_data Save the monitor object as RDS (default TRUE)
#' @param save_plots Generate and save diagnostic plots (default TRUE)
#' @param verbose Print progress messages (default TRUE)
#'
#' @return Invisibly returns the monitor object
#' @export
checkpoint <- function(monitor, name, output_dir = ".",
                       save_data = TRUE, save_plots = TRUE, verbose = TRUE) {

  # Create checkpoint directory
  checkpoint_dir <- file.path(output_dir, "checkpoints", name)
  dir.create(checkpoint_dir, recursive = TRUE, showWarnings = FALSE)

  if (verbose) {
    cat("\n", strrep("=", 60), "\n", sep = "")
    cat("CHECKPOINT:", name, "\n")
    cat(strrep("=", 60), "\n\n")
  }

  # Generate summary
  summary_text <- generateCheckpointSummary(monitor, name)

  # Print summary
  if (verbose) cat(summary_text)

  # Save summary to file
  writeLines(summary_text, file.path(checkpoint_dir, "summary.txt"))

  # Save data
  if (save_data) {
    saveRDS(monitor, file.path(checkpoint_dir, "monitor.rds"))
    if (verbose) cat("\nSaved: monitor.rds\n")
  }

  # Generate plots
  if (save_plots) {
    generateCheckpointPlots(monitor, checkpoint_dir, verbose)
  }

  if (verbose) {
    cat("\nCheckpoint saved to:", checkpoint_dir, "\n")
    cat(strrep("=", 60), "\n\n")
  }

  invisible(monitor)
}

#' Generate Checkpoint Summary
#'
#' @param monitor A MonitorS4 object
#' @param name Checkpoint name
#'
#' @return Character string with summary
#' @keywords internal
generateCheckpointSummary <- function(monitor, name) {

  lines <- c()
  lines <- c(lines, paste("Checkpoint:", name))
  lines <- c(lines, paste("Timestamp:", Sys.time()))
  lines <- c(lines, "")
  lines <- c(lines, "--- Data Summary ---")

  # Basic dimensions
  if (!is.null(monitor@assays$mt)) {
    n_timepoints <- nrow(monitor@assays$mt)
    n_channels <- ncol(monitor@assays$mt)
    n_days <- round(n_timepoints / 1440, 1)

    lines <- c(lines, paste("Timepoints:", n_timepoints, sprintf("(%.1f days)", n_days)))
    lines <- c(lines, paste("Channels:", n_channels))
  }

  # List available assays
  assay_names <- names(monitor@assays)
  assay_names <- assay_names[!sapply(monitor@assays, is.null)]
  lines <- c(lines, paste("Assays:", paste(assay_names, collapse = ", ")))

  # Metadata summary
  if (nrow(monitor@meta.data) > 0) {
    lines <- c(lines, "")
    lines <- c(lines, "--- Metadata Summary ---")
    lines <- c(lines, paste("Metadata rows:", nrow(monitor@meta.data)))

    if ("Phenotype" %in% colnames(monitor@meta.data)) {
      phenotypes <- table(monitor@meta.data$Phenotype)
      lines <- c(lines, "Phenotype distribution:")
      for (p in names(phenotypes)) {
        lines <- c(lines, paste("  ", p, ":", phenotypes[p]))
      }
    }
  }

  # Activity summary
  if (!is.null(monitor@assays$mt)) {
    lines <- c(lines, "")
    lines <- c(lines, "--- Activity Summary ---")
    total_activity <- colSums(monitor@assays$mt, na.rm = TRUE)
    lines <- c(lines, paste("Mean total activity:", round(mean(total_activity), 1)))
    lines <- c(lines, paste("Range:", round(min(total_activity), 1), "-", round(max(total_activity), 1)))
  }

  # Sleep summary (if available)
  if (!is.null(monitor@assays$sleep)) {
    lines <- c(lines, "")
    lines <- c(lines, "--- Sleep Summary ---")
    total_sleep <- colSums(monitor@assays$sleep, na.rm = TRUE)
    lines <- c(lines, paste("Mean total sleep (min):", round(mean(total_sleep), 1)))
    lines <- c(lines, paste("Range:", round(min(total_sleep), 1), "-", round(max(total_sleep), 1)))

    # Calculate percent sleep
    percent_sleep <- total_sleep / nrow(monitor@assays$sleep) * 100
    lines <- c(lines, paste("Mean % time asleep:", round(mean(percent_sleep), 1), "%"))
  }

  paste(lines, collapse = "\n")
}

#' Generate Checkpoint Plots
#'
#' @param monitor A MonitorS4 object
#' @param output_dir Directory for plots
#' @param verbose Print progress
#'
#' @keywords internal
generateCheckpointPlots <- function(monitor, output_dir, verbose = TRUE) {

  # Activity heatmap (simple version without ComplexHeatmap dependency)
  if (!is.null(monitor@assays$mt)) {
    if (verbose) cat("Generating activity heatmap...")

    # Sample subset for visualization
    n_ch <- min(30, ncol(monitor@assays$mt))
    n_tp <- min(2880, nrow(monitor@assays$mt))  # Max 2 days

    sample_data <- monitor@assays$mt[1:n_tp, 1:n_ch]

    # Create simple heatmap with ggplot
    df <- expand.grid(
      time = 1:n_tp,
      channel = 1:n_ch
    )
    df$activity <- as.vector(as.matrix(sample_data))

    p <- ggplot2::ggplot(df, ggplot2::aes(x = time, y = channel, fill = activity)) +
      ggplot2::geom_tile() +
      ggplot2::scale_fill_gradient(low = "white", high = "#2E86AB", na.value = "gray90") +
      ggplot2::labs(title = "Activity Heatmap (sample)", x = "Time", y = "Channel") +
      theme_rmb(grid = "none")

    ggplot2::ggsave(file.path(output_dir, "activity_heatmap.png"), p,
                    width = 10, height = 6, dpi = 150)
    if (verbose) cat(" done\n")
  }

  # Activity distribution
  if (!is.null(monitor@assays$mt)) {
    if (verbose) cat("Generating activity distribution...")

    total_activity <- colSums(monitor@assays$mt, na.rm = TRUE)
    df <- data.frame(activity = total_activity)

    p <- ggplot2::ggplot(df, ggplot2::aes(x = activity)) +
      ggplot2::geom_histogram(bins = 30, fill = "#2E86AB", color = "white", alpha = 0.8) +
      ggplot2::labs(title = "Total Activity Distribution", x = "Total Activity", y = "Count") +
      theme_rmb()

    ggplot2::ggsave(file.path(output_dir, "activity_distribution.png"), p,
                    width = 8, height = 5, dpi = 150)
    if (verbose) cat(" done\n")
  }

  # Sleep distribution (if available)
  if (!is.null(monitor@assays$sleep)) {
    if (verbose) cat("Generating sleep distribution...")

    total_sleep <- colSums(monitor@assays$sleep, na.rm = TRUE)
    df <- data.frame(sleep = total_sleep)

    p <- ggplot2::ggplot(df, ggplot2::aes(x = sleep)) +
      ggplot2::geom_histogram(bins = 30, fill = "#E94560", color = "white", alpha = 0.8) +
      ggplot2::labs(title = "Total Sleep Distribution", x = "Total Sleep (minutes)", y = "Count") +
      theme_rmb()

    ggplot2::ggsave(file.path(output_dir, "sleep_distribution.png"), p,
                    width = 8, height = 5, dpi = 150)
    if (verbose) cat(" done\n")
  }
}

#' Summarize MonitorS4 Object
#'
#' Prints a comprehensive summary of a MonitorS4 object
#'
#' @param monitor A MonitorS4 object
#' @param by_group Summarize by group if metadata available (default TRUE)
#'
#' @return Invisibly returns summary statistics as a list
#' @export
summarizeMonitor <- function(monitor, by_group = TRUE) {

  cat("\n")
  cat(strrep("=", 50), "\n")
  cat("MonitorS4 Summary\n")
  cat(strrep("=", 50), "\n\n")

  results <- list()

  # Basic info
  cat("Name:", monitor@monitor_name, "\n")
  results$name <- monitor@monitor_name

  if (!is.null(monitor@assays$mt)) {
    n_tp <- nrow(monitor@assays$mt)
    n_ch <- ncol(monitor@assays$mt)
    n_days <- n_tp / 1440

    cat("Dimensions:", n_tp, "timepoints x", n_ch, "channels\n")
    cat("Duration:", round(n_days, 1), "days\n")

    results$n_timepoints <- n_tp
    results$n_channels <- n_ch
    results$n_days <- n_days
  }

  # Assays
  cat("\nAssays available:\n")
  for (name in names(monitor@assays)) {
    if (!is.null(monitor@assays[[name]])) {
      dims <- dim(monitor@assays[[name]])
      cat("  ", name, ": ", dims[1], " x ", dims[2], "\n", sep = "")
    }
  }

  # Group summaries
  if (by_group && nrow(monitor@meta.data) > 0 && "Phenotype" %in% colnames(monitor@meta.data)) {

    cat("\nBy Phenotype:\n")
    cat(strrep("-", 40), "\n")

    groups <- unique(monitor@meta.data$Phenotype)
    group_stats <- list()

    for (g in groups) {
      idx <- which(monitor@meta.data$Phenotype == g)

      cat("\n", g, " (n=", length(idx), ")\n", sep = "")

      # Activity
      if (!is.null(monitor@assays$mt)) {
        act <- colSums(monitor@assays$mt[, idx, drop = FALSE], na.rm = TRUE)
        cat("  Activity: ", round(mean(act), 1), " ± ", round(sd(act), 1), "\n", sep = "")
        group_stats[[g]]$activity_mean <- mean(act)
        group_stats[[g]]$activity_sd <- sd(act)
      }

      # Sleep
      if (!is.null(monitor@assays$sleep)) {
        slp <- colSums(monitor@assays$sleep[, idx, drop = FALSE], na.rm = TRUE)
        cat("  Sleep (min): ", round(mean(slp), 1), " ± ", round(sd(slp), 1), "\n", sep = "")
        group_stats[[g]]$sleep_mean <- mean(slp)
        group_stats[[g]]$sleep_sd <- sd(slp)
      }
    }

    results$by_group <- group_stats
  }

  cat("\n")

  invisible(results)
}

#' Generate Analysis Report
#'
#' Creates a comprehensive HTML or PDF report of the analysis
#'
#' @param monitor A MonitorS4 object
#' @param output_file Output file path (extension determines format)
#' @param title Report title
#' @param author Report author
#' @param include_methods Include methods description (default TRUE)
#'
#' @return Path to generated report
#' @export
generateReport <- function(monitor, output_file = "RMB_report.html",
                           title = "RMB Analysis Report",
                           author = "",
                           include_methods = TRUE) {

  # For now, generate a simple text report
  # Full Rmarkdown report would require rmarkdown/knitr

  output_dir <- dirname(output_file)
  if (output_dir == ".") output_dir <- getwd()

  report_lines <- c(
    paste("#", title),
    "",
    if (author != "") paste("Author:", author) else NULL,
    paste("Generated:", Sys.time()),
    "",
    "## Summary",
    "",
    capture.output(summarizeMonitor(monitor)),
    "",
    "## Checkpoints",
    "",
    "Analysis checkpoints saved in: checkpoints/",
    ""
  )

  # Write report
  writeLines(report_lines, output_file)

  message("Report saved to: ", output_file)
  return(output_file)
}
