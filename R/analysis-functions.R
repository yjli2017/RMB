#' Analysis Functions for RMB Package
#'
#' @description Functions for analyzing Drosophila behavioral data

#' Calculate sleep analysis from activity data
#'
#' @param object MonitorS4 object
#' @param sleep_threshold Minimum minutes of inactivity to be considered sleep (default: 5)
#' @param bin_minutes Bin size in minutes for sleep analysis (default: 30)
#' @return MonitorS4 object with sleep assay added
#' @export
calculate_sleep <- function(object, sleep_threshold = 5, bin_minutes = 30) {
  if (!is(object, "MonitorS4")) {
    stop("Object must be of class MonitorS4")
  }
  
  if (!"mt" %in% names(object@assays)) {
    stop("Motor activity (mt) assay required for sleep analysis")
  }
  
  mt_data <- object@assays$mt
  channel_cols <- grep("^Channel_", colnames(mt_data), value = TRUE)
  
  # Convert to long format for processing
  long_data <- wide_to_long(mt_data, "activity")
  
  # Calculate sleep for each channel
  sleep_data <- long_data %>%
    dplyr::arrange(Channel, DateTime) %>%
    dplyr::group_by(Channel) %>%
    dplyr::mutate(
      # Mark inactive periods (activity = 0)
      inactive = as.numeric(activity == 0),
      # Calculate cumulative inactive periods
      inactive_run = cumsum(c(0, diff(inactive) != 0)),
      # Calculate run lengths for inactive periods
      run_length = ave(inactive, inactive_run, FUN = length),
      # Mark sleep (inactive periods >= threshold)
      sleep = ifelse(inactive == 1 & run_length >= sleep_threshold, 1, 0)
    ) %>%
    dplyr::ungroup()
  
  # Bin sleep data
  sleep_data <- sleep_data %>%
    dplyr::mutate(
      time_bin = lubridate::floor_date(DateTime, paste(bin_minutes, "minutes"))
    ) %>%
    dplyr::group_by(Channel, time_bin) %>%
    dplyr::summarise(
      sleep_minutes = sum(sleep, na.rm = TRUE),
      activity_sum = sum(activity, na.rm = TRUE),
      .groups = "drop"
    )
  
  # Convert back to wide format
  sleep_wide <- sleep_data %>%
    dplyr::select(time_bin, Channel, sleep_minutes) %>%
    tidyr::pivot_wider(
      names_from = Channel,
      values_from = sleep_minutes,
      names_prefix = "Channel_"
    )
  
  # Add metadata columns
  sleep_wide$DateTime <- sleep_wide$time_bin
  sleep_wide$Monitor_Name <- mt_data$Monitor_Name[1]
  sleep_wide$Measure_Type <- "Sleep"
  
  # Reorder columns to match original format
  meta_cols <- c("DateTime", "Monitor_Name", "Measure_Type")
  channel_cols_sleep <- grep("^Channel_", colnames(sleep_wide), value = TRUE)
  sleep_wide <- sleep_wide[, c(meta_cols, channel_cols_sleep)]
  
  # Add to assays
  object@assays$sleep <- sleep_wide
  
  cat("Sleep analysis complete\n")
  cat("Sleep threshold:", sleep_threshold, "minutes\n")
  cat("Bin size:", bin_minutes, "minutes\n")
  
  return(object)
}

#' Calculate activity patterns and statistics
#'
#' @param object MonitorS4 object
#' @param bin_hours Bin size in hours for activity analysis (default: 1)
#' @return List with activity statistics and patterns
#' @export
analyze_activity <- function(object, bin_hours = 1) {
  if (!is(object, "MonitorS4")) {
    stop("Object must be of class MonitorS4")
  }
  
  if (!"mt" %in% names(object@assays)) {
    stop("Motor activity (mt) assay required for activity analysis")
  }
  
  mt_data <- object@assays$mt
  long_data <- wide_to_long(mt_data, "activity")
  
  # Add time variables
  long_data <- long_data %>%
    dplyr::mutate(
      hour = lubridate::hour(DateTime),
      date = as.Date(DateTime),
      time_bin = lubridate::floor_date(DateTime, paste(bin_hours, "hours"))
    )
  
  # Calculate hourly patterns
  hourly_patterns <- long_data %>%
    dplyr::group_by(Channel, hour) %>%
    dplyr::summarise(
      mean_activity = mean(activity, na.rm = TRUE),
      median_activity = median(activity, na.rm = TRUE),
      total_activity = sum(activity, na.rm = TRUE),
      .groups = "drop"
    )
  
  # Calculate daily totals
  daily_totals <- long_data %>%
    dplyr::group_by(Channel, date) %>%
    dplyr::summarise(
      total_activity = sum(activity, na.rm = TRUE),
      mean_activity = mean(activity, na.rm = TRUE),
      peak_activity = max(activity, na.rm = TRUE),
      .groups = "drop"
    )
  
  # Calculate overall statistics
  overall_stats <- long_data %>%
    dplyr::group_by(Channel) %>%
    dplyr::summarise(
      total_activity = sum(activity, na.rm = TRUE),
      mean_activity = mean(activity, na.rm = TRUE),
      median_activity = median(activity, na.rm = TRUE),
      sd_activity = sd(activity, na.rm = TRUE),
      peak_activity = max(activity, na.rm = TRUE),
      active_periods = sum(activity > 0, na.rm = TRUE),
      inactive_periods = sum(activity == 0, na.rm = TRUE),
      .groups = "drop"
    )
  
  # Merge with metadata if available
  if (nrow(object@meta.data) > 0) {
    if ("Channel" %in% colnames(object@meta.data)) {
      overall_stats <- dplyr::left_join(overall_stats, object@meta.data, by = "Channel")
    }
  }
  
  results <- list(
    hourly_patterns = hourly_patterns,
    daily_totals = daily_totals,
    overall_stats = overall_stats,
    analysis_params = list(bin_hours = bin_hours, analysis_date = Sys.time())
  )
  
  return(results)
}

#' Calculate circadian rhythm parameters
#'
#' @param object MonitorS4 object
#' @param method Method for circadian analysis ("cosinor" or "chi_square")
#' @param period_hours Expected period in hours (default: 24)
#' @return List with circadian analysis results
#' @export
analyze_circadian <- function(object, method = "cosinor", period_hours = 24) {
  if (!is(object, "MonitorS4")) {
    stop("Object must be of class MonitorS4")
  }
  
  if (!"mt" %in% names(object@assays)) {
    stop("Motor activity (mt) assay required for circadian analysis")
  }
  
  mt_data <- object@assays$mt
  long_data <- wide_to_long(mt_data, "activity")
  
  # Add time variables for circadian analysis
  long_data <- long_data %>%
    dplyr::mutate(
      time_numeric = as.numeric(DateTime - min(DateTime, na.rm = TRUE)) / 3600,  # hours from start
      hour_of_day = lubridate::hour(DateTime) + lubridate::minute(DateTime)/60
    )
  
  # Bin data by hours
  binned_data <- long_data %>%
    dplyr::mutate(
      hour_bin = floor(hour_of_day)
    ) %>%
    dplyr::group_by(Channel, hour_bin) %>%
    dplyr::summarise(
      mean_activity = mean(activity, na.rm = TRUE),
      .groups = "drop"
    )
  
  if (method == "cosinor") {
    # Simple cosinor analysis
    circadian_results <- binned_data %>%
      dplyr::group_by(Channel) %>%
      dplyr::summarise(
        # Calculate rhythm strength as coefficient of variation
        rhythm_strength = sd(mean_activity, na.rm = TRUE) / mean(mean_activity, na.rm = TRUE),
        peak_hour = hour_bin[which.max(mean_activity)],
        trough_hour = hour_bin[which.min(mean_activity)],
        amplitude = max(mean_activity, na.rm = TRUE) - min(mean_activity, na.rm = TRUE),
        mesor = mean(mean_activity, na.rm = TRUE),  # Average activity level
        .groups = "drop"
      )
    
  } else if (method == "chi_square") {
    # Chi-square periodogram analysis (simplified)
    circadian_results <- binned_data %>%
      dplyr::group_by(Channel) %>%
      dplyr::summarise(
        # Calculate variability across hours as a rhythm measure
        chi_square_stat = sum((mean_activity - mean(mean_activity, na.rm = TRUE))^2, na.rm = TRUE),
        peak_hour = hour_bin[which.max(mean_activity)],
        trough_hour = hour_bin[which.min(mean_activity)],
        amplitude = max(mean_activity, na.rm = TRUE) - min(mean_activity, na.rm = TRUE),
        mesor = mean(mean_activity, na.rm = TRUE),
        .groups = "drop"
      )
  }
  
  # Add phase information
  circadian_results <- circadian_results %>%
    dplyr::mutate(
      phase_difference = (peak_hour - 12) %% 24,  # Hours from noon
      rhythm_classification = dplyr::case_when(
        peak_hour >= 6 & peak_hour <= 18 ~ "Diurnal",
        peak_hour >= 18 | peak_hour <= 6 ~ "Nocturnal",
        TRUE ~ "Arrhythmic"
      )
    )
  
  # Merge with metadata if available
  if (nrow(object@meta.data) > 0 && "Channel" %in% colnames(object@meta.data)) {
    circadian_results <- dplyr::left_join(circadian_results, object@meta.data, by = "Channel")
  }
  
  results <- list(
    circadian_parameters = circadian_results,
    hourly_averages = binned_data,
    analysis_params = list(
      method = method,
      period_hours = period_hours,
      analysis_date = Sys.time()
    )
  )
  
  return(results)
}

#' Analyze position and movement patterns
#'
#' @param object MonitorS4 object
#' @return List with position analysis results
#' @export
analyze_position <- function(object) {
  if (!is(object, "MonitorS4")) {
    stop("Object must be of class MonitorS4")
  }
  
  if (!"pn" %in% names(object@assays)) {
    stop("Position (pn) assay required for position analysis")
  }
  
  pn_data <- object@assays$pn
  long_data <- wide_to_long(pn_data, "position")
  
  # Calculate position statistics
  position_stats <- long_data %>%
    dplyr::group_by(Channel) %>%
    dplyr::summarise(
      mean_position = mean(position, na.rm = TRUE),
      median_position = median(position, na.rm = TRUE),
      sd_position = sd(position, na.rm = TRUE),
      min_position = min(position, na.rm = TRUE),
      max_position = max(position, na.rm = TRUE),
      position_range = max_position - min_position,
      # Calculate movement (position changes)
      total_movement = sum(abs(diff(position)), na.rm = TRUE),
      .groups = "drop"
    )
  
  # Calculate position preferences (binned positions)
  position_preferences <- long_data %>%
    dplyr::mutate(
      position_bin = cut(position, breaks = 5, labels = c("Bottom", "Low", "Middle", "High", "Top"))
    ) %>%
    dplyr::group_by(Channel, position_bin) %>%
    dplyr::summarise(
      time_spent = dplyr::n(),
      .groups = "drop"
    ) %>%
    dplyr::group_by(Channel) %>%
    dplyr::mutate(
      percentage = time_spent / sum(time_spent) * 100
    )
  
  # Merge with metadata if available
  if (nrow(object@meta.data) > 0 && "Channel" %in% colnames(object@meta.data)) {
    position_stats <- dplyr::left_join(position_stats, object@meta.data, by = "Channel")
  }
  
  results <- list(
    position_stats = position_stats,
    position_preferences = position_preferences,
    analysis_params = list(analysis_date = Sys.time())
  )
  
  return(results)
}

#' Generate comprehensive analysis summary
#'
#' @param object MonitorS4 object
#' @param include_sleep Logical, include sleep analysis (default: TRUE)
#' @param include_circadian Logical, include circadian analysis (default: TRUE)
#' @param include_position Logical, include position analysis (default: TRUE)
#' @return List with all analysis results
#' @export
comprehensive_analysis <- function(object, 
                                 include_sleep = TRUE,
                                 include_circadian = TRUE,
                                 include_position = TRUE) {
  if (!is(object, "MonitorS4")) {
    stop("Object must be of class MonitorS4")
  }
  
  cat("Running comprehensive analysis...\n")
  results <- list()
  
  # Activity analysis (always included)
  cat("- Activity analysis\n")
  results$activity <- analyze_activity(object)
  
  # Sleep analysis
  if (include_sleep && "mt" %in% names(object@assays)) {
    cat("- Sleep analysis\n")
    object_with_sleep <- calculate_sleep(object)
    results$sleep <- analyze_activity(object_with_sleep)  # Analyze sleep patterns
  }
  
  # Circadian analysis
  if (include_circadian && "mt" %in% names(object@assays)) {
    cat("- Circadian analysis\n")
    results$circadian <- analyze_circadian(object)
  }
  
  # Position analysis
  if (include_position && "pn" %in% names(object@assays)) {
    cat("- Position analysis\n")
    results$position <- analyze_position(object)
  }
  
  # Add summary statistics
  results$summary <- list(
    total_flies = nrow(object@meta.data),
    available_assays = names(object@assays),
    time_range = paste(format(object@time$start_time), "to", format(object@time$end_time)),
    duration_hours = object@time$duration_hours,
    analysis_date = Sys.time()
  )
  
  cat("Comprehensive analysis complete!\n")
  return(results)
}

#' Plot average group activity over time with binning
#'
#' @description Creates activity plots showing average activity patterns for groups 
#' over the analysis period. Uses metadata grouping variables and time binning.
#'
#' @param object MonitorS4 object with only alive flies (filtered dataset)
#' @param group_by Metadata column for grouping (e.g., "Sex", "Treatment", "Genotype")
#' @param bin_minutes Time bin size in minutes (default: 30)
#' @param plot_type Type of plot: "line", "ribbon", or "both" (default: "both")
#' @param save_plot Logical, whether to save plot (default: FALSE)
#' @param output_path Path for saving plot
#' @return ggplot2 object
#' @export
plot_group_activity_timecourse <- function(object, 
                                          group_by = NULL, 
                                          bin_minutes = 30,
                                          plot_type = "both",
                                          save_plot = FALSE,
                                          output_path = NULL) {
  
  if (!is(object, "MonitorS4")) {
    stop("Object must be of class MonitorS4")
  }
  
  if (!"mt" %in% names(object@assays)) {
    stop("Motor activity (mt) assay required")
  }
  
  # Check if dataset is filtered to alive flies
  if ("included_in_analysis" %in% colnames(object@meta.data)) {
    if (!all(object@meta.data$included_in_analysis)) {
      warning("Dataset appears to contain excluded flies. Consider using filter_to_alive_flies() first.")
    }
  }
  
  mt_data <- object@assays$mt
  metadata <- object@meta.data
  
  # Get channel columns and check they match metadata
  channel_cols <- grep("^Channel_", colnames(mt_data), value = TRUE)
  
  cat("Processing activity data:\n")
  cat("- Time points:", nrow(mt_data), "\n")
  cat("- Channels:", length(channel_cols), "\n")
  cat("- Bin size:", bin_minutes, "minutes\n")
  
  # Convert to long format
  long_data <- wide_to_long(mt_data, "activity")
  
  # Add metadata information
  long_data <- long_data %>%
    dplyr::left_join(metadata, by = "Channel")
  
  # Create time bins
  long_data <- long_data %>%
    dplyr::mutate(
      time_bin = lubridate::floor_date(DateTime, paste(bin_minutes, "minutes")),
      date = as.Date(DateTime),
      hour = lubridate::hour(DateTime) + lubridate::minute(DateTime) / 60
    )
  
  # Aggregate by bins and groups
  if (!is.null(group_by) && group_by %in% colnames(metadata)) {
    cat("- Grouping by:", group_by, "\n")
    
    # Check available groups
    groups <- unique(metadata[[group_by]])
    groups <- groups[!is.na(groups)]
    cat("- Groups found:", paste(groups, collapse = ", "), "\n")
    
    # Calculate group averages
    plot_data <- long_data %>%
      dplyr::group_by(time_bin, !!rlang::sym(group_by)) %>%
      dplyr::summarise(
        mean_activity = mean(activity, na.rm = TRUE),
        se_activity = sd(activity, na.rm = TRUE) / sqrt(dplyr::n()),
        n_flies = dplyr::n_distinct(Channel),
        date = first(date),
        hour = first(hour),
        .groups = "drop"
      ) %>%
      dplyr::filter(!is.na(!!rlang::sym(group_by)))
    
    group_label <- group_by
    
  } else {
    cat("- No grouping specified, using all flies\n")
    
    # Calculate overall averages
    plot_data <- long_data %>%
      dplyr::group_by(time_bin) %>%
      dplyr::summarise(
        mean_activity = mean(activity, na.rm = TRUE),
        se_activity = sd(activity, na.rm = TRUE) / sqrt(dplyr::n()),
        n_flies = dplyr::n_distinct(Channel),
        date = first(date),
        hour = first(hour),
        .groups = "drop"
      ) %>%
      dplyr::mutate(Group = "All Flies")
    
    group_by <- "Group"
    group_label <- "All Flies"
  }
  
  cat("- Data points in plot:", nrow(plot_data), "\n")
  
  # Get analysis period info for title
  analysis_info <- ""
  if (!is.null(object@time$analysis_period)) {
    analysis_info <- paste(object@time$analysis_period$total_days, "days:",
                          object@time$analysis_period$start_date, "to", 
                          object@time$analysis_period$end_date)
  }
  
  # Create base plot
  p <- ggplot2::ggplot(plot_data, ggplot2::aes(x = time_bin, y = mean_activity, color = !!rlang::sym(group_by)))
  
  # Add ribbon for error bars if requested
  if (plot_type %in% c("ribbon", "both")) {
    p <- p + ggplot2::geom_ribbon(ggplot2::aes(ymin = mean_activity - se_activity, 
                                              ymax = mean_activity + se_activity,
                                              fill = !!rlang::sym(group_by)), 
                                 alpha = 0.3, color = NA)
  }
  
  # Add line
  if (plot_type %in% c("line", "both")) {
    p <- p + ggplot2::geom_line(linewidth = 0.8)
  }
  
  # Formatting
  monitor_name <- unique(object@meta.data$Monitor_number)[1]
  alive_flies <- sum(object@meta.data$included_in_analysis %||% rep(TRUE, nrow(object@meta.data)))
  
  p <- p +
    ggplot2::scale_x_datetime(
      date_breaks = "6 hours",
      date_labels = "%m/%d\n%H:%M"
    ) +
    ggplot2::labs(
      title = paste("Activity Timecourse -", monitor_name),
      subtitle = paste("Average activity by", group_label, "| Bin size:", bin_minutes, "min |", 
                      alive_flies, "flies |", analysis_info),
      x = "Time",
      y = paste("Mean Activity per", bin_minutes, "min"),
      color = group_label,
      fill = group_label,
      caption = paste("Error bands show ± SEM |", "Generated:", Sys.Date())
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      plot.title = ggplot2::element_text(size = 16, face = "bold"),
      plot.subtitle = ggplot2::element_text(size = 12, color = "gray50"),
      axis.text.x = ggplot2::element_text(angle = 45, hjust = 1),
      legend.position = "bottom",
      panel.grid.minor = ggplot2::element_blank()
    )
  
  # Add day/night shading if we have light schedule info
  # Add vertical lines for day boundaries
  if (!is.null(object@time$analysis_period)) {
    # Add day separators
    day_starts <- seq(min(plot_data$time_bin), max(plot_data$time_bin), by = "1 day")
    day_starts <- day_starts[day_starts <= max(plot_data$time_bin)]
    
    if (length(day_starts) > 1) {
      p <- p + ggplot2::geom_vline(xintercept = day_starts, 
                                  linetype = "dashed", alpha = 0.5, color = "gray60")
    }
  }
  
  # Save plot if requested
  if (save_plot) {
    if (is.null(output_path)) {
      output_path <- paste0("activity_timecourse_", monitor_name, "_", 
                           gsub("[^A-Za-z0-9]", "_", group_label), ".png")
    }
    
    ggplot2::ggsave(output_path, p, width = 14, height = 8, dpi = 300, bg = "white")
    cat("Plot saved to:", output_path, "\n")
  }
  
  return(p)
}