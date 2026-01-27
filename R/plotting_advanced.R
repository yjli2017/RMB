#' @title Advanced Plotting Functions
#' @description Publication-quality visualization functions for RMB data
#' @name plotting_advanced

#' Activity Raster Plot
#'
#' Creates a raster/actogram-style plot showing activity over multiple days
#'
#' @param monitor A MonitorS4 object or data frame of activity data
#' @param channel Channel number or column name to plot (default 1)
#' @param title Plot title
#' @param minutes_per_day Minutes per day (default 1440)
#' @param double_plot Double-plot actogram format (default FALSE)
#' @param color_low Color for low activity (default "white")
#' @param color_high Color for high activity (default "#2E86AB")
#'
#' @return A ggplot2 object
#' @export
plotActivityRaster <- function(monitor, channel = 1, title = "Activity Raster",
                                minutes_per_day = 1440, double_plot = FALSE,
                                color_low = "white", color_high = "#2E86AB") {

  # Extract data
  if (isS4(monitor)) {
    if (is.null(monitor@assays$mt)) stop("No movement data in monitor")
    data <- monitor@assays$mt[, channel]
  } else if (is.data.frame(monitor)) {
    data <- monitor[, channel]
  } else {
    data <- as.numeric(monitor)
  }

  # Calculate days and reshape
  n_points <- length(data)
  n_days <- ceiling(n_points / minutes_per_day)

  # Pad data to complete days
  padded <- c(data, rep(NA, n_days * minutes_per_day - n_points))

  # Create matrix (rows = days, cols = time of day)
  mat <- matrix(padded, nrow = n_days, ncol = minutes_per_day, byrow = TRUE)

  # Convert to long format for ggplot
  df <- expand.grid(
    day = 1:n_days,
    minute = 1:minutes_per_day
  )
  df$activity <- as.vector(t(mat))

  # Double-plot format
  if (double_plot) {
    df2 <- df
    df2$minute <- df2$minute + minutes_per_day
    df <- rbind(df, df2)
    x_max <- minutes_per_day * 2
  } else {
    x_max <- minutes_per_day
  }

  # Create plot
  p <- ggplot2::ggplot(df, ggplot2::aes(x = minute, y = day, fill = activity)) +
    ggplot2::geom_tile() +
    ggplot2::scale_fill_gradient(
      low = color_low,
      high = color_high,
      na.value = "gray90",
      name = "Activity"
    ) +
    ggplot2::scale_y_reverse() +
    ggplot2::scale_x_continuous(
      breaks = seq(0, x_max, by = 360),
      labels = function(x) paste0("ZT", (x / 60) %% 24)
    ) +
    ggplot2::labs(
      title = title,
      x = "Time (ZT hours)",
      y = "Day"
    ) +
    theme_rmb(grid = "none") +
    ggplot2::theme(
      panel.border = ggplot2::element_blank(),
      axis.ticks = ggplot2::element_blank()
    )

  # Add light/dark annotations
  p <- p + annotate_light_dark(n_days = n_days, minutes_per_day = minutes_per_day)

  return(p)
}

#' Sleep Profile Plot
#'
#' Creates a sleep profile showing average sleep across the day by group
#'
#' @param monitor A MonitorS4 object with sleep data
#' @param bin_minutes Binning interval in minutes (default 30)
#' @param group_col Metadata column for grouping (default "Phenotype")
#' @param title Plot title
#' @param show_ribbon Show SEM ribbon (default TRUE)
#' @param show_light_dark Show light/dark annotation (default TRUE)
#'
#' @return A ggplot2 object
#' @export
plotSleepProfile <- function(monitor, bin_minutes = 30, group_col = "Phenotype",
                              title = "Sleep Profile", show_ribbon = TRUE,
                              show_light_dark = TRUE) {

  # Get sleep data
  if (is.null(monitor@assays$sleep)) {
    stop("No sleep data. Run awakeToSleep() first.")
  }

  sleep_data <- monitor@assays$sleep

  # Get groups from metadata
  if (nrow(monitor@meta.data) == 0 || !(group_col %in% colnames(monitor@meta.data))) {
    groups <- rep("All", ncol(sleep_data))
  } else {
    groups <- monitor@meta.data[[group_col]]
  }

  # Bin the data
  minutes_per_day <- 1440
  n_rows <- nrow(sleep_data)
  time_of_day <- ((1:n_rows) - 1) %% minutes_per_day
  bin_idx <- floor(time_of_day / bin_minutes)

  # Calculate average sleep per bin per fly
  bins_per_day <- minutes_per_day / bin_minutes

  # Aggregate
  sleep_binned <- aggregate(
    as.matrix(sleep_data),
    by = list(bin = bin_idx),
    FUN = mean,
    na.rm = TRUE
  )
  sleep_binned$bin <- NULL

  # Convert to long format with groups
  df_list <- lapply(1:ncol(sleep_binned), function(i) {
    data.frame(
      bin = 0:(bins_per_day - 1),
      sleep = sleep_binned[1:bins_per_day, i],
      group = groups[i],
      fly = i
    )
  })
  df <- do.call(rbind, df_list)

  # Calculate ZT time
  df$zt <- df$bin * bin_minutes / 60

  # Summarize by group
  df_summary <- df %>%
    dplyr::group_by(group, zt) %>%
    dplyr::summarise(
      mean_sleep = mean(sleep, na.rm = TRUE),
      sem = sd(sleep, na.rm = TRUE) / sqrt(dplyr::n()),
      .groups = "drop"
    )

  # Create plot
  p <- ggplot2::ggplot(df_summary, ggplot2::aes(x = zt, y = mean_sleep * 100, color = group, fill = group)) +
    ggplot2::geom_line(linewidth = 1) +
    ggplot2::labs(
      title = title,
      x = "Time (ZT hours)",
      y = "Sleep (%)",
      color = group_col,
      fill = group_col
    ) +
    ggplot2::scale_x_continuous(breaks = seq(0, 24, by = 4)) +
    ggplot2::scale_y_continuous(limits = c(0, 100)) +
    scale_color_rmb() +
    scale_fill_rmb() +
    theme_rmb()

  if (show_ribbon) {
    p <- p + ggplot2::geom_ribbon(
      ggplot2::aes(ymin = (mean_sleep - sem) * 100, ymax = (mean_sleep + sem) * 100),
      alpha = 0.2, color = NA
    )
  }

  if (show_light_dark) {
    # Add shading for dark period (ZT12-24)
    p <- p + ggplot2::annotate(
      "rect",
      xmin = 12, xmax = 24,
      ymin = -Inf, ymax = Inf,
      fill = "gray30", alpha = 0.15
    )
  }

  return(p)
}

#' Circadian Actogram
#'
#' Creates a double-plotted actogram for circadian analysis
#'
#' @param monitor A MonitorS4 object
#' @param channel Channel to plot
#' @param title Plot title
#' @param bar_width Width of activity bars (default 1)
#'
#' @return A ggplot2 object
#' @export
plotCircadianActogram <- function(monitor, channel = 1, title = "Actogram",
                                   bar_width = 1) {

  # Get activity data
  if (isS4(monitor)) {
    data <- monitor@assays$mt[, channel]
  } else {
    data <- as.numeric(monitor)
  }

  minutes_per_day <- 1440
  n_days <- ceiling(length(data) / minutes_per_day)

  # Pad and reshape
  padded <- c(data, rep(0, n_days * minutes_per_day - length(data)))
  mat <- matrix(padded, nrow = n_days, ncol = minutes_per_day, byrow = TRUE)

  # Create double-plot data
  df_list <- list()
  for (day in 1:n_days) {
    # First copy
    df_list[[length(df_list) + 1]] <- data.frame(
      day = day,
      time = 1:minutes_per_day,
      activity = mat[day, ]
    )
    # Second copy (shifted)
    if (day < n_days) {
      df_list[[length(df_list) + 1]] <- data.frame(
        day = day,
        time = (1:minutes_per_day) + minutes_per_day,
        activity = mat[day + 1, ]
      )
    }
  }
  df <- do.call(rbind, df_list)

  # Create actogram plot
  p <- ggplot2::ggplot(df, ggplot2::aes(x = time, y = activity)) +
    ggplot2::geom_bar(stat = "identity", fill = "#2E86AB", width = bar_width) +
    ggplot2::facet_wrap(~day, ncol = 1, strip.position = "left") +
    ggplot2::scale_x_continuous(
      breaks = c(0, 720, 1440, 2160, 2880),
      labels = c("0", "12", "24/0", "12", "24")
    ) +
    ggplot2::labs(
      title = title,
      x = "Time (hours)",
      y = NULL
    ) +
    theme_rmb(grid = "none") +
    ggplot2::theme(
      strip.text.y.left = ggplot2::element_text(angle = 0),
      axis.text.y = ggplot2::element_blank(),
      axis.ticks.y = ggplot2::element_blank(),
      panel.spacing = ggplot2::unit(0, "lines")
    )

  # Add light/dark bars
  p <- p +
    ggplot2::annotate("rect", xmin = 720, xmax = 1440, ymin = -Inf, ymax = Inf,
                      fill = "gray30", alpha = 0.15) +
    ggplot2::annotate("rect", xmin = 2160, xmax = 2880, ymin = -Inf, ymax = Inf,
                      fill = "gray30", alpha = 0.15)

  return(p)
}

#' Group Comparison Plot
#'
#' Creates a grouped bar plot with individual data points for comparing phenotypes
#'
#' @param monitor A MonitorS4 object with metadata
#' @param metric Which metric to plot: "total_activity", "total_sleep", "sleep_latency"
#' @param group_col Metadata column for grouping
#' @param title Plot title
#' @param y_label Y-axis label
#' @param show_points Show individual data points (default TRUE)
#' @param show_stats Show p-values (default FALSE)
#'
#' @return A ggplot2 object
#' @export
plotGroupComparison <- function(monitor, metric = "total_activity",
                                 group_col = "Phenotype", title = NULL,
                                 y_label = NULL, show_points = TRUE,
                                 show_stats = FALSE) {

  # Calculate metric per fly
  if (metric == "total_activity") {
    values <- colSums(monitor@assays$mt, na.rm = TRUE)
    if (is.null(y_label)) y_label <- "Total Activity (beam crosses)"
    if (is.null(title)) title <- "Total Activity by Group"
  } else if (metric == "total_sleep") {
    if (is.null(monitor@assays$sleep)) stop("No sleep data")
    values <- colSums(monitor@assays$sleep, na.rm = TRUE)
    if (is.null(y_label)) y_label <- "Total Sleep (minutes)"
    if (is.null(title)) title <- "Total Sleep by Group"
  } else {
    stop("Unknown metric: ", metric)
  }

  # Get groups
  if (nrow(monitor@meta.data) == 0) {
    stop("No metadata for grouping")
  }
  groups <- monitor@meta.data[[group_col]]

  # Create data frame
  df <- data.frame(
    value = values,
    group = groups
  )

  # Calculate summary
  df_summary <- df %>%
    dplyr::group_by(group) %>%
    dplyr::summarise(
      mean = mean(value, na.rm = TRUE),
      sem = sd(value, na.rm = TRUE) / sqrt(dplyr::n()),
      .groups = "drop"
    )

  # Create plot
  p <- ggplot2::ggplot() +
    ggplot2::geom_bar(
      data = df_summary,
      ggplot2::aes(x = group, y = mean, fill = group),
      stat = "identity",
      alpha = 0.7,
      width = 0.7
    ) +
    ggplot2::geom_errorbar(
      data = df_summary,
      ggplot2::aes(x = group, ymin = mean - sem, ymax = mean + sem),
      width = 0.2,
      linewidth = 0.5
    ) +
    scale_fill_rmb() +
    ggplot2::labs(
      title = title,
      x = NULL,
      y = y_label
    ) +
    theme_rmb() +
    ggplot2::theme(legend.position = "none")

  if (show_points) {
    p <- p + ggplot2::geom_jitter(
      data = df,
      ggplot2::aes(x = group, y = value),
      width = 0.15,
      size = 2,
      alpha = 0.6
    )
  }

  return(p)
}

#' Multi-panel Summary Figure
#'
#' Creates a comprehensive multi-panel figure summarizing the experiment
#'
#' @param monitor A processed MonitorS4 object
#' @param output_file Optional file path to save the figure
#' @param width Figure width in inches
#' @param height Figure height in inches
#'
#' @return A patchwork object combining multiple plots
#' @export
plotSummaryFigure <- function(monitor, output_file = NULL, width = 14, height = 10) {

  if (!requireNamespace("patchwork", quietly = TRUE)) {
    stop("Package 'patchwork' required for multi-panel figures")
  }

  # Ensure we have the required data
  if (is.null(monitor@assays$sleep)) {
    monitor@assays$awake <- activityToAwake(monitor@assays$mt)
    monitor@assays$sleep <- awakeToSleep(monitor@assays$awake)
  }

  # Create individual plots
  p1 <- plotActivityRaster(monitor, channel = 1, title = "A. Activity Raster (Fly 1)")

  p2 <- plotSleepProfile(monitor, title = "B. Sleep Profile by Group")

  p3 <- plotGroupComparison(monitor, metric = "total_activity",
                            title = "C. Total Activity")

  p4 <- plotGroupComparison(monitor, metric = "total_sleep",
                            title = "D. Total Sleep")

  # Combine with patchwork
  combined <- patchwork::wrap_plots(p1, p2, p3, p4, ncol = 2) +
    patchwork::plot_annotation(
      title = "RMB Analysis Summary",
      theme = theme_rmb()
    )

  # Save if requested
  if (!is.null(output_file)) {
    ggplot2::ggsave(output_file, combined, width = width, height = height, dpi = 300)
  }

  return(combined)
}
