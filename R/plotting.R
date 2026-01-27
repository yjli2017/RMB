# Plotting Functions for RMB
# Consolidated plotting utilities - replaces bar_plot.R and plotting_utilities.R

# Load required packages
suppressPackageStartupMessages({
  if (!require("ggplot2", quietly = TRUE)) {
    install.packages("ggplot2")
    library(ggplot2)
  }
  if (!require("dplyr", quietly = TRUE)) {
    install.packages("dplyr")
    library(dplyr)
  }
  if (!require("tidyr", quietly = TRUE)) {
    install.packages("tidyr")
    library(tidyr)
  }
})

# Suppress R CMD check notes
utils::globalVariables(c("category", "value", "fly", "phenotype", "proportion", 
                         "avg_proportion", "std_error", "Time", "Value",
                         "Micromovements", "Genotype", "Switch"))

#' Prepare data for bar plots
#' 
#' Transforms frequency data into long format suitable for ggplot2
#' 
#' @param monitor A MonitorS4 object with meta.data
#' @param freq_data Frequency distribution data frame
#' @param output_dir Optional directory to save the long-format CSV
#' @param group_col Column name in metadata for grouping (default "Phenotype")
#' @return A data frame in long format
#' @export
prepareBarPlotData <- function(monitor, freq_data, output_dir = NULL, 
                                group_col = "Phenotype") {
  
  # Get grouping from metadata
  if (is.list(monitor) && !is.null(monitor$meta.data)) {
    groups <- monitor$meta.data[[group_col]]
  } else if (isS4(monitor) && nrow(monitor@meta.data) > 0) {
    groups <- monitor@meta.data[[group_col]]
  } else {
    stop("Monitor object must have meta.data with grouping column")
  }
  
  # Transpose and convert to data frame
  df <- as.data.frame(t(freq_data))
  df$fly <- rownames(df)
  df$phenotype <- groups
  
  # Get number of categories (positions)
  n_cats <- ncol(freq_data)
  
  # Pivot to long format
  df_long <- df %>%
    pivot_longer(cols = 1:n_cats, names_to = "category", values_to = "value") %>%
    mutate(
      category = factor(category, levels = unique(category)),
      value = as.numeric(value)
    ) %>%
    group_by(fly) %>%
    mutate(proportion = value / sum(value, na.rm = TRUE)) %>%
    ungroup()
  
  # Optionally save to file
  if (!is.null(output_dir)) {
    output_file <- file.path(output_dir, "freq_data_long.csv")
    write.csv(df_long, file = output_file, row.names = FALSE)
  }
  
  return(df_long)
}

#' Calculate average proportions by phenotype
#' 
#' @param df_long Long-format data frame from prepareBarPlotData
#' @param target_category Category to focus on (default "1" for position 1)
#' @return List with df_avg (summary stats) and df_filtered (individual points)
#' @export
calculateAvgProportion <- function(df_long, target_category = "1") {
  
  # Filter for target category
  df_filtered <- df_long %>% 
    filter(category == target_category)
  
  # Calculate summary statistics
  df_avg <- df_filtered %>%
    group_by(phenotype) %>%
    summarise(
      avg_proportion = mean(proportion, na.rm = TRUE),
      std_error = sd(proportion, na.rm = TRUE) / sqrt(n()),
      n = n(),
      .groups = "drop"
    )
  
  return(list(df_avg = df_avg, df_filtered = df_filtered))
}

#' Create a bar plot with error bars and individual points
#' 
#' @param df_avg Summary statistics data frame
#' @param df_individual Individual data points data frame
#' @param output_file Path to save the plot (optional)
#' @param title Plot title (optional)
#' @param y_label Y-axis label (default "Proportion")
#' @param width Plot width in inches (default 8)
#' @param height Plot height in inches (default 6)
#' @return A ggplot object
#' @export
createBarPlot <- function(df_avg, df_individual, output_file = NULL,
                           title = NULL, y_label = "Proportion",
                           width = 8, height = 6) {
  
  p <- ggplot(df_avg, aes(x = phenotype, y = avg_proportion)) +
    geom_bar(aes(fill = phenotype), stat = "identity", alpha = 0.7) +
    geom_errorbar(
      aes(ymin = avg_proportion - std_error, 
          ymax = avg_proportion + std_error),
      width = 0.2,
      linewidth = 0.5
    ) +
    geom_point(
      data = df_individual,
      aes(y = proportion),
      color = "black",
      size = 2,
      alpha = 0.6
    ) +
    labs(
      title = title,
      x = NULL,
      y = y_label
    ) +
    theme_bw() +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1),
      legend.position = "none"
    )
  
  if (!is.null(output_file)) {
    ggsave(filename = output_file, plot = p, dpi = 300, 
           width = width, height = height)
  }
  
  return(p)
}

#' Plot time series data
#' 
#' @param data Numeric vector or single column of time series data
#' @param y_label Y-axis label (default "Activity")
#' @param title Plot title (optional)
#' @param show_day_lines Show vertical lines at day boundaries (default TRUE)
#' @return A ggplot object
#' @export
plotTimeSeries <- function(data, y_label = "Activity", title = NULL,
                            show_day_lines = TRUE) {
  
  df <- data.frame(
    Time = seq_along(data),
    Value = as.numeric(data)
  )
  
  p <- ggplot(df, aes(x = Time, y = Value)) +
    geom_line(color = "steelblue", linewidth = 0.3) +
    labs(title = title, y = y_label) +
    theme_bw()
  
  if (show_day_lines && max(df$Time) > 1440) {
    # Add vertical lines at day boundaries (every 1440 minutes)
    day_breaks <- seq(1440, max(df$Time), by = 1440)
    p <- p + geom_vline(xintercept = day_breaks, 
                        linetype = "dashed", color = "gray50", alpha = 0.5)
  }
  
  return(p)
}

#' Plot 30-minute binned sleep/activity data
#' 
#' @param data Data frame with Time, Genotype, and value columns
#' @param value_col Column name for the y-axis values
#' @param title Plot title
#' @param show_light_dark Show light/dark transition (default TRUE)
#' @return A ggplot object
#' @export
plotBinned <- function(data, value_col = "Micromovements", title = "",
                        show_light_dark = TRUE) {
  
  p <- ggplot(data, aes_string(x = "Time", y = value_col, color = "Genotype")) +
    geom_line(linewidth = 0.8) +
    scale_x_continuous(breaks = c(0, 6, 12, 18, 24)) +
    labs(title = title) +
    theme_bw()
  
  if (show_light_dark) {
    p <- p + geom_vline(xintercept = 12, color = "orange", 
                        linetype = "dotted", linewidth = 1)
  }
  
  return(p)
}

#' Plot light switches for time range selection
#' 
#' @param monitor_list List of monitor objects
#' @param light_col Column index or name containing light on/off data
#' @return A ggplot object
#' @export
plotLightSwitches <- function(monitor_list, light_col = 10) {
  
  # Get light data from first monitor
  if (is.list(monitor_list[[1]]) && !is.null(monitor_list[[1]]$time)) {
    light_data <- monitor_list[[1]]$time[, light_col]
  } else if (isS4(monitor_list[[1]]) && nrow(monitor_list[[1]]@time) > 0) {
    light_data <- monitor_list[[1]]@time[, light_col]
  } else {
    stop("Could not extract time/light data from monitor")
  }
  
  # Find switch points
  switch_indices <- which(diff(light_data) != 0)
  
  if (length(switch_indices) == 0) {
    warning("No light switches detected")
  }
  
  df <- data.frame(
    Time = seq_along(light_data),
    Value = light_data
  )
  
  switch_df <- data.frame(
    Switch = seq_along(switch_indices),
    Time = switch_indices,
    Value = light_data[switch_indices]
  )
  
  p <- ggplot(df, aes(x = Time, y = Value)) +
    geom_line() +
    geom_vline(data = switch_df, aes(xintercept = Time), 
               color = "red", linetype = "dashed") +
    geom_text(data = switch_df, aes(label = Switch), 
              vjust = -1, size = 3) +
    labs(y = "Light on/off") +
    theme_bw()
  
  cat("Light switch indices:", switch_indices, "\n")
  
  return(p)
}

#' Quick plot for position frequency distribution
#' 
#' Creates a heatmap-style plot for position frequencies
#' 
#' @param freq_data Frequency data (positions x flies)
#' @param title Plot title
#' @return A ggplot object
#' @export
plotPositionHeatmap <- function(freq_data, title = "Position Frequency") {
  
  # Convert to long format
  df <- as.data.frame(freq_data)
  df$position <- rownames(df)
  
  df_long <- df %>%
    pivot_longer(-position, names_to = "fly", values_to = "count") %>%
    mutate(position = factor(position, levels = rev(unique(position))))
  
  p <- ggplot(df_long, aes(x = fly, y = position, fill = count)) +
    geom_tile() +
    scale_fill_gradient(low = "white", high = "steelblue") +
    labs(title = title, x = "Fly", y = "Position") +
    theme_bw() +
    theme(axis.text.x = element_text(angle = 90, hjust = 1, size = 6))
  
  return(p)
}
