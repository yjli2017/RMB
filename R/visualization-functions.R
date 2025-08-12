#' Visualization Functions for RMB Package
#'
#' @description Functions for creating various plots and visualizations

#' Create activity heatmap
#'
#' @param object MonitorS4 object or analysis results
#' @param assay_type Type of assay to plot ("mt", "sleep", "pn")
#' @param interactive Logical, create interactive plot using plotly (default: FALSE)
#' @param color_scale Color scale to use (default: "viridis")
#' @return ggplot2 object or plotly object if interactive=TRUE
#' @export
plot_heatmap <- function(object, assay_type = "mt", interactive = FALSE, color_scale = "viridis") {
  
  if (is(object, "MonitorS4")) {
    if (!assay_type %in% names(object@assays)) {
      stop("Assay type '", assay_type, "' not found in object")
    }
    data <- object@assays[[assay_type]]
  } else {
    stop("Object must be of class MonitorS4")
  }
  
  # Convert to long format for plotting
  long_data <- wide_to_long(data, "value")
  
  # Create base heatmap
  p <- ggplot2::ggplot(long_data, ggplot2::aes(x = DateTime, y = factor(Channel), fill = value)) +
    ggplot2::geom_tile() +
    ggplot2::labs(
      title = paste("Activity Heatmap -", toupper(assay_type)),
      x = "Time",
      y = "Channel",
      fill = ifelse(assay_type == "sleep", "Sleep (min)", 
                   ifelse(assay_type == "pn", "Position", "Activity"))
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      axis.text.x = ggplot2::element_text(angle = 45, hjust = 1),
      plot.title = ggplot2::element_text(hjust = 0.5, size = 14, face = "bold")
    )
  
  # Apply color scale
  if (color_scale == "viridis") {
    p <- p + viridis::scale_fill_viridis_c()
  } else {
    p <- p + ggplot2::scale_fill_gradient(low = "white", high = "darkblue")
  }
  
  # Make interactive if requested
  if (interactive) {
    p <- plotly::ggplotly(p)
  }
  
  return(p)
}

#' Create activity bar plot
#'
#' @param analysis_results Results from analyze_activity() or comprehensive_analysis()
#' @param plot_type Type of plot ("daily_total", "hourly_pattern", "overall_stats")
#' @param group_by Grouping variable from metadata (optional)
#' @param interactive Logical, create interactive plot using plotly (default: FALSE)
#' @return ggplot2 object or plotly object if interactive=TRUE
#' @export
plot_activity_bars <- function(analysis_results, plot_type = "daily_total", group_by = NULL, interactive = FALSE) {
  
  if ("activity" %in% names(analysis_results)) {
    activity_data <- analysis_results$activity
  } else if ("overall_stats" %in% names(analysis_results)) {
    activity_data <- analysis_results
  } else {
    stop("Invalid analysis results format")
  }
  
  if (plot_type == "daily_total") {
    if (!"daily_totals" %in% names(activity_data)) {
      stop("Daily totals not found in analysis results")
    }
    
    plot_data <- activity_data$daily_totals
    
    p <- ggplot2::ggplot(plot_data, ggplot2::aes(x = date, y = total_activity)) +
      ggplot2::geom_col(fill = "steelblue", alpha = 0.7) +
      ggplot2::facet_wrap(~Channel, scales = "free_y") +
      ggplot2::labs(
        title = "Daily Activity Totals",
        x = "Date",
        y = "Total Activity"
      ) +
      ggplot2::theme_minimal() +
      ggplot2::theme(
        axis.text.x = ggplot2::element_text(angle = 45, hjust = 1),
        plot.title = ggplot2::element_text(hjust = 0.5, size = 14, face = "bold")
      )
    
  } else if (plot_type == "hourly_pattern") {
    if (!"hourly_patterns" %in% names(activity_data)) {
      stop("Hourly patterns not found in analysis results")
    }
    
    plot_data <- activity_data$hourly_patterns
    
    p <- ggplot2::ggplot(plot_data, ggplot2::aes(x = hour, y = mean_activity)) +
      ggplot2::geom_line(color = "steelblue", size = 1) +
      ggplot2::geom_point(color = "steelblue", size = 2) +
      ggplot2::facet_wrap(~Channel, scales = "free_y") +
      ggplot2::labs(
        title = "Hourly Activity Patterns",
        x = "Hour of Day",
        y = "Mean Activity"
      ) +
      ggplot2::scale_x_continuous(breaks = c(0, 6, 12, 18, 24)) +
      ggplot2::theme_minimal() +
      ggplot2::theme(
        plot.title = ggplot2::element_text(hjust = 0.5, size = 14, face = "bold")
      )
    
  } else if (plot_type == "overall_stats") {
    if (!"overall_stats" %in% names(activity_data)) {
      stop("Overall stats not found in analysis results")
    }
    
    plot_data <- activity_data$overall_stats
    
    # Apply grouping if specified
    if (!is.null(group_by) && group_by %in% colnames(plot_data)) {
      p <- ggplot2::ggplot(plot_data, ggplot2::aes(x = factor(Channel), y = total_activity, fill = !!ggplot2::sym(group_by))) +
        ggplot2::geom_col(position = "dodge", alpha = 0.8) +
        ggplot2::labs(
          title = paste("Total Activity by", group_by),
          x = "Channel",
          y = "Total Activity",
          fill = group_by
        ) +
        ggplot2::theme_minimal() +
        ggplot2::theme(
          plot.title = ggplot2::element_text(hjust = 0.5, size = 14, face = "bold"),
          legend.position = "bottom"
        )
    } else {
      p <- ggplot2::ggplot(plot_data, ggplot2::aes(x = factor(Channel), y = total_activity)) +
        ggplot2::geom_col(fill = "steelblue", alpha = 0.7) +
        ggplot2::labs(
          title = "Total Activity by Channel",
          x = "Channel",
          y = "Total Activity"
        ) +
        ggplot2::theme_minimal() +
        ggplot2::theme(
          plot.title = ggplot2::element_text(hjust = 0.5, size = 14, face = "bold")
        )
    }
  }
  
  # Make interactive if requested
  if (interactive) {
    p <- plotly::ggplotly(p)
  }
  
  return(p)
}

#' Create circadian rhythm plot
#'
#' @param analysis_results Results from analyze_circadian() or comprehensive_analysis()
#' @param plot_type Type of plot ("rhythm_wheel", "phase_plot", "amplitude_plot")
#' @param group_by Grouping variable from metadata (optional)
#' @return ggplot2 object
#' @export
plot_circadian <- function(analysis_results, plot_type = "rhythm_wheel", group_by = NULL) {
  
  if ("circadian" %in% names(analysis_results)) {
    circadian_data <- analysis_results$circadian
  } else if ("circadian_parameters" %in% names(analysis_results)) {
    circadian_data <- analysis_results
  } else {
    stop("Invalid analysis results format")
  }
  
  if (plot_type == "rhythm_wheel") {
    if (!"hourly_averages" %in% names(circadian_data)) {
      stop("Hourly averages not found in circadian results")
    }
    
    plot_data <- circadian_data$hourly_averages
    
    p <- ggplot2::ggplot(plot_data, ggplot2::aes(x = hour_bin, y = mean_activity)) +
      ggplot2::geom_line(color = "darkblue", size = 1) +
      ggplot2::geom_point(color = "darkblue", size = 2) +
      ggplot2::facet_wrap(~Channel, scales = "free_y") +
      ggplot2::labs(
        title = "Circadian Activity Patterns",
        x = "Hour of Day",
        y = "Mean Activity"
      ) +
      ggplot2::scale_x_continuous(
        breaks = c(0, 6, 12, 18),
        labels = c("0:00", "6:00", "12:00", "18:00")
      ) +
      ggplot2::theme_minimal() +
      ggplot2::theme(
        plot.title = ggplot2::element_text(hjust = 0.5, size = 14, face = "bold")
      )
    
  } else if (plot_type == "phase_plot") {
    if (!"circadian_parameters" %in% names(circadian_data)) {
      stop("Circadian parameters not found in results")
    }
    
    plot_data <- circadian_data$circadian_parameters
    
    p <- ggplot2::ggplot(plot_data, ggplot2::aes(x = factor(Channel), y = peak_hour)) +
      ggplot2::geom_point(size = 3, alpha = 0.7) +
      ggplot2::labs(
        title = "Peak Activity Phase",
        x = "Channel", 
        y = "Peak Hour"
      ) +
      ggplot2::scale_y_continuous(
        breaks = c(0, 6, 12, 18, 24),
        labels = c("0:00", "6:00", "12:00", "18:00", "24:00")
      ) +
      ggplot2::theme_minimal() +
      ggplot2::theme(
        plot.title = ggplot2::element_text(hjust = 0.5, size = 14, face = "bold")
      )
    
    # Apply grouping if specified
    if (!is.null(group_by) && group_by %in% colnames(plot_data)) {
      p <- p + ggplot2::aes(color = !!ggplot2::sym(group_by)) +
        ggplot2::labs(color = group_by)
    } else {
      p <- p + ggplot2::aes(color = rhythm_classification) +
        ggplot2::labs(color = "Rhythm Type")
    }
    
  } else if (plot_type == "amplitude_plot") {
    if (!"circadian_parameters" %in% names(circadian_data)) {
      stop("Circadian parameters not found in results")
    }
    
    plot_data <- circadian_data$circadian_parameters
    
    if (!is.null(group_by) && group_by %in% colnames(plot_data)) {
      p <- ggplot2::ggplot(plot_data, ggplot2::aes(x = !!ggplot2::sym(group_by), y = amplitude, fill = !!ggplot2::sym(group_by))) +
        ggplot2::geom_boxplot(alpha = 0.7) +
        ggplot2::geom_jitter(width = 0.2, alpha = 0.5) +
        ggplot2::labs(
          title = paste("Rhythm Amplitude by", group_by),
          x = group_by,
          y = "Amplitude",
          fill = group_by
        )
    } else {
      p <- ggplot2::ggplot(plot_data, ggplot2::aes(x = rhythm_classification, y = amplitude, fill = rhythm_classification)) +
        ggplot2::geom_boxplot(alpha = 0.7) +
        ggplot2::geom_jitter(width = 0.2, alpha = 0.5) +
        ggplot2::labs(
          title = "Rhythm Amplitude by Classification",
          x = "Rhythm Classification",
          y = "Amplitude",
          fill = "Classification"
        )
    }
    
    p <- p + ggplot2::theme_minimal() +
      ggplot2::theme(
        plot.title = ggplot2::element_text(hjust = 0.5, size = 14, face = "bold"),
        legend.position = "none"
      )
  }
  
  return(p)
}

#' Create position analysis plots
#'
#' @param analysis_results Results from analyze_position() or comprehensive_analysis()
#' @param plot_type Type of plot ("position_distribution", "movement_stats", "preference_heatmap")
#' @param group_by Grouping variable from metadata (optional)
#' @return ggplot2 object
#' @export
plot_position <- function(analysis_results, plot_type = "position_distribution", group_by = NULL) {
  
  if ("position" %in% names(analysis_results)) {
    position_data <- analysis_results$position
  } else if ("position_stats" %in% names(analysis_results)) {
    position_data <- analysis_results
  } else {
    stop("Invalid analysis results format")
  }
  
  if (plot_type == "position_distribution") {
    if (!"position_preferences" %in% names(position_data)) {
      stop("Position preferences not found in results")
    }
    
    plot_data <- position_data$position_preferences
    
    p <- ggplot2::ggplot(plot_data, ggplot2::aes(x = position_bin, y = percentage, fill = position_bin)) +
      ggplot2::geom_col(alpha = 0.8) +
      ggplot2::facet_wrap(~Channel, scales = "free_y") +
      ggplot2::labs(
        title = "Position Preferences",
        x = "Position Bin",
        y = "Percentage of Time",
        fill = "Position"
      ) +
      ggplot2::theme_minimal() +
      ggplot2::theme(
        plot.title = ggplot2::element_text(hjust = 0.5, size = 14, face = "bold"),
        axis.text.x = ggplot2::element_text(angle = 45, hjust = 1),
        legend.position = "none"
      ) +
      viridis::scale_fill_viridis_d()
    
  } else if (plot_type == "movement_stats") {
    if (!"position_stats" %in% names(position_data)) {
      stop("Position stats not found in results")
    }
    
    plot_data <- position_data$position_stats
    
    if (!is.null(group_by) && group_by %in% colnames(plot_data)) {
      p <- ggplot2::ggplot(plot_data, ggplot2::aes(x = !!ggplot2::sym(group_by), y = total_movement, fill = !!ggplot2::sym(group_by))) +
        ggplot2::geom_boxplot(alpha = 0.7) +
        ggplot2::geom_jitter(width = 0.2, alpha = 0.5) +
        ggplot2::labs(
          title = paste("Total Movement by", group_by),
          x = group_by,
          y = "Total Movement",
          fill = group_by
        )
    } else {
      p <- ggplot2::ggplot(plot_data, ggplot2::aes(x = factor(Channel), y = total_movement)) +
        ggplot2::geom_col(fill = "steelblue", alpha = 0.7) +
        ggplot2::labs(
          title = "Total Movement by Channel",
          x = "Channel",
          y = "Total Movement"
        )
    }
    
    p <- p + ggplot2::theme_minimal() +
      ggplot2::theme(
        plot.title = ggplot2::element_text(hjust = 0.5, size = 14, face = "bold")
      )
  }
  
  return(p)
}

#' Create summary dashboard plot
#'
#' @param analysis_results Results from comprehensive_analysis()
#' @param metrics Vector of metrics to include in dashboard
#' @param group_by Grouping variable from metadata (optional)
#' @return ggplot2 object with multiple panels
#' @export
plot_dashboard <- function(analysis_results, 
                          metrics = c("activity", "sleep", "circadian", "position"),
                          group_by = NULL) {
  
  plots <- list()
  
  # Activity plot
  if ("activity" %in% metrics && "activity" %in% names(analysis_results)) {
    plots$activity <- plot_activity_bars(analysis_results, "hourly_pattern")
  }
  
  # Circadian plot
  if ("circadian" %in% metrics && "circadian" %in% names(analysis_results)) {
    plots$circadian <- plot_circadian(analysis_results, "phase_plot", group_by)
  }
  
  # Position plot
  if ("position" %in% metrics && "position" %in% names(analysis_results)) {
    plots$position <- plot_position(analysis_results, "movement_stats", group_by)
  }
  
  if (length(plots) == 0) {
    stop("No valid plots could be generated with the provided data and metrics")
  }
  
  # Combine plots using patchwork if available, otherwise return list
  if (requireNamespace("patchwork", quietly = TRUE)) {
    combined_plot <- patchwork::wrap_plots(plots, ncol = 2)
    return(combined_plot)
  } else {
    return(plots)
  }
}