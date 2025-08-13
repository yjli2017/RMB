#' Analysis Functions for RMB Package
#'
#' @description Functions for analyzing processed TriKinetics monitor data
#' This file contains visualization and analysis functions for the RMB package.

#' Plot group activity timecourse with 30-minute bins
#'
#' @param object MonitorS4 object with processed data
#' @param group_by Column name to group by (default: NULL for no grouping)
#' @param bin_minutes Minutes for time binning (default: 30)
#' @param plot_type Type of plot: "mean", "individual", or "both" (default: "mean")
#' @param save_plot Whether to save the plot (default: FALSE)
#' @param output_path Directory to save plot (default: NULL)
#' @return ggplot object
#' @export
plot_group_activity_timecourse <- function(object, 
                                         group_by = NULL, 
                                         bin_minutes = 30,
                                         plot_type = "mean",
                                         save_plot = FALSE,
                                         output_path = NULL) {
  
  if (!is(object, "MonitorS4")) {
    stop("Object must be of class MonitorS4")
  }
  
  if (!"mortality_status" %in% colnames(object@meta.data)) {
    warning("Dataset appears to contain excluded flies. Consider using filter_to_alive_flies() first.")
  }
  
  # Get data components
  mt_data <- object@assays[["mt"]]
  metadata <- object@meta.data
  
  # Extract time data from DateTime column or create sequence
  if ("DateTime" %in% colnames(mt_data)) {
    time_data <- mt_data$DateTime
  } else {
    # Fallback: create time sequence based on analysis period
    if (!is.null(object@time$analysis_period)) {
      date_range <- object@time$analysis_period$date_range
      total_points <- object@time$analysis_period$total_points
      time_data <- seq(as.POSIXct(paste(date_range[1], "00:00:00")), 
                       as.POSIXct(paste(date_range[2], "23:59:00")), 
                       length.out = total_points)
    } else {
      stop("No time information available in object")
    }
  }
  
  # Create light schedule (11 AM - 11 PM = Light)
  light_phase <- ifelse(hour(time_data) >= 11 & hour(time_data) < 23, "Light", "Dark")
  
  # Process alive flies only if mortality analysis was done
  if ("included_in_analysis" %in% colnames(metadata)) {
    alive_metadata <- metadata[metadata$included_in_analysis == TRUE, ]
  } else {
    alive_metadata <- metadata
  }
  
  cat("Processing activity data:\n")
  cat("- Time points:", length(time_data), "\n")
  
  # Extract channel data
  channel_cols <- grep("Channel_", colnames(mt_data))
  all_activity_data <- data.frame()
  
  for (i in 1:nrow(alive_metadata)) {
    fly_channel <- alive_metadata$Channel[i]
    channel_col_name <- paste0("Channel_", fly_channel)
    
    if (channel_col_name %in% colnames(mt_data)) {
      activity_values <- mt_data[[channel_col_name]]
      
      # Create time bins
      bin_interval <- paste0(bin_minutes, " minutes")
      bin_30min <- floor_date(time_data, bin_interval)
      
      fly_df <- data.frame(
        DateTime = time_data,
        bin_time = bin_30min,
        activity = as.numeric(activity_values),
        light_phase = light_phase,
        Fly = alive_metadata$Fly[i],
        Channel = fly_channel,
        stringsAsFactors = FALSE
      )
      
      # Add grouping variables if specified
      if (!is.null(group_by) && group_by %in% colnames(alive_metadata)) {
        fly_df[[group_by]] <- alive_metadata[[group_by]][i]
      }
      
      # Add other metadata columns that might be useful
      for (col in c("Treatment", "Sex", "Group")) {
        if (col %in% colnames(alive_metadata)) {
          fly_df[[col]] <- alive_metadata[[col]][i]
        }
      }
      
      all_activity_data <- rbind(all_activity_data, fly_df)
    }
  }
  
  cat("- Channels:", length(unique(all_activity_data$Channel)), "\n")
  cat("- Bin size:", bin_minutes, "minutes\n")
  
  # Create grouping column
  if (!is.null(group_by) && group_by %in% colnames(all_activity_data)) {
    grouping_col <- group_by
    cat("- Grouping by:", group_by, "\n")
    groups_found <- unique(all_activity_data[[group_by]])
    cat("- Groups found:", paste(groups_found, collapse = ", "), "\n")
  } else {
    # Create a default group
    all_activity_data$Default_Group <- "All"
    grouping_col <- "Default_Group"
    groups_found <- "All"
  }
  
  # Aggregate by time bins and groups
  if (plot_type %in% c("mean", "both")) {
    grouped_activity <- all_activity_data %>%
      group_by(bin_time, !!sym(grouping_col), light_phase) %>%
      summarise(
        mean_activity = mean(activity, na.rm = TRUE),
        se_activity = sd(activity, na.rm = TRUE) / sqrt(n()),
        n_flies = n_distinct(Fly),
        .groups = "drop"
      ) %>%
      filter(!is.na(mean_activity))
    
    cat("- Data points in plot:", nrow(grouped_activity), "\n")
  }
  
  # Create the plot
  p <- ggplot()
  
  # Add light/dark background
  if (nrow(all_activity_data) > 0) {
    dark_periods <- all_activity_data %>%
      filter(light_phase == "Dark") %>%
      distinct(bin_time) %>%
      mutate(xmin = bin_time, xmax = bin_time + minutes(bin_minutes))
    
    if (nrow(dark_periods) > 0) {
      p <- p + geom_rect(data = dark_periods,
                         aes(xmin = xmin, xmax = xmax, ymin = -Inf, ymax = Inf),
                         fill = "gray30", alpha = 0.15, inherit.aes = FALSE)
    }
  }
  
  # Add data layers based on plot type
  if (plot_type %in% c("individual", "both")) {
    p <- p + geom_line(data = all_activity_data,
                       aes(x = bin_time, y = activity, 
                           group = interaction(Fly, !!sym(grouping_col)),
                           color = !!sym(grouping_col)),
                       alpha = 0.3, linewidth = 0.3)
  }
  
  if (plot_type %in% c("mean", "both")) {
    p <- p +
      geom_ribbon(data = grouped_activity,
                  aes(x = bin_time, 
                      ymin = pmax(0, mean_activity - se_activity),
                      ymax = mean_activity + se_activity,
                      fill = !!sym(grouping_col)),
                  alpha = 0.2, color = NA) +
      geom_line(data = grouped_activity,
                aes(x = bin_time, y = mean_activity, color = !!sym(grouping_col)),
                linewidth = 1.2, alpha = 0.9)
  }
  
  # Add faceting if multiple groups
  if (length(groups_found) > 1) {
    p <- p + facet_wrap(as.formula(paste("~", grouping_col)), scales = "free_y")
  }
  
  # Formatting
  p <- p +
    labs(
      title = "Activity Timecourse",
      subtitle = paste0(bin_minutes, "-minute bins"),
      x = "Time",
      y = paste0("Activity (beam breaks per ", bin_minutes, " min)"),
      color = grouping_col,
      fill = grouping_col,
      caption = "Dark gray background indicates dark phase (lights OFF)"
    ) +
    theme_minimal() +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1, size = 9),
      legend.position = "bottom",
      strip.text = element_text(size = 11, face = "bold"),
      plot.title = element_text(size = 14, face = "bold"),
      plot.subtitle = element_text(size = 11),
      panel.grid.minor = element_blank()
    ) +
    scale_x_datetime(
      date_labels = function(x) {
        if (length(x) > 0) {
          start_date <- min(x, na.rm = TRUE)
          days_elapsed <- as.numeric(difftime(x, start_date, units = "days"))
          day_num <- floor(days_elapsed) + 1
          time_of_day <- format(x, "%H:%M")
          paste0("Day", day_num, "\n", time_of_day)
        } else {
          character(0)
        }
      },
      date_breaks = "6 hours"
    )
  
  # Add colors if multiple groups
  if (length(groups_found) > 1) {
    colors <- c("#1f77b4", "#ff7f0e", "#2ca02c", "#d62728", "#9467bd", "#8c564b")
    p <- p +
      scale_color_manual(values = colors[1:length(groups_found)]) +
      scale_fill_manual(values = colors[1:length(groups_found)])
  }
  
  # Save plot if requested
  if (save_plot && !is.null(output_path)) {
    if (!dir.exists(dirname(output_path))) {
      dir.create(dirname(output_path), recursive = TRUE)
    }
    
    filename <- paste0("activity_timecourse_", 
                       ifelse(is.null(group_by), "all", group_by), "_",
                       bin_minutes, "min.png")
    full_path <- file.path(output_path, filename)
    
    ggsave(full_path, plot = p, width = 14, height = 8, dpi = 300)
    cat("Plot saved to:", full_path, "\n")
  }
  
  return(p)
}

#' Create combined activity plot from multiple monitors
#'
#' @param monitor_list List of processed MonitorS4 objects
#' @param group_by Column name to group by (default: "Group")
#' @param bin_minutes Minutes for time binning (default: 30)
#' @param save_plot Whether to save the plot (default: TRUE)
#' @param output_path Path for saving plot (default: "./results/")
#' @return ggplot object
#' @export
plot_combined_monitors_activity <- function(monitor_list,
                                          group_by = "Group",
                                          bin_minutes = 30,
                                          save_plot = TRUE,
                                          output_path = "./results/") {
  
  if (!is.list(monitor_list) || length(monitor_list) == 0) {
    stop("monitor_list must be a non-empty list of MonitorS4 objects")
  }
  
  # Combine data from all monitors
  all_activity_data <- data.frame()
  
  for (i in seq_along(monitor_list)) {
    monitor <- monitor_list[[i]]
    monitor_name <- names(monitor_list)[i]
    if (is.null(monitor_name)) monitor_name <- paste0("Monitor", i)
    
    # Get data components
    mt_data <- monitor@assays[["mt"]]
    metadata <- monitor@meta.data[monitor@meta.data$included_in_analysis == TRUE, ]
    
    # Extract time data
    if ("DateTime" %in% colnames(mt_data)) {
      time_data <- mt_data$DateTime
    } else {
      date_range <- monitor@time$analysis_period$date_range
      total_points <- monitor@time$analysis_period$total_points
      time_data <- seq(as.POSIXct(paste(date_range[1], "00:00:00")), 
                       as.POSIXct(paste(date_range[2], "23:59:00")), 
                       length.out = total_points)
    }
    
    # Create light schedule
    light_phase <- ifelse(hour(time_data) >= 11 & hour(time_data) < 23, "Light", "Dark")
    
    # Process each fly
    for (j in 1:nrow(metadata)) {
      fly_channel <- metadata$Channel[j]
      channel_col_name <- paste0("Channel_", fly_channel)
      
      if (channel_col_name %in% colnames(mt_data)) {
        activity_values <- mt_data[[channel_col_name]]
        bin_time <- floor_date(time_data, paste0(bin_minutes, " minutes"))
        
        fly_df <- data.frame(
          DateTime = time_data,
          bin_time = bin_time,
          activity = as.numeric(activity_values),
          light_phase = light_phase,
          Fly = metadata$Fly[j],
          Monitor = monitor_name,
          Channel = fly_channel,
          stringsAsFactors = FALSE
        )
        
        # Add metadata columns
        for (col in colnames(metadata)) {
          if (!col %in% colnames(fly_df)) {
            fly_df[[col]] <- metadata[[col]][j]
          }
        }
        
        all_activity_data <- rbind(all_activity_data, fly_df)
      }
    }
  }
  
  cat("Combined data from", length(monitor_list), "monitors\n")
  cat("Total flies:", length(unique(all_activity_data$Fly)), "\n")
  cat("Groups:", paste(unique(all_activity_data[[group_by]]), collapse = ", "), "\n")
  
  # Aggregate by time bins and groups
  grouped_activity <- all_activity_data %>%
    group_by(bin_time, !!sym(group_by), light_phase) %>%
    summarise(
      mean_activity = mean(activity, na.rm = TRUE),
      se_activity = sd(activity, na.rm = TRUE) / sqrt(n()),
      n_flies = n_distinct(Fly),
      .groups = "drop"
    ) %>%
    filter(!is.na(mean_activity))
  
  # Create the plot
  p <- ggplot(grouped_activity, aes(x = bin_time, y = mean_activity, color = !!sym(group_by))) +
    # Add light/dark background
    geom_rect(data = grouped_activity %>% 
                filter(light_phase == "Dark") %>% 
                distinct(bin_time) %>%
                mutate(xmin = bin_time, xmax = bin_time + minutes(bin_minutes)),
              aes(xmin = xmin, xmax = xmax, ymin = -Inf, ymax = Inf),
              fill = "gray30", alpha = 0.15, inherit.aes = FALSE) +
    
    # Add activity data
    geom_ribbon(aes(ymin = pmax(0, mean_activity - se_activity), 
                    ymax = mean_activity + se_activity, 
                    fill = !!sym(group_by)), 
                alpha = 0.2, color = NA) +
    geom_line(linewidth = 1.2, alpha = 0.9) +
    
    # Facet by group
    facet_wrap(as.formula(paste("~", group_by)), ncol = 3, scales = "free_y") +
    
    # Formatting
    labs(
      title = "Combined Monitor Activity Timecourse by Experimental Group",
      subtitle = paste0(bin_minutes, "-minute bins over analysis period\nLight schedule: 11 AM - 11 PM LIGHT ON (dark gray background = dark phase)"),
      x = "Time",
      y = paste0("Mean Activity (beam breaks per ", bin_minutes, " min)"),
      color = group_by,
      fill = group_by,
      caption = paste("Data from", length(monitor_list), "monitors with", 
                     length(unique(all_activity_data$Fly)), "total flies")
    ) +
    theme_minimal() +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1, size = 9),
      legend.position = "bottom",
      strip.text = element_text(size = 11, face = "bold"),
      plot.title = element_text(size = 15, face = "bold"),
      plot.subtitle = element_text(size = 11),
      plot.caption = element_text(size = 9, hjust = 0),
      panel.grid.minor = element_blank()
    ) +
    scale_x_datetime(
      date_labels = function(x) {
        if (length(x) > 0) {
          start_date <- min(x, na.rm = TRUE)
          days_elapsed <- as.numeric(difftime(x, start_date, units = "days"))
          day_num <- floor(days_elapsed) + 1
          time_of_day <- format(x, "%H:%M")
          paste0("Day", day_num, "\n", time_of_day)
        } else {
          character(0)
        }
      },
      date_breaks = "6 hours"
    ) +
    scale_color_manual(values = c(
      "Male_CS" = "#1f77b4", "Female_CS" = "#ff7f0e",
      "Male_Gaboxdol" = "#2ca02c", "Female_Gaboxdol" = "#d62728",
      "Male_Caff" = "#9467bd", "Female_Caff" = "#8c564b"
    )) +
    scale_fill_manual(values = c(
      "Male_CS" = "#1f77b4", "Female_CS" = "#ff7f0e",
      "Male_Gaboxdol" = "#2ca02c", "Female_Gaboxdol" = "#d62728",
      "Male_Caff" = "#9467bd", "Female_Caff" = "#8c564b"
    ))
  
  # Save plot if requested
  if (save_plot) {
    if (!dir.exists(output_path)) {
      dir.create(output_path, recursive = TRUE)
    }
    
    filename <- paste0("combined_monitors_activity_", group_by, "_", bin_minutes, "min.png")
    full_path <- file.path(output_path, filename)
    
    ggsave(full_path, plot = p, width = 18, height = 12, dpi = 300)
    cat("Combined plot saved to:", full_path, "\n")
  }
  
  return(p)
}

#' Generate activity summary statistics by group
#'
#' @param object MonitorS4 object or list of MonitorS4 objects
#' @param group_by Column name to group by (default: "Group")
#' @param bin_minutes Minutes for time binning (default: 30)
#' @return Data frame with activity statistics
#' @export
summarize_activity_by_group <- function(object, group_by = "Group", bin_minutes = 30) {
  
  # Handle single object or list
  if (is(object, "MonitorS4")) {
    object_list <- list(object)
  } else if (is.list(object)) {
    object_list <- object
  } else {
    stop("Input must be a MonitorS4 object or list of MonitorS4 objects")
  }
  
  # Combine data from all objects
  all_activity_data <- data.frame()
  
  for (monitor in object_list) {
    mt_data <- monitor@assays[["mt"]]
    metadata <- monitor@meta.data[monitor@meta.data$included_in_analysis == TRUE, ]
    
    # Extract time data
    if ("DateTime" %in% colnames(mt_data)) {
      time_data <- mt_data$DateTime
    } else {
      date_range <- monitor@time$analysis_period$date_range
      total_points <- monitor@time$analysis_period$total_points
      time_data <- seq(as.POSIXct(paste(date_range[1], "00:00:00")), 
                       as.POSIXct(paste(date_range[2], "23:59:00")), 
                       length.out = total_points)
    }
    
    light_phase <- ifelse(hour(time_data) >= 11 & hour(time_data) < 23, "Light", "Dark")
    
    for (i in 1:nrow(metadata)) {
      fly_channel <- metadata$Channel[i]
      channel_col_name <- paste0("Channel_", fly_channel)
      
      if (channel_col_name %in% colnames(mt_data)) {
        activity_values <- mt_data[[channel_col_name]]
        bin_time <- floor_date(time_data, paste0(bin_minutes, " minutes"))
        
        fly_df <- data.frame(
          bin_time = bin_time,
          activity = as.numeric(activity_values),
          light_phase = light_phase,
          Fly = metadata$Fly[i],
          stringsAsFactors = FALSE
        )
        
        # Add grouping column
        if (group_by %in% colnames(metadata)) {
          fly_df[[group_by]] <- metadata[[group_by]][i]
        }
        
        all_activity_data <- rbind(all_activity_data, fly_df)
      }
    }
  }
  
  # Calculate summary statistics
  summary_stats <- all_activity_data %>%
    group_by(!!sym(group_by), light_phase) %>%
    summarise(
      n_flies = n_distinct(Fly),
      mean_activity = round(mean(activity, na.rm = TRUE), 2),
      sd_activity = round(sd(activity, na.rm = TRUE), 2),
      median_activity = round(median(activity, na.rm = TRUE), 2),
      total_activity = sum(activity, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    pivot_wider(
      names_from = light_phase,
      values_from = c(mean_activity, sd_activity, median_activity, total_activity),
      names_sep = "_"
    ) %>%
    mutate(
      light_dark_ratio = round(mean_activity_Light / mean_activity_Dark, 2),
      total_light_dark_ratio = round(total_activity_Light / total_activity_Dark, 2)
    ) %>%
    arrange(!!sym(group_by))
  
  return(summary_stats)
}