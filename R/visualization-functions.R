#' Visualization Functions for RMB Package
#'
#' @description Quality control and diagnostic plotting functions for TriKinetics data
#' This file contains QC plots and diagnostic visualizations for the RMB package.

#' Plot light schedule verification
#'
#' @param object MonitorS4 object
#' @param save_plot Whether to save the plot (default: FALSE)
#' @param output_path Directory to save plot (default: NULL)
#' @return ggplot object
#' @export
plot_light_schedule <- function(object, save_plot = FALSE, output_path = NULL) {
  
  if (!is(object, "MonitorS4")) {
    stop("Object must be of class MonitorS4")
  }
  
  mt_data <- object@assays[["mt"]]
  
  if (!"Light" %in% colnames(mt_data)) {
    stop("Light column not found in MT data")
  }
  
  # Create time data
  if ("DateTime" %in% colnames(mt_data)) {
    time_data <- mt_data$DateTime
  } else {
    # Fallback time sequence
    time_data <- seq(as.POSIXct("2023-01-01 00:00:00"), 
                     length.out = nrow(mt_data), by = "1 min")
  }
  
  # Create light schedule data
  light_df <- data.frame(
    DateTime = time_data,
    Light = mt_data$Light,
    Date = as.Date(time_data),
    Hour = hour(time_data),
    stringsAsFactors = FALSE
  )
  
  # Create the plot
  p <- ggplot(light_df, aes(x = DateTime, y = Light)) +
    geom_line(color = "orange", linewidth = 1) +
    facet_wrap(~ Date, scales = "free_x", ncol = 2) +
    labs(
      title = "Light Schedule Verification",
      subtitle = "Expected: Light ON 11:00-23:00 (1), Light OFF 23:00-11:00 (0)",
      x = "Time",
      y = "Light Status (0=OFF, 1=ON)",
      caption = "Verify that light schedule matches experimental design"
    ) +
    theme_minimal() +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1),
      strip.text = element_text(size = 10, face = "bold"),
      plot.title = element_text(size = 14, face = "bold")
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
    scale_y_continuous(breaks = c(0, 1), labels = c("OFF", "ON"))
  
  # Save plot if requested
  if (save_plot && !is.null(output_path)) {
    if (!dir.exists(dirname(output_path))) {
      dir.create(dirname(output_path), recursive = TRUE)
    }
    
    filename <- "light_schedule_verification.png"
    full_path <- file.path(output_path, filename)
    
    ggsave(full_path, plot = p, width = 12, height = 8, dpi = 300)
    cat("Light schedule plot saved to:", full_path, "\n")
  }
  
  return(p)
}

#' Plot monitor QC with binary activity visualization
#'
#' @param object MonitorS4 object
#' @param save_plot Whether to save the plot (default: FALSE)
#' @param output_path Directory to save plot (default: NULL)
#' @return ggplot object
#' @export
plot_monitor_qc_binary <- function(object, save_plot = FALSE, output_path = NULL) {
  
  if (!is(object, "MonitorS4")) {
    stop("Object must be of class MonitorS4")
  }
  
  mt_data <- object@assays[["mt"]]
  metadata <- object@meta.data
  
  # Extract time data
  if ("DateTime" %in% colnames(mt_data)) {
    time_data <- mt_data$DateTime
  } else {
    time_data <- seq(as.POSIXct("2023-01-01 00:00:00"), 
                     length.out = nrow(mt_data), by = "1 min")
  }
  
  # Get channel columns
  channel_cols <- grep("Channel_", colnames(mt_data))
  
  # Create long format data for all flies
  plot_data <- data.frame()
  
  for (i in seq_along(channel_cols)) {
    channel_name <- colnames(mt_data)[channel_cols[i]]
    channel_num <- as.numeric(gsub("Channel_", "", channel_name))
    
    # Find fly information
    fly_info <- metadata[metadata$Channel == channel_num, ]
    
    if (nrow(fly_info) > 0) {
      fly_data <- data.frame(
        DateTime = time_data,
        Activity = mt_data[[channel_name]],
        Channel = channel_num,
        Fly = fly_info$Fly[1],
        Active = ifelse(mt_data[[channel_name]] > 0, 1, 0),
        stringsAsFactors = FALSE
      )
      
      plot_data <- rbind(plot_data, fly_data)
    }
  }
  
  # Create binary activity plot
  p <- ggplot(plot_data, aes(x = DateTime, y = factor(Fly), fill = factor(Active))) +
    geom_tile(height = 0.8) +
    scale_fill_manual(
      values = c("0" = "white", "1" = "black"),
      labels = c("0" = "No Activity", "1" = "Activity"),
      name = "Status"
    ) +
    labs(
      title = "Monitor QC: Binary Activity Visualization",
      subtitle = "Each row represents one fly over time\nDead flies appear as continuous white (no activity) rows",
      x = "Time",
      y = "Fly ID",
      caption = "White = no activity, Black = activity detected\nUse this plot to visually identify dead flies"
    ) +
    theme_minimal() +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1),
      axis.text.y = element_text(size = 6),
      plot.title = element_text(size = 14, face = "bold"),
      legend.position = "bottom"
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
  
  # Add mortality information if available
  if ("mortality_status" %in% colnames(metadata)) {
    dead_flies <- metadata$Fly[metadata$mortality_status == "dead"]
    if (length(dead_flies) > 0) {
      p <- p + labs(caption = paste("White = no activity, Black = activity detected\n",
                                   "Dead flies identified:", paste(dead_flies, collapse = ", ")))
    }
  }
  
  # Save plot if requested
  if (save_plot && !is.null(output_path)) {
    if (!dir.exists(dirname(output_path))) {
      dir.create(dirname(output_path), recursive = TRUE)
    }
    
    monitor_name <- "monitor"
    if ("Monitor_Name" %in% colnames(mt_data)) {
      monitor_name <- mt_data$Monitor_Name[1]
    }
    
    filename <- paste0(monitor_name, "_qc_binary.png")
    full_path <- file.path(output_path, filename)
    
    ggsave(full_path, plot = p, width = 14, height = 10, dpi = 300)
    cat("QC binary plot saved to:", full_path, "\n")
  }
  
  return(p)
}

#' Plot individual fly activity timecourse
#'
#' @param object MonitorS4 object
#' @param fly_ids Vector of fly IDs to plot (default: NULL for all flies)
#' @param max_flies Maximum number of flies to plot (default: 16)
#' @param save_plot Whether to save the plot (default: FALSE)
#' @param output_path Directory to save plot (default: NULL)
#' @return ggplot object
#' @export
plot_individual_flies <- function(object, fly_ids = NULL, max_flies = 16, 
                                 save_plot = FALSE, output_path = NULL) {
  
  if (!is(object, "MonitorS4")) {
    stop("Object must be of class MonitorS4")
  }
  
  mt_data <- object@assays[["mt"]]
  metadata <- object@meta.data
  
  # Filter to alive flies if mortality analysis was done
  if ("included_in_analysis" %in% colnames(metadata)) {
    metadata <- metadata[metadata$included_in_analysis == TRUE, ]
  }
  
  # Select flies to plot
  if (is.null(fly_ids)) {
    fly_ids <- head(metadata$Fly, max_flies)
  } else {
    fly_ids <- fly_ids[fly_ids %in% metadata$Fly]
    fly_ids <- head(fly_ids, max_flies)
  }
  
  # Extract time data
  if ("DateTime" %in% colnames(mt_data)) {
    time_data <- mt_data$DateTime
  } else {
    time_data <- seq(as.POSIXct("2023-01-01 00:00:00"), 
                     length.out = nrow(mt_data), by = "1 min")
  }
  
  # Create light schedule
  light_phase <- ifelse(hour(time_data) >= 11 & hour(time_data) < 23, "Light", "Dark")
  
  # Prepare data for selected flies
  plot_data <- data.frame()
  
  for (fly_id in fly_ids) {
    fly_info <- metadata[metadata$Fly == fly_id, ]
    if (nrow(fly_info) > 0) {
      channel_num <- fly_info$Channel[1]
      channel_col <- paste0("Channel_", channel_num)
      
      if (channel_col %in% colnames(mt_data)) {
        fly_df <- data.frame(
          DateTime = time_data,
          Activity = mt_data[[channel_col]],
          light_phase = light_phase,
          Fly = fly_id,
          Channel = channel_num,
          stringsAsFactors = FALSE
        )
        
        plot_data <- rbind(plot_data, fly_df)
      }
    }
  }
  
  if (nrow(plot_data) == 0) {
    stop("No data found for specified flies")
  }
  
  # Create the plot
  p <- ggplot(plot_data, aes(x = DateTime, y = Activity)) +
    # Add light/dark background
    geom_rect(data = plot_data %>% 
                filter(light_phase == "Dark") %>% 
                distinct(DateTime) %>%
                mutate(xmin = DateTime, xmax = DateTime + minutes(1)),
              aes(xmin = xmin, xmax = xmax, ymin = -Inf, ymax = Inf),
              fill = "gray30", alpha = 0.1, inherit.aes = FALSE) +
    
    geom_line(color = "steelblue", alpha = 0.7) +
    facet_wrap(~ Fly, scales = "free_y", ncol = 4) +
    labs(
      title = "Individual Fly Activity Timecourse",
      subtitle = "Activity patterns for selected flies (dark gray background = dark phase)",
      x = "Time",
      y = "Activity (beam breaks per minute)",
      caption = "Each panel shows one fly's activity over time"
    ) +
    theme_minimal() +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1, size = 8),
      strip.text = element_text(size = 9, face = "bold"),
      plot.title = element_text(size = 14, face = "bold")
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
  
  # Save plot if requested
  if (save_plot && !is.null(output_path)) {
    if (!dir.exists(dirname(output_path))) {
      dir.create(dirname(output_path), recursive = TRUE)
    }
    
    filename <- paste0("individual_flies_", length(fly_ids), "flies.png")
    full_path <- file.path(output_path, filename)
    
    ggsave(full_path, plot = p, width = 16, height = 12, dpi = 300)
    cat("Individual flies plot saved to:", full_path, "\n")
  }
  
  return(p)
}

#' Create mortality summary visualization
#'
#' @param object MonitorS4 object with mortality analysis
#' @param save_plot Whether to save the plot (default: FALSE)
#' @param output_path Directory to save plot (default: NULL)
#' @return ggplot object
#' @export
plot_mortality_summary <- function(object, save_plot = FALSE, output_path = NULL) {
  
  if (!is(object, "MonitorS4")) {
    stop("Object must be of class MonitorS4")
  }
  
  if (!"mortality_status" %in% colnames(object@meta.data)) {
    stop("No mortality information found. Run detect_dead_flies_final_day() first.")
  }
  
  metadata <- object@meta.data
  
  # Create summary data
  mortality_summary <- metadata %>%
    group_by(mortality_status, mortality_type) %>%
    summarise(count = n(), .groups = "drop") %>%
    mutate(
      percentage = round(count / sum(count) * 100, 1),
      label = paste0(count, " (", percentage, "%)")
    )
  
  # Create pie chart
  p1 <- ggplot(mortality_summary, aes(x = "", y = count, fill = mortality_status)) +
    geom_bar(stat = "identity", width = 1) +
    coord_polar("y", start = 0) +
    geom_text(aes(label = label), position = position_stack(vjust = 0.5)) +
    labs(
      title = "Mortality Summary",
      fill = "Status"
    ) +
    theme_void() +
    scale_fill_manual(values = c("alive" = "lightgreen", "dead" = "red"))
  
  # Create mortality type breakdown if there are dead flies
  if (sum(metadata$mortality_status == "dead") > 0) {
    dead_flies <- metadata[metadata$mortality_status == "dead", ]
    
    type_summary <- dead_flies %>%
      group_by(mortality_type) %>%
      summarise(count = n(), .groups = "drop") %>%
      mutate(
        percentage = round(count / sum(count) * 100, 1),
        label = paste0(count, " (", percentage, "%)")
      )
    
    p2 <- ggplot(type_summary, aes(x = mortality_type, y = count, fill = mortality_type)) +
      geom_bar(stat = "identity") +
      geom_text(aes(label = label), vjust = -0.5) +
      labs(
        title = "Dead Fly Types",
        x = "Mortality Type",
        y = "Count",
        fill = "Type"
      ) +
      theme_minimal() +
      scale_fill_manual(values = c("completely_inactive" = "darkred", 
                                  "final_day_inactive" = "orange"))
    
    # Combine plots
    p <- gridExtra::grid.arrange(p1, p2, ncol = 2)
  } else {
    p <- p1
  }
  
  # Save plot if requested
  if (save_plot && !is.null(output_path)) {
    if (!dir.exists(dirname(output_path))) {
      dir.create(dirname(output_path), recursive = TRUE)
    }
    
    filename <- "mortality_summary.png"
    full_path <- file.path(output_path, filename)
    
    ggsave(full_path, plot = p, width = 12, height = 6, dpi = 300)
    cat("Mortality summary plot saved to:", full_path, "\n")
  }
  
  return(p)
}