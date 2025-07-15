# Time Range Selection Functions
# This file contains functions for selecting time ranges from monitor data

# Function to select time range from monitor list
select_time_range <- function(monitor_list, start_hour = 0, end_hour = NULL, 
                             start_day = 1, end_day = NULL) {
  cat("Selecting time range for", length(monitor_list), "monitors\n")
  
  # For this implementation, we'll keep all data but could add filtering logic
  # Typical DAM data is collected every minute (1440 points per day)
  
  for (i in 1:length(monitor_list)) {
    monitor <- monitor_list[[i]]
    
    if (!is.null(monitor$assays$mt)) {
      total_points <- nrow(monitor$assays$mt)
      cat("  Monitor", i, ":", total_points, "time points\n")
      
      # If end_day is specified, truncate the data
      if (!is.null(end_day)) {
        points_per_day <- 1440  # Assuming 1 minute intervals
        max_points <- end_day * points_per_day
        
        if (total_points > max_points) {
          monitor_list[[i]]$assays$mt <- monitor$assays$mt[1:max_points, , drop = FALSE]
          if (!is.null(monitor$assays$pn)) {
            monitor_list[[i]]$assays$pn <- monitor$assays$pn[1:max_points, , drop = FALSE]
          }
          cat("    Truncated to", max_points, "points (", end_day, "days)\n")
        }
      }
    }
  }
  
  return(monitor_list)
}
