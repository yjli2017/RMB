# Data Transformation Functions for RMB
# Consolidated transformations - replaces data_transformations.R, convert_sequence.R, 
# flip_binary.R, df_to_freq_dist.R

#' Convert activity to awake/sleep binary
#' 
#' Marks a minute as "asleep" (0) if part of a sequence of 5+ consecutive 
#' minutes without movement. All other minutes are marked as "awake" (1).
#' This follows the standard Drosophila sleep definition.
#' 
#' @param df Data frame of activity data (rows = time, cols = flies)
#' @param threshold Number of consecutive zero-activity minutes for sleep (default 5)
#' @return Data frame with binary awake (1) / asleep (0) values
#' @export
activityToAwake <- function(df, threshold = 5) {
  
  result <- df
  
  result[] <- lapply(df, function(x) {
    # Use run-length encoding to find consecutive sequences
    rle_x <- rle(x)
    
    # Mark short sequences of zeros as "brief rest" (not sleep)
    rle_x$values <- ifelse(
      rle_x$values == 0 & rle_x$lengths < threshold,
      1,  # Not enough consecutive zeros to be sleep
      rle_x$values
    )
    
    # Convert back to vector
    result_vec <- inverse.rle(rle_x)
    
    # Binarize: any movement = awake (1), no movement = asleep (0)
    result_vec <- ifelse(result_vec != 0, 1, 0)
    
    return(result_vec)
  })
  
  return(result)
}

#' Convert awake binary to sleep binary
#' 
#' Simply flips the binary values: awake (1) -> sleep (0), asleep (0) -> sleep (1)
#' 
#' @param df Data frame with binary awake/asleep data
#' @return Data frame with binary sleep data (1 = sleeping)
#' @export
awakeToSleep <- function(df) {
  result <- df
  result[] <- lapply(df, function(x) {
    ifelse(x == 0, 1, 0)
  })
  return(result)
}

#' Flip binary values (0 <-> 1)
#' 
#' @param df Data frame with binary values
#' @return Data frame with flipped binary values
#' @export
flipBinary <- function(df) {
  result <- df
  result[] <- lapply(df, function(x) {
    ifelse(x == 0, 1, ifelse(x == 1, 0, x))
  })
  return(result)
}

#' Calculate position frequency distribution
#' 
#' Counts how many times each position (1-15) was occupied by each fly
#' 
#' @param df Data frame of position data
#' @param positions Vector of position values to count (default 1:15)
#' @return Data frame with frequency counts (rows = positions, cols = flies)
#' @export
positionFrequency <- function(df, positions = 1:15) {
  
  freq_list <- lapply(df, function(col) {
    # Count frequency of each position
    freq <- table(factor(col, levels = positions))
    return(as.vector(freq))
  })
  
  # Combine into data frame
  freq_df <- as.data.frame(do.call(cbind, freq_list))
  colnames(freq_df) <- colnames(df)
  rownames(freq_df) <- as.character(positions)
  
  return(freq_df)
}

#' Calculate proportion of time at each position
#' 
#' @param freq_df Frequency data from positionFrequency()
#' @return Data frame with proportions (each column sums to 1)
#' @export
positionProportion <- function(freq_df) {
  prop_df <- freq_df
  prop_df[] <- lapply(freq_df, function(x) {
    total <- sum(x, na.rm = TRUE)
    if (total > 0) x / total else rep(0, length(x))
  })
  return(prop_df)
}

#' Mask position data by awake status
#' 
#' Sets position to 0 when fly is asleep (useful for analyzing only awake positions)
#' 
#' @param position_df Data frame of position data
#' @param awake_df Data frame of binary awake data (1 = awake)
#' @return Data frame with positions only when awake
#' @export
maskByAwake <- function(position_df, awake_df) {
  if (!identical(dim(position_df), dim(awake_df))) {
    stop("Position and awake data frames must have same dimensions")
  }
  return(position_df * awake_df)
}

#' Bin time series data
#' 
#' Aggregates data into bins (e.g., 30-minute bins for circadian analysis)
#' 
#' @param df Data frame of time series data
#' @param bin_size Number of time points per bin (default 30 for 30-minute bins)
#' @param fun Aggregation function (default sum)
#' @return Data frame with binned data
#' @export
binTimeSeries <- function(df, bin_size = 30, fun = sum) {
  
  n_rows <- nrow(df)
  n_bins <- ceiling(n_rows / bin_size)
  
  # Create bin indices
  bin_idx <- rep(1:n_bins, each = bin_size)[1:n_rows]
  
  # Aggregate each column
  result <- aggregate(df, by = list(bin = bin_idx), FUN = fun)
  result$bin <- NULL  # Remove the bin column
  
  return(result)
}

#' Calculate daily summaries
#' 
#' Splits data by day and calculates summary statistics
#' 
#' @param df Data frame of time series data
#' @param minutes_per_day Minutes per day (default 1440)
#' @param fun Summary function (default mean)
#' @return List of data frames, one per day
#' @export
dailySummary <- function(df, minutes_per_day = 1440, fun = mean) {
  
  n_days <- ceiling(nrow(df) / minutes_per_day)
  
  results <- list()
  
  for (day in 1:n_days) {
    start_row <- (day - 1) * minutes_per_day + 1
    end_row <- min(day * minutes_per_day, nrow(df))
    
    day_data <- df[start_row:end_row, , drop = FALSE]
    
    results[[paste0("day", day)]] <- data.frame(
      day = day,
      t(sapply(day_data, fun, na.rm = TRUE))
    )
  }
  
  return(results)
}

#' Extract day/night data
#' 
#' Splits data into light and dark periods
#' 
#' @param df Data frame of time series data  
#' @param light_on Hour when lights turn on (default 0, i.e., ZT0)
#' @param light_off Hour when lights turn off (default 12, i.e., ZT12)
#' @param minutes_per_day Minutes per day (default 1440)
#' @return List with 'light' and 'dark' data frames
#' @export
splitLightDark <- function(df, light_on = 0, light_off = 12, minutes_per_day = 1440) {
  
  minutes_per_hour <- 60
  
  # Create time-of-day index (0-1439 for each day)
  time_of_day <- (seq_len(nrow(df)) - 1) %% minutes_per_day
  hour_of_day <- time_of_day / minutes_per_hour
  
  # Define light period
  if (light_on < light_off) {
    is_light <- hour_of_day >= light_on & hour_of_day < light_off
  } else {
    # Handles case where light period spans midnight
    is_light <- hour_of_day >= light_on | hour_of_day < light_off
  }
  
  return(list(
    light = df[is_light, , drop = FALSE],
    dark = df[!is_light, , drop = FALSE]
  ))
}

#' Normalize data
#' 
#' @param df Data frame to normalize
#' @param method Normalization method: "zscore", "minmax", or "proportion"
#' @return Normalized data frame
#' @export
normalizeData <- function(df, method = "zscore") {
  
  result <- df
  
  result[] <- lapply(df, function(x) {
    x <- as.numeric(x)
    
    switch(method,
      "zscore" = {
        m <- mean(x, na.rm = TRUE)
        s <- sd(x, na.rm = TRUE)
        if (s == 0) return(rep(0, length(x)))
        return((x - m) / s)
      },
      "minmax" = {
        mi <- min(x, na.rm = TRUE)
        ma <- max(x, na.rm = TRUE)
        if (ma == mi) return(rep(0.5, length(x)))
        return((x - mi) / (ma - mi))
      },
      "proportion" = {
        total <- sum(x, na.rm = TRUE)
        if (total == 0) return(rep(0, length(x)))
        return(x / total)
      },
      stop("Unknown normalization method: ", method)
    )
  })
  
  return(result)
}

# Legacy function aliases for backward compatibility
convert_sequences <- activityToAwake
flip_binary <- flipBinary
df_to_freq_dist <- positionFrequency
