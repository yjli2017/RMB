# Data Transformation Utilities
# Combined functions for data transformation and conversion

# Function to convert sequences of five or more continuous zeros to 0, and all other values to 1
convert_sequences <- function(df) {
  df[] <- lapply(df, function(x) {
    rle_x <- rle(x)
    rle_x$values <- ifelse(rle_x$values == 0 & rle_x$lengths < 5, 1, rle_x$values)
    inverse.rle(rle_x)
  })
  df[] <- lapply(df, function(x) ifelse(x != 0, 1, x))
  return(df)
}

# Function to flip binary values (0 to 1, 1 to 0)
flip_binary <- function(df) {
  df[] <- lapply(df, function(x) ifelse(x == 0, 1, ifelse(x == 1, 0, x)))
  return(df)
}

# Function to convert dataframe to frequency distribution
df_to_freq_dist <- function(df) {
  freq_dist_list <- lapply(df, function(col) {
    # Count the frequency of numbers from 1 to 15
    freq_dist <- table(factor(col, levels = 1:15))
    # Convert the table to a data frame
    freq_dist_df <- data.frame(Counts = as.vector(freq_dist))
    return(freq_dist_df)
  })
  # Combine the data frames
  freq_dist_df <- do.call(cbind, freq_dist_list)
  return(freq_dist_df)
}
