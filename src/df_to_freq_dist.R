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