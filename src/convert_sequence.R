# Convert Sequences Function
# This file contains the convert_sequences function

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
