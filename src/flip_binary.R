flip_binary <- function(df) {
  df[] <- lapply(df, function(x) ifelse(x == 0, 1, ifelse(x == 1, 0, x)))
  return(df)
}