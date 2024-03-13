# function to plot time series with 1440 intervals
plot_time_series <- function(time_series_column, ylab = "Value") {
    df <- data.frame(Time = 1:length(time_series_column),
                     Value = time_series_column)
    p <- ggplot(df, aes(x = Time, y = Value)) +
        geom_line() +
        ylab(ylab)
        
    return(p)
}
