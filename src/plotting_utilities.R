# Plotting Utilities
# Combined functions for data visualization

library(tidyverse)
library(ggplot2)

# Function to prepare data for bar plots
prepare_data <- function(monitor, pn_awake_freq, data_dir) {
  df <- t(pn_awake_freq)
  df <- as.data.frame(df)
  df$fly <- rownames(df)
  df$phenotype <- monitor$meta.data$Group
  
  df_long <- df %>% 
    pivot_longer(cols = 1:15, names_to = "category", values_to = "value") %>%
    mutate(category = factor(category, levels = unique(category))) %>%
    mutate(value = as.numeric(value)) %>%
    group_by(fly) %>%
    mutate(proportion = value / sum(value))
  
  write.csv(df_long, file=file.path(data_dir, "pn_awake_freq_long.csv"))
  
  df_long
}

# Function to calculate average proportion for bar plots
calculate_avg_proportion <- function(df_long) {
  df_cat1 <- df_long %>% filter(category == 1)
  
  df_avg <- df_cat1 %>%
    group_by(phenotype) %>%
    summarise(
      avg_proportion = mean(proportion, na.rm = TRUE),
      std_error = sd(proportion, na.rm = TRUE)/sqrt(n())
    )
  
  list(df_avg = df_avg, df_cat1 = df_cat1)
}

# Function to create bar plot
create_bar_plot <- function(df_avg, df_cat1, plot_file) {
  plot <- ggplot(df_avg, aes(x = phenotype, y = avg_proportion)) +
    geom_bar(aes(fill = phenotype), stat = 'identity') +
    geom_errorbar(
      aes(ymin = avg_proportion - std_error, ymax = avg_proportion + std_error),
      width = 0.2
    ) + 
    geom_point(aes(y = proportion), df_cat1, color = "black") +
    theme(axis.text.x = element_blank())
  
  ggsave(filename = plot_file, plot = plot, dpi = 300, width = 10, height = 10)
}

# Function to plot time series with 1440 intervals
plot_time_series <- function(time_series_column, ylab = "Value") {
  df <- data.frame(Time = 1:length(time_series_column),
                   Value = time_series_column)
  p <- ggplot(df, aes(x = Time, y = Value)) +
    geom_line() +
    ylab(ylab)
    
  return(p)
}

# Function to plot data for every 30 mins (sleep analysis)
plot_every_30 <- function(x, y) {
  # x is a table and its three columns are Time, Genotype, Micromovements
  # y is the title of the plot
  # x is in long format
  ggplot(x, aes(Time, Micromovements, color = Genotype)) + 
    geom_line() +
    ggtitle("") +
    scale_x_continuous(breaks = c(0, 6, 12, 18, 24)) +
    theme_bw() +
    geom_vline(xintercept = 12, color = "red", linetype = "dotted") +
    ggtitle(y)
}

# Function to plot light switches for time range selection
plot_light_switches <- function(monitor_list) {
  # Find the indices where the light switches
  lightSwitchIndices <- which(diff(monitor_list[[1]]@time$V10) != 0)
  print(lightSwitchIndices)
  
  # Create a data frame for plotting
  df <- data.frame(Time = 1:length(monitor_list[[1]]@time$V10),
                   Value = monitor_list[[1]]@time$V10)

  # Create a data frame for the switch numbers
  switch_df <- data.frame(Switch = 1:length(lightSwitchIndices),
                          Time = lightSwitchIndices,
                          Value = monitor_list[[1]]@time$V10[lightSwitchIndices])

  ggplot(df, aes(x = Time, y = Value)) +
    geom_line() +
    geom_vline(data = switch_df, aes(xintercept = Time), color = "red", linetype = "dashed") +
    geom_text(data = switch_df, aes(label = Switch), vjust = -1) +
    ylab("Light on/off")
}
