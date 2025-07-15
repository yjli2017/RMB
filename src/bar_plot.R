# Bar Plot Functions
# This file contains the bar plot functions referenced in the main scripts

# Load required packages
if (!require("dplyr", quietly = TRUE)) {
  install.packages("dplyr")
  library(dplyr)
}

if (!require("tidyr", quietly = TRUE)) {
  install.packages("tidyr")
  library(tidyr)
}

if (!require("ggplot2", quietly = TRUE)) {
  install.packages("ggplot2")
  library(ggplot2)
}

# Suppress warnings about variable bindings
utils::globalVariables(c("category", "value", "fly", "phenotype", "proportion", "avg_proportion", "std_error"))

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
