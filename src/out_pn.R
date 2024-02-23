dat <- as.matrix(monitor@assays$pn_awake)
colnames(dat) <- colnames(monitor@assays$pn_awake)
source("./src/df_to_freq_dist.R")
source("./src/bar_plot.R")
pn_awake_freq <- df_to_freq_dist(monitor@assays$pn_awake)
colnames(pn_awake_freq) <- colnames(monitor@assays$pn_awake)
rownames(pn_awake_freq) <- c(1:15)
# pn_awake_freq[16,] <- monitor@meta.data$Phenotype
write.csv(pn_awake_freq, file=file.path(data_dir, "pn_awake_freq_all.csv"))
# Prepare data
df_long <- prepare_data(monitor, pn_awake_freq, data_dir)
results <- calculate_avg_proportion(df_long)
create_bar_plot(results$df_avg, results$df_cat1, file.path(data_dir, "barplot_pn1_awake_all.png"))

# Process each day separately
for (day in 1:5) {
  start_row <- (day - 1) * 1440 + 1
  end_row <- day * 1440
  pn_awake_freq <- df_to_freq_dist(monitor@assays$pn_awake[start_row:end_row,])
  colnames(pn_awake_freq) <- colnames(monitor@assays$pn_awake)
  rownames(pn_awake_freq) <- c(1:15)
  write.csv(pn_awake_freq, file=file.path(data_dir, paste0("pn_awake_freq_day", day, ".csv")))
  df_long <- prepare_data(monitor, pn_awake_freq, data_dir)
  results <- calculate_avg_proportion(df_long)
  create_bar_plot(results$df_avg, results$df_cat1, file.path(data_dir, paste("barplot_pn1_awake_day", day, ".png")))
}