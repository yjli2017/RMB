# Author: Yongjun Li 2024-02-15

################################################################################
# Load the config
source("./config/config.R")

################################################################################
# Update the metadata

################################################################################
data_dir = "./test"
# Load data
# Get a list of all the .txt files in the directory
monitor_list <- load_monitor_files(data_dir)
monitor_list <- sapply(monitor_list, create_monitorS4)
################################################################################
# Run metadata R first
metadata_list <- list()
metadata_list <- list.files(path = file.path(data_dir, "metadata/"),
                            pattern = "metadata", full.names = TRUE)

for (i in 1:length(metadata_list)) {
  monitor_list[[i]]@meta.data <- read.csv(metadata_list[i])
}

dim(monitor_list[[1]]@meta.data)
################################################################################
# process the data
for (i in 1:length(monitor_list)) {
  monitor_list[[i]] <- process_monitorS4_data(monitor_list[[i]])
}
################################################################################
# select time range based on light on and off
source("./src/select_time_range.R")
monitor_list <- select_time_range(monitor_list)
################################################################################
# merge the data
monitor <- mergeMonitorS4(monitor_list)
################################################################################
# get rid of dead flies
source("./src/remove_dead_flies.R")
monitor <- remove_dead_flies(monitor)

################################################################################
# sleep analysis
source("./src/convert_sequence.R")
monitor@assays$awake_mt <- convert_sequences(monitor@assays$mt)
# View(monitor@assays$awake_mt)
source("./src/flip_binary.R")
monitor@assays$sleep_mt <- flip_binary(monitor@assays$awake_mt)

################################################################################
# only position when flies are awake
# Perform the operation
monitor@assays$pn_awake <- monitor@assays$pn * monitor@assays$awake_mt
dat <- as.matrix(monitor@assays$pn_awake)
colnames(dat) <- colnames(monitor@assays$pn_awake)

################################################################################
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

