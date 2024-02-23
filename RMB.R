# Author: Yongjun Li 2024-02-15

################################################################################
# Load the config
base_dir <- "C:/Users/yongj/OneDrive/Desktop/20240221_RMB"
setwd(base_dir)
source("./config.R")
source("./metadata.R")

################################################################################
# Update the metadata
# source("./update_metadata.R")

################################################################################
data_dir = "./data/0502pn"
date <- "20230502"
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
source("./src/process_monitorS4_data.R")
for (i in 1:length(monitor_list)) {
  monitor_list[[i]] <- process_monitorS4_data(monitor_list[[i]])
}

################################################################################
# select time range
source("./src/select_time_range.R")
monitor_list <- select_time_range(monitor_list)
################################################################################
# merge the data
source("./src/mergeMonitorS4.R")
monitor <- mergeMonitorS4(monitor_list)
################################################################################
# get rid of dead flies
source("./src/remove_dead_flies.R")
monitor <- remove_dead_flies(monitor)
################################################################################
# heatmap of mt
# monitor@assays$mt
dat <- as.matrix(monitor@assays$mt)
colnames(dat) <- colnames(monitor@assays$mt)

# Create a factor that divides the rows into groups of 1440
row_groups <- as.factor((seq_len(nrow(dat)) - 1) %/% 1440 + 1)

ht <- Heatmap(dat,
        name = "mt",
        col = colorRamp2(c(0, 1, 10), c("black", "white", "red")),
        cluster_rows = FALSE,
        cluster_columns = FALSE,
        row_names_gp = gpar(fontsize = 0),
        column_names_gp = gpar(fontsize = 0),
        column_split = monitor@meta.data$Phenotype,
        row_split = row_groups)
# ht
png(file=file.path(data_dir, "heatmap_mt.png"), width=8000, height=4000, res=300)
draw(ht)
dev.off()

################################################################################
# heatmap of pn
dat <- as.matrix(monitor@assays$pn)
colnames(dat) <- colnames(monitor@assays$pn)

# Create a factor that divides the rows into groups of 1440
row_groups <- as.factor((seq_len(nrow(dat)) - 1) %/% 1440 + 1)

ht <- Heatmap(dat,
        name = "pn",
        col = colorRamp2(c(1, 8, 15), c("blue", "white", "red")),
        cluster_rows = FALSE,
        cluster_columns = FALSE,
        row_names_gp = gpar(fontsize = 0),
        column_names_gp = gpar(fontsize = 0),
        column_split = monitor@meta.data$Phenotype,
        row_split = row_groups)

png(file=file.path(data_dir, "heatmap_pn.png"), width=8000, height=4000, res=300)
draw(ht)
dev.off()

################################################################################
# sleep analysis
source("./src/convert_sequence.R")
monitor@assays$awake_mt <- convert_sequences(monitor@assays$mt)
# View(monitor@assays$awake_mt)
source("./src/flip_binary.R")
monitor@assays$sleep_mt <- flip_binary(monitor@assays$awake_mt)

################################################################################
# heatmap of sleep
dat <- as.matrix(monitor@assays$sleep_mt)
colnames(dat) <- colnames(monitor@assays$sleep_mt)

# Create a factor that divides the rows into groups of 1440
row_groups <- as.factor((seq_len(nrow(dat)) - 1) %/% 1440 + 1)

ht <- Heatmap(dat,
              name = "sleep_mt",
              col = colorRamp2(c(0,1), c("white", "red")),
              cluster_rows = FALSE,
              cluster_columns = FALSE,
              row_names_gp = gpar(fontsize = 0),
              column_names_gp = gpar(fontsize = 0),
              column_split = monitor@meta.data$Phenotype,
              row_split = row_groups)

png(file=file.path(data_dir, "heatmap_sleep.png"), width=8000, height=4000, res=300)
draw(ht)
dev.off()

################################################################################
# only position when flies are awake
# Perform the operation
monitor@assays$pn_awake <- monitor@assays$pn * monitor@assays$awake_mt
dat <- as.matrix(monitor@assays$pn_awake)
colnames(dat) <- colnames(monitor@assays$pn_awake)

# Create a factor that divides the rows into groups of 1440
row_groups <- as.factor((seq_len(nrow(dat)) - 1) %/% 1440 + 1)

ht <- Heatmap(dat,
              name = "pn_awake",
              col = colorRamp2(c(0, 1, 15), c("white", "blue", "red")),
              cluster_rows = FALSE,
              cluster_columns = FALSE,
              row_names_gp = gpar(fontsize = 0),
              column_names_gp = gpar(fontsize = 0),
              column_split = monitor@meta.data$Phenotype,
              row_split = row_groups)

png(file=file.path(data_dir, "heatmap_pn_awake.png"), width=8000, height=4000, res=300)
draw(ht)
dev.off()

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
################################################################################





