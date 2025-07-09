# Author: Yongjun Li 2024-02-15

################################################################################
# Load the config and modify here as needed
base_dir <- "/scr1/users/liy27/20250709_RMB/dep/RMB" # change this to where you own RMB path
setwd(base_dir)
data_dir <- "./test/" # change this to where you data is
date <- "20250709"
################################################################################
source("./config/config.R")
# Source only the essential scripts initially
source("./src/metadata.R")
source("./src/metadata_workflow.R")

# Source essential processing scripts
source("./src/process_monitorS4_data.R")
source("./src/data_transformations.R")

tryCatch({
  source("./src/experiment_processing.R") 
}, error = function(e) {
  cat("Note: experiment_processing.R not loaded:", e$message, "\n")
})

tryCatch({
  source("./src/plotting_utilities.R")
}, error = function(e) {
  cat("Note: plotting_utilities.R not loaded:", e$message, "\n")
})

################################################################################
# Generate metadata first using the new workflow
cat("Generating metadata files...\n")
source("./generate_metadata_test.R")

################################################################################
# Update the metadata
# source("./update_metadata.R")

################################################################################
# Load data
# Get a list of all the .txt files in the directory
monitor_files <- load_monitor_files(data_dir)
cat("Found", length(monitor_files), "monitor files:\n")
for (i in 1:length(monitor_files)) {
  cat("  ", i, ":", basename(monitor_files[i]), "\n")
}

# Check if create_monitorS4 function exists, if not it's already loaded from process_monitorS4_data.R
if (!exists("create_monitorS4")) {
  stop("create_monitorS4 function not found even after sourcing process_monitorS4_data.R")
}

# Create monitor list properly
monitor_list <- list()
for (i in 1:length(monitor_files)) {
  monitor_list[[i]] <- create_monitorS4(monitor_files[i])
}

cat("Created monitor_list with", length(monitor_list), "elements\n")

################################################################################
# Load metadata files
metadata_files <- list.files(path = file.path(data_dir, "metadata/"),
                            pattern = "metadata", full.names = TRUE)

if (length(metadata_files) > 0) {
  cat("Found", length(metadata_files), "metadata files\n")
  
  for (i in 1:min(length(metadata_files), length(monitor_list))) {
    metadata <- read.csv(metadata_files[i])
    monitor_list[[i]]$meta.data <- metadata
    cat("Loaded metadata for monitor", i, "- dimensions:", dim(metadata), "\n")
  }
  
  if (length(monitor_list) > 0 && !is.null(monitor_list[[1]]$meta.data)) {
    cat("Sample metadata columns:", paste(colnames(monitor_list[[1]]$meta.data), collapse = ", "), "\n")
  }
} else {
  cat("Warning: No metadata files found in", file.path(data_dir, "metadata/"), "\n")
}
################################################################################
# process the data
if (file.exists("./src/process_monitorS4_data.R")) {
  source("./src/process_monitorS4_data.R")
  for (i in 1:length(monitor_list)) {
    monitor_list[[i]] <- process_monitorS4_data(monitor_list[[i]])
  }
} else {
  cat("Warning: process_monitorS4_data.R not found. Skipping data processing step.\n")
}

################################################################################
# select time range
if (file.exists("./src/select_time_range.R")) {
  source("./src/select_time_range.R")
  monitor_list <- select_time_range(monitor_list)
} else {
  cat("Warning: select_time_range.R not found. Skipping time range selection.\n")
}

################################################################################
# merge the data
if (file.exists("./src/mergeMonitorS4.R")) {
  source("./src/mergeMonitorS4.R")
  monitor <- mergeMonitorS4(monitor_list)
} else {
  cat("Warning: mergeMonitorS4.R not found. Cannot proceed with data merging.\n")
  cat("This is required for the visualization steps.\n")
  stop("Missing required mergeMonitorS4.R function")
}

################################################################################
# get rid of dead flies
if (file.exists("./src/remove_dead_flies.R")) {
  source("./src/remove_dead_flies.R")
  monitor <- remove_dead_flies(monitor)
} else {
  cat("Warning: remove_dead_flies.R not found. Skipping dead fly removal.\n")
}
################################################################################
# heatmap of mt
dat <- as.matrix(monitor$assays$mt)
colnames(dat) <- colnames(monitor$assays$mt)

# Create a factor that divides the rows into groups of 1440
row_groups <- as.factor((seq_len(nrow(dat)) - 1) %/% 1440 + 1)

ht <- Heatmap(dat,
        name = "mt",
        col = colorRamp2(c(0, 1, 10), c("black", "white", "red")),
        cluster_rows = FALSE,
        cluster_columns = FALSE,
        row_names_gp = gpar(fontsize = 0),
        column_names_gp = gpar(fontsize = 0),
        column_split = monitor$meta.data$Group,
        row_split = row_groups)
# ht
png(file=file.path(data_dir, "heatmap_mt.png"), width=8000, height=4000, res=300)
draw(ht)
dev.off()

################################################################################
# heatmap of pn
dat <- as.matrix(monitor$assays$pn)
colnames(dat) <- colnames(monitor$assays$pn)

# Create a factor that divides the rows into groups of 1440
row_groups <- as.factor((seq_len(nrow(dat)) - 1) %/% 1440 + 1)

ht <- Heatmap(dat,
        name = "pn",
        col = colorRamp2(c(1, 8, 15), c("blue", "white", "red")),
        cluster_rows = FALSE,
        cluster_columns = FALSE,
        row_names_gp = gpar(fontsize = 0),
        column_names_gp = gpar(fontsize = 0),
        column_split = monitor$meta.data$Group,
        row_split = row_groups)

png(file=file.path(data_dir, "heatmap_pn.png"), width=8000, height=4000, res=300)
draw(ht)
dev.off()

################################################################################
# sleep analysis
monitor$assays$awake_mt <- convert_sequences(monitor$assays$mt)
monitor$assays$sleep_mt <- flip_binary(monitor$assays$awake_mt)

################################################################################
# heatmap of sleep
dat <- as.matrix(monitor$assays$sleep_mt)
colnames(dat) <- colnames(monitor$assays$sleep_mt)

# Create a factor that divides the rows into groups of 1440
row_groups <- as.factor((seq_len(nrow(dat)) - 1) %/% 1440 + 1)

ht <- Heatmap(dat,
              name = "sleep_mt",
              col = colorRamp2(c(0,1), c("white", "red")),
              cluster_rows = FALSE,
              cluster_columns = FALSE,
              row_names_gp = gpar(fontsize = 0),
              column_names_gp = gpar(fontsize = 0),
              column_split = monitor$meta.data$Group,
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
              column_split = monitor@meta.data$Group,
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





