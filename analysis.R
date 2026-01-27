# RMB Analysis Script
# Clean, refactored version of the main analysis pipeline
# Author: Yongjun Li
# Optimized by Catbot 2026-01-27

################################################################################
# Configuration - EDIT THESE PATHS
################################################################################

# Set your RMB directory
base_dir <- "/home/liy27/projects/RMB"

# Set your data directory  
data_dir <- "/home/liy27/projects/RMB/data"

# Experiment date (for metadata)
experiment_date <- "20230410"

################################################################################
# Setup
################################################################################

setwd(base_dir)
source("./config/config.R")
load_rmb(base_dir, verbose = TRUE)
load_required_packages(verbose = TRUE)

# Load ComplexHeatmap for heatmaps
if (require("ComplexHeatmap", quietly = TRUE)) {
  library(circlize)
} else {
  warning("ComplexHeatmap not available - heatmaps will be skipped")
}

################################################################################
# Load Data
################################################################################

cat("\n=== Loading Data ===\n")

# Find monitor files
monitor_files <- load_monitor_files(data_dir)
cat("Found", length(monitor_files), "monitor files\n")

# Create MonitorS4 objects
monitor_list <- lapply(monitor_files, function(f) {
  m <- createMonitor(f, verbose = TRUE)
  m <- processMonitor(m, verbose = FALSE)
  return(m)
})

################################################################################
# Load Metadata
################################################################################

cat("\n=== Loading Metadata ===\n")

metadata_dir <- file.path(data_dir, "metadata")
if (dir.exists(metadata_dir)) {
  metadata_files <- list.files(metadata_dir, pattern = "metadata\\.csv$", 
                               full.names = TRUE)
  
  for (i in seq_along(metadata_files)) {
    if (i <= length(monitor_list)) {
      monitor_list[[i]]@meta.data <- read.csv(metadata_files[i], 
                                               stringsAsFactors = FALSE)
      cat("Loaded metadata for", monitor_list[[i]]@monitor_name, "\n")
    }
  }
} else {
  warning("No metadata directory found at: ", metadata_dir)
}

################################################################################
# Merge Monitors
################################################################################

cat("\n=== Merging Monitors ===\n")

monitor <- mergeMonitors(monitor_list, verbose = TRUE)

################################################################################
# Remove Dead Flies
################################################################################

cat("\n=== Removing Dead Flies ===\n")

monitor <- removeDeadFlies(monitor, min_movement = MIN_MOVEMENT_THRESHOLD, 
                           verbose = TRUE)

################################################################################
# Generate Heatmaps
################################################################################

cat("\n=== Generating Heatmaps ===\n")

if (exists("Heatmap")) {
  
  # Movement (MT) heatmap
  if (!is.null(monitor@assays$mt)) {
    dat <- as.matrix(monitor@assays$mt)
    
    # Split by day (1440 minutes)
    row_groups <- factor((seq_len(nrow(dat)) - 1) %/% MINUTES_PER_DAY + 1)
    
    # Get phenotype for column split if available
    col_split <- NULL
    if (nrow(monitor@meta.data) > 0 && "Phenotype" %in% colnames(monitor@meta.data)) {
      col_split <- monitor@meta.data$Phenotype
    }
    
    ht <- Heatmap(dat,
      name = "Activity",
      col = colorRamp2(c(0, 1, 10), c("black", "white", "red")),
      cluster_rows = FALSE,
      cluster_columns = FALSE,
      row_names_gp = gpar(fontsize = 0),
      column_names_gp = gpar(fontsize = 0),
      column_split = col_split,
      row_split = row_groups
    )
    
    png(file.path(data_dir, "heatmap_activity.png"), 
        width = 8000, height = 4000, res = 300)
    draw(ht)
    dev.off()
    cat("Saved: heatmap_activity.png\n")
  }
  
  # Position (Pn) heatmap
  if (!is.null(monitor@assays$pn)) {
    dat <- as.matrix(monitor@assays$pn)
    row_groups <- factor((seq_len(nrow(dat)) - 1) %/% MINUTES_PER_DAY + 1)
    
    ht <- Heatmap(dat,
      name = "Position",
      col = colorRamp2(c(1, 8, 15), c("blue", "white", "red")),
      cluster_rows = FALSE,
      cluster_columns = FALSE,
      row_names_gp = gpar(fontsize = 0),
      column_names_gp = gpar(fontsize = 0),
      column_split = col_split,
      row_split = row_groups
    )
    
    png(file.path(data_dir, "heatmap_position.png"), 
        width = 8000, height = 4000, res = 300)
    draw(ht)
    dev.off()
    cat("Saved: heatmap_position.png\n")
  }
}

################################################################################
# Sleep Analysis
################################################################################

cat("\n=== Sleep Analysis ===\n")

# Convert activity to awake/sleep
monitor@assays$awake <- activityToAwake(monitor@assays$mt, threshold = SLEEP_THRESHOLD)
monitor@assays$sleep <- awakeToSleep(monitor@assays$awake)

# Mask position by awake status
if (!is.null(monitor@assays$pn)) {
  monitor@assays$pn_awake <- maskByAwake(monitor@assays$pn, monitor@assays$awake)
}

cat("Sleep data calculated\n")

# Sleep heatmap
if (exists("Heatmap") && !is.null(monitor@assays$sleep)) {
  dat <- as.matrix(monitor@assays$sleep)
  row_groups <- factor((seq_len(nrow(dat)) - 1) %/% MINUTES_PER_DAY + 1)
  
  ht <- Heatmap(dat,
    name = "Sleep",
    col = colorRamp2(c(0, 1), c("white", "darkblue")),
    cluster_rows = FALSE,
    cluster_columns = FALSE,
    row_names_gp = gpar(fontsize = 0),
    column_names_gp = gpar(fontsize = 0),
    column_split = col_split,
    row_split = row_groups
  )
  
  png(file.path(data_dir, "heatmap_sleep.png"), 
      width = 8000, height = 4000, res = 300)
  draw(ht)
  dev.off()
  cat("Saved: heatmap_sleep.png\n")
}

################################################################################
# Position Frequency Analysis
################################################################################

cat("\n=== Position Frequency Analysis ===\n")

if (!is.null(monitor@assays$pn_awake)) {
  
  # Calculate frequency distribution (all data)
  pn_freq <- positionFrequency(monitor@assays$pn_awake)
  write.csv(pn_freq, file.path(data_dir, "position_freq_all.csv"))
  cat("Saved: position_freq_all.csv\n")
  
  # Calculate for each day
  n_days <- ceiling(nrow(monitor@assays$pn_awake) / MINUTES_PER_DAY)
  
  for (day in 1:n_days) {
    start_row <- (day - 1) * MINUTES_PER_DAY + 1
    end_row <- min(day * MINUTES_PER_DAY, nrow(monitor@assays$pn_awake))
    
    day_data <- monitor@assays$pn_awake[start_row:end_row, , drop = FALSE]
    day_freq <- positionFrequency(day_data)
    
    write.csv(day_freq, file.path(data_dir, paste0("position_freq_day", day, ".csv")))
    cat("Saved: position_freq_day", day, ".csv\n")
  }
  
  # Create bar plots if metadata has grouping
  if (nrow(monitor@meta.data) > 0) {
    group_col <- if ("Phenotype" %in% colnames(monitor@meta.data)) "Phenotype" else
                 if ("Group" %in% colnames(monitor@meta.data)) "Group" else NULL
    
    if (!is.null(group_col)) {
      df_long <- prepareBarPlotData(monitor, pn_freq, data_dir, group_col)
      results <- calculateAvgProportion(df_long, target_category = "1")
      
      createBarPlot(results$df_avg, results$df_filtered,
                    output_file = file.path(data_dir, "barplot_position1_all.png"),
                    title = "Position 1 Frequency (All Days)")
      cat("Saved: barplot_position1_all.png\n")
    }
  }
}

################################################################################
# Save Processed Data
################################################################################

cat("\n=== Saving Processed Data ===\n")

saveRDS(monitor, file.path(data_dir, "monitor_processed.rds"))
cat("Saved: monitor_processed.rds\n")

cat("\n=== Analysis Complete ===\n")
