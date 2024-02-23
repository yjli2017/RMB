# Author: Yongjun Li 2024-02-15

################################################################################
# Load the config
source("./config/config.R")
################################################################################
# Update the metadata
# Run the generate_metadata_test.R first
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
source("./src/process_monitorS4_data.R")
for (i in 1:length(monitor_list)) {
  monitor_list[[i]] <- process_monitorS4_data(monitor_list[[i]])
}
################################################################################
# select time range based on light on and off
source("./src/select_time_range.R")
plot_light_switches(monitor_list)
# determine the range based on plot, change m and n accordingly
m <- 4
n <- 10
monitor_list <- select_time_range(monitor_list, m, n)
plot_light_switches(monitor_list)
################################################################################
# merge the data
source("./src/mergeMonitorS4.R")
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
################################################################################
# save monitor object
saveRDS(monitor, file = file.path(data_dir, "monitor.rds"))