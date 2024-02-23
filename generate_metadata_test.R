# Create or update the the metadata for the test data

# load the config file
source("./config/config.R")

################################################################################
# update as you need
Date <- "20230606"
tube_type <- "normal"
User <- "yjli"
Experiment <- "test"
################################################################################
# test
data_dir <- "./test"
metadata_info <- read.csv("./test/metadata_info.csv")

monitor_list <- load_monitor_files(data_dir)
print(monitor_list)
print(length(monitor_list))
metadata_list <- create_metadata_list(monitor_list)
metadata_list <- update_metadata(monitor_list, metadata_list, Date, tube_type, User, Experiment)

# load metadata_info
metadata_info <- read.csv("./test/metadata_info.csv")

# modify as you can or manually update metadata_
# Update metadata_list with information from metadata_info
for (i in 1:6) {
  metadata_list[[i]]$Monitor_number <- metadata_info$Monitor_number[i]
  metadata_list[[i]]$Phenotype <- metadata_info$Phenotype[i]
}

write_metadata(data_dir, monitor_list, metadata_list)
