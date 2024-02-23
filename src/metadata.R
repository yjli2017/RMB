# Function to load monitor files
load_monitor_files <- function(data_dir) {
  monitor_list <- list.files(path = data_dir, pattern = "Monitor", full.names = TRUE)
  monitorLC_path <- list.files(path = data_dir, pattern = "MonitorLC", full.names = TRUE)
  monitor_list <- setdiff(monitor_list, monitorLC_path)
  return(monitor_list)
}

# Function to create metadata list
create_metadata_list <- function(monitor_list) {
  metadata <- read.csv("./template/monitor_metadata_template.csv")
  metadata_list <- replicate(length(monitor_list), metadata, simplify = FALSE)
  return(metadata_list)
}

# Function to update metadata
update_metadata <- function(monitor_list, metadata_list, Date, tube_type, User, Experiment) {
  for (i in 1:length(monitor_list)) {
    metadata_list[[i]]$Monitor_number <- tools::file_path_sans_ext(basename(monitor_list[i]))  
    metadata_list[[i]]$Date <- Date
    metadata_list[[i]]$Tube_type <- tube_type
    metadata_list[[i]]$User <- User
    metadata_list[[i]]$Temperature <- 25
    metadata_list[[i]]$Experiment_name <- Experiment
    metadata_list[[i]]$Fly <- paste(metadata_list[[i]]$Lab,
                                    metadata_list[[i]]$User,
                                    metadata_list[[i]]$Date,
                                    metadata_list[[i]]$Experiment_name,
                                    metadata_list[[i]]$Monitor_number,
                                    metadata_list[[i]]$Channel,
                                    sep = "_")
  }
  return(metadata_list)
}

# Function to write metadata
write_metadata <- function(data_dir, monitor_list, metadata_list) {
  # Create the metadata directory if it doesn't exist
  dir_path <- file.path(data_dir, "metadata")
  if (!dir.exists(dir_path)) {
    dir.create(dir_path, recursive = TRUE)
  }
  
  # Write each metadata to a CSV file
  for (i in 1:length(monitor_list)) {
    write.csv(metadata_list[[i]], file = paste0(dir_path, "/", tools::file_path_sans_ext(basename(monitor_list[i])), "_metadata.csv"), row.names = FALSE)
  }
}