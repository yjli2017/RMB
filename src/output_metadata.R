# output the metadata
dir_path <- file.path(data_dir, "metadata")
if (!dir.exists(dir_path)) {
  dir.create(dir_path, recursive = TRUE)
}

for (i in 1:length(monitor_list)) {
  write.csv(metadata_list[[i]], file = paste0(dir_path, "/", tools::file_path_sans_ext(basename(monitor_list[i])), "_metadata.csv"), row.names = FALSE)
}