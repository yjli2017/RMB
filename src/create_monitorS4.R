# Yongjun Li 2024-02-15
# Create a MonitorS4 object from the single raw monitor data

create_monitorS4 <- function(monitor_path){
  # Define the class
  setClass(
    "MonitorS4",
    representation(
      meta.data = "data.frame", # rows are samples, columns are metadata
      assays = "list", # list of data.frames, one for each assay
      active.assay = "data.frame", 
      time = "data.frame"
    )
  )
  
  # Define the constructor function
  MonitorS4 <- function(data) {
    meta.data <- data.frame()
    assays <- list(raw_data = data)
    active.assay <- data.frame()
    time <- data.frame()
    
    if (!is(meta.data, "data.frame")) stop("meta.data must be a data.frame")
    if (!is(assays, "list")) stop("assays must be a list")
    if (!is(active.assay, "data.frame")) stop("active.assay must be a data.frame")
    if (!is(time, "data.frame")) stop("time must be a data.frame")
    
    new("MonitorS4", meta.data = meta.data, assays = assays, active.assay = active.assay, time = time)
  }
  
  # Load data
  data <- read.table(monitor_path, header = FALSE, sep = "\t")
  
  # Create MonitorS4 object
  monitor <- tryCatch(
    MonitorS4(data),
    error = function(e) {
      stop("Failed to load data: ", e)
    }
  )
  
  print("Monitor data loaded successfully")
  return(monitor)
}