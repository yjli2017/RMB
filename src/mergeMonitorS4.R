mergeMonitorS4 <- function(monitor_list) {
  # Check if all elements in the list are MonitorS4 objects
  if (!all(sapply(monitor_list, is, "MonitorS4"))) {
    stop("All elements in the list must be MonitorS4 objects.")
  }
  
  # Initialize empty objects for the slots
  meta.data <- data.frame()
  assays <- list(mt = data.frame(), ct = data.frame(), pn = data.frame())
  active.assay <- data.frame()
  time <- data.frame()
  
  # Loop through the list and merge the slots
  for (monitor in monitor_list) {
    meta.data <- rbind(meta.data, slot(monitor, "meta.data"))
    
    # Check if the data frames in assays are empty before binding
    assays$mt <- if (ncol(assays$mt) == 0) monitor@assays$mt else cbind(assays$mt, monitor@assays$mt)
    assays$ct <- if (ncol(assays$ct) == 0) monitor@assays$ct else cbind(assays$ct, monitor@assays$ct)
    assays$pn <- if (ncol(assays$pn) == 0) monitor@assays$pn else cbind(assays$pn, monitor@assays$pn)
    
    active.assay <- rbind(active.assay, slot(monitor, "active.assay"))
    time <- monitor@time
  }
  
  # Create a new MonitorS4 object with the merged slots
  combined_monitor <- new("MonitorS4", meta.data = meta.data, assays = assays, active.assay = active.assay, time = time)
  
  return(combined_monitor)
}