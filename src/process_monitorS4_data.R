process_monitorS4_data <- function(monitor) {
  # Filter the raw_data based on the value of V8 and assign to new assay types
  monitor@assays$mt <- monitor@assays$raw_data[monitor@assays$raw_data$V8 == "MT",]
  monitor@assays$ct <- monitor@assays$raw_data[monitor@assays$raw_data$V8 == "CT",]
  monitor@assays$pn <- monitor@assays$raw_data[monitor@assays$raw_data$V8 == "Pn",]
  
  # Assign the first 10 columns of mt assay to time
  monitor@time <- monitor@assays$mt[,1:10]
  
  # Remove the first 10 columns from mt, ct, and pn assays
  monitor@assays$mt <- monitor@assays$mt[,11:ncol(monitor@assays$mt)]
  monitor@assays$ct <- monitor@assays$ct[,11:ncol(monitor@assays$ct)]
  monitor@assays$pn <- monitor@assays$pn[,11:ncol(monitor@assays$pn)]
  
  # Update the column names of mt, ct, and pn assays with the Fly column from meta.data
  colnames(monitor@assays$mt) <- monitor@meta.data$Fly
  colnames(monitor@assays$ct) <- monitor@meta.data$Fly
  colnames(monitor@assays$pn) <- monitor@meta.data$Fly
  
  # Return the processed MonitorS4 object
  return(monitor)
}