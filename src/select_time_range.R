select_time_range <- function(monitor_list) {
  par(mfrow = c(1, 2))
  # plot light on and off
  plot(monitor_list[[1]]@time$V10,
       ylab = "Light on/off")
  # select the 2-5 full days for further analysis
  lightOnOff <- which(diff(monitor_list[[1]]@time$V10) != 0)
  print(lightOnOff)
  selected_range <- c((lightOnOff[4]+1):lightOnOff[10])
  plot(monitor_list[[1]]@time$V10[selected_range],
       ylab="Light on/off")
  for (i in 1:length(monitor_list)) {
    monitor_list[[i]]@assays$pn <- monitor_list[[i]]@assays$pn[selected_range,]
    monitor_list[[i]]@assays$mt <- monitor_list[[i]]@assays$ct[selected_range,]
    monitor_list[[i]]@assays$ct <- monitor_list[[i]]@assays$ct[selected_range,]
    monitor_list[[i]]@time <- monitor_list[[i]]@time[selected_range,]
  }
  return(monitor_list)
}