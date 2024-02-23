plot_light_switches <- function(monitor_list) {
  # Find the indices where the light switches
  lightSwitchIndices <- which(diff(monitor_list[[1]]@time$V10) != 0)
  print(lightSwitchIndices)
  # Create a data frame for plotting
  df <- data.frame(Time = 1:length(monitor_list[[1]]@time$V10),
                   Value = monitor_list[[1]]@time$V10)

  # Create a data frame for the switch numbers
  switch_df <- data.frame(Switch = 1:length(lightSwitchIndices),
                          Time = lightSwitchIndices,
                          Value = monitor_list[[1]]@time$V10[lightSwitchIndices])

  ggplot(df, aes(x = Time, y = Value)) +
    geom_line() +
    geom_vline(data = switch_df, aes(xintercept = Time), color = "red", linetype = "dashed") +
    geom_text(data = switch_df, aes(label = Switch), vjust = -1) +
    ylab("Light on/off")
}

select_time_range <- function(monitor_list, m, n) {
  par(mfrow = c(1, 2))
  # plot light on and off
  plot(monitor_list[[1]]@time$V10,
       ylab = "Light on/off")
  # select the 2-5 full days for further analysis
  lightOnOff <- which(diff(monitor_list[[1]]@time$V10) != 0)
  print(lightOnOff)
  selected_range <- c((lightOnOff[m]+1):lightOnOff[n])
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