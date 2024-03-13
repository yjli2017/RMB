setwd("/home/liy27/projects/RMB")

dat <- read.delim("test/Monitor36.txt", header = FALSE, sep = "\t", dec = ".")
head(dat)

source("src/plot_time_series.R")

plot_time_series(dat$V10, ylab = "Light on/off")
