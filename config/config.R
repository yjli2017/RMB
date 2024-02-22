# This file is sourced at the beginning of Main.R.

# setup the working directory and load libraries and functions
# mac
# base_dir = "/Users/yongjunli/Library/CloudStorage/OneDrive-Personal/Desktop/20240221_RMB/" # nolint
# win
base_dir <- "C:/Users/yongj/OneDrive/Desktop/20240221_RMB"
setwd(base_dir)

# Check the current working directory
print(getwd())

################################################################################
# Load libraries
library(lubridate)
library(ComplexHeatmap)
library(circlize)
library(ggplot2)
library(rstudioapi)

################################################################################
# Load functions
# Get a list of all the .R files in the directory
function_list <- list.files(path = "./src", pattern = "*.R", full.names = TRUE)
print(function_list)
# Source each file
sapply(function_list, source)