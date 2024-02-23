# This file is sourced at the beginning of all R scripts in the root

################################################################################
# change this to where you own RMB path
base_dir <- "C:/Users/yongj/OneDrive/文档/GitHub/RMB"
################################################################################
# Check the current working directory
setwd(base_dir)
print(getwd())

################################################################################
# install and load libraries
# install.packages("tidyverse")
# install.packages("lubridate")
# install.packages("ComplexHeatmap")
# install.packages("circlize")
# install.packages("ggplot2")
# install.packages("rstudioapi")
library(tidyverse)
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

################################################################################
# global parameters
