# This file is sourced at the beginning of all R scripts in the root

################################################################################
# change this to where you own RMB path
base_dir <- "/home/liy27/projects/RMB" 
################################################################################
# Check the current working directory
setwd(base_dir)
print(getwd())

################################################################################
# install and load libraries
# install.packages("tidyverse")
# install.packages("lubridate")
# if (!requireNamespace("BiocManager", quietly = TRUE))
#     install.packages("BiocManager")
# BiocManager::install("ComplexHeatmap")
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
# global parameters
