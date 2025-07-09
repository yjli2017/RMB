# This file is sourced at the beginning of all R scripts in the root

################################################################################
# change this to where you own RMB path
base_dir <- "/scr1/users/liy27/20250709_RMB/dep/RMB" 
################################################################################
# Check the current working directory
setwd(base_dir)
print(getwd())

################################################################################
# uncomment the following lines to install and load required libraries
# Note: Uncomment only if the libraries are not already installed.
# install and load libraries
# install.packages("tidyverse")
# install.packages("lubridate")
# if (!requireNamespace("BiocManager", quietly = TRUE))
#     install.packages("BiocManager")
# BiocManager::install("ComplexHeatmap")
# install.packages("circlize")
# install.packages("ggplot2")
# install.packages("rstudioapi")
# library(tidyverse)
# library(lubridate)
# library(ComplexHeatmap)
# library(circlize)
# library(ggplot2)
# library(rstudioapi)

################################################################################
# global parameters

# Fly will be written by generate_metadata.R
Lab = "Sehgal"
User = "yjli"
# Other info for metadata for experiments should be set in the sample metadata CSV file
Tube_type = "Normal"  # Default tube type, can be overridden in metadata CSV
Incubator = "MB"  # Default incubator, can be overridden in metadata CSV
Temperature = 25  # Default temperature, can be overridden in metadata CSV
