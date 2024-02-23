################################################################################
# Create or update the the metadata

# 0104cas9pn
data_dir <- "./data/0104cas9pn"
date <- "20240104"
config_file <- read.delim("./data/0104cas9pn/0104cas9_config_1h.txt",
                          header = FALSE,
                          sep = " ")

source("./metadata.R")
print(config_file[3:10,1:5])
metadata_list[[1]][1:22,]$Phenotype <- "repoGal4>UASacp98Cas9_f_inj"
metadata_list[[1]][23:32,]$Phenotype <- "repoGal4>UASacp98Cas9_f_HC"
metadata_list[[2]][1:24,]$Phenotype <- "repoGal4>UAStimCas9_f_inj"
metadata_list[[2]][25:32,]$Phenotype <- "repoGal4>UAStimCas9_f_HC"
metadata_list[[3]][1:24,]$Phenotype <- "repoGal4>iso_f_inj"
metadata_list[[3]][25:32,]$Phenotype <- "repoGal4>iso_f_HC"
metadata_list[[4]][2:10,]$Phenotype <- "repoGal4>UASperCas9_f_inj"
metadata_list[[4]][11:14,]$Phenotype <- "repoGal4>UASperCas9_f_HC"
source("./output_metadata.R")

# 0117clk_pn
data_dir <- "./data/0117clk_pn/"
date <- "20240117"
config_file <- read.delim("./data/0117clk_pn/0117clkZ18_config_1h.txt",
                          header = FALSE,
                          sep = " ")

source("./metadata.R")
print(config_file[4:9,1:6])
metadata_list[[1]][1:18,]$Phenotype <- "repoGal4>UASclkCas9_f_inj"
metadata_list[[1]][19:23,]$Phenotype <- "repoGal4>UASclkCas9_f_HC"
metadata_list[[2]][1:16,]$Phenotype <- "clk>w118_f_inj"
metadata_list[[2]][17:21,]$Phenotype <- "clk>w118_f_HC"
metadata_list[[3]][1:20,]$Phenotype <- "iso>clk_f_inj"
metadata_list[[3]][21:30,]$Phenotype <- "iso>clk_f_HC"
source("./output_metadata.R")

# 0117repoZ0pn
data_dir <- "./data/0117repoZ0pn/"
date <- "20240117"
config_file <- read.delim("./data/0117repoZ0pn/0117repoZ0_config_4h.txt",
                          header = FALSE,
                          sep = " ")

source("./metadata.R")
print(config_file[11:14,1:6])
metadata_list[[1]][1:22,]$Phenotype <- "repoGal4>UASperCas9_f_inj"
metadata_list[[1]][23:32,]$Phenotype <- "repoGal4>UASperCas9_f_HC"
metadata_list[[2]][1:20,]$Phenotype <- "repoGal4>UASacp98Cas9_f_inj"
metadata_list[[2]][21:30,]$Phenotype <- "repoGal4>UASacp98Cas9_f_HC"
source("./output_metadata.R")

# 0129HSpn
data_dir <- "./data/0129HSpn/"
date <- "20240129"
config_file <- read.delim("./data/0129HSpn/0129HS40z0_config_1h.txt",
                          header = FALSE,
                          sep = " ")

source("./metadata.R")
print(config_file[10:13,1:6])
metadata_list[[1]][1:26,]$Phenotype <- "repo>perCas9_f_HS"
metadata_list[[2]][23:26,]$Phenotype <- "repo>acp98Cas9_f_HS"
metadata_list[[3]][1:32,]$Phenotype <- "repo>timCas9_f_HS"
source("./output_metadata.R")

# 0502pn
data_dir <- "./data/0502pn/"
date <- "20230502"
config_file <- read.delim("./data/0502pn/0502TE_config_1h.txt",
                          header = FALSE,
                          sep = " ")

source("./metadata.R")
print(config_file[13:18,1:6])
metadata_list[[1]][1:23,]$Phenotype <- "repoGal4>UASperCas9_f"
metadata_list[[2]][1:32,]$Phenotype <- "repoGal4>UASperCas9_m"
metadata_list[[3]][1:32,]$Phenotype <- "repoGal4>UAStimCas9_f"
metadata_list[[4]][1:32,]$Phenotype <- "repoGal4>UAStimCas9_m"
metadata_list[[5]][1:32,]$Phenotype <- "repoGal4>UASacp98Cas9_f"
metadata_list[[6]][1:26,]$Phenotype <- "repoGal4>UASacp98Cas9_m"
source("./output_metadata.R")
