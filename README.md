# RMB: A R scripts to analysis Drosophila TriKinetics multi_beam (MB) data

Yongjun Li, 2023-02-22
<yongjunli2017@gmail.com>

## Introduction

This R script is used to analyze Drosophila (fruit fly)TriKinetics (<https://www.trikinetics.com/>) data. The data is collected from the TriKinetics multi-beam (MB) or single-beam (SB) system, which is a high-throughput system to measure Drosophila activity. The data is in the form of a text file, which contains the beam crossing of each fly in each locomotor tube. The script will read the data, convert the data into a S4 class object (similar to Seurat object for single cell sequencing), and then analyze the data to calculate the activity and sleep and postional changes of each fly. It allows to generate metadata.csv for each monitor and then you can combine any monitors together to generate combined monitor object, which can be used to analyze and store the data from multiple monitors, multiple experiments and multiple labs.

## Installation

This is a purely R package, so just download the RMB from the github and unzip it. Then you are ready to go. It requies some levels of R programming skills to use it. The RMB is a R package, so you can use it in R environment or Rstudio.

Before you use it, change the config.R file to fit your environment. You need to change the path of the RMB, install and load the required R packages, load all the functions in ./src and set the default parameters for the RMB.

```R

## Metadata and data format

- For each experiments, you just put them into ./data folder and idealy give the folder name as the date and the experiment name.

- Then you need to have the metadata.csv file for each monitor data file, which is saved in the same folder, eg ./data/20240222_aging/metadata. There is a metadata tempelate file in the ./template folder. Here you could either use the template file to edit the metadata.csv file manulally for each monitor or use the generate_metadata.R script to generate the metadata.csv file. The metadata.csv file is used to store the information of each monitor and each fly, such as the monitor name, the genotype, the data file, the user, the lab, the date, the time, the temperature, the humidity, the light cycle etc. It's totally up to you, but information from the templete file is highly recommended to be filled in the metadata.csv file.

- After metadata and raw data are ready, you can use the RMB to analyze the data. Just run the run_analysis.R script and the data will be analyzed and the results will be saved in the subfolder of the ./data/expeirments folder, eg ./data/20240222_aging/analysis.

- There is a test folder with some test data and metadata.csv file, you can use it to test the RMB.

- The RMB autnomatically generate the combined monitor object for the folder with the second to forth full day, and the saved monitor object will be saved in the ./data folder, eg ./data/combined_monitor.RData, this can be merged with other combined monitor object to generate the combined monitor object for again and again.

## monitor S4 class object

With the metadata and raw beam crossign data, the RMB will generate the **monitor** S4 class object, which is similar to Seurat object for single cell sequencing. The monitor object contains the following slots: meta.data, assays (a list of mt, ct, pn ...), active.assay, and time. The meta.data slot contains the metadata of the monitor. The assays slot contains the data of the monitor, such as the beam crossing data, the activity data, the sleep data, the positional data etc. The active.assay slot is used to switch between data in the assay slot. The time slot contains the time information of the monitor, such as the start time, the end time, the time zone etc, basically the first 10 columns of the raw data file.

## Output

The RMB will generate the following output:

Especially, the RMB will generate all different types of  individual data and could be easily used to generate plots and figures by pasting them into graphpad or excel.

The RMB will also generate the combined monitor object, which can be used to analyze the data from multiple monitors, multiple experiments and multiple labs.It could be good way to store all your fly behavior data in this format.

## Other

- The RMB is still under development and I will keep updating it whenever I have time.
- With the power of R, you can easily modify the RMB to fit your own needs, and make nice plots and figures by ggplot2 and other R packages.
- Contributions are highly welcome. If you have any suggestions or find any bugs, please feel free to contact me.

## Contact

I hope you enjoy the RMB and find it useful. If you have any questions, please feel free to contact me <yongjunli2017@gmail> | <yjli@sas.upenn.edu>
