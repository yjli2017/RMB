# Example: Automated Metadata Generation for TriKinetics Experiments
# This script demonstrates how to generate metadata for common experimental designs

# Load the metadata generation functions
source("R/generate_metadata.R")

# Also load required libraries
library(readr)
library(tools)
library(lubridate)

# =============================================================================
# Example 1: Split Design (Most Common)
# Channels 1-16: Control genotype
# Channels 17-32: Treatment genotype
# =============================================================================

cat("=== Example 1: Split Half Design ===\n")

split_design_experiment <- list(
  name = "sleep_mutant_comparison",
  data_dir = "./inst/extdata/",  # Using package example data
  date = "20230606",
  lab = "Sehgal",
  user = "yjli",
  monitor_type = "MB",
  tube_type = "MB",
  incubator = "Incubator1", 
  temperature = 25,
  other = "Wcs",
  assignments = list(
    list(
      monitor = "Monitor36",
      design = "split_half",
      first_half = list(
        genotype = "Male_CS",
        treatment = "Control",
        sex = "Male"
      ),
      second_half = list(
        genotype = "Male_CS", 
        treatment = "Control",
        sex = "Male"
      )
    ),
    list(
      monitor = "Monitor37",
      design = "split_half",
      first_half = list(
        genotype = "Female_CS",
        treatment = "Control", 
        sex = "Female"
      ),
      second_half = list(
        genotype = "Female_CS",
        treatment = "Control",
        sex = "Female"
      )
    ),
    list(
      monitor = "Monitor38", 
      design = "split_half",
      first_half = list(
        genotype = "Male_Gaboxdol",
        treatment = "Gaboxdol",
        sex = "Male"
      ),
      second_half = list(
        genotype = "Male_Gaboxdol",
        treatment = "Gaboxdol", 
        sex = "Male"
      )
    )
  )
)

# =============================================================================
# Example 2: All Same Design  
# All 32 channels have the same experimental condition
# =============================================================================

cat("=== Example 2: All Same Design ===\n")

all_same_experiment <- list(
  name = "dose_response_study",
  data_dir = "./data/dose_response/", 
  date = format(Sys.Date(), "%Y%m%d"),
  lab = "MyLab",
  user = "researcher",
  assignments = list(
    list(
      monitor = "Monitor01",
      design = "all_same",
      genotype = "w1118",
      treatment = "Vehicle_Control",
      sex = "Male"
    ),
    list(
      monitor = "Monitor02", 
      design = "all_same",
      genotype = "w1118",
      treatment = "Drug_10uM",
      sex = "Male"
    ),
    list(
      monitor = "Monitor03",
      design = "all_same", 
      genotype = "w1118",
      treatment = "Drug_100uM",
      sex = "Male"
    )
  )
)

# =============================================================================
# Example 3: Custom Design
# Complex experimental designs with multiple groups per monitor
# =============================================================================

cat("=== Example 3: Custom Design ===\n")

custom_experiment <- list(
  name = "sex_genotype_interaction",
  data_dir = "./data/interaction_study/",
  date = format(Sys.Date(), "%Y%m%d"), 
  lab = "MyLab",
  user = "researcher",
  assignments = list(
    list(
      monitor = "Monitor01",
      design = "custom",
      groups = list(
        list(channels = 1:8,   genotype = "WT_Male",     treatment = "Control", sex = "Male"),
        list(channels = 9:16,  genotype = "WT_Female",   treatment = "Control", sex = "Female"),
        list(channels = 17:24, genotype = "Mutant_Male", treatment = "Control", sex = "Male"),
        list(channels = 25:32, genotype = "Mutant_Female", treatment = "Control", sex = "Female")
      )
    )
  )
)

# =============================================================================
# Generate metadata for the example experiment (using package data)
# =============================================================================

cat("Generating metadata for split design example...\n")

# First, let's analyze the monitor files to understand their structure
cat("\n--- Analyzing Monitor Files ---\n")
monitor_files <- list.files("./inst/extdata/", pattern = "^Monitor.*\\.txt$", full.names = TRUE)

for (monitor_file in monitor_files[1:2]) {  # Just analyze first 2 files
  analysis <- analyze_monitor_file(monitor_file)
  cat("\nFile:", basename(analysis$file_path), "\n")
  cat("Columns:", analysis$total_columns, "(", analysis$data_columns, "data columns)\n")  
  cat("Measure types:", paste(analysis$measure_types, collapse = ", "), "\n")
  cat("Suggestions:\n")
  for (suggestion in analysis$suggestions) {
    cat(" ", suggestion, "\n")
  }
}

# Generate metadata files
cat("\n--- Generating Metadata ---\n")
generated_files <- generate_experiment_metadata(list(split_design_experiment))

cat("\nGenerated files:\n")
for (file in generated_files) {
  cat(" ", file, "\n")
}

# =============================================================================
# Helper function to create templates
# =============================================================================

cat("\n=== Creating Templates ===\n")

# Create template configurations
split_template <- create_experiment_template("split_half")
all_same_template <- create_experiment_template("all_same")
custom_template <- create_experiment_template("custom")

cat("Available templates created:\n")
cat(" - split_half: For 1-16 vs 17-32 comparisons\n")
cat(" - all_same: For uniform conditions across all channels\n") 
cat(" - custom: For complex multi-group designs\n")

# =============================================================================
# Usage Instructions
# =============================================================================

cat("\n=== Usage Instructions ===\n")
cat("1. Choose your experimental design:\n")
cat("   - split_half: Most common (channels 1-16 vs 17-32)\n")
cat("   - all_same: All 32 channels same condition\n") 
cat("   - custom: Multiple groups within monitor\n\n")

cat("2. Create experiment configuration:\n")
cat("   exp_config <- create_experiment_template('split_half')\n")
cat("   # Edit the configuration for your experiment\n\n")

cat("3. Generate metadata:\n")
cat("   generate_experiment_metadata(list(exp_config))\n\n")

cat("4. Use with loadRMB():\n")
cat("   data <- loadRMB(data = 'path/to/data', metadata = 'path/to/metadata')\n\n")

cat("✅ Metadata generation example complete!\n")