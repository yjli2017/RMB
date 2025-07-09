############################################################################
# Test Metadata Generation Configuration
# This file uses the new workflow system with sample information from files

# Load required source files
# setwd("/scr1/users/liy27/20250709_RMB/dep/RMB")
source("./src/metadata.R")
source("./src/metadata_workflow.R")
source("./config/config.R")

# Read sample information from test_metadata.csv
metadata_info <- read.csv("./test/test_metadata.csv", stringsAsFactors = FALSE)

# Define test experiment configuration using the loaded metadata info
experiments <- list(
  list(
    name = "caffeine_gaboxdol_test",
    data_dir = "./test",
    date = "20250709",
    config_file = metadata_info,  # Pass the loaded data frame directly
    user = User,  # From config.R
    tube_type = "MB"
  )
)

# Run the test metadata generation using the new workflow
cat("RMB  Metadata Generation\n")
cat("============================\n")
cat("Using metadata.csv and config.R for sample information\n\n")

# Display the loaded sample information
cat("Sample Information from metadata.csv:\n")
for (i in 1:nrow(metadata_info)) {
  cat("  ", metadata_info$Monitor_number[i], ": ", metadata_info$Group[i], 
      " (Treatment: ", metadata_info$Treatment[i], ", Sex: ", metadata_info$Sex[i], 
      ", Genotype: ", metadata_info$Genotype[i], ")\n", sep = "")
}
cat("\n")

# Process  experiment using the new workflow
results <- process_experiments(experiments, verbose = TRUE)

# Validation function to check metadata structure
validate_metadata_structure <- function(metadata_list) {
  cat("Validating metadata structure...\n")
  
  required_columns <- c("Fly", "Lab", "User", "Date", "Experiment_name", 
                       "Monitor_type", "Monitor_number", "Channel", "Group", 
                       "Tube_type", "Incubator", "Temperature", "Treatment","Sex",
                       "Other", "Alive")
  
  for (i in seq_along(metadata_list)) {
    metadata <- metadata_list[[i]]
    missing_cols <- setdiff(required_columns, colnames(metadata))
    
    if (length(missing_cols) > 0) {
      cat("  ⚠️  Monitor", i + 35, "missing columns:", paste(missing_cols, collapse = ", "), "\n")
    } else {
      cat("  ✓ Monitor", i + 35, "has all required columns\n")
    }
  }
}

# Validation
if (!is.null(results[[1]])) {
  cat("Validating generated metadata...\n")
  validate_metadata_structure(results[[1]])
  
  # Check if metadata files were created
  metadata_dir <- file.path("./test", "metadata")
  if (dir.exists(metadata_dir)) {
    metadata_files <- list.files(metadata_dir, pattern = "_metadata.csv")
    cat("Generated metadata files:\n")
    for (file in metadata_files) {
      cat("  ✓", file, "\n")
    }
    
    # Show sample from first file
    if (length(metadata_files) > 0) {
      first_file <- file.path(metadata_dir, metadata_files[1])
      test_metadata <- read.csv(first_file)
      
      # Show unique treatments and groups
      unique_treatments <- unique(test_metadata$Treatment)
      unique_groups <- unique(test_metadata$Group)
      
      cat("  ✓ Assigned treatments:", paste(unique_treatments, collapse = ", "), "\n")
      cat("  ✓ Assigned groups:", paste(unique_groups, collapse = ", "), "\n")
      
      # Show sample rows
      cat("  ✓ Sample metadata (first 3 rows):\n")
      print(test_metadata[1:3, c("Monitor_number", "Channel", "Group", "Treatment", "Sex", "User", "Lab")])
    }
    
    cat("\n🎉 Test completed successfully! 🎉\n")
  } else {
    cat("⚠️  No metadata directory found.\n")
  }
} else {
  cat("❌ Test failed.\n")
}

cat("\nTest metadata generation completed!\n")
cat("Files based on template:", "template/monitor_metadata_template.csv", "\n")
cat("Sample info from:", "test/test_metadata.csv", "\n")
cat("Config from:", "config/config.R", "\n")

