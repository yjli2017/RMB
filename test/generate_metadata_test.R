################################################################################
# Test Metadata Generation Configuration
# This file contains only test experiment configurations - no functions

# Load the workflow functions
source("./src/metadata_workflow.R")

# Define test experiment configuration
test_experiments <- list(
  list(
    name = "test_caffeine_experiment",
    data_dir = "./test",  # Use the test folder with Monitor36-41.txt files
    date = "20250709",    # Current date for testing
    config_file = "./test/test_config.csv",  # CSV with Monitor,Start_channel,End_channel,Phenotype
    user = "test_user",
    tube_type = "Normal"
  )
)

# Run the test metadata generation
cat("RMB Test Metadata Generation\n")
cat("============================\n\n")

# Process test experiments
results <- process_experiments(test_experiments, verbose = TRUE)

# Additional validation for testing
if (!is.null(results[[1]])) {
  cat("\nValidating generated metadata...\n")
  
  # Check if metadata files were created
  metadata_dir <- file.path("./test", "metadata")
  if (dir.exists(metadata_dir)) {
    metadata_files <- list.files(metadata_dir, pattern = "_metadata.csv")
    cat("Generated metadata files:\n")
    for (file in metadata_files) {
      cat("  ✓", file, "\n")
    }
    
    # Validate structure of first metadata file
    if (length(metadata_files) > 0) {
      first_file <- file.path(metadata_dir, metadata_files[1])
      test_metadata <- read.csv(first_file)
      
      required_cols <- c("Fly", "Lab", "User", "Date", "Experiment_name", 
                        "Monitor_type", "Monitor_number", "Channel", "Phenotype", 
                        "Tube_type", "Incubator", "Temperature", "Treatment", 
                        "Other", "Alive")
      
      missing_cols <- setdiff(required_cols, colnames(test_metadata))
      
      if (length(missing_cols) == 0) {
        cat("  ✓ All required columns present\n")
      } else {
        cat("  ⚠️  Missing columns:", paste(missing_cols, collapse = ", "), "\n")
      }
      
      # Check treatment assignment
      unique_treatments <- unique(test_metadata$Treatment)
      cat("  ✓ Assigned treatments:", paste(unique_treatments, collapse = ", "), "\n")
      
      # Check phenotype assignment
      unique_phenotypes <- unique(test_metadata$Phenotype)
      cat("  ✓ Assigned phenotypes:", paste(unique_phenotypes, collapse = ", "), "\n")
    }
    
    cat("\n🎉 Test completed successfully! 🎉\n")
  } else {
    cat("⚠️  No metadata directory found. Test may have failed.\n")
  }
} else {
  cat("❌ Test failed. No results generated.\n")
}

cat("\nTest metadata generation completed!\n")