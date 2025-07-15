# RMB Test Suite

This folder contains test data and scripts for validating the RMB metadata generation functionality.

## Test Data

The test folder contains sample data files:

- **Monitor36.txt - Monitor41.txt**: Sample monitor data files (6 monitors total)
- **metadata/**: Directory containing pre-generated metadata files
- **Various output files**: Example outputs from previous test runs

## Test Scripts

### `generate_metadata_test.R`
Main test script with comprehensive testing functionality:

- **`process_experiment_test()`**: Enhanced version of the main processing function with testing features
- **`run_metadata_tests()`**: Automated test runner with validation
- **`validate_metadata_structure()`**: Validates metadata file structure
- **`interactive_test()`**: Interactive testing mode

### `run_tests.R`
Simple command-line test runner:
```bash
cd /path/to/RMB
R --file=test/run_tests.R
```

### `example_usage.R`
Examples demonstrating how to use the test functions:
```r
source("./test/example_usage.R")
```

## How to Run Tests

### Method 1: Command Line
```bash
cd /path/to/RMB
R --file=test/run_tests.R
```

### Method 2: Within R
```r
# Set working directory to RMB root
setwd("/path/to/RMB")

# Load test functions
source("./test/generate_metadata_test.R")

# Run automated tests
results <- run_metadata_tests()

# Run interactive test
interactive_results <- interactive_test()
```

### Method 3: Individual Functions
```r
# Test individual experiment processing
source("./test/generate_metadata_test.R")

result <- process_experiment_test(
  experiment_name = "my_test",
  data_dir = "./test",
  date = "20250709",
  phenotype_assignments = list(
    list(monitor = 1, rows = 1:16, phenotype = "control"),
    list(monitor = 1, rows = 17:32, phenotype = "treatment")
  )
)
```

## Test Configuration

The test uses the following configuration:

- **Data Directory**: `./test` (contains actual Monitor*.txt files)
- **Date**: Current date (20250709)
- **User**: `test_user`
- **Experiment**: `test_data_sample`
- **Phenotype Assignments**: 6 monitors with realistic genotype names

## Expected Outputs

After running tests, you should see:

1. **Console Output**: 
   - Progress messages
   - File discovery information
   - Phenotype assignment details
   - Validation results

2. **Files Created**:
   - `./test/metadata/Monitor36_metadata.csv` through `Monitor41_metadata.csv`
   - Updated metadata files with test phenotype assignments

3. **Validation Results**:
   - Structure validation for all required columns
   - File existence confirmation
   - Test success/failure summary

## Troubleshooting

### Common Issues

1. **"Required source file not found"**
   - Ensure you're running from the RMB root directory
   - Check that `./src/metadata.R` exists

2. **"Test data not found"**
   - Verify that Monitor*.txt files exist in `./test/`
   - Check file permissions

3. **"Function not found"**
   - Source the test script first: `source("./test/generate_metadata_test.R")`
   - Check that all required functions are loaded

### Debug Mode

For detailed debugging information:
```r
# Enable debugging
options(error = recover)

# Run with verbose output
debug(process_experiment_test)
results <- run_metadata_tests()
```

## Test Validation

The test suite validates:

- ✓ Metadata file structure (all required columns present)
- ✓ File creation success
- ✓ Phenotype assignment accuracy
- ✓ Monitor file discovery
- ✓ Error handling for missing files/data
- ✓ Output directory creation

## Extending Tests

To add new test cases:

1. **Add test data**: Place new Monitor*.txt files in `./test/`
2. **Create test configuration**: Add new entries to `test_experiments` list
3. **Define phenotype assignments**: Specify realistic phenotype assignments
4. **Run validation**: Use `validate_metadata_structure()` to verify outputs

## Integration with CI/CD

The test suite can be integrated into automated testing pipelines:

```bash
# Basic test run
R --file=test/run_tests.R

# With exit code handling
R --file=test/run_tests.R && echo "Tests passed" || echo "Tests failed"
```

## Contact

For issues with the test suite, please refer to the main RMB documentation or contact the maintainers.
