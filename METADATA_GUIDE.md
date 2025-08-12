# Automated Metadata Generation Guide

The RMB package includes automated metadata generation for common TriKinetics experimental designs. This eliminates the tedious manual creation of metadata files.

## Quick Start

```r
# Load metadata generation functions
source("generate_metadata_example.R")

# This will analyze your data and generate metadata files automatically
```

## Common Experimental Designs

### 1. Split Half Design (Most Common)
**Use case**: Compare two genotypes or treatments  
**Setup**: Channels 1-16 vs Channels 17-32

```r
experiment_config <- list(
  name = "genotype_comparison",
  data_dir = "./data/my_experiment/",
  date = "20240315",
  lab = "MyLab", 
  user = "researcher",
  assignments = list(
    list(
      monitor = "Monitor01",
      design = "split_half",
      first_half = list(
        genotype = "Control_CS",
        treatment = "Vehicle", 
        sex = "Male"
      ),
      second_half = list(
        genotype = "sleep_mutant",
        treatment = "Vehicle",
        sex = "Male"
      )
    )
  )
)

# Generate metadata
generate_experiment_metadata(list(experiment_config))
```

### 2. All Same Design
**Use case**: All flies have same genotype, different monitors for different treatments  
**Setup**: All 32 channels identical

```r
experiment_config <- list(
  name = "dose_response",
  data_dir = "./data/dose_study/",
  date = "20240315",
  assignments = list(
    list(
      monitor = "Monitor01",
      design = "all_same",
      genotype = "w1118",
      treatment = "Control",
      sex = "Male"
    ),
    list(
      monitor = "Monitor02", 
      design = "all_same",
      genotype = "w1118",
      treatment = "Drug_10uM",
      sex = "Male"
    )
  )
)
```

### 3. Custom Design
**Use case**: Complex experiments with multiple groups per monitor  
**Setup**: User-defined channel groupings

```r
experiment_config <- list(
  name = "factorial_design",
  data_dir = "./data/factorial/",
  date = "20240315",
  assignments = list(
    list(
      monitor = "Monitor01",
      design = "custom",
      groups = list(
        list(channels = 1:8,   genotype = "WT_Male",   treatment = "Control", sex = "Male"),
        list(channels = 9:16,  genotype = "WT_Female", treatment = "Control", sex = "Female"), 
        list(channels = 17:24, genotype = "Mut_Male",  treatment = "Drug",    sex = "Male"),
        list(channels = 25:32, genotype = "Mut_Female", treatment = "Drug",   sex = "Female")
      )
    )
  )
)
```

## Data File Analysis

The system can analyze your Monitor*.txt files to understand the structure:

```r
# Analyze a monitor file
analysis <- analyze_monitor_file("./data/Monitor01.txt")

# View analysis results
print(analysis$suggestions)
```

**Typical Monitor File Structure:**
- **Columns 1-8**: Metadata (Index, Date, Time, Status, Monitor, Light, Measure_Type)
- **Columns 9+**: Channel data (32 channels = columns 9-40)
- **Data Types**: MT (Motor activity), CT (Continuous tracking), Pn (Position)

## Templates

Use built-in templates for quick setup:

```r
# Create template configurations
split_template <- create_experiment_template("split_half")
all_same_template <- create_experiment_template("all_same") 
custom_template <- create_experiment_template("custom")

# Modify as needed for your experiment
split_template$name <- "my_experiment_name"
split_template$data_dir <- "./data/my_experiment/"

# Generate metadata
generate_experiment_metadata(list(split_template))
```

## Integration with loadRMB()

Once metadata is generated, use it directly with the main analysis function:

```r
# Load data with generated metadata
data <- loadRMB(data = "./data/my_experiment/", 
               metadata = "./data/my_experiment/metadata/")

# Generate report
generateReport(data)
```

## Configuration Options

### Required Fields
- `name`: Experiment name
- `data_dir`: Directory containing Monitor*.txt files  
- `date`: Experiment date (YYYYMMDD format)
- `assignments`: List of monitor assignments

### Optional Fields
- `lab`: Lab name (default: "Lab")
- `user`: User name (default: "User")
- `monitor_type`: Monitor type (default: "MB")
- `tube_type`: Tube type (default: "MB")
- `incubator`: Incubator name (default: "Incubator1")
- `temperature`: Temperature (default: 25)
- `other`: Other notes (default: "Wcs")

## Best Practices

1. **Consistent Naming**: Use clear, consistent naming for experiments
2. **Date Format**: Always use YYYYMMDD format for dates
3. **Channel Organization**: 
   - Split design: Channels 1-16 vs 17-32 is standard
   - Custom design: Group logically (e.g., by 8s or 4s)
4. **File Organization**: Keep Monitor files and metadata in organized directories

## Example Output

Generated metadata files will be saved as:
```
./data/my_experiment/metadata/
├── Monitor01_metadata.csv
├── Monitor02_metadata.csv  
└── ...
```

Each file contains complete metadata for all 32 channels with proper experimental groupings.

## Troubleshooting

**Problem**: "No Monitor*.txt files found"  
**Solution**: Check that Monitor files are in the specified data_dir

**Problem**: "Unusual channel count detected"  
**Solution**: Verify Monitor file format - should have 32 data columns after metadata

**Problem**: Files not generated  
**Solution**: Check that data_dir exists and is writable

## Advanced Usage

For complex experiments, you can:
1. Generate templates for multiple monitors
2. Mix different designs across monitors
3. Customize channel assignments beyond standard patterns
4. Analyze existing files before generating metadata

This system handles the most common TriKinetics experimental setups while remaining flexible for specialized designs.