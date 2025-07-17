#!/bin/bash

# RMB Analysis Environment Setup Script
# This script creates a conda environment for the RMB project

echo "Setting up RMB Analysis Environment..."
echo "======================================"

# Check if conda is installed
if ! command -v conda &> /dev/null; then
    echo "Error: conda is not installed or not in PATH"
    echo "Please install Miniconda or Anaconda first"
    exit 1
fi

# Create the conda environment from the YAML file
echo "Creating conda environment from environment.yml..."
conda env create -f environment.yml

if [ $? -eq 0 ]; then
    echo "✅ Conda environment 'rmb-analysis' created successfully!"
    echo ""
    echo "To activate the environment, run:"
    echo "  conda activate rmb-analysis"
    echo ""
    echo "To install additional Python packages, run:"
    echo "  pip install -r requirements.txt"
    echo ""
    echo "To deactivate the environment when done, run:"
    echo "  conda deactivate"
else
    echo "❌ Failed to create conda environment"
    echo "Please check the environment.yml file and try again"
    exit 1
fi

echo ""
echo "Environment setup complete!"
echo "You can now activate the environment and run your R scripts."
