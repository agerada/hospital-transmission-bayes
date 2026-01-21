#!/bin/bash -l
#SBATCH -D ./
#SBATCH --export=ALL
#SBATCH -J hospital-calibrate
#SBATCH -p local           # change partition if your cluster uses a different one
#SBATCH -o slurm_out/slurm-hospital-calibrate-%j.out
#SBATCH --ntasks=1
#SBATCH --cpus-per-task=102
#SBATCH --mem=312G
#SBATCH --time=14-00:00:00

# Single job (no array) requesting 102 CPU cores and 312GB RAM
# 2 CPU cores are reserved for overhead, leaving 100 cores for parallel processing in R
# Create output directory for Slurm logs
mkdir -p slurm_out

# Load any optional modules here. We use renv so will just source that below.

# Move to repository root (adjust path if submitting from elsewhere)
# cd /path/to/repo || exit 1

# Run the calibrate script with renv activated for reproducible packages
# Note: Rscript does not source .Rprofile, so we explicitly activate renv here
Rscript -e 'source("renv/activate.R"); source("scripts/calibrate.R")'
