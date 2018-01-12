#!/bin/bash

#SBATCH --job-name=mclapply_demo
#SBATCH --partition=broadwl
#SBATCH --mem=2G
#SBATCH --nodes=1
#SBATCH --ntasks=1
#SBATCH --cpus-per-task=8
#SBATCH --time=00:10:00

# For reproducibility, add version number to "module load" call.
module load R/3.4.1

# Run the R script.
echo "Running demo.mclapply.R"
Rscript demo.mclapply.R
