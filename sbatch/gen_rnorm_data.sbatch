#!/bin/bash

# Explain here what this script does, and how to use it.
#
# Example:
#
#   sbatch --job-name=gen-rnormdata-sbatch --output=sbatch.out \
#          --error=sbatch.err gen_rnorm_data.sbatch \
#          normaldata1.txt 1 20 4
#

#SBATCH --time=00:05:00
#SBATCH --partition=broadwl
#SBATCH --nodes=1
#SBATCH --ntasks-per-node=1
#SBATCH --mem=1000

# Get the script parameters specified as command-line arguments.
OUTFILE=${1}
SEED=${2}
M=${3}
N=${4}

# Run gen.rnorm.data.R in R 3.3.2.
module load R/3.3
Rscript gen.rnorm.data.R ${SEED} ${M} ${N} ${OUTFILE}
module unload R
