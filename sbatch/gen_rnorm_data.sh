#!/bin/bash

# Explain here what this script does, and how to use it.
#
#    sbatch ./gen_rnormdata_sbatch.sbatch 
#

#SBATCH --job-name=gen_rnormdata_sbatch
#SBATCH --output=../results/gen_rnormdata_sbatch.out
#SBATCH --error=../results/gen_rnormdata_sbatch.err
#SBATCH --time=00:02:00
#SBATCH --partition=broadwl
#SBATCH --nodes=1
#SBATCH --ntasks-per-node=1
#SBATCH --mem=1000

# Explain here what these lines of code do.
module load R/3.3
Rscript gen.rnorm.data.R 1 100 20 ../results/normaldata.txt
module unload R
