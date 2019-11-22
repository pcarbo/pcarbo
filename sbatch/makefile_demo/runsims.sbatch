#!/bin/bash

# This bash script illustrates how to run large number of
# heterogeneous computations in parallel using GNU Make. To submit
# these computations to the SLURM scheduler, simply run
# 
#   sbatch runsims.sbatch
#
# With the experiment and SLURM settings I've provided, it should take
# a few minutes to run on the RCC cluster.
#
# Note that GNU Make will not distribute computations across multiple
# nodes, so it is important to set --nodes=1.

#SBATCH --partition=broadwl
#SBATCH --nodes=1
#SBATCH --ntasks=1
#SBATCH --cpus-per-task=10
#SBATCH --mem-per-cpu=4G
#SBATCH --time=00:10:00

# For reproducibility, add version number to "module load" call.
module load R/3.4.3

# Generate all the results using the Makefile. Here, 10 CPUs
# ("threads") are used.
make --jobs=10 all
