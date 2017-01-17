#!/bin/bash

# This bash script illustrates how to use the sbatch command in SLURM
# to submit a job to the broadwl partition, in which some of the
# script parameters are specified in the command line. The output of
# running this script is a text file containing normally distributed
# random numbers; see the comments at the top of gen.rnorm.data.R for
# details.
#
# Here is an example of running this script using SLURM from the
# shell:
#
#   sbatch --job-name=gen-rnormdata-sbatch --output=sbatch.out \
#          --error=sbatch.err gen_rnorm_data.sbatch \
#          normaldata.txt 3 20 4
# 
# When the SLURM job is completed, file normaldata.txt will contain a
# 20 x 4 matrix of numbers drawn from the standard normal
# distribution, in which the pseudorandom number generator in R is
# initialized with seed=3.

#SBATCH --time=00:05:00
#SBATCH --partition=mstephens
#SBATCH --account=pi-mstephens
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
