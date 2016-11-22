#!/bin/bash

# This bash script illustrates how one can implement a bash script
# combined with SLURM to schedule several jobs simultaneously. For
# example, to generate 5 data sets in R using script gen.rnorm.data.R,
# enter the following in the shell command line:
#
#   bash ./gen_many_normal_datasets.sh 5
#
# In this example, we generate data sets with 1000 samples (rows) and
# 20 features (columns), but this is easily modified in the for-loop
# below.
#
# Note that, as of this writing, there is a limit of 50 jobs per user
# on the broadwl partition, so this script will not work if you set
# the first argument to a number greater than 50. Run the command
# "rcchelp qos" on midway to get more details about this.

# Get the script parameter specified as a command-line argument.
NUMDATASETS=${1}

# Schedule a separate job in SLURM for each data set that we would
# like to generate.
for (( i=1 ; i <= ${NUMDATASETS} ; i++ )); do
  sbatch --job-name=gen-rnormdata-sbatch-${i} \
         --output=../results/sbatch-${i}.out \
         --error=../results/sbatch-${i}.err \
         gen_rnorm_data.sbatch ../results/normal-data-${i}.txt ${i} 1000 20
done
