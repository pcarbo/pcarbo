#!/bin/bash

# This bash script illustrates how one can submit multiple SLURM jobs
# simultaneously according to the argument names or a list of
# files. For example, to generate 3 data sets saved to files
# data1.txt, data2.txt and data3.txt using R script gen.rnorm.data.R,
# enter the following in the shell command line:
#
#   bash ./gen_normal_datasets_by_filename.sh data1 data2 data3
#
# In this example, we generate data sets with 1000 samples (rows) and
# 20 features (columns), but this is easily modified below.
#
# Alternatively, if you 3 have files in the current directory named
# "data1", data2" and "data3" (and no other files that start with
# "data"), the following command will produce the same result:
#
#   bash ./gen_normal_datasets_by_filename.sh data*
#
# Run the command "rcchelp qos" on midway to get more details about
# the number of jobs that can be run simultaneously.
#
SEED=1
M=1000
N=20

for i in $*
do
  sbatch --job-name=gen-rnormdata-sbatch-${i} \
         --output=../results/sbatch-${i}.out \
         --error=../results/sbatch-${i}.err \
         gen_rnorm_data.sbatch ../results/${i}.txt $SEED $M $N
done
