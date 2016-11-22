#!/bin/bash

# Explain here what this command does, and how to use it.
#
#   bash ./gen_many_normal_datasets.sh 5
#
NUMDATASETS=${1}

for (( i=1 ; i <= ${NUMDATASETS} ; i++ )); do
  sbatch --job-name=gen-rnormdata-sbatch-${i} \
         --output=../results/sbatch-${i}.out \
         --error=../results/sbatch-${i}.err \
         gen_rnorm_data.sbatch ../results/normal-data-${i}.txt ${i} 20 4
done
