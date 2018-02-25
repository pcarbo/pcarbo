# This R script illustrates how one can implement an R script to
# schedule several SLURM jobs simultaneously. For example, to generate
# 5 data sets in R using script gen.rnorm.data.R, run the following
# commands in R:
#
#   n <- 5
#   source("gen.many.normal.datasets.R")
#
# In this example, we generate data sets with 1000 samples (rows) and
# 20 features (columns), but this is easily modified in the code
# below.
#

# Schedule a separate SLURM job for each data set that we would like
# to generate.
for (i in 1:n) {
  cmd.str <- sprintf(paste("sbatch --job-name=gen-rnormdata-sbatch-%d",
                           "--output=../results/sbatch-%d.out",
                           "--error=../results/sbatch-%d.err",
                           "gen_rnorm_data.sbatch",
                           "../results/normal-data-%d.txt %d 1000 20"),
                     i,i,i,i,i)
  cat(cmd.str,"\n")                   
  system(cmd.str)
}
