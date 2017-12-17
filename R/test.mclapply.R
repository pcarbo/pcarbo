# Instructions on the RCC cluster:
#
# 1. Allocate 20 CPUs on a single node:
#
#   sinteractive --mem=8G --partition=broadwl --cpus-per-task=20
#
# 2. Load R:
#
#   module load R
#   R --no-save
#
# 3. Try different settings for "nc", and run the script with
#
#   source("test.mclapply.R")
#
library(MASS)
library(parallel)

# SCRIPT PARAMETERS
# -----------------
k  <- 50   # Dimension of multivariate normals.
d  <- 20   # Number of data sets to generate.
n  <- 1e5  # Number of samples in each data set.
nc <- 10   # Number of threads to use.

set.seed(1)

# Generate a mean and variance for each of the d data sets.
mu <- vector("list",d)
S <- vector("list",d)
for (i in 1:d) {
  X       <- matrix(rnorm(k*100),100,k)
  mu[[i]] <- colMeans(X)
  S[[i]]  <- cov(X)
}

# Draw samples for the ith data set.
get.mvnorm.data <- function (i)
  mvrnorm(n,mu[[i]],S[[i]])

# Generate samples for all d data sets.
cat(sprintf("Generating data sets using %d threads.\n",nc))
timing <- system.time(out <- mclapply(1:d,get.mvnorm.data,mc.cores = nc))
cat(sprintf("Computation took %0.2f seconds.\n",timing["elapsed"]))


