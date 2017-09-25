# NOTES:
#
#   - Explain how to install cfwlab package.
#
#   - sinteractive --partition=broadwl --account=rcc-staff \
#       --cpus-per-task=10 --mem=8G
#
#   - export OPENBLAS_NUM_THREADS=10
#
#   - R CMD SHLIB setblas.o
#
#   - Does not work on Windows (due to mclapply).
#
# TO DO:
#
#   - Explain here what this script does, and how to use it.
#
#   - Convert this script to an R Markdown document.
#
library(parallel)
library(cfwlab)
dyn.load("setblas.so")

# SCRIPT PARAMETERS
# -----------------
trait <- "soleus" # Phenotype to analyze.
ns    <- 100      # Number of Monte Carlo samples.

# Initialize the random number generator.
set.seed(1)

# If "nc" is not already specified, attempt to automatically detect
# the number of CPU cores (but if this automatic detection fails, set
# nc = 1).
if (!exists("nc")) {
  nc <- detectCores()
  if (is.na(nc))
    nc <- 1
}

# FUNCTION DEFINITIONS
# --------------------
# Distribute the elements of x evenly (or as evenly as possible) into
# k list elements.
distribute <- function (x, k)
  split(x,rep(1:k,length.out = length(x)))

# Replicate vector x to create an n x m matrix, where m = length(x).
rep.row <- function (x, n)
  matrix(x,n,length(x),byrow = TRUE)

# Set the number of threads used by OpenBLAS.
set.blas.num.threads <- function (n) {
  .Call("set_blas_Call",n = as.integer(n))
  return(n)
}

# Takes as input an array of unnormalized log-importance weights and
# returns normalized importance weights such that the sum of the
# normalized importance weights is equal to one. I guard against
# underflow or overflow by adjusting the log-importance weights so
# that the largest importance weight is one.
normalizelogweights <- function (logw) {
  c <- max(logw)
  w <- exp(logw - c)
  return(w / sum(w))
}

# Computes the marginal log-likelihood the regression model of Y given
# X assuming that the prior variance of the regression coefficients is
# sa. Here K is the "kinship" matrix K = tcrossprod(X)/p.
compute.log.weight <- function (K, y, sa, use.backsolve = TRUE) {
    
  # Compute the covariance of Y (divided by residual variance) and
  # its Choleksy decomposition. If H is not positive definite, L = FALSE.
  H <- diag(n) + sa*K
  R <- t(tryCatch(chol(H),error = function(e) FALSE))

  # Compute the log-importance weight.
  if (is.matrix(R)) {
    if (use.backsolve)
      x <- backsolve(R,forwardsolve(t(R),y))
    else
      x <- solve(H,y)
    logw <- (-determinant(sum(y*x)*H,logarithm = TRUE)$modulus/2)
  } else
    logw <- 0
  return(logw)
}

# Compute the marginal log-likelihood for multiple settings of the
# prior variance parameter.
compute.log.weights <- function (K, y, sa, use.backsolve = TRUE) {
  n    <- length(sa)
  logw <- rep(0,n)
  for (i in 1:n)
    logw[i] <- compute.log.weight(K,y,sa[i],use.backsolve)
  return(logw)
}

# This is a multicore variant of compute.log.weights.
compute.log.weights.multicore <- function (K, y, sa, nc = 2,
                                           use.backsolve = TRUE) {
  n       <- length(sa)
  samples <- distribute(1:n,nc)
  logw    <- mclapply(samples,
               function (i) compute.log.weights(K,y,sa[i],use.backsolve),
               mc.cores = nc)
  logw    <- do.call(c,logw)
  logw[unlist(samples)] <- logw
  return(logw)
}

# LOAD DATA
# ---------
# Load the phenotype and genotype data.
cat("Loading genotype and phenotype data.\n")
data(cfw.pheno)
data(cfw.geno)
X <- cfw.geno
y <- cfw.pheno[[trait]]
rm(cfw.geno,cfw.pheno)

# Remove rows containing missing data.
rows <- which(!is.na(y))
y    <- y[rows]
X    <- X[rows,]

# PREPROCESSING STEPS
# -------------------
# Center y and the columns of X.
cat("Centering X and y.\n")
n <- nrow(X)
p <- ncol(X)
X <- X - rep.row(colMeans(X),n)
y <- y - mean(y)

# Compute the kinship matrix.
cat("Computing kinship matrix.\n")
r <- system.time(K <- tcrossprod(X)/p)
cat(sprintf("Computation took %0.2f seconds.\n",summary(r)["elapsed"]))

# COMPUTE IMPORTANCE SAMPLING ESTIMATE
# ------------------------------------
# First, draw samples of the PVE estimate from the proposal
# distribution, which is uniform on [0,1].
cat("Drawing",ns,"samples from the uniform proposal distribution.\n")
h <- runif(ns)

# Get settings for the prior variance of the "background" polygenic
# effects.
cat("Acquiring settings for prior variance of polygenic effects.\n")
sx <- sum(apply(X,2,sd)^2)
sa <- p*h/(1-h)/sx

stop()

# Compute the log-importance weights.
cat("Computing importance weights for",ns,"hyperparameter settings.\n")
r <- system.time(logw <- compute.log.weights(K,y,sa,use.backsolve = TRUE))
cat(sprintf("Computation took %0.2f seconds.\n",summary(r)["elapsed"]))

r <- system.time(logw <- compute.log.weights.multicore(K,y,sa,nc = nc))

# Normalize the importance weights.
w <- normalizelogweights(logw)

# Compute the mean and variance of the PVE estimate (h).
# TO DO.

