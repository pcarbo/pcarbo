# TO DO: Explain here what this script does, and how to use it.
#
# NOTES:
#
#   - Explain how to install cfwlab package.
#
#   - sinteractive --partition=broadwl --account=rcc-staff \
#       --cpus-per-task=10 --mem=8G
#
#   - export OPENBLAS_NUM_THREADS=10
#
# TO DO:
#
#   - Change this to a uniform grid instead of a Monte Carlo estimate.
#
library(parallel)
library(cfwlab)

# SCRIPT PARAMETERS
# -----------------
trait <- "soleus" # Phenotype to analyze.
nc    <- 10       # Number of cores (CPUs) to use.
ns    <- 20       # Number of Monte Carlo samples.

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
compute.log.weight <- function (K, y, sa) {
    
  # Compute the covariance of Y (divided by residual variance) and
  # its Choleksy decomposition. If H is not positive definite, L = FALSE.
  H <- diag(n) + sa*K
  R <- t(tryCatch(chol(H),error = function(e) FALSE))

  # Compute the log-importance weight.
  if (is.matrix(R)) {
    # This first line is equivalent to x <- solve(H,y), but faster.
    x   <- backsolve(R,forwardsolve(t(R),y))
    out <- (-determinant(sum(y*x)*H,logarithm = TRUE)$modulus/2)
  } else
    out <- 0
  return(out)
}

# Compute the marginal log-likelihood for multiple settings of the
# prior variance parameter.
compute.log.weights <- function (K, y, sa, verbose = TRUE) {
  n   <- length(sa)
  out <- rep(0,n)
  for (i in 1:n) {
    if (verbose)
      cat(sprintf("%d ",i))
    out[i] <- compute.log.weight(K,y,sa[i])
  }
  if (verbose)
    cat("\n")
  return(out)
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
cat(sprintf("Computation took %0.2f seconds (multicore speedup = %0.1fx).\n",
            summary(r)["elapsed"],summary(r)["user"]/summary(r)["elapsed"]))

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
  
# Compute the log-importance weights.
cat("Computing importance weights for",ns,"hyperparameter settings.\n")
r <- system.time(logw <- compute.log.weights(K,y,sa))
cat(sprintf("Computation took %0.2f seconds (multicore speedup = %0.1fx).\n",
            summary(r)["elapsed"],summary(r)["user"]/summary(r)["elapsed"]))

stop()

# Aggregate the outputs from the individual CPUs.
logw <- do.call(c,logw)
logw[unlist(samples)] <- logw

# Normalize the importance weights.
w <- normalizelogweights(logw)

# Compute the mean and variance of the PVE estimate (h).
# TO DO.

# SAVE RESULTS TO FILE
# --------------------
# TO DO.
