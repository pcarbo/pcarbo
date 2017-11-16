# TO DO: Explain here what this script is for, and how to use it on
# the RCC cluster.

# SCRIPT PARAMETERS
# -----------------
n  <- 1000  # Size of symmetric matrix.
m  <- 20    # Number of linear systems to solve.
nc <- 4     # Number of threads to use in solving linear systems.

which.method <- "A"

# SET UP ENVIRONMENT
# ------------------
library(parallel)

# GENERATE DATA
# -------------
# Generate an n x n symmetric positive definite matrix.
X <- matrix(rnorm(2*n^2),2*n,n)
A <- t(X) %*% X
rm(X)
gc(verbose = FALSE)

# Generate the B matrix in A*X = B.
B <- matrix(rnorm(n*m),n,m)

# SOLVE LINEAR SYSTEMS
# --------------------
solve.mclapply1 <- function (A, B) {
  m <- ncol(B)
  mclapply()
}
