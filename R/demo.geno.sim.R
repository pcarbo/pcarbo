# TO DO: Explain here what this script does.

# SCRIPT PARAMETERS
# -----------------
r <- 0.6         # Proportion of variance explained.
d <- 0.4         # Proportion of additive genetic variance due to QTLs.
n <- 400         # Number of samples.
p <- 1000        # Number of markers (SNPs).
i <- c(127,354)  # Indices of causal variants (QTLs).

# GENERATE DATA SET
# -----------------
set.seed(1)

# ASSESS SUPPORT FOR ASSOCIATIONS USING GEMMA
# -------------------------------------------
# Generate the minor allele frequencies. They are uniformly
# distributed on [0.05,0.5].
maf <- 0.05 + 0.45 * runif(p)

# Simulate genotype data X from an idealized population, according to the 
# specified minor allele frequencies.
X <- (runif(n*p) < maf) +
     (runif(n*p) < maf)
X <- matrix(as.double(X),n,p,byrow = TRUE)

# Center the columns of X.
rep.row <- function (x, n)
  matrix(x,n,length(x),byrow = TRUE)
X <- X - rep.row(colMeans(X),n)

# Generate (small) polygenic additive effects for the SNPs.
u <- rnorm(p)

# Generate (large) QTL effects for the SNPs.
beta    <- rep(0,p)
beta[i] <- rnorm(length(i))

# Adjust the additive effects so that we control for the proportion
# of additive genetic variance that is due to QTL effects (D) and the
# proportion of variance explained (R). That is, we adjust BETA and U
# so that
#
#   R = A/(A+1)
#   D = B/A,
#
# where I've defined
#
#   A = (U + BETA)' * COV(X) * (U + BETA),
#   B = BETA' * COV(X) * BETA.
#
st   <- c(r/(1-r) * d/var(X %*% beta))
beta <- sqrt(st) * beta
sa   <- max(Re(polyroot(c(c(var(X %*% beta) - r/(1-r)),
                          2*sum((X %*% beta) * (X %*% u))/n,
                          c(var(X %*% u))))))^2

# Generate the quantitative trait measurements.
y <- c(X %*% (u + beta) + rnorm(n))
