# TO DO: Explain here what this script does, and how to use it.

# SCRIPT PARAMETERS
# -----------------
r <- 0.6         # Proportion of variance in trait explained by SNPs.
d <- 0.4         # Proportion of additive genetic variance due to QTLs.
n <- 400         # Number of samples.
p <- 1000        # Number of markers (SNPs).
i <- c(127,354)  # Indices of the QTLs ("causal variants").

# Initialize random number generation so that the results are
# reproducible.
set.seed(1)

# GENERATE DATA SET
# -----------------
# Generate the minor allele frequencies. They are uniformly
# distributed on [0.05,0.5].
maf <- 0.05 + 0.45 * runif(p)

# Simulate genotype data X from an idealized population according to the 
# minor allele frequencies generated above.
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

# Adjust the additive effects so that we control for the proportion of
# additive genetic variance that is due to QTL effects (d) and the
# total proportion of variance explained (r). That is, we adjust beta
# and u so that
#
#   r = a/(a+1)
#   d = b/a,
#
# where I've defined
#
#   a = (u + beta)'*cov(X)*(u + beta),
#   b = beta'*cov(X)*beta.
#
# Note: this code only works if d or r are not exactly 0 or exactly 1.
st   <- c(r/(1-r) * d/var(X %*% beta))
beta <- sqrt(st) * beta
sa   <- max(Re(polyroot(c(c(var(X %*% beta) - r/(1-r)),
                          2*sum((X %*% beta) * (X %*% u))/n,
                          c(var(X %*% u))))))^2
u    <- sqrt(sa) * u

# Generate the quantitative trait measurements.
y <- c(X %*% (u + beta) + rnorm(n))

# Check that the data are simulated in the way we requested.
a <- c(var(X %*% beta)/var(y))
b <- c(var(X %*% (u + beta))/var(y))
cat("Proportion of variance in Y explained by genotypes =  ",b,"\n")
cat("Proportion of variance in Y explained by QTL effects =",a,"\n")
cat("Proportion of genetic variance in Y due to QTLs =     ",a/b,"\n")
