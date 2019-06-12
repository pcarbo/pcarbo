# TO DO: Explain here what this script does, and how to use it.

# SCRIPT PARAMETERS
# -----------------
r <- 0.6   # Proportion of variance in trait explained by SNPs.
n <- 400   # Number of samples.
p <- 1000  # Number of genetic markers (SNPs).

# Candidate values for the PVE estimates.
h <- seq(0.01,0.99,0.005)

# Initialize the sequence of pseudorandom numbers.
set.seed(1)

# GENERATE DATA
# -------------
# Generate SNP minor allele frequencies so that they are uniformly
# distributed on [0.05,0.5].
maf <- 0.05 + 0.45 * runif(p)

# Simulate SNP genotype data X from an idealized population according
# to the minor allele frequencies generated above.
X <- (runif(n*p) < maf) +
     (runif(n*p) < maf)
X <- matrix(as.double(X),n,p,byrow = TRUE)

# Center the columns of X.
X <- scale(X,center = TRUE,scale = FALSE)

# Generate "polygenic" additive effects for the SNPs.
b <- rnorm(p)

# Adjust the QTL effects so that we control for the proportion of variance
# explained (r). That is, we adjust beta so that r = a/(a+1), where I've
# defined a = beta'*cov(X)*beta. Here, sb is the variance of the (nonzero)
# QTL effects.
sb <- r/(1-r)/var(drop(X %*% b))
b  <- sqrt(sb) * b

# Generate the quantitative trait measurements.
y <- drop(X %*% b + rnorm(n))
