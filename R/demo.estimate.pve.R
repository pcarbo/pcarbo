# TO DO: Explain here what this script does, and how to use it.

# SCRIPT PARAMETERS
# -----------------
r <- 0.6   # Proportion of variance in trait explained by SNPs.
n <- 400   # Number of samples.
p <- 1000  # Number of genetic markers (SNPs).

# Candidate values for the PVE estimates.
h <- seq(0.01,0.99,0.005)

# SET UP ENVIRONMENT
# ------------------
source("pve.R")
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

# Adjust the QTL effects so that we control for the proportion of
# variance explained (r). That is, we adjust b so that r = a/(a+1),
# where I've defined a = b'*cov(X)*b. Here, sb is the variance of the
# (nonzero) QTL effects.
sb <- r/(1-r)/var(drop(X %*% b))
b  <- sqrt(sb) * b

# Generate the quantitative trait measurements.
y <- drop(X %*% b + rnorm(n))

# ESTIMATE PVE
# ------------
# For each setting of h, get the "sa" parameter under a "polygenic"
# model in which all the SNP effects (the regression coefficients) are
# normal with zero mean and variance s*sa, where "s" is the residual
# variance parameter.
sx <- sum(apply(X,2,sd)^2)
sa <- p*h/(1-h)/sx

# Compute the kinship matrix.
K <- tcrossprod(X)/p

# Compute the log-likelihood for each candidate setting of the prior
# variance parameter, "sa".
ns   <- length(sa)
logw <- rep(0,ns)
for (i in 1:ns)
  logw[i] <- compute.log.weight(K,y,sa[i])
    
# Compute the posterior mean estimate of h.
w <- exp(logw - max(logw))
w <- w/sum(w)
cat(sprintf("Mean PVE estimate is %0.3f.\n",sum(w*h)))
