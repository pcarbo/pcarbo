# This is a short script to demonstrate a rudimentary implementation
# of variational inference for the BSLMM model.

# SCRIPT PARAMETERS
# -----------------
n  <- 1000    # Number of samples.
p  <- 2000    # Number of variables (genetic markers).
na <- 20      # Number of quantitative trait loci (QTLs).
su <- 0.03^2  # Variance of (small) polygenic effects.
sb <- 0.3^2   # Variance of (large) QTL effects.

# Candidate settings for the hyperparameters: the residual variance
# (sigma); the prior log-odds that a variable is included in the
# regression model (logodds); the variance of the polygenic effects
# (su); and the variance of the QTL effects (sb).
hyperparams <-
  list(sigma   = 1,
       logodds = -2,
       su      = 10^seq(-4,-2,length.out = 10),
       sb      = 10^seq(-2,0,length.out = 10))

# SET UP ENVIRONMENT
# ------------------
library(varbvs)
source("varbslmm.R")
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

# Generate (small) polygenic additive effects for the SNPs.
u <- sqrt(su)*rnorm(p)

# Generate (large) QTL effects for the SNPs.
i    <- sample(p,na)
b    <- rep(0,p)
b[i] <- sqrt(sb)*rnorm(na)

# Generate the quantitative trait measurements.
y <- drop(X %*% (u + b) + rnorm(n))
y <- y - mean(y)

# FIT BSLMM MODEL
# ---------------
# Fit the variational approximation to the posterior distribution.
hyperparams        <- with(hyperparams,expand.grid(sigma,logodds,su,sb))
names(hyperparams) <- c("sigma","logodds","su","sb")
fit <- with(hyperparams,varbvslmm(X,y,sigma,logodds,su,sb))
w   <- exp(fit$logw - max(fit$logw))
w   <- w/sum(w)

# SUMMARIZE FIT
# -------------
# Compare the ground-truth QTL effects against the posterior mean
# estimates.
b.est <- fit$mu %*% w
plot(b,b.est,pch = 4)
abline(a = 0,b = 1,col = "dodgerblue",lty = "dotted")

# Summarize the posterior distribution over the hyperparameters.
cat(sprintf("Posterior mean of sqrt(su) is %0.6f.\n",
            sum(w*sqrt(hyperparams$su))))
cat(sprintf("Posterior mean of sqrt(sb) is %0.6f.\n",
            sum(w*sqrt(hyperparams$sb))))

# Show the hyperparameter settings with the largest weights.
cat("Hyperparameter settings with weight > 0.01:\n")
dat <- transform(hyperparams,
                 "sqrt(su)" = sqrt(su),
                 "sqrt(sb)" = sqrt(sb),
                 check.names = FALSE)
dat <- cbind(dat,data.frame(weight = w))
dat <- subset(dat,w > 0.01)
i   <- order(dat$weight,decreasing = TRUE)
print(dat[i,c("sigma","logodds","sqrt(su)","sqrt(sb)","weight")])
