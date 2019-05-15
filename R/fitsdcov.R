# TO DO: Explain here what this script does.
library(mvtnorm)

# Script parameters.
n <- 100
S <- rbind(c(1.0, 0.2),
           c(0.2, 0.6))
D <- diag(c(1,1))

# Simulate data.
set.seed(1)
X <- rmvnorm(n,sigma = S + D)

# Find best fit natively.


# Fit model.


