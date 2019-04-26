# TO DO: Explain here what this script is for.

# SCRIPT PARAMETERS
# -----------------
n    <- 100     # Number of samples.
xcor <- 0.99    # Correlation between X1 and X2.
beta <- c(3,0)  # Coefficients used to simulate data.

# Compute the variational lower bound (ELBO) for all these candidate
# values of the conditional posterior mean of the coefficients ("mu")
mu <- seq(-5,5,0.1)

# FUNCTION DEFINITIONS
# --------------------
# Define some functions used in the analysis below.
sigmoid <- function (x)
  1/(1 + exp(-x))

# Compute the posterior inclusion probabilities ("alpha") given the
# posterior mean coefficients ("mu") and posterior variances ("s")
# assuming the prior inclusion probabilities are 1/2, the residual
# variance is 1, and the prior variance on the nonzero coefficients is
# also 1.
compute.alpha <- function (mu, s)
  sigmoid(log(s)/2 + mu^2/(2*s))  

# SET UP ENVIRONMENT
# ------------------
# Load the packages used in the analysis below.
library(MASS)
library(varbvs)
library(ggplot2)
library(cowplot)

# Initialize the sequence of pseudorandom numbers.
set.seed(1)

# GENERATE DATA SET
# -----------------
# Simulate the n x p matrix of predictors, X, with p = 2.
cat("Generating data set.\n")
S <- rbind(c(1,xcor),
           c(xcor,1))
X <- mvrnorm(n,c(0,0),S)
X <- scale(X,center = TRUE,scale = FALSE)

# Simulate the continuously-valued outcomes.
y <- drop(X %*% beta + rnorm(n))
y <- y - mean(y)

# COMPUTE OBJECTIVE SURFACE
# -------------------------
cat("Computing variational objective surface.\n")

# Compute the variational estimates of standard deviations ("s")
# assuming the residual variance is 1 and the prior on the
# coefficients is normal with zero mean and s.d. 1.
d <- diag(crossprod(X))
s <- 1/(d + 1)

# Create a 2-d grid for the conditional posterior mean of the
# coefficients ("mu").
M  <- as.matrix(expand.grid(X1 = mu,X2 = mu))
ns <- nrow(M)
for (i in 1:ns) {

  # Compute the posterior inclusion probabilities ("alpha").
  mu    <- M[i,]
  alpha <- compute.alpha(mu,s)
}

# Compute the variational lower bound (ELBO) for each 
