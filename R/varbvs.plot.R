# TO DO: Explain here what this script is for.

# SCRIPT PARAMETERS
# -----------------
n    <- 40      # Number of samples.
xcor <- 0.99    # Correlation between X1 and X2.
beta <- c(3,0)  # Coefficients used to simulate data.

# Compute the variational lower bound (ELBO) for all these candidate
# values of the conditional posterior mean of the coefficients ("mu")
mu <- seq(-2,4,0.025)

# FUNCTION DEFINITIONS
# --------------------
# Define some functions used in the analysis below.
dot <- function (x,y)
  sum(x*y)

norm2 <- function (x)
  sqrt(sum(x^2))

sigmoid <- function (x)
  1/(1 + exp(-x))

betavar <- function (p, mu, s)
  p*(s + (1 - p)*mu^2)

# Compute the posterior inclusion probabilities ("alpha") given the
# posterior mean coefficients ("mu") and posterior variances ("s")
# assuming the prior inclusion probabilities are 1/2, the residual
# variance is 1, and the prior variance on the nonzero coefficients is
# also 1.
compute.alpha <- function (mu, s)
  sigmoid(log(s)/2 + mu^2/(2*s))  

# TO DO: Explain here what this function does.
computeKL <- function (X, a, mu, s) {
  e <- 1e-15
  d <- diag(crossprod(X))
  r <- a*mu
  v <- betavar(a,mu,s)
  return(norm2(y - X %*% r)^2/2 + dot(d,v)/2
         + sum(a*log(2*a + e) + (1-a)*log(2*(1-a)+e))
         - dot(a,1 + log(s) - (s + mu^2))/2)
}

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
dat <- expand.grid(X1 = mu,X2 = mu)
dat <- as.matrix(cbind(dat,data.frame(KL = 0)))
ns  <- nrow(dat)
for (i in 1:ns) {

  # Compute the posterior inclusion probabilities ("alpha").
  mu <- dat[i,1:2]
  a  <- compute.alpha(mu,s)

  # Compute the variational lower bound (ELBO) at the given settings
  # of the variational parameters.
  dat[i,"KL"] <- computeKL(X,a,mu,s)
}

# PLOT OBJECTIVE SURFACE
# ======================
dat <- as.data.frame(dat)
p1  <- ggplot(dat,aes(X1,X2,z = log10(KL))) +
       geom_contour(bins = 50)
print(p1)
