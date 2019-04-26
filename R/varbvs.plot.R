# TO DO: Explain here what this script is for.
#
# TO DO:
#
#  + Run varbvs with different initializations to recover local minima.
#
#  + Verify that my calculations of alpha and the variational lower
#     bound are correct.
# 

# SCRIPT PARAMETERS
# -----------------
n    <- 40      # Number of samples.
xcor <- 0.99    # Correlation between X1 and X2.
beta <- c(3,0)  # Coefficients used to simulate data.

# Compute the variational lower bound (ELBO) for all these candidate
# values of the conditional posterior mean of the coefficients ("mu")
mu_grid <- seq(-1,3.5,0.01)

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
dat <- expand.grid(mu1 = mu_grid,
                   mu2 = mu_grid)
dat <- as.matrix(cbind(dat,data.frame(alpha1 = 0,alpha2 = 0,KL = 0)))
ns  <- nrow(dat)
for (i in 1:ns) {

  # Compute the posterior inclusion probabilities ("alpha").
  mu <- dat[i,1:2]
  a  <- compute.alpha(mu,s)
  dat[i,"alpha1"] <- a[1]
  dat[i,"alpha2"] <- a[2]
  
  # Compute the variational lower bound (ELBO) at the given settings
  # of the variational parameters.
  dat[i,"KL"] <- computeKL(X,a,mu,s)
}

# PLOT OBJECTIVE SURFACE
# ======================
klmin <- min(dat[,"KL"])
dat   <- as.data.frame(dat)
dat   <- transform(dat,
                   beta1 = alpha1*mu1,
                   beta2 = alpha2*mu2,
                   dist = log10(KL - klmin + 1e-8))
p1 <- ggplot(dat,aes(beta1,beta2,z = dist)) +
  geom_contour(color = "black",bins = 50) +
  scale_x_continuous(breaks = seq(-10,10,0.25)) +
  scale_y_continuous(breaks = seq(-10,10,0.25)) +
  theme_cowplot(font_size = 10) +
  theme(panel.grid.major = element_line(color = "gray",size = 0.25))
print(p1)

# Create a 2-d grid for the conditional posterior mean of the
# coefficients ("mu").
dat <- data.frame(mu1 = mu_grid)
dat <- transform(dat,mu2 = -0.923*mu1 + 2.538)
dat <- cbind(dat,data.frame(alpha1 = 0,alpha2 = 0,KL = 0))
dat <- as.matrix(dat)
ns  <- nrow(dat)
for (i in 1:ns) {

  # Compute the posterior inclusion probabilities ("alpha").
  mu <- dat[i,1:2]
  a  <- compute.alpha(mu,s)
  dat[i,"alpha1"] <- a[1]
  dat[i,"alpha2"] <- a[2]
  
  # Compute the variational lower bound (ELBO) at the given settings
  # of the variational parameters.
  dat[i,"KL"] <- computeKL(X,a,mu,s)
}

# Second plot.
klmin <- min(dat[,"KL"])
dat   <- as.data.frame(dat)
dat   <- transform(dat,
                   beta1 = alpha1*mu1,
                   dist  = KL - klmin + 1e-8)
p2 <- ggplot(dat,aes(x = beta1,y = dist)) +
    geom_line()

    
