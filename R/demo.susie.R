# EXPERIMENT PARAMETERS
# ---------------------
n <- 800
b <- c(1,0,-1)

# SET UP ENVIRONMENT
# ------------------
library(mvtnorm)
library(susieR)

# Set the sequence of pseudorandom numbers.
set.seed(1)

# SIMULATE DATA
# -------------
X <- rmvnorm(n,sigma = rbind(c(1,    0.99, 0),
                             c(0.99, 1,    0),
                             c(0,    0,    1,   0.99),
                             c(0,    0,    0.99,1,),
                             c(0,    0,    0,   0),
                             c(0,    0,    0,   0, 1, 0.99
                             c(0,    0,    0,   0, 0.99, 1

# Generate the quantitative trait measurements.
y <- c(X %*% b + rnorm(n))

# Center y and the columns of X so that an intercept is not needed in
# the linear regression.
X <- scale(X,center = TRUE,scale = FALSE)
y <- y - mean(y)

# FIT SUSIE MODEL
# ---------------
fit <- susie(X,y,L = 2)

susie_get_CS(fit)
