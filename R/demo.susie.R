# TO DO: Explain here what this script does.

# SET UP ENVIRONMENT
# ------------------
# Load the susie and mvtnorm packages used in the toy example below.
library(mvtnorm)
library(varbvs)
library(susieR)

# Initialize the sequence of pseudorandom numbers.
set.seed(1)

# SIMULATE DATA
# -------------
cat("Generating data set.\n")
n <- 150
b <- c(1.1,0,-0.75,0,0)
S <- rbind(c(   1, 0.99,  0.5,  0.5, 0.8),
           c(0.99,    1,  0.5,  0.5, 0.8),
           c( 0.5,  0.5,    1, 0.99, 0.8),
           c( 0.5,  0.5, 0.99,    1, 0.8),
           c( 0.8,  0.8,  0.8,  0.8,   1))
X <- rmvnorm(n,sigma = S)

# Generate the quantitative trait measurements.
y <- c(X %*% b + 3*rnorm(n))

# Center y and the columns of X so that an intercept is not needed in
# the linear regression.
X <- scale(X,center = TRUE,scale = FALSE)
y <- y - mean(y)

# GET UNIVARIATE P-VALUES
# -----------------------
cat("Computing single-regression p-values.\n")
for (i in 1:5) {
  dat        <- data.frame(cbind(X[,i],y))
  names(dat) <- c("x","y")
  cat(sprintf("x%d: %0.3f\n",i,
              summary(lm(y ~ x,dat))$coefficients["x","Pr(>|t|)"]))
}

# FIT VARBVS MODEL
# ----------------
fit1 <- varbvs(X,NULL,y,logodds = log10(0.5),sigma = 1)
print(summary(fit1)$top.vars)

# FIT SUSIE MODEL
# ---------------
fit3 <- susie(X[,c(1:2,5)],y,L = 1,standardize = FALSE,
              estimate_prior_variance = FALSE,
              scaled_prior_variance = 1,
              min_abs_corr = 0,tol = 1e-8)
print(susie_get_cs(fit3,X,min_abs_corr = 0))

fit4 <- susie(X[,3:5],y,L = 1,standardize = FALSE,
              estimate_prior_variance = FALSE,
              scaled_prior_variance = 1,
              min_abs_corr = 0,tol = 1e-8)
print(susie_get_cs(fit4,X,min_abs_corr = 0))

fit2 <- susie(X,y,L = 2,standardize = FALSE,estimate_prior_variance = FALSE,
              scaled_prior_variance = 1,min_abs_corr = 0,tol = 1e-8)
print(susie_get_cs(fit2,X,min_abs_corr = 0))

