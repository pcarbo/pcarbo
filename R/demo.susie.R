# TO DO: Explain here what this script does.

# SET UP ENVIRONMENT
# ------------------
# Load the susie and mvtnorm packages used in the toy example below.
library(mvtnorm)
library(susieR)

# Initialize the sequence of pseudorandom numbers.
set.seed(1)

# SIMULATE DATA
# -------------
cat("Simulating")
n <- 100
b <- c(1,0,1,0,0)
S <- rbind(c(   1, 0.99,  0.5,  0.5, 0.8),
           c(0.99,    1,  0.5,  0.5, 0.8),
           c( 0.5,  0.5,    1, 0.99, 0.8),
           c( 0.5,  0.5, 0.99,    1, 0.8),
           c( 0.8,  0.8,  0.8,  0.8,   1))
X <- rmvnorm(n,sigma = S)

# Generate the quantitative trait measurements.
y <- c(X %*% b + 4*rnorm(n))

# Center y and the columns of X so that an intercept is not needed in
# the linear regression.
X <- scale(X,center = TRUE,scale = FALSE)
y <- y - mean(y)

# GET UNIVARIATE P-VALUES
# -----------------------
cat()
for (i in 1:5) {
  dat        <- data.frame(cbind(X[,i],y))
  names(dat) <- c("x","y")
  cat(sprintf("x%d: %0.3f\n",i,
              summary(lm(y ~ x,dat))$coefficients["x","Pr(>|t|)"]))
}
  
# FIT SUSIE MODEL
# ---------------
fit1 <- susie(X,y,L = 2,standardize = FALSE,estimate_prior_variance = FALSE,
              scaled_prior_variance = 1,min_abs_corr = 0,tol = 1e-8,
              s_init = susie_init_coef(c(1,3),c(1,1),5))
print(susie_get_cs(fit1,X,min_abs_corr = 0))

fit2 <- susie(X,y,L = 2,standardize = FALSE,estimate_prior_variance = FALSE,
              scaled_prior_variance = 1,min_abs_corr = 0,tol = 1e-8,
              s_init = susie_init_coef(1:5,c(1,-3,-1,2,1),5))
print(susie_get_cs(fit2,X,min_abs_corr = 0))
