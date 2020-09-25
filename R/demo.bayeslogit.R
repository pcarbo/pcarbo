library(BayesLogit)

# SIMULATE DATA SET
# -----------------
set.seed(1)
n   <- 1000
p   <- 20
maf <- 0.05 + 0.45*runif(p)
X   <- (runif(n*p) < maf) +
       (runif(n*p) < maf)
X   <- matrix(as.double(X),n,p,byrow = TRUE)

# Generate additive effects for the markers so that exactly na of them
# have a nonzero effect on the trait.
b <- rnorm(p)

# Simulate the binary outcome as a coin toss with success rates given
# by the logistic regression.
sigmoid <- function (x)
  1/(1 + exp(-x))
y <- as.double(runif(n) < sigmoid(drop(X %*% b)))

# RUN BAYESIAN LOGISTIC REGRESSION
# --------------------------------
# TO DO: Add more comments here.
fit <- BayesLogit:::logit.R(y,X,samp = 1000,burn = 2000,verbose = 100)
plot(b,colMeans(fit$beta),pch = 20,xlab = "true",ylab = "BayesLogit")
abline(a = 0,b = 1,lty = "dotted",col = "skyblue")
