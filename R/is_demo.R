# In this script we illustrate computing moments and other integrals
# for a polynomial exponential distribution in a numerically stable
# way using importance sampling.
library(pracma)
set.seed(1)

# We will restrict our attention to the interval [a,b].
a <- -6
b <- 6
    
# These are the coefficients of the polynomial (first input passed
# to "polyval").
p <- c(-0.05,0.1,0.3,-0.2,0.1)

# Plot the polynomial exponential density.
logf <- function (x)
  polyval(p,x)
f <- function (x)
  exp(logf(x))
n0 <- 1000
x <- seq(a,b,length.out = n0)
y <- f(x)
plot(x,y/sum(y),type = "l",lwd = 3,col = "cyan",ylab = "pdf(x)")

# First, compute the mean and standard deviation under the polynomial
# exponentiial density using quadrature with a low tolerance (for
# better accuracy). We'll use this as a point of comparison for our
# importance sampling estimates.
Z  <- quad(function (x) f(x),a,b,tol = 1e-14)
m  <- quad(function (x) x*f(x)/Z,a,b,tol = 1e-14)
m2 <- quad(function (x) x^2*f(x)/Z,a,b,tol = 1e-14)
s  <- sqrt(m2 - m^2)
cat("Quadrature estimates:\n")
cat(sprintf("mean=%0.8f\n",m))
cat(sprintf("s.d.=%0.8f\n",s))
cat(sprintf("logZ=%0.8f\n",log(Z)))

# Draw a small number of "random samples" uniformly from the interval
# [a,b].
n <- 24
x <- seq(a,b,length.out = n)

# Compute the log-importance weights.
logw <- logf(x)

# This function takes as input an array of unnormalized
# log-probabilities logw and returns normalized probabilities such
# that the sum is equal to 1.
normalizelogweights <- function (logw) {
  c <- max(logw)
  w <- exp(logw - c)
  return(w/sum(w))
}

# This function computes log(mean(w)) given input logw = log(w) in a
# numerically stable way.
logmean <- function (logw) {
  c <- max(logw)
  w <- exp(logw - c)
  return(c + log(mean(w)))
}

# Estimate the logarithm of the normalizing constant, Z, such that
# f(x)/Z integrates to 1, and where f(x) is the polynomial exponential
# density.
logZ <- logmean(logw) + log(b - a)

# Compute the normalized importance weights, and plot them.
w <- normalizelogweights(logw)
lines(x,w*n/n0,lwd = 1,col = "darkblue")
points(x,w*n/n0,pch = 20,cex = 0.65,col = "darkblue")

# Compute importance sampling estimates of the mean and s.d. under the
# polynomial exponential density.
m  <- sum(x*w)
m2 <- sum(x^2*w)
s  <- sqrt(m2 - m^2)
cat("Monte Carlo estimates:\n")
cat(sprintf("mean: %0.8f\n",m))
cat(sprintf("s.d.: %0.8f\n",s))
cat(sprintf("logZ: %0.8f\n",logZ))
