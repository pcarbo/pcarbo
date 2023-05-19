# TO DO: Explain here what this script is for, and how to use it.
library(pracma)
set.seed(1)

# We will restrict our attention to the interval [a,b].
a <- -6
b <- 6
    
# Plot the polynomial density.
p <- c(-0.05,0.1,0.3,-0.2,0.1)
logf <- function (x)
  polyval(p,x)
f <- function (x)
  exp(logf(x))
n0 <- 1000
x <- seq(a,b,length.out = n0)
y <- f(x)
plot(x,y/sum(y),type = "l",lwd = 3,col = "cyan",ylab = "pdf(x)")

# Compute the mean and standard deviation under the polynomial density
# using quadrature.
Z  <- quad(function (x) f(x),a,b,tol = 1e-14)
m  <- quad(function (x) x*f(x)/Z,a,b,tol = 1e-14)
m2 <- quad(function (x) x^2*f(x)/Z,a,b,tol = 1e-14)
s  <- sqrt(m2 - m^2)
cat("Quadrature estimates:\n")
cat(sprintf("mean: %0.8f\n",m))
cat(sprintf("s.d.: %0.8f\n",s))
cat(sprintf("logZ: %0.8f\n",log(Z)))

# Draw samples uniformly "at random" from the interval [a,b].
n <- 24
x <- seq(a,b,length.out = n)
# x <- sort(runif(n,a,b))

# Compute the log-importance weights.
logw <- logf(x)

# This function takes as input an array of unnormalized
# log-probabilities logw and returns normalized probabilities such
a# that the sum is equal to 1.
normalizelogweights <- function (logw) {

  # Guard against underflow or overflow by adjusting the
  # log-probabilities so that the largest probability is 1.
  c <- max(logw)
  w <- exp(logw - c)

  # Normalize the probabilities.
  return(w/sum(w))
}

# TO DO: Explain here what this function does, and how to use it.
logmean <- function (logw) {
  c <- max(logw)
  w <- exp(logw - c)
  return(c + log(mean(w)))
}

# Compute the normalized importance weights.
logZ <- logmean(logw) + log(b - a)
w  <- normalizelogweights(logw)
m  <- sum(x*w)
m2 <- sum(x^2*w)
s  <- sqrt(m2 - m^2)
lines(x,w*n/n0,lwd = 1,col = "darkblue")
points(x,w*n/n0,pch = 20,cex = 0.65,col = "darkblue")
cat("Monte Carlo estimates:\n")
cat(sprintf("mean: %0.8f\n",m))
cat(sprintf("s.d.: %0.8f\n",s))
cat(sprintf("logZ: %0.8f\n",logZ))
