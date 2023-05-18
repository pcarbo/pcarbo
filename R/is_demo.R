# TO DO: Explain here what this script is for, and how to use it.
library(pracma)
set.seed(1)

# We will restrict our attention to the interval [a,b].
a <- -6
b <- 6
    
# Plot the polynomial density.
p <- c(-0.05,0.1,0.3,-0.2,0.1)
f <- function (x)
  exp(polyval(p,x))
x <- seq(a,b,length.out = 1000)
y <- f(x)
plot(x,y,type = "l")

# Compute the mean and standard deviation under the polynomial density
# using quadrature.
Z  <- quad(function (x) f(x),a,b)
m  <- quad(function (x) x*f(x)/Z,a,b)
m2 <- quad(function (x) x^2*f(x)/Z,a,b)
s  <- sqrt(m2 - m^2)
cat(sprintf("mean: %0.4f\n",m))
cat(sprintf("mean: %0.4f\n",s))
cat(sprintf("logZ: %0.4f\n",log(Z)))
