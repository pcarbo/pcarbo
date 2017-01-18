# Demonstration estimation of noise parameter in "noisy" regression
# model is not possible.
library(ggplot2)
library(ggthemes)

# SCRIPT PARAMETERS
# -----------------
n    <- 500      # Number of data samples.
ns   <- 1000     # Number of Monte Carlo samples.
se   <- sqrt(2)  # True standard deviation of "noise".
beta <- (-2)     # True coefficient.

# Grid of candidate values for the logistic regression coefficient
# (beta) and the standard deviation of the noise (se).
b <- seq(-5,5,0.2)
s <- 10^seq(-3,3,0.1)

# SOME USEFUL FUNCTIONS
# ---------------------
# Sigmoid of x.
sigmoid <- function (x)
  1/(1 + exp(-x))

# Compute log-likelihood for logistic regression model (as it is
# defined below).
computeloglikelihood <- function (counts, b, e)
  counts["-1","0"] * log(1 - sigmoid(e - b)) +
  counts[ "1","0"] * log(1 - sigmoid(e + b)) +
  counts["-1","1"] * log(sigmoid(e - b)) +
  counts[ "1","1"] * log(sigmoid(e + b))

# Compute Monte Carlo estimate of log(I(f)), where I(f) is the
# integral over f(x) p(x), in which the input gives the logarithm of
# function f(x) at points x drawn from distribution p(x). This is done
# in a way to avoid underflow or overflow which can easily occur when
# computing likelihoods.
mclogint <- function (x)
  max(x) + log(mean(exp(x - max(x))))

# GENERATE DATA SET
# -----------------
# Generate binary observations (fair coin with values of -1 or +1).
cat("Generating data set.\n")
set.seed(1)
x <- 1 - 2*as.numeric(runif(n) < 0.5)

# Simulate coin flips y such that
#
#   Pr(y = 1) = f(x*beta + e), 
#
# where f is the sigmoid function, and e ~ N(0,se^2)
y <- as.numeric(runif(n) < sigmoid(x*beta + se * rnorm(n)))

# Check the data.
print(summary(factor(x)))
print(summary(factor(y)))
print(mean(x[y == 0]))
print(mean(x[y == 1]))

# COMPUTE LOG-LIKELIHOOD SURFACE
# ------------------------------
cat("Computing log-likelihood for each candidate combination.\n")
counts <- table(x = factor(x),y = factor(y))
out    <- cbind(expand.grid(beta = b,se = s),
                data.frame(logw = 0))
for (i in 1:nrow(out)) {
  
  # Compute a simple Monte Carlo estimate of the log-likelihood.
  b <- out$beta[i]
  s <- out$se[i]
  out$logw[i] <- mclogint(computeloglikelihood(counts,b,s*rnorm(ns)))
}

# Plot the likelihood surface (up to a normalizing constant).
out <- transform(out,w = exp(logw - max(logw)))
print(ggplot(out,aes(x = beta,y = log10(se),colour = w)) +
      geom_point() +
      scale_x_continuous(breaks = seq(min(out$beta),max(out$beta))) +
      scale_y_continuous(breaks = seq(min(log10(out$se)),max(log10(out$se)))) +
      scale_color_gradient(low = "lightcyan",high = "orangered") +
      theme_minimal() +
      theme(panel.grid.major = element_blank(),
            panel.grid.minor = element_blank()) +
      ggtitle("Likelihood surface for 'noisy' regression model.") +
      xlab("beta") +
      ylab("log10(se)"))
