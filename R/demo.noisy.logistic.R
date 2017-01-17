# TO DO: Explain here what this script does, and how to use it.
library(ggplot2)
library(ggthemes)

# SCRIPT PARAMETERS
# -----------------
n    <- 500      # Number of data samples.
ns   <- 1000     # Number of Monte Carlo samples.
se   <- sqrt(2)  # True standard deviation of "noise".
beta <- (-1.5)   # True coefficient.

# Grid of candidate values for the logistic regression coefficient
# (beta) and the standard deviation of the noise (se).
b <- seq(-5,5,0.2)
s <- 10^seq(-3,3,0.2)

# USEFUL FUNCTIONS
# ----------------
# Sigmoid of x.
sigmoid <- function (x)
  1/(1 + exp(-x))

# Compute log-likelihood for logistic regression model.
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
cat("Generating data set.\n")
# Generate a binary observation (fair coin -1 or +1).
x <- 1 - 2*as.numeric(runif(n) < 0.5)

# Generate "noise".
e <- se * rnorm(n)

# Simulate coin flips y such that
#
#   Pr(y = 1) = f(x*beta + e), 
#
# where f is the sigmoid function.
y <- as.numeric(runif(n) < sigmoid(x*beta + e))

# Check the data.
print(summary(factor(x)))
print(summary(factor(y)))
print(mean(x[y == 0]))
print(mean(x[y == 1]))

# COMPUTE LOG-LIKELIHOOD SURFACE
# ------------------------------
cat("Computing log-likelihood surface.\n")
counts <- table(x = factor(x),y = factor(y))
params <- cbind(expand.grid(b = b,s = s),
                data.frame(L = 0))
for (i in 1:nrow(params)) {
  
  # Compute a Monte Carlo estimate of the log-likelihood.
  e             <- params[i,"s"] * rnorm(ns)
  params[i,"L"] <- mclogint(computeloglikelihood(counts,params[i,"b"],e))
}

# Plot the likelihood surface (up to a normalizing constant).
print(ggplot(params,aes(x = b,y = log10(s),colour = exp(L - max(L)))) +
      geom_point() +
      scale_x_continuous(breaks = seq(min(b),max(b))) +
      scale_color_gradient(low = "lightcyan",high = "orangered") +
      theme_minimal() +
      theme(panel.grid.major = element_blank(),
            panel.grid.minor = element_blank()) +
      ggtitle("Likelihood surface for 'noisy' regression model.") +
      xlab("beta") +
      ylab("log10(se)"))
