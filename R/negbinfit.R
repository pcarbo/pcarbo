# A short script illustrating how to implement a simple co-ordinate
# ascent algorithm to fit a negative binomial model when the samples
# are weighted. Precisely, this computes maximum-likelihood estimates
# of the "size" (t) and "probability of success" (p) parameters
# specifying the negative binomial distribution.

# SIMULATE DATA
# -------------
# Draw samples from the negative binomial distribution with mean
# t*(1-p)/p and variance t*(1-p)/p^2. Randomly assign weights to the
# samples. Variables t and p specify the "size" and "prob" arguments,
# respectively, to the "dnbinom" function.
set.seed(1)
n <- 200
t <- 2
p <- 0.1
x <- rnbinom(n,t,p)
w <- rep(1,n)

# Without loss of generality, normalize the probabilities so that sum
# to 1.
w <- w/sum(w)

# FIT MODEL
# ---------
# Initialize the estimates of t and p.
t <- 4
p <- 0.5

# Repeat the co-ordinate ascent updates for 50 iterations.
tmin <- 0.01
tmax <- 100
cat("iter log-likelihood\n")
for (i in 1:50) {

  # Update the success probability (p) with the "size" parameter (t)
  # fixed.
  y <- t/sum(w*x)
  p <- y/(y + 1)

  # Update the "size" parameter (t) with the success probability (p)
  # fixed.
  g <- function (t) sum(w * digamma(x + t)) - digamma(t) + log(p)
  t <- uniroot(g,c(tmin,tmax))$root

  # Print the log-likelihood at the current solution.
  cat(sprintf("%4d %+0.11f\n",i,sum(w * dnbinom(x,t,p,log = TRUE))))
}
