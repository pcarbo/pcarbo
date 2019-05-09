# TO DO: Explain here what this script is for.

# SIMULATE DATA
# -------------
# Draw samples from the negative binomial distribution with mean
# t*(1-p)/p and variance t*(1-p)/p^2. Randomly assign weights to the
# samples.
set.seed(1)
n <- 100
t <- 2
p <- 0.1
x <- rnbinom(n,t,p)
w <- runif(n)

# FIT MODEL
# ---------
# Repeat for 100 iterations.
t <- 4
p <- 0.5
for (i in 1:100) {

  # Update the success probability (p) with the "size" parameter (t)
  # fixed.
  y <- t*sum(w)/sum(w*x)
  p <- y/(y + 1)

  # Update the "size" parameter (t) with the success probability (p)
  # fixed.
  # TO DO.
}
