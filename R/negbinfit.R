# TO DO: Explain here what this script is for.

# SIMULATE DATA
# -------------
# Draw samples from the negative binomial distribution with mean
# t*(1-p)/p and variance t*(1-p)/p^2. Randomly assign weights to the
# samples.
set.seed(1)
n <- 200
t <- 2
p <- 0.1
x <- rnbinom(n,t,p)
w <- rep(1,n)

# Plot the log-likelihood surface.
params <- expand.grid(t = seq(0.1,5,0.25),p = seq(0.01,0.25,0.01))
params <- cbind(params,data.frame(logl = 0))
m      <- nrow(params)
for (i in 1:m) {
  t <- params[i,"t"]
  p <- params[i,"p"]
  params[i,"logl"] <- sum(w * dnbinom(x,t,p,log = TRUE))
}
library(ggplot2)
library(cowplot)
ggplot(params,aes(x = t,y = p,z = logl)) +
       geom_contour(color = "dodgerblue",bins = 50)

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
