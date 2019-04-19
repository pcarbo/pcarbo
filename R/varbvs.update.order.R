# This is a small script to compare performance of different
# co-ordinate ascent update orders.
library(MASS)
library(varbvs)
library(ggplot2)
library(cowplot)

# SCRIPT PARAMETERS
# -----------------
nsim  <- 40   # Number of simulations.
n     <- 200  # Number of samples in training set.
ntest <- 100  # Number of samples in test set.
p     <- 40   # Number of predictors.
na    <- 4    # Number of non-zero effects.
s     <- 0.5  # Correlation between predictors.
se    <- 3    # s.d. of the residual.

# Initialize the sequence of pseudorandom numbers.
set.seed(1)

# Initialize storage for the results.
numiter1 <- rep(0,nsim)
numiter2 <- rep(0,nsim)
logw1    <- rep(0,nsim)
logw2    <- rep(0,nsim)

# Repeat for each simulation.
for (j in 1:nsim) {
  cat("Simulation",j,"out of",nsim,"\n")
  
  # GENERATE DATA
  # -------------
  # Generate the predictors.
  S       <- matrix(s,p,p)
  diag(S) <- 1
  X       <- mvrnorm(n,rep(0,p),S)

  # Generate coefficient so that exactly na of them have
  # a nonzero effect on the outcome.
  i    <- sample(p,na)
  b    <- rep(0,p)
  b[i] <- rnorm(na)

  # Generate the outcomes.
  y <- drop(X %*% b + rnorm(n,sd = se))

  # FIT MODEL
  # ---------
  sa      <- 1/se
  logodds <- log10(na/p)
  mu0     <- rep(0,p)
  out1 <- capture.output(
    fit1 <- varbvs(X,NULL,y,sa = sa,logodds = logodds,mu = mu0,
                   update.order = c(1:p,1:p)))
  out2 <- capture.output(
    fit2 <- varbvs(X,NULL,y,sa = sa,logodds = logodds,mu = mu0,
                   update.order = c(1:(p-1),p:2)))
  
  # STORE OPTIMIZATION RESULTS
  # --------------------------
  numiter1[j] <- length(out1) - 17
  numiter2[j] <- length(out2) - 17
  logw1[j]    <- fit1$logw
  logw2[j]    <- fit2$logw
}

# SUMMARIZE RESULTS
# -----------------
p1 <- quickplot(numiter1,numiter2) +
      geom_abline(intercept = 0,slope = 1,color = "darkorange",
                  linetype = "dotted") +
      xlim(0,75) +
      ylim(0,75) +
      labs(x = "# iter (forward pass only)",
           y = "# iter (forward-backward pass)") +
      theme_cowplot(font_size = 11)
p2 <- quickplot(logw1,logw2) +
      geom_abline(intercept = 0,slope = 1,color = "darkorange",
                  linetype = "dotted") +
      labs(x = "ELBO (forward pass only)",
           y = "ELBO (forward-backward pass)") +
      theme_cowplot(font_size = 11)
plot_grid(p1,p2)
