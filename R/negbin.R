library(MASS)

# SIMULATE DATA
# -------------
# Simulate data from negative binomial.
# In the notation of Huang et al (2019), r = a and theta = p.
set.seed(1)
n <- 400
a <- 20   # Number of successes.
p <- 0.1  # success rate.
x <- rnbinom(n,a,p)

# Fit negative binomial model using fitdistr.
fit <- fitdistr(x,"negative binomial")
a   <- fit$estimate["size"]
mu  <- fit$estimate["mu"]
fit$estimate <- c(a,a/(a + mu))
names(fit$estimate) <- c("a","p")

# Fit negative binomial model using EM algorithm Adamidis (1999) and
# Huang et al (2019).
p <- 0.5
a <- 1
numiter <- 250
loglik <- rep(0,numiter)
for (iter in 1:numiter) {

  # E-step
  # ------
  b <- 1 - 1/(1 - p) - 1/log(p)
  d <- a*(digamma(a + x) - digamma(a))
    
  # M-step
  # ------
  y <- mean(d)
  p <- b*mean(d)/mean(x - (1-b)*d)
  a <- -y/log(p)

  # Compute the log-likelihood at the current estimates.
  loglik[iter] <- sum(dnbinom(x,a,p,log = TRUE))
}

# Compare the solutions.
print(sum(dnbinom(x,fit$estimate["a"],fit$estimate["p"],log = TRUE)),digits = 12)
print(sum(dnbinom(x,a,p,log = TRUE)),digits = 12)
