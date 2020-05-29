# A small script demonstrating the EM algorithm of Adamidis (1999) and
# Huang et al (2019) for fitting a negative binomial, and how it can
# be accelerated using DAAREM.
library(MASS)
library(daarem)

# SIMULATE DATA
# -------------
# Simulate data from negative binomial.
# In the notation of Huang et al (2019), r = a and theta = p.
set.seed(1)
n <- 400
a <- 20   # number of successes ("size").
p <- 0.1  # success rate.
x <- rnbinom(n,a,p)

# Randomly assign weights to the samples. When all the weights are the
# same, the EM algorithm should recover the maximum-likelihood
# estimate (MLE) computed by fitdistr.
# w <- runif(n)
w <- rep(1,n)

# Estimate the "size" (a) and success rate (p) parameters of the
# negative binomial using fitdistr.
fit <- fitdistr(x,"negative binomial")
a   <- fit$estimate["size"]
mu  <- fit$estimate["mu"]
fit <- list(a = a,p = a/(a + mu))

# Compute the log-likelihood for the negative binomial.
nbinom_loglik <- function (x, w, a, p, e = 1e-14) {
  a <- pmax(a,e)
  p <- pmin(pmax(p,e),1-e)
  return(sum(w*dnbinom(x,a,p,log = TRUE)))
}

# This implements a single EM update using the expressions given in
# Adamidis (1999) and Huang et al (2019).
nbinom_update <- function (x, w, a, p, e = 1e-14) {

  # Make sure a > 0 and p is [0,1].
  a <- pmax(a,e)
  p <- pmin(pmax(p,e),1-e)
    
  # E-step
  # ------
  b <- -(p/(1-p) + 1/log(p))
  d <- a*(digamma(a+x) - digamma(a))
    
  # M-step
  # ------
  p <- sum(b*w*d)/sum(w*(x + (b-1)*d))
  a <- -sum(w*d)/(log(p)*sum(w))
  
  # Make sure a > 0 and p is [0,1].
  a <- pmax(a,e)
  p <- pmin(pmax(p,e),1-e)
  return(list(a = a,p = p))
}

# Fit negative binomial model using EM, as it is described in Adamidis
# (1999) and Huang et al (2019).
p       <- 0.5  # Initial estimate of success rate.
a       <- 1    # Initial estimate of "size".
numiter <- 500
loglik  <- rep(0,numiter)
for (i in 1:numiter) {
  out       <- nbinom_update(x,w,a,p)
  a         <- out$a
  p         <- out$p
  loglik[i] <- nbinom_loglik(x,w,a,p)
}

# See if daarem finds a good solution more quickly than basic EM.
daarfit <- daarem(c(1,0.5),
                  function (par) unlist(nbinom_update(x,w,par[1],par[2])),
                  function (par) nbinom_loglik(x,w,par[1],par[2]))

# The solution should improve at each iteration.
loglik.best <- daarfit$value.objfn
plot(1:numiter,loglik.best - loglik + 1e-4,type = "l",
     col = "dodgerblue",lwd = 2,log = "y",xlab = "iteration",
     ylab = "distance from best loglik",ylim = c(1e-4,500))

# daarem may not improve the solution at eacch iteration, but should
# arrive at a fixed point more quickly.
lines(1:length(daarfit$objfn.track),loglik.best - daarfit$objfn.track + 1e-4,
      col = "darkorange",lwd = 2)

# Compare the EM and "fitdistr" solutions.
cat(sprintf("fitdistr loglik: %0.12f\n",nbinom_loglik(x,w,fit$a,fit$p)))
cat(sprintf("EM loglik:       %0.12f\n",nbinom_loglik(x,w,a,p)))
cat(sprintf("daarem loglik:   %0.12f\n",nbinom_loglik(x,w,daarfit$par["a"],
                                                      daarfit$par["p"])))
cat(sprintf("             a      p\n"))
cat(sprintf("fitdistr %5.2f %0.4f\n",fit$a,fit$p))
cat(sprintf("      EM %5.2f %0.4f\n",a,p))    
cat(sprintf("  daarem %5.2f %0.4f\n",daarfit$par["a"],daarfit$par["p"]))    
