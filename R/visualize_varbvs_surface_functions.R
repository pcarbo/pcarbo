# Some functions used in the "visualize_varbvs_surface" analysis.
dot <- function (x,y)
  sum(x*y)

norm2 <- function (x)
  sqrt(sum(x^2))

sigmoid <- function (x)
  1/(1 + exp(-x))

betavar <- function (p, mu, s)
  p*(s + (1 - p)*mu^2)

# Compute the posterior inclusion probabilities ("alpha") given the
# posterior mean coefficients ("mu") and posterior variances ("s")
# assuming the prior inclusion probabilities are 1/2, the residual
# variance is 1, and the prior variance on the nonzero coefficients is
# also 1.
compute.alpha <- function (mu, s)
  sigmoid(log(s)/2 + mu^2/(2*s))  

# Compute the Kullback-Leibler divergence for the Bayesian variable
# selection model (the negative of the variational lower bound, or
# "ELBO") given the data (X, y) and the parameters specifying the
# approximate posterior distribution (a, mu, s). This is assuming the
# prior inclusion probabilities are 1/2, the residual variance is 1,
# and the prior variance of the nonzero coefficients is also 1.
computeKL <- function (X, y, a, mu, s) {
  n <- length(y)
  e <- 1e-15
  d <- diag(crossprod(X))
  r <- a*mu
  v <- betavar(a,mu,s)
  return(n*log(2*pi)/2 + log(n)/2 + norm2(y - X %*% r)^2/2 + dot(d,v)/2
         + sum(a*log(2*a + e) + (1-a)*log(2*(1-a)+e))
         - dot(a,1 + log(s) - (s + mu^2))/2)
}

