# This is a very rudimentary implementation of the BSLMM model, in
# which the posterior distribution of the QTL (large) effects is
# computed using a fully-factorized variational approximation. In this
# implementation, the prior on the (small) polygenic effects is normal
# with mean zero and variance sigma*su, and the prior on the (large)
# QTL effects is the spike-and-slab, in which the prior inclusion
# probability is determined by the "logodds" hyperparameter
# (specifically, "logodds" it is the base-10 log-odds of being
# included in the model), and the "slab" is normal with zero mean and
# variance sigma*sb. Note that this parameterization is slightly
# different from the one used in the BSLMM paper (Zhou, Carbonetto &
# Stephens, 2013), so please pay close attention to the details.
varbvslmm <- function (X, y, sigma, logodds, su, sb) {

  # Get the number of samples (n), the number of variables (p), and
  # the number of hyperparameter settings (ns).
  n  <- nrow(X)
  p  <- ncol(X)
  ns <- length(sigma)

  # Compute the kinship matrix.
  K <- tcrossprod(X)

  # Initialize storage for the outputs.
  logw  <- rep(0,ns)
  alpha <- matrix(0,p,ns)
  mu    <- matrix(0,p,ns)
  s     <- matrix(0,p,ns)
          
  # Repeat for each setting of the hyperparameters.
  cat("iter -sigma- logodds ---su--- ---sb--- -----ELBO-----\n")
  for (i in 1:ns) {
    
    # Compute H, such that the covariance of Y is sigma*H.
    H <- diag(n) + su[i]*K
    R <- tryCatch(chol(H),error = function(e) FALSE)
    if (!is.matrix(R))
      logw <- 0
    else {

      # Remove the (small) polygenic effects.
      L    <- t(R)
      Xhat <- solve(L,X)
      yhat <- solve(L,y)

      # Compute the fully-factorized variational approximation for the
      # Bayesian variable selection model.
      #
      # Note that the "logodds" input argument in varbvsnorm is
      # defined in terms of the natural logarithm, so it is important
      # to make the appropriate adjustment.
      out <- varbvsnorm(Xhat,yhat,sigma[i],sb[i],rep(log(10)*logodds[i],p),
                        alpha = rep(0,p),mu = rep(0,p),update.order = 1:p,
                        update.sigma = FALSE,update.sa = FALSE,verbose = FALSE)
      alpha[,i] <- out$alpha
      mu[,i]    <- out$mu
      s[,i]     <- out$s
      logw[i]   <- max(out$logw) - determinant(H,logarithm = TRUE)$modulus/2
    }

    cat(sprintf("%4d %0.1e %+7.2f %0.2e %0.2e %+0.7e\n",
                i,sigma[i],logodds[i],su[i],sb[i],logw[i]))
  }

  return(list(logw = logw,alpha = alpha,mu = mu,s = s))
}
