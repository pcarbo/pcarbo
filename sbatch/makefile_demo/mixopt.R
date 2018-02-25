# Compute the mixture distribution objective function given n x k
# conditional likelihood matrix L and mixture weights w, where n is
# the number of samples and k is the number of mixture components.
mixopt.objective <- function (L, w) {
 if (any(w < 0))
   return(Inf)
 else
   return(-sum(log(drop(L %*% w) + eps)))
}

# Fit a mixture model using EM. Input argument L is the n x k
# conditional likelihood matrix, where n is the number of samples and
# k is the number of mixture components; optional input argument w is
# the initial estimate of the mixture weights.
mixopt.em <- function (L, w, maxiter = 1e4, tol = 1e-4, verbose = TRUE) {

  # Get the number of mixture components.
  k <- ncol(L)
    
  # Initialize the mixture weights.
  if (missing(w))
    w <- rep(1/k,k)
    
  # Initialize storage for outputs obj and maxd.
  obj  <- rep(0,maxiter)
  maxd <- rep(0,maxiter)

  # Initialize storage for the timings output.
  timing           <- matrix(0,maxiter,3)
  timing[1,]       <- summary(proc.time())
  colnames(timing) <- names(summary(proc.time()))

  # Compute the objective function value at the initial iterate.
  f <- mixopt.objective(L,w)
  
  # Repeat until convergence criterion is met, or until the maximum
  # number of iterations is reached.
  if (verbose)
    cat("iter     objective max delta\n")
  for (iter in 2:maxiter) {

    # Save the current estimate of the mixture weights and the current
    # objective function value.
    f0 <- f
    w0 <- w

    # E STEP
    # Compute the posterior probabilities
    P <- scale.cols(L,w)
    P <- P / (rowSums(P) + eps)

    # M STEP
    # Update the mixture weights.
    w <- colMeans(P)
    
    # COMPUTE OBJECTIVE
    f <- mixopt.objective(L,w)
    
    # CHECK CONVERGENCE
    # Print the status of the algorithm and check the convergence
    # criterion. Convergence is reached when the maximum difference
    # between the mixture weights at two successive iterations is less
    # than the specified tolerance, or when objective increases.
    maxd[iter]    <- max(abs(w - w0))
    obj[iter]     <- f
    timing[iter,] <- summary(proc.time())
    if (verbose) {
      progress.str <- sprintf("%4d %+0.6e %0.3e",iter,f,maxd[iter])
      cat(progress.str)
      cat(rep("\r",nchar(progress.str)))
    }
    if (f > f0) {
      maxd[iter] <- 0
      obj[iter]  <- f0
      w          <- w0
      break
    } else if (maxd[iter] < tol)
      break
  }
  if (verbose)
    cat("\n")

  # Reset the timings to zero.
  timing <- timing[1:iter,]
  timing <- subtract.cols(timing,timing[1,])
  
  # Return the fitted model parameters and other optimization info.
  fit <- list(L = L,w = w,maxd = maxd[1:iter],obj = obj[1:iter],
              timing = timing)
  class(fit) <- c("mixopt.em","list")
  return(fit)
}
