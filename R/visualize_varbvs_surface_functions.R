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
compute_alpha <- function (mu, s)
  sigmoid(log(s)/2 + mu^2/(2*s))  

# Compute the Kullback-Leibler divergence for the Bayesian variable
# selection model (the negative of the variational lower bound, or
# "ELBO") given the data (X, y) and the parameters specifying the
# approximate posterior distribution (a, mu, s). This is assuming the
# prior inclusion probabilities are 1/2, the residual variance is 1,
# and the prior variance of the nonzero coefficients is also 1.
compute_kl <- function (X, y, a, mu, s) {
  n <- length(y)
  e <- 1e-15
  d <- diag(crossprod(X))
  r <- a*mu
  v <- betavar(a,mu,s)
  return(n*log(2*pi)/2 + log(n)/2 + norm2(y - X %*% r)^2/2 + dot(d,v)/2
         + sum(a*log(2*a + e) + (1-a)*log(2*(1-a)+e))
         - dot(a,1 + log(s) - (s + mu^2))/2)
}

# Apply function compute_kl to each 2-d grid point of the "mu"
# variational parameters; input argument "mu" is an n x 2 matrix or
# data frame, where n is the number of settings of the variational
# parameters. The output is a data frame with five columns containing
# the input, the posterior inclusion probabilities ("alpha1",
# "alpha2") and the value of the KL-divergence ("KL").
compute_kl_grid <- function (X, y, mu) {

  # Compute the variational estimates of standard deviations ("s")
  # assuming the residual variance is 1 and the prior on the
  # coefficients is normal with zero mean and s.d. 1.
  d <- diag(crossprod(X))
  s <- 1/(d + 1)

  # Initialize the return value.  
  out <- as.matrix(cbind(mu,data.frame(alpha1 = 0,alpha2 = 0,KL = 0)))

  # Repeat for each setting of "mu".
  n <- nrow(out)
  for (i in 1:n) {
    mu <- out[i,1:2]
    a  <- compute_alpha(mu,s)
    out[i,"alpha1"] <- a[1]
    out[i,"alpha2"] <- a[2]
    out[i,"KL"]     <- compute_kl(X,y,a,mu,s)
  }

  return(out)
}

# Create a contour plot from matrix or data frame "dat", which is an
# output of function "compute_kl_grid". Argument "dat" should have (at
# least) three columns, "mu", "mu2" and "KL". Additional points to be
# plotted are contained in matrix or data frame "pts" containing
# columns "X1" and "X2".
create_contour_plot <- function (dat, pts) {
  dat <- as.data.frame(dat)
  pts <- as.data.frame(pts)

  # Get the global minimum of the K-L objective.
  klmin <- min(dat$KL)

  # For each setting of "mu", compute the posterior mean estimate and
  # the distance to the global minimum.
  dat <- transform(dat,
                   beta1 = alpha1*mu1,
                   beta2 = alpha2*mu2,
                   dist  = log10(KL - klmin + 1e-8))

  # Create the contour plot, and add points to the contour plot.
  return(ggplot(dat,aes_string(x = "beta1",y = "beta2",z = "dist")) +
         geom_contour(color = "dodgerblue",bins = 50) +
         geom_point(data = pts,mapping = aes_string(x = "X1",y = "X2"),
                    color = "black",shape = 4,inherit.aes = FALSE) +
         scale_x_continuous(breaks = seq(-10,10)) +
         scale_y_continuous(breaks = seq(-10,10)) +
         theme_cowplot(font_size = 12) +
         labs(x = "posterior mean of beta1",
              y = "posterior mean of beta2"))
}
