# Create a 2-d grid for the conditional posterior mean of the
# coefficients ("mu").
dat <- data.frame(mu1 = mu_grid)
dat <- transform(dat,mu2 = -0.9275*mu1 + 2.5696)
dat <- cbind(dat,data.frame(alpha1 = 0,alpha2 = 0,KL = 0))
dat <- as.matrix(dat)
ns  <- nrow(dat)
for (i in 1:ns) {

  # Compute the posterior inclusion probabilities ("alpha").
  mu <- dat[i,1:2]
  a  <- compute.alpha(mu,s)
  dat[i,"alpha1"] <- a[1]
  dat[i,"alpha2"] <- a[2]
  
  # Compute the variational lower bound (ELBO) at the given settings
  # of the variational parameters.
  dat[i,"KL"] <- computeKL(X,y,a,mu,s)
}

# Second plot.
klmin <- min(dat[,"KL"])
dat   <- as.data.frame(dat)
dat   <- transform(dat,
                   beta1 = alpha1*mu1,
                   dist  = KL - klmin + 1e-8)
p2 <- ggplot(dat,aes(x = beta1,y = dist)) +
    geom_line()

    
