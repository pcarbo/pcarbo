# PLOT OBJECTIVE SURFACE
# ======================
klmin <- min(dat[,"KL"])
pdat1 <- as.data.frame(dat)
pdat1 <- transform(pdat1,
                   beta1 = alpha1*mu1,
                   beta2 = alpha2*mu2,
                   dist = log10(KL - klmin + 1e-8))
pdat2 <- rbind(fit1$beta,
               fit2$beta,
               fit3$beta)
pdat2 <- as.data.frame(pdat2)
p1 <- ggplot(pdat1,aes(beta1,beta2,z = dist)) +
  geom_contour(color = "dodgerblue",bins = 50) +
  geom_point(data = pdat2,mapping = aes(x = X1,y = X2),
             color = "black",shape = 4,inherit.aes = FALSE) +
  scale_x_continuous(breaks = seq(-10,10,0.5)) +
  scale_y_continuous(breaks = seq(-10,10,0.5)) +
  theme_cowplot(font_size = 10)
print(p1)

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

    
