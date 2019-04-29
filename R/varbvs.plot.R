# GENERATE DATA SET
# -----------------
# Simulate the n x p matrix of predictors, X, with p = 2.
cat("Generating data set.\n")
S <- rbind(c(1,xcor),c(xcor,1))
X <- mvrnorm(n,c(0,0),S)
X <- scale(X,center = TRUE,scale = FALSE)

# Simulate the continuously-valued outcomes.
y <- drop(X %*% beta + rnorm(n))
y <- y - mean(y)

# RUN VARBVS WITH DIFFERENT INITIALIZATIONS
# -----------------------------------------
cat("Fitting varbvs with three different initializations.\n")
fit1 <- varbvs(X,NULL,y,sigma = 1,sa = 1,logodds = 0,tol = 0,
               alpha = alpha0[1,],mu = mu0[1,],verbose = FALSE)
fit2 <- varbvs(X,NULL,y,sigma = 1,sa = 1,logodds = 0,tol = 0,
               alpha = alpha0[2,],mu = mu0[2,],verbose = FALSE)
fit3 <- varbvs(X,NULL,y,sigma = 1,sa = 1,logodds = 0,tol = 0,
               alpha = alpha0[3,],mu = mu0[3,],verbose = FALSE)

# COMPUTE OBJECTIVE SURFACE
# -------------------------
cat("Computing variational objective surface.\n")

# Compute the variational estimates of standard deviations ("s")
# assuming the residual variance is 1 and the prior on the
# coefficients is normal with zero mean and s.d. 1.
d <- diag(crossprod(X))
s <- 1/(d + 1)

# Create a 2-d grid for the conditional posterior mean of the
# coefficients ("mu").
dat <- expand.grid(mu1 = mu_grid,mu2 = mu_grid)
dat <- as.matrix(cbind(dat,data.frame(alpha1 = 0,alpha2 = 0,KL = 0)))
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

    
