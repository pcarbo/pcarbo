# A short script to illustrate that the co-ordinate ascent updates for
# varbvs do not always converge to a fixed point of the variational
# lower bound (ELBO).
library(MASS)
library(varbvs)
set.seed(1)

# The third varbvs call will not "escape" from the saddle point unless
# this non-negative scalar is set to 0.6 or greater.
delta <- 0.6

# Generate the data set.
n <- 100
S <- rbind(c(1,0.99),
           c(0.99,1))
X <- mvrnorm(n,c(0,0),S)
X <- scale(X,center = TRUE,scale = FALSE)
b <- c(3,0)
y <- drop(X %*% b + rnorm(n))
y <- y - mean(y)

# Fit the model (this should be a better fit).
alpha0 <- c(0.5,0.5)
mu0    <- c(0,0)
fit1   <- varbvs(X,NULL,y,sigma = 1,sa = 1,logodds = 0,update.sa = FALSE,
                 update.sigma = FALSE,alpha = alpha0,mu = mu0,
                 tol = 1e-10,verbose = FALSE,update.order = 1:2)

# Fit the model again (this should be a worse fit).
fit2 <- varbvs(X,NULL,y,sigma = 1,sa = 1,logodds = 0,update.sa = FALSE,
               update.sigma = FALSE,alpha = alpha0,mu = mu0,
               tol = 1e-10,verbose = FALSE,update.order = 2:1)

# Fit the model once more (this should stop at a "saddle point").
alpha0 <- c(1,1)
mu0    <- c(2.03,0.96) + c(delta,-delta)
fit3   <- varbvs(X,NULL,y,sigma = 1,sa = 1,logodds = 0,update.sa = FALSE,
                 update.sigma = FALSE,alpha = alpha0,mu = mu0,
                 maxiter = 20,tol = 1e-10,verbose = TRUE,update.order = 1:2)
print(max(abs(fit3$mu - mu0)))

# These are the estimated (posterior mean) coefficients for the three
# different fitted models.
print(round(cbind(coef(fit1),coef(fit2),coef(fit3)),digits=3))
