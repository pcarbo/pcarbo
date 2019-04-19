# TO DO: Explain here what this script is for.
library(MASS)
library(varbvs)
set.seed(1)

# Generate the data set.
S <- rbind(c(1,0.9),
           c(0.9,1))
X <- mvrnorm(n,rep(0,p),S)
b <- c(3,0)
y <- drop(X %*% b + rnorm(n))

# Fit the model.
mu0    <- rep(0,2)
alpha0 <- rep(0.5,2)
fit    <- varbvs(X,NULL,y,sa = 1,logodds = 0,update.sa = FALSE,
                 update.sigma = FALSE,alpha = alpha0,mu = mu0)


