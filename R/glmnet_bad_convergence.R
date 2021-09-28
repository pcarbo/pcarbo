library(glmnet)
library(mr.ash.alpha)

# Simulate data from the linear regression model y = x1*b1 + x2*b2 + e,
# e ~ N(0,1) with b1 = 0.1, b2 = 0.1.
set.seed(1)
n    <- 1000
x    <- rnorm(n)
X    <- cbind(x,x)
beta <- c(0.1,0.1)
y    <- drop(X %*% beta + rnorm(n))

# With the default settings, the glmnet estimates of b1 and b2 are
# different.
fit <- glmnet(X,y,alpha = 0.9,standardize = FALSE)
b   <- as.matrix(coef(fit))[-1,]
print(max(abs(b[1,] - b[2,])))
print(fit$npasses)

# If we make the convergence criteria more stringent, the estimates
# become the same, but it takes many more iterations of the
# coordinate-wise updates to reach the solution (in which the two
# estimates are the same, or nearly the same).
fit <- glmnet(X,y,alpha = 0.9,thresh = 1e-15,standardize = FALSE)
b   <- as.matrix(coef(fit))[-1,]
print(max(abs(b[1,] - b[2,])))
print(fit$npasses)

# Does the coordinate ascent algorithm for mr.ash have a similar
# convergence difficulty? Here we fit a mr.ash model with prior b ~
# 0.5*N(0,1) + 0.5*N(0,2). Even after running 100 iterations, the
# estimates of b1 and b2 are very different still.
fit <- mr.ash(X,y,sigma = 1,sa = c(0,1,2),pi = c(0,0.5,0.5),
              update.pi = FALSE,update.sigma = FALSE,max.iter = 100)
print(fit$beta)

# The coordinate ascent updates eventually reach the solution but only
# after many more iterations (in which the two estimates are the same,
# or nearly the same).
fit <- mr.ash(X,y,sigma = 1,sa = c(0,1,2),pi = c(0,0.5,0.5),
              update.pi = FALSE,update.sigma = FALSE,max.iter = 8000)
print(min(fit$varobj),digits = 12)
print(fit$beta)
