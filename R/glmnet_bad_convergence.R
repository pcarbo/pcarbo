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
print(summary(range(b[1,] - b[2,])))
print(fit$npasses)

# If we make the convergence criteria more stringent, the estimates
# become the same, but it takes many more iterations of the
# coordinate-wise updates to reach the solution.
fit <- glmnet(X,y,alpha = 0.9,thresh = 1e-15,standardize = FALSE)
b   <- as.matrix(coef(fit))[-1,]
print(summary(range(b[1,] - b[2,])))
print(fit$npasses)

fit <- mr.ash(X,y,sa = c(0,1,2),pi = c(0,0.5,0.5),
              update.pi = FALSE,update.sigma = FALSE,max.iter = 100)
print(min(fit$varobj),digits = 12)
print(fit$beta)
fit <- mr.ash(X,y,sa = c(0,1,2),pi = c(0,0.5,0.5),
              update.pi = FALSE,update.sigma = FALSE,max.iter = 10000)
print(min(fit$varobj),digits = 12)
print(fit$beta)
