library(glmnet)
library(mr.ash.alpha)
set.seed(1)
n    <- 1000
x    <- rnorm(n)
X    <- cbind(x,x)
beta <- c(0.1,0.1)
y    <- drop(X %*% beta + rnorm(n))
# coefficients are different
fit <- glmnet(X,y,alpha = 0.9,standardize = FALSE)
b   <- as.matrix(coef(fit))[-1,]
print(summary(range(b[1,] - b[2,])))
# coefficients are (nearly) the same
fit <- glmnet(X,y,alpha = 0.9,thresh = 1e-15,standardize = FALSE)
b   <- as.matrix(coef(fit))[-1,]
print(summary(range(b[1,] - b[2,])))
fit <- mr.ash(X,y,sa = c(0,1,2),pi = c(0,0.5,0.5),
              update.pi = FALSE,update.sigma = FALSE,max.iter = 100)
print(min(fit$varobj),digits = 12)
print(fit$beta)
fit <- mr.ash(X,y,sa = c(0,1,2),pi = c(0,0.5,0.5),
              update.pi = FALSE,update.sigma = FALSE,max.iter = 10000)
print(min(fit$varobj),digits = 12)
print(fit$beta)
