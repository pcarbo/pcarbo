# TO DO: Explain here what this script does.
library(mvtnorm)

# Script parameters.
n <- 40
m <- 2
S <- rbind(c(1.0, 0.2),
           c(0.2, 0.6))
D <- diag(c(0.5,0.5))

# Simulate data.
set.seed(1)
X <- rmvnorm(n,sigma = S + D)

# Find best fit naively.
dgrid        <- expand.grid(seq(0,1,0.01),seq(0,1,0.01))
dgrid        <- cbind(dgrid,0)
names(dgrid) <- c("d1","d2","logl")
k            <- nrow(dgrid)
for (i in 1:k) {
  d <- dgrid[i,1:2]
  dgrid[i,"logl"] <- sum(dmvnorm(X,sigma = S + diag(d),log = TRUE))
}
    
# Fit model.
C <- crossprod(X)/n - S
d <- pmax(0,diag(C))

i <- which.max(dgrid$logl)
print(dgrid[i,],digits=12)
print(d)
