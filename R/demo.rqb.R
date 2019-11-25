# A short script to compare the accuracy and runtime of the QR
# decomposition in base R against the randomized "QB" algorithm
# implemented in the rsvd package.
library(rsvd)
set.seed(1)

# SCRIPT PARAMETERS
# -----------------
n   <- 1e5
m   <- 300
tol <- 1e-3

# GENERATE DATA
# -------------
# Generate an n x m matrix.
logspace <- function (x, y, n)
  2^seq(log2(x),log2(y),length = n)
k <- floor(n/4)
x <- c(rnorm(n - 2*k),3*rnorm(k),6*rnorm(k))
s <- c(0,logspace(1/10,2*sqrt(max(x^2 - 1)),m - 1))
L <- matrix(0,n,m)
for (i in 1:m) 
  L[,i] <- dnorm(x,sd = sqrt(1 + s[i]^2))

# COMPUTE QR DECOMPOSITION
# ------------------------
cat("Computing QR decomposition.\n")
t1 <- system.time(out <- qr(L,tol = tol))
r     <- out$rank
p     <- out$pivot
Q     <- qr.Q(out)
R     <- qr.R(out)
Q     <- Q[,1:(r + 10)]
R[,p] <- R
R     <- R[1:(r + 10),]
cat(sprintf("Computation took %0.2f seconds.\n",t1["elapsed"]))
cat(sprintf("rank = %d\n",r + 10))

# COMPUTE RANDOMIZED QB DECOMPOSITION
# -----------------------------------
cat("Computing randomized QB decomposition.\n")
t2 <- system.time(out <- rqb(L,r))
Q2 <- out$Q
B  <- out$B
cat(sprintf("Computation took %0.2f seconds.\n",t2["elapsed"]))

# COMPARE FACTORIZATIONS
# ----------------------
x <- rep(1,m)
print(max(abs(L %*% x - Q %*% (R %*% x))))
print(max(abs(L %*% x - Q2 %*% (B %*% x))))
