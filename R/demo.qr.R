# This illustrates how to use the qr() function in R to compute a
# low-rank approximation to a matrix. Note that one has to be careful
# about column pivoting.
set.seed(1)

# Compute a low-rank factorization of X, in which the "effective" rank
# is estimated using qr().
qr.incomplete <- function (X, tol = 1e-7) {
  out   <- qr(X,tol = tol)
  r     <- out$rank
  p     <- out$pivot
  Q     <- qr.Q(out)
  R     <- qr.R(out)
  R[,p] <- R
  return(list(Q = Q[,1:r],R = R[1:r,],rank = r))
}

# EXAMPLE WITHOUT PIVOTING
# ------------------------
# Generate a rank-3 20 x 10 matrix.
X <- matrix(rnorm(60),20,3) %*% matrix(rnorm(30),3,10)
y <- rnorm(20)
b <- rnorm(10)

# Compute a low-rank QR factorization of X.
out <- qr.incomplete(X)
Q   <- out$Q
R   <- out$R
r   <- out$rank

# Compute X*b in two ways.
u1 <- drop(X %*% b)
u2 <- drop(Q %*% (R %*% b))

# Compute X'*y in two ways.
v1 <- drop(t(X) %*% y)
v2 <- drop((y %*% Q) %*% R)

# Summarize results:
cat(sprintf("rank = %d\n",r))
cat(sprintf("largest error in X*b:  %0.1e\n",max(abs(u1 - u2))))
cat(sprintf("largest error in X'*y: %0.1e\n",max(abs(v1 - v2))))

# EXAMPLE WITH PIVOTING
# ---------------------
# This example comes from the RcppEigen vignette.
dd <- data.frame(f1 = gl(4,6,labels = LETTERS[1:4]),
                 f2 = gl(3,2,labels = letters[1:3]))[-(7:8),]
X  <- model.matrix(~f1*f2,dd)
y  <- rnorm(22)
b  <- rnorm(12)

# Compute a low-rank QR factorization of X.
out <- qr.incomplete(X)
Q   <- out$Q
R   <- out$R
r   <- out$rank

# Compute X*b in two ways.
u1 <- drop(X %*% b)
u2 <- drop(Q %*% (R %*% b))

# Compute X'*y in two ways.
v1 <- drop(t(X) %*% y)
v2 <- drop((y %*% Q) %*% R)

# Summarize results:
cat(sprintf("rank = %d\n",r))
cat(sprintf("largest error in X*b:  %0.1e\n",max(abs(u1 - u2))))
cat(sprintf("largest error in X'*y: %0.1e\n",max(abs(v1 - v2))))
