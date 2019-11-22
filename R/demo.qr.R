# TO DO: Explain here what this script does, and how to use it.
set.seed(1)

# Compute a low-rank factorization of X.
qr.incomplete <- function (X, tol = 1e-8) {
  out <- qr(X,tol = tol)
  r   <- out$rank
  Q   <- qr.Q(out)
  R   <- qr.R(out)
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

stop()

# EXAMPLE WITH PIVOTING
# ---------------------
dd <- data.frame(f1 = gl(4, 6, labels = LETTERS[1:4]),
                 f2 = gl(3, 2, labels = letters[1:3]))[-(7:8), ]
mm <- model.matrix(~ f1 * f2, dd)
set.seed(1)
dd$y <- mm %*% seq_len(ncol(mm)) + rnorm(nrow(mm), sd = 0.1)
fm1 <- lm(y ~ f1 * f2, dd)


