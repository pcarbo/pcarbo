# Shorthand for machine precision.
eps <- .Machine$double.eps

# Subtract b[i] from each column A[,i].
subtract.cols <- function (A, b)
  t(t(A) - b)

# Scale each column A[,i] by b[i].
scale.cols <- function (A, b)
  t(t(A) * b)

