# Small script to assess speed of matrix-vector operations in R.
library(Matrix)
library(Rcpp)
library(microbenchmark)

# Script parameters.
n <- 1e3
m <- 1e5
k <- 100

# Define Rcpp function for computing matrix-vector product A x b.
cppFunction("NumericVector mymult(const NumericMatrix & A,
             const NumericVector & b) {
  int m = A.nrow();
  int n = A.ncol();
  double y;
  NumericVector x(m);
  for (int i = 0; i < m; i++) {
    y = 0;
    for (int j = 0; j < n; j++) 
      y += A(i,j) * b[j];
    x[i] = y;
  }
  return x;
}")

# Create two large matrices.
cat("Generating data.\n")
A <- matrix(rnorm(m*n),m,n)
b <- rnorm(n)

# Compute the matrix-vector product A x b using the base matrix
# multiplication operator.
cat("Computing A x b using base.\n")
out1 <- microbenchmark(x1 <- c(A %*% b),times = k)

# Compute the matrix-vector product A x b using the multiplication
# operator defined in the Matrix package.
cat("Computing A x b using Matrix package.\n")
M    <- Matrix(A)
out2 <- microbenchmark(x2 <- as.vector(M %*% b),times = k)

# Compute the matrix product A x b using the Rcpp function.
cat("Computing A x b using Rcpp.\n")
out3 <- microbenchmark(x3 <- mymult(A,b),times = k)

# Summarize the results.
out           <- rbind(summary(out1,unit = "s"),
                       summary(out2,unit = "s"),
                       summary(out3,unit = "s"))
rownames(out) <- c("base","Matrix","Rcpp")
print(out)
