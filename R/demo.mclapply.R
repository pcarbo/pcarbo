# TO DO: Explain here what this script is for, and how to use it on
# the RCC cluster.

# SCRIPT PARAMETERS
# -----------------
n  <- 7500 # Size of symmetric matrix.
m  <- 16   # Number of linear systems to solve.
nc <- 1    # Number of threads to use in solving linear systems.

which.method <- "parLapply"

# SET UP ENVIRONMENT
# ------------------
library(methods)
library(parallel)

# GENERATE DATA
# -------------
# Generate an n x n symmetric positive definite matrix.
cat("Generating data set.\n")
X <- matrix(rnorm(2*n^2),2*n,n)
A <- t(X) %*% X
rm(X)
gc(verbose = FALSE)

# Generate the B matrix in A*X = B.
B <- matrix(rnorm(n*m),n,m)

# SOLVE SET OF LINEAR SYSTEMS
# ---------------------------
# The first method uses the lapply functon to solve a set of linear
# systems. Of course, in practice you wouldn't actually do it this way
# because a single call to solve will be much more efficient.
solve.lapply <- function (A, B) {
  if (is.vector(B))
    return(solve(A,B))
  else {
    n <- ncol(B)
    X <- lapply(as.list(1:n),function (i) solve(A,B[,i]))
    return(do.call(cbind,X))
  }
}

# The first method uses the mclapply function to eventy distribute
# the computation among nc threads. 
solve.mclapply1 <- function (A, B, nc = 1) {
  cols <- splitIndices(ncol(B),nc)
  X    <- mclapply(cols,function (i) solve.lapply(A,B[,i]))
  X    <- do.call(cbind,X)
  X[,unlist(cols)] <- X
  return(X)
}

# TO DO: Explain what this function does.
solve.parLapply <- function (A, B, nc = 1) {
  cl <- makeCluster(nc)
  clusterExport(cl,"solve.lapply")
  cols <- clusterSplit(cl,1:ncol(B))
  X    <- parLapply(cl,cols,function (i, A, B) solve.lapply(A,B[,i]), A, B)
  X    <- do.call(cbind,X)
  X[,unlist(cols)] <- X
  stopCluster(cl)
  return(X)
}

# The second method uses the mclapply function as well, but the
# computation is distributed multiple times. Here it is assumed that
# the number of linear systems (columns of B) is a multiple of the
# number of threads.
solve.mclapply2 <- function (A, B, nc = 1) {
  n <- ncol(B)
  X <- B
  for (i in seq(1,n,nc)) {
    rows     <- i:(i+nc-1)
    out      <- mclapply(as.list(rows),function (i) solve(A,B[,i]),
                         mc.cores = nc)
    X[,rows] <- do.call(cbind,out)
  }
  return(X)
}

# Use one of the three parallelization schemes.
cat("Solving linear systems using",nc,"threads.\n")
cat("method =",which.method,"\n")
if (which.method == "lapply") {
  timing <- system.time(X <- solve.lapply(A,B))
} else if (which.method == "mclapply1") {
  timing <- system.time(X <- solve.mclapply1(A,B,nc = nc))
} else if (which.method == "parLapply") {
  timing <- system.time(X <- solve.parLapply(A,B,nc = nc))
}
cat("Computation took",timing["elapsed"],"seconds.\n")

# VERIFY SOLUTIONS
# ----------------
cat("Maximum residual is ",max(abs(A %*% X - B)),".\n",sep="")
