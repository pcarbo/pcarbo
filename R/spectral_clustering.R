library(Matrix)
library(ggplot2)
library(cowplot)
set.seed(1)

# Simulate 2-d data points that can clearly be separated into 4
# distinct clusters.
n <- 100
X <- matrix(rnorm(4*n,rep(6*c(-1,-1,-1,+1,+1,-1,+1,+1),n)),4*n,2,byrow = TRUE)
rownames(X) <- paste0("s",seq(1,4*n))
colnames(X) <- c("x1","x2")
ggplot(as.data.frame(X),aes(x = x1,y = x2)) +
  geom_point(color = "white",fill = "black",shape = 21,size = 2) +
  theme_cowplot()

# Create the k-NN Laplacian, for k = 10.
k <- 10
D <- matrix(0,n,n)
for (i in 1:n)
  for (j in 1:n)
    D[i,j] <- sqrt(sum((X[i,] - X[j,])^2))
is <- NULL
js <- NULL
for (i in 1:n) {
  is <- c(is,rep(i,k+1))
  js <- c(js,order(D[i,])[seq(1,k+1)])
}
W <- sparseMatrix(i = is,j = js,x = rep(1,(k+1)*n))
W <- W + t(W)
W[W > 0] <- 1
D <- .symDiagonal(n,1/sqrt(rowSums(W)))
L  <- D %*% W %*% D

# Project the points onto the first 2 PCs of the Laplacian.
scale.cols <- function (A, b)
  t(t(A) * b)
out <- eigen(L)
pcs <- scale.cols(out$vectors[,1:2],sqrt(out$values[1:2]))
colnames(pcs) <- c("pc1","pc2")
pdat <- cbind(pcs,
              data.frame(cluster = factor(rep(1:4,n)),
                         dist    = sqrt(X[,1]^2 + X[,2]^2)))
ggplot(pdat,aes(x = pc1,y = pc2,fill = cluster)) +
  geom_point(color = "white",shape = 21,size = 2) +
  theme_cowplot()
ggplot(pdat,aes(x = pc1,y = pc2,fill = dist)) +
  geom_point(color = "white",shape = 21,size = 2) +
  theme_cowplot()
