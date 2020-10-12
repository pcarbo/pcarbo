set.seed(1)
library(mvtnorm)
library(Rtsne)
library(uwot)
library(ggplot2)
library(cowplot)
set.seed(1)

# Simulate data.
X <- rbind(rmvnorm(100,c(-1,0),diag(2)/30),
           rmvnorm(900,c(+1,0),diag(c(0.1,3))))
cluster <- factor(c(rep(1,100),rep(2,900)))
colnames(X) <- c("x1","x2")
p1 <- qplot(X[,1],X[,2],shape = I(21),color = I("white"),fill = cluster) +
  scale_fill_manual(values = c("tomato","dodgerblue")) +
  xlim(-6,6) +
  ylim(-6,6) +
  labs(x = "x1",y = "x2") +
  theme_cowplot(font_size = 12)
print(p1)

# Compute 1-d embedding using first PC.
pca <- drop(prcomp(X)$x[,1])
pdat <- data.frame(cluster,pca)
p2 <- ggplot(pdat,aes(x = cluster,y = pca)) +
  geom_boxplot(width = 0.25) +
  theme_cowplot(font_size = 12)
print(p2)

# Comptue 1-d using t-SNE.
tsne <- drop(Rtsne(X,dims = 1,perplexity = 30,theta = 0.1,max_iter = 1000,
                   eta = 200,normalize = FALSE,pca = FALSE,verbose = TRUE)$Y)
pdat <- data.frame(cluster,tsne)
p3 <- ggplot(pdat,aes(x = cluster,y = tsne)) +
  geom_boxplot(width = 0.25) +
  theme_cowplot(font_size = 12)
print(p3)
