# A small script to illustrate that t-SNE can "invent" clusters.
library(Rtsne)
library(ggplot2)
library(cowplot)
library(viridis)

# Script parameters
n <- 4000
perplexity <- 100

# Draw points uniformly on a line between 0 and 1.
set.seed(1)
x <- runif(n)

# Compute a 2-d nonlinear embedding of the points using t-SNE.
out <- Rtsne(matrix(x),pca = FALSE,normalize = FALSE,theta = 0.1,
             max_iter = 1000,eta = 200,perplexity = perplexity,
             num_threads = 4,verbose = TRUE)

# Plot a 2-d projection of the points.
dat <- as.data.frame(cbind(x,out$Y))
names(dat) <- c("x","tsne1","tsne2")
p <- ggplot(dat,aes(x = tsne1,y = tsne2,color = x)) +
  geom_point() +
  scale_color_gradientn(colors = plasma(5)) +
  theme_cowplot(font_size = 12) +
  guides(color = "none")
print(p)
