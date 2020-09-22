# A small script to illustrate that t-SNE can "invent" clusters.
library(Rtsne)
library(uwot)
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
out1 <- Rtsne(matrix(x),pca = FALSE,normalize = FALSE,theta = 0.1,
              max_iter = 1000,eta = 200,perplexity = perplexity,
              num_threads = 4,verbose = TRUE)

# Run UMAP.
out2 <- umap(matrix(x),n_neighbors = 30,n_epochs = 1000,min_dist = 0.1,
             scale = "none",learning_rate = 1,verbose = TRUE)

# Plot the t-SNE embedding.
dat <- as.data.frame(cbind(x,out1$Y))
names(dat) <- c("x","tsne1","tsne2")
p1 <- ggplot(dat,aes(x = tsne1,y = tsne2,color = x)) +
  geom_point() +
  scale_color_gradientn(colors = plasma(5)) +
  theme_cowplot(font_size = 12) +
  guides(color = "none")

# Plot the UMAP embedding.
dat <- as.data.frame(cbind(x,out2))
names(dat) <- c("x","umap1","umap2")
p2 <- ggplot(dat,aes(x = umap1,y = umap2,color = x)) +
  geom_point() +
  scale_color_gradientn(colors = plasma(5)) +
  theme_cowplot(font_size = 12) +
  guides(color = "none")
plot_grid(p1,p2)
