# A short script to plot the kinship matrix using kpca as a sanity
# check that we recover global (continent-level) population structure.
library(kernlab)
library(ggplot2)
library(cowplot)

# Load the kinship matrix and the population labels.
load("1kg_kinship.RData")

# Compute the top two PCs using kernel PCA.
out <- kpca(K,feature = 2)

# Plot the 1kg sampled embedded onto the first two PCs.
pdat           <- as.data.frame(rotated(out))
names(pdat)    <- c("PC1","PC2")
pdat           <- cbind(pdat,labels)
rownames(pdat) <- NULL
colors <- rep(c("#E69F00","#56B4E9","#009E73","#F0E442","#0072B2",
                "#D55E00","#CC79A7"),times = 3)
shapes <- rep(c(19,17,8),each = 7)
quickplot(data = pdat,x = PC1,y = PC2,color = pop,shape = pop) +
  geom_point(size = 2) +
  scale_color_manual(values = colors) +
  scale_shape_manual(values = shapes)
