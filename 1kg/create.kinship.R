# TO DO: Explain here (very briefly) what this script does.
library(data.table)

# Read an n x p genotype matrix from a .raw file, where n is the
# number of samples and p is the number of genetic markers (SNPs).
read.geno.raw <- function (geno.file) {
  geno <- fread(geno.file,sep = " ",header = TRUE,stringsAsFactors = FALSE,
                showProgress = FALSE)
  class(geno)    <- "data.frame"
  ids            <- with(geno,paste(FID,IID,sep = "_"))
  geno           <- geno[-(1:6)]
  rownames(geno) <- ids
  geno           <- as.matrix(geno)
  storage.mode(geno) <- "double"
  return(geno)
}

# Load the genotype matrix into R, and fix the ids.
geno           <- read.geno.raw("1kg_recoded.raw")
ids            <- sapply(strsplit(rownames(geno),"_"),function (x) x[1])
rownames(geno) <- ids

# Load the population labels.
labels <- read.table("omni_samples.20141118.panel",sep = " ",
                     header = TRUE,as.is = "id")
labels <- subset(labels,is.element(labels$id,ids))
rows   <- match(ids,labels$id)
labels <- labels[rows,]

# Fill in the missing genotypes.
p <- ncol(geno)
for (j in 1:p) {
  i         <- which(is.na(geno[,j]))
  geno[i,j] <- mean(geno[,j],na.rm = TRUE)
}

# Compute the kinship matrix.
K <- tcrossprod(geno)/p

# Save the matrix and the population labels.
save(list = c("labels","K"),file = "1kg_kinship.RData")
