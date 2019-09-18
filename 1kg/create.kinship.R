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

# Load the 1000 Genomes population labels.
labels <- read.table("omni_samples.20141118.panel",sep = " ",
                     header = TRUE,as.is = "id")

# Load the genotype matrix into R.
geno <- read.geno.raw("1kg_recoded.raw")

# Fill in the missing genotypes.
p <- ncol(geno)
for (j in 1:p) {
  i         <- which(is.na(geno[,j]))
  geno[i,j] <- mean(geno[,j],na.rm = TRUE)
}

# Center the genotype matrix.
geno <- scale(geno,center = TRUE,scale = FALSE)

# Compute the kinship matrix.
# TO DO.
