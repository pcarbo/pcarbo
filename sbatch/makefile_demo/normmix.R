# Here, we assess performance of EM for optimizing the mixture weights
# of a simple mixture model.
source("misc.R")
source("datasim.R")
source("likelihood.R")
source("mixopt.R")

# SCRIPT PARAMETERS
# -----------------
args     <- commandArgs(trailingOnly = TRUE)
n        <- as.integer(args[1])  # Number of data samples.
k        <- as.integer(args[2])  # Number of mixture components.
seed     <- as.integer(args[3])  # random number generator seed.
out.file <- args[4]
set.seed(seed)

# These are additional variables determining how the data set is
# generated: the standard errors of the samples (se), and the standard
# deviations (s) and mixture weights (w) used to simulate the data.
se <- rep(0.1,n)
s  <- c(0.01,10^(seq(-2,0,length.out = k)))
w  <- runif(k)
w  <- w/sum(w)

# GENERATE DATA SET
# -----------------
# Simulate a data set with n samples.
cat(sprintf("Simulating data set with %d observations.\n",n))
x <- datasim.norm(w,s,se)

# COMPUTE LIKELIHOOD MATRIX
# -------------------------
# Compute the n x k conditional likelihood matrix.
cat(sprintf("Computing the %d x %d conditional likelihood matrix.\n",n,k))
L <- condlikmatrix.norm(x,se,s)

# FIT MIXTURE MODEL
# -----------------
# Fit mixture model using EM.
timing <- system.time(fit.em <- mixopt.em(L,tol = 1e-4,verbose = FALSE))
cat(sprintf("Model fitting took %d iterations and %0.2f seconds.\n",
            length(fit.em$maxd),timing["elapsed"]))

# SAVE RESULTS TO FILE
# --------------------
# Save the script parameters and results to an .RData file.
save(list = c("n","k","seed","w","s","se","timing","fit.em"),
     file = paste(out.file,"RData",sep = "."))
