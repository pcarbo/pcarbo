# Here, we assess performance of EM for optimizing the mixture weights
# of a simple mixture model.
source("misc.R")
source("datasim.R")
source("likelihood.R")
source("mixopt.R")

# SCRIPT PARAMETERS
# -----------------
# Read the script parameters from a small CSV file.
args     <- commandArgs(trailingOnly = TRUE)
dat      <- read.csv(args[1],stringsAsFactors = FALSE,header = TRUE)
n        <- dat$n     # Number of data samples.
k        <- dat$k     # Number of mixture components.
seed     <- dat$seed  # random number generator seed.
out.file <- dat$out.file
cat(sprintf("Setting seed to %d.\n",seed))
set.seed(seed)
rm(dat)

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
out.file <- paste(out.file,"RData",sep = ".")
cat(sprintf("Saving results to %s.\n",out.file))
save(list = c("n","k","seed","w","s","se","timing","fit.em"),
     file = out.file)
