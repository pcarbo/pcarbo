# Run this script to generate CSV files containing the simulation
# configurations.

# SCRIPT PARAMETERS
# -----------------
# Generate configurations for all combinations of these parameters:
# the data set size (n), the number of mixture components (k) and the
# random number generator seeds (seeds).
params <- list(n     = c(100,500,1e3,5e3,1e4),
               k     = c(4,8,12,16,20),
               seeds = 1:10)

# Save each configuration to a CSV file.
i <- 0
for (n in params$n)
  for (k in params$k)
    for (seed in params$seeds) {
      i        <- i + 1
      dat      <- data.frame(n = n,k = k,seed = seed)
      out.file <- sprintf("sims/sim%04d.csv",i)
      write.csv(dat,out.file,row.names = FALSE,quote = FALSE)
    }
