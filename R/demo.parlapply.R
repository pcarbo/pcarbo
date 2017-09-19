# TO DO: Explain here what this script does, and how to use it.
#
# NOTES:
#
#   - Explain how to install cfwlab package.
#
library(parallel)
library(cfwlab)

# SCRIPT PARAMETERS
# -----------------


# If "nc" is not already specified, attempt to automatically detect
# the number of CPU cores.
if (!exists("nc")) {
  nc <- detectCores()
  if (is.na(nc))
    nc <- 1
}

