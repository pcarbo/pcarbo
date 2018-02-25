# This is a very simple script that generates an m x n matrix of
# reproducible, normally distributed random numbers, and saves the
# random matrix to a space-delimited text file. To run this script in
# batch mode, type the following in the shell command line:
#
#   Rscript gen.rnorm.data.R <seed> <m> <n> <outfile>
#
# where <seed> specifies the sequence of pseudorandom numbers, <m> is
# the number of matrix rows, <n> is the number of matrix columns, and
# <outfile> is the name of the saved text file.
args    <- commandArgs(trailingOnly = TRUE)
seed    <- as.integer(args[1])
m       <- as.integer(args[2])
n       <- as.integer(args[3])
outfile <- as.character(args[4])

# Initialize the pseudorandom number sequence.
cat("Initializing pseudorandom number sequence with seed=",seed,".\n",sep="")
set.seed(seed)

# Generate the matrix of normally distributed samples.
cat("Generating",m,"x",n,"random matrix.\n")
X <- matrix(rnorm(m*n),m,n)

# Save the matrix to a space-delimited text file.
cat("Writing matrix to ",outfile,".\n",sep="")
write.table(round(X,digits = 4),outfile,sep = " ",row.names = FALSE,
            col.names = FALSE)
