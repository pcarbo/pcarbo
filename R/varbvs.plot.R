# TO DO: Explain here what this script is for.

# SCRIPT PARAMETERS
# -----------------
n    <- 100     # Number of samples.
xcor <- 0.99    # Correlation between X1 and X2.
beta <- c(3,0)  # Coefficients used to simulate data.

# FUNCTION

# SET UP ENVIRONMENT
# ------------------
# Load the packages used in the analysis below.
library(MASS)
library(varbvs)
library(ggplot2)
library(cowplot)

# Initialize the sequence of pseudorandom numbers.
set.seed(1)

# GENERATE DATA SET
# -----------------
# Simulate the n x p matrix of predictors, X, with p = 2.
cat("Generating data set.\n")
S <- rbind(c(1,xcor),
           c(xcor,1))
X <- mvrnorm(n,c(0,0),S)
X <- scale(X,center = TRUE,scale = FALSE)

# Simulate the continuously-valued outcomes.
y <- drop(X %*% beta + rnorm(n))
y <- y - mean(y)

