library(Matrix)
library(mvtnorm)
library(susieR)

# SCRIPT PARAMETERS
ns <- 1000          # Number of simulations.
n  <- 600           # Number of samples.
b  <- c(0,1,1,0,0)  # True effects.
se <- 3             # residual s.d.

# S  <- rbind(c(   1, 0.99,  0.7,  0.7, 0.9),
#             c(0.99,    1,  0.7,  0.7, 0.9),
#             c( 0.7,  0.7,    1, 0.99, 0.8),
#             c( 0.7,  0.7, 0.99,    1, 0.8),
#             c( 0.9,  0.9,  0.8,  0.8,   1))
S <- rbind(c(1.0,  0.92, 0.7,  0.7,  0.9),
           c(0.92, 1.0,  0.7,  0.7,  0.7),
           c(0.7,  0.7,  1.0,  0.92, 0.8),
           c(0.7,  0.7,  0.92, 1.0,  0.8),
           c(0.9,  0.7,  0.8,  0.8,  1.0))
S <- as.matrix(nearPD(S)$mat)
set.seed(1)

# Repeat for each simulation.
res <- vector("list",ns)
for (i in 1:ns) {
  cat("*")
  
  # Simulate data.
  X <- rmvnorm(n,sigma = S)
  y <- drop(X %*% b + se*rnorm(n))

  # Fit the susie model, and store the inferred CS's.
  fit <- susie(X,y,L = 2,
               standardize = FALSE,
               estimate_prior_variance = FALSE,
               scaled_prior_variance = 1,
               min_abs_corr = 0)
  res[[i]] <- susie_get_cs(fit,X,min_abs_corr = 0)$cs
}
cat("\n")

# Summarize the inferred CSs across all simulations.
out <- sapply(res,function (x) paste(as.character(x),collapse = ","))
out <- table(factor(out))
out <- sort(out,decreasing = TRUE)
out <- as.data.frame(out)
names(out) <- c("CSs","count")
print(out)
