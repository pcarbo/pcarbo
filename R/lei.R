# SETUP ENVIRONMENT
# -----------------
# Load the packages.
library(varbvs)
library(ggplot2)
library(cowplot)

# Initialize the sequence of pseudorandom numbers.
set.seed(1)

# GENERATE DATA
# -------------
# Generate the data set, in which the first 100 variables have a
# effect on the continuous outcome, and the rest have no effect on the
# outcome.
n     <- 1000
n1    <- n + 10
X     <- matrix(rnorm(n1*n),n1,n)
beta  <- rep(0,n)
beta[1:200] <- 100
y     <- X %*% beta + rnorm(n1)

# RUN MULTIPLE REGRESSION METHOD
# ------------------------------
# Compute the multiple regression (least squares) association p-values.
fit   <- lm(y ~ X - 1)
p.mlr <- log10(summary(fit)$coef[,"Pr(>|t|)"])

# RUN UNIVARIATE REGRESSION METHOD
# --------------------------------

# Compute the marginal association p-values, ignoring the (small)
# correlations between the variables X.
p.mar <- rep(0,n)
for (i in 1:n) {
  fit      <- lm(y ~ X[,i] - 1)
  p.mar[i] <- log10(summary(fit)$coefficients[,"Pr(>|t|)"])
}

# SUMMARIZE RESULTS
# -----------------
p1 <- ggplot(data.frame(x = 1:n,y = p.mlr),aes(x = x,y = y)) +
  geom_point(shape = 20,color = "darkblue") +
  geom_hline(yintercept = log10(0.05/n),color = "magenta",
             linetype = "dotted") +
  scale_x_continuous(breaks = seq(0,n,200)) +
  labs(x     = "variable",
       y     = "log10(p-value)",
       title = "multiple regression") +
  theme_cowplot(font_size = 11)

p2 <- ggplot(data.frame(x = 1:n,y = p.mar),aes(x = x,y = y)) +
  geom_point(shape = 20,color = "darkblue") +
  geom_hline(yintercept = log10(0.05/n),color = "magenta",
             linetype = "dotted") +
  scale_x_continuous(breaks = seq(0,n,200)) +
  labs(x     = "variable",
       y     = "log10(p-value)",
       title = "univariate regression") +
  theme_cowplot(font_size = 11)

print(plot_grid(p1,p2,ncol = 1))
