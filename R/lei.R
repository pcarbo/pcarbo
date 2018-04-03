# A small demo similar to the example show in Fig. 6 of Su, Bogdan &
# Candes, Annals of Statistics 45: 2133--2150, 2017. Instead of the
# lasso, here we compare the accuracy of variable selection based on
# multiple regression, univariate regression and Bayesian variable
# selection.

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
# Generate the data set, in which the first 200 variables have a
# effect on the continuous outcome, and the rest have no effect on the
# outcome.
n     <- 1000
m     <- n + 10
X     <- matrix(rnorm(m*n),m,n)
beta  <- rep(0,n)
beta[1:200] <- 100
y     <- X %*% beta + rnorm(m)

# RUN MULTIPLE REGRESSION METHOD
# ------------------------------
# Compute the multiple regression (least squares) association p-values.
fit   <- lm(y ~ X)
p.mlr <- log10(summary(fit)$coef[-1,"Pr(>|t|)"])

# RUN UNIVARIATE REGRESSION METHOD
# --------------------------------
# Compute the marginal association p-values, ignoring the (small)
# correlations between the variables X.
p.mar <- rep(0,n)
for (i in 1:n) {
  fit      <- lm(y ~ X[,i])
  p.mar[i] <- log10(summary(fit)$coefficients[-1,"Pr(>|t|)"])
}

# RUN VARBVS
# ----------
# Compute posterior inclusion probabilities (PIPs) using varbvs. Note
# that the local false discovery rate (LFDR) is simply 1 - PIP. You
# can use varbvs:::computelfsrmix to compute the local false sign
# rates (LFSR).
out.varbvs <- varbvs(X,NULL,y,logodds = seq(-3,0,0.25),verbose = FALSE)
pip        <- out.varbvs$pip

# SUMMARIZE RESULTS
# -----------------
# Plot the multiple regression association p-values for all 1,000
# variables.
p1 <- ggplot(data.frame(x = 1:n,y = p.mlr),aes(x = x,y = y)) +
  geom_point(shape = 20,color = "darkblue") +
  geom_hline(yintercept = log10(0.05/n),color = "magenta",
             linetype = "dotted") +
  scale_x_continuous(breaks = seq(0,n,200)) +
  labs(x     = "variable",
       y     = "log10(p-value)",
       title = "multiple regression") +
  theme_cowplot(font_size = 11)

# Plot the univariate regression p-values for all 1,000 variables.
p2 <- ggplot(data.frame(x = 1:n,y = p.mar),aes(x = x,y = y)) +
  geom_point(shape = 20,color = "darkblue") +
  geom_hline(yintercept = log10(0.05/n),color = "magenta",
             linetype = "dotted") +
  scale_x_continuous(breaks = seq(0,n,200)) +
  labs(x     = "variable",
       y     = "log10(p-value)",
       title = "univariate regression") +
  theme_cowplot(font_size = 11)

# Plot the varbvs posterior inclusion probabilities (PIPs) for all
# 1,000 variables.
p3 <- ggplot(data.frame(x = 1:n,y = log10(pip)),aes(x = x,y = y)) +
    geom_point(shape = 20,color = "darkblue") +
  scale_x_continuous(breaks = seq(0,n,200)) +
  labs(x     = "variable",
       y     = "log10(PIP)",
       title = "varbvs") +
  theme_cowplot(font_size = 11)

# Combine the plots into a single figure.
print(plot_grid(p1,p2,p3,ncol = 1))
