# TO DO: Explain here what this script is for.
library(glmnet)
library(ggplot2)
library(cowplot)

# SCRIPT PARAMETERS
# -----------------
r    <- 0.9        # Proportion of variance explained by X.
n    <- 400        # Number of samples.
p    <- 1000       # Number of candidate predictors.
i    <- c(127,354) # Indices of predictors affecting outcome.
b    <- rep(0,p)   # Coefficients used to simulate data.
b[i] <- c(-1,1)

# SIMULATE DATA
# -------------
set.seed(1)
X <- matrix(rnorm(n*p),n,p)
X <- scale(X,center = TRUE,scale = FALSE)

# Adjust the QTL effects so that we control for the proportion of variance
# explained (r). That is, we adjust b so that r = a/(a+1), where I've
# defined a = b'*cov(X)*b. Here, sb is the variance of the (nonzero)
# QTL effects.
sb <- r/(1-r)/var(drop(X %*% b))
b  <- sqrt(sb) * b

# Generate the quantitative trait measurements.
y <- drop(X %*% b + rnorm(n))

# Check that the data are roughly simulated in the way we requested.
cat(sprintf("Proportion of variance in Y explained by X = %0.3f\n",
            var(drop(X %*% b))/var(y)))

# Split the data into training and test sets.
i <- 1:200
j <- 201:400
Xtrain <- X[i,]
Xtest  <- X[j,]
ytrain <- y[i]
ytest  <- y[j]

# FIT REGRESSION MODELS
# ---------------------
# Fit an Elastic Net model, in which the penalty strength is estimated
# by k-fold cross-validation.
fit.glmnet <- glmnet::cv.glmnet(Xtrain,ytrain,alpha = 0.95)

# Fit a ridge regression model using glmnet, in which the penalty
# strength is estimated by k-fold cross-validation.
fit.ridge <- glmnet::cv.glmnet(Xtrain,ytrain,alpha = 0)

# Fit a ridge 

# Use the fitted Elastic Net model to predict the outcomes in the test
# examples.
y.glmnet <- predict(fit.glmnet,Xtest,s = "lambda.min")
plot(ytest,y.glmnet,pch = 20,col = "dodgerblue")
abline(a = 0,b = 1,col = "magenta",lty = "dotted")

# Use the fitted ridge regression model to predict the outcomes in the
# test examples.
y.ridge <- predict(fit.ridge,Xtest,s = "lambda.1se")
plot(ytest,y.ridge,pch = 20,col = "indianred")
abline(a = 0,b = 1,col = "magenta",lty = "dotted")
