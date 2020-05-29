# TO DO: Explain here what this script is for.
library(glmnet)
library(ridge)
library(varbvs)
library(ggplot2)
library(cowplot)

# SCRIPT PARAMETERS
# -----------------
r    <- 0.9        # Proportion of variance explained by X.
n    <- 800        # Number of samples.
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
colnames(Xtrain) <- paste0("x",1:p)
colnames(Xtest)  <- paste0("x",1:p)

# FIT REGRESSION MODELS
# ---------------------
# Fit an Elastic Net model, in which the penalty strength is estimated
# by k-fold cross-validation.
fit.glmnet <- cv.glmnet(Xtrain,ytrain,alpha = 0.95)

# Fit a ridge regression model using glmnet, in which the penalty
# strength is estimated by k-fold cross-validation.
fit.ridge <- cv.glmnet(Xtrain,ytrain,alpha = 0)

# Fit a ridge regression model in which the penalty strength is
# estimated by maximum-likelihood.
dat        <- as.data.frame(cbind(y,X))
names(dat) <- c("y",paste0("x",1:p))
fit.ridge2 <- linearRidge(y ~ .,dat)

# Fit the varbvs model.
fit.varbvs <- varbvs(Xtrain,NULL,ytrain,verbose = FALSE)

# PREDICT RESPONSES IN TEST CASES
# -------------------------------
# Use the fitted regression models to predict the responses in the
# text examples.
y1 <- drop(predict(fit.glmnet,Xtest,s = "lambda.min"))
y2 <- drop(predict(fit.ridge,Xtest,s = "lambda.min"))
y3 <- drop(predict(fit.ridge,Xtest,s = "lambda.1se"))
y4 <- predict(fit.ridge2,as.data.frame(Xtest))
y5 <- predict(fit.varbvs,Xtest)

# PLOT TRUE VS. ESTIMATED RESPONSES
# ---------------------------------
create_scatterplot <- function (x, y, title)
  qplot(x,y,size = I(1)) +
    geom_abline(slope = 1,intercept = 0,color = "magenta",
                linetype = "dotted") +
    xlim(c(-12,12)) + 
    ylim(c(-12,12)) + 
    labs(x = "true",y = "estimated",title = title) +
    theme(plot.title = element_text(face = "plain",size = 10))
theme_set(theme_cowplot(10))
p1 <- create_scatterplot(ytest,y1,"elastic net (lambda.min)")
p2 <- create_scatterplot(ytest,y2,"ridge (lambda.min)")
p3 <- create_scatterplot(ytest,y3,"ridge (lambda.1se)")
p4 <- create_scatterplot(ytest,y4,"ridge (MLE)")
p5 <- create_scatterplot(ytest,y5,"varbvs")
print(plot_grid(p1,p2,p3,p4,p5))

# Use the fitted ridge regression model to predict the responses in the
# test examples.

