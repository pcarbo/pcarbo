library(glmnet)
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
fit.glmnet <- cv.glmnet(Xtrain,ytrain,alpha = 0.95,standardize = FALSE)

# Fit a ridge regression model using glmnet, in which the penalty
# strength is estimated using k-fold cross-validation with the default
# settings.
fit.ridge <- cv.glmnet(Xtrain,ytrain,alpha = 0,standardize = FALSE)

# Fit a ridge regression model using glmnet, in which the penalty
# strength is estimated using k-fold cross-validation, with a
# customized setting of "lambda".
fit.ridge2 <- cv.glmnet(Xtrain,ytrain,alpha = 0,standardize = FALSE,
                        lambda = 10^seq(4,-2,length.out = 1000))

# Show lambda vs. cross-validation error.
i <- which(with(fit.ridge2,lambda == lambda.min))
j <- which(with(fit.ridge2,lambda == lambda.1se))
with(fit.ridge2,plot(lambda,cvm,log = "x",type = "l",col = "darkorange",
                     lwd = 2))
with(fit.ridge,lines(lambda,cvm,col = "dodgerblue",lty = "dashed",lwd = 2))
with(fit.ridge2,points(lambda[i],cvm[i],pch = 20,col = "darkorange"))
with(fit.ridge2,points(lambda[j],cvm[j],pch = 18,col = "darkorange"))
     
# PREDICT RESPONSES IN TEST CASES
# -------------------------------
# Use the fitted regression models to predict the responses in the
# text examples.
y1 <- drop(predict(fit.glmnet,Xtest,s = "lambda.min"))
y2 <- drop(predict(fit.ridge,Xtest,s  = "lambda.1se"))
y3 <- drop(predict(fit.ridge,Xtest,s = "lambda.min"))
y4 <- drop(predict(fit.ridge2,Xtest,s = "lambda.min"))
cat(sprintf("elastic net MSE        = %0.3f\n",mean((ytest - y1)^2)))
cat(sprintf("ridge (lambda.1se) MSE = %0.3f\n",mean((ytest - y2)^2)))
cat(sprintf("ridge (lambda.min) MSE = %0.3f\n",mean((ytest - y3)^2)))
cat(sprintf("ridge (custom) MSE     = %0.3f\n",mean((ytest - y4)^2)))

# EVALUATE PREDICTIONS
# --------------------
create_scatterplot <- function (x, y, title)
  qplot(x,y,size = I(1)) +
    geom_abline(slope = 1,intercept = 0,color = "magenta",
                linetype = "dotted") +
    xlim(c(-8,8)) + 
    ylim(c(-8,8)) + 
    labs(x = "true",y = "estimated",title = title) +
    theme(plot.title = element_text(face = "plain",size = 10))
theme_set(theme_cowplot(10))
p1 <- create_scatterplot(ytest,y1,"elastic net (lambda.min)")
p2 <- create_scatterplot(ytest,y2,"ridge (lambda.1se)")
p3 <- create_scatterplot(ytest,y3,"ridge (lambda.min)")
p4 <- create_scatterplot(ytest,y4,"ridge (custom)")
dev.new()
print(plot_grid(p1,p2,p3,p4))
