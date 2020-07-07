# SCRIPT PARAMETERS
n <- 40
p <- 0.8

# SIMULATE BINOMIAL DATA
set.seed(1)
x <- rbinom(1,n,p)

# PLOT BINOMIAL & POISSON LIKELIHOOD SURFACES
p  <- seq(0,1,length.out = 1000)
f1 <- dbinom(x,n,p,log = TRUE)
f2 <- dpois(x,n*p,log = TRUE)
f1 <- exp(f1 - max(f1))
f2 <- exp(f2 - max(f2))
f1 <- f1/sum(f1)
f2 <- f2/sum(f2)
plot(p,f1,type = "l",col = "dodgerblue",lwd = 2)
lines(p,f2,col = "darkorange",lwd = 1)
