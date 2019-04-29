# Second plot.
klmin <- min(dat[,"KL"])
dat   <- as.data.frame(dat)
dat   <- transform(dat,
                   beta1 = alpha1*mu1,
                   dist  = KL - klmin + 1e-8)
p2 <- ggplot(dat,aes(x = beta1,y = dist)) +
    geom_line()
