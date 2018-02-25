# Here, we assess performance of EM for optimizing the mixture weights
# of a simple mixture model.

# SCRIPT PARAMETERS
# -----------------
# Set the number of data samples.
# TO DO.

# Set the number of mixture components.
# TO DO.

# Set the random number generator seed.
# TO DO.
set.seed(seed)

# These are additional variables determining how the data set is
# generated: the standard errors of the samples (se), and the standard
# deviations (s) and mixture weights (w) used to simulate the data.

```{r data}
se <- rep(0.1,n)
s  <- c(0.01,10^(seq(-2,0,length.out = k)))
w  <- runif(k)
w  <- w/sum(w)
```

# GENERATE DATA SET
# -----------------
# Simulate a data set with n samples.
cat(sprintf("Simulating data set with %d observations.\n",n))
x <- datasim.norm(w,s,se)

# COMPUTE LIKELIHOOD MATRIX
# -------------------------
# Compute the n x k conditional likelihood matrix.

```{r, calc-likelihood}
cat(sprintf("Computing the %d x %d conditional likelihood matrix.\n",n,k))
L <- condlikmatrix.norm(x,se,s)
```

## Fit mixture model using EM

First, fit the mixture model using the EM algorithm. This is a very
simple algorithm---after computing the conditional likelihood matrix,
the E and M steps can be implemented in only a few lines of code:

```{r, eval=FALSE}
# E Step
P <- t(t(L) * w)
P <- P / (rowSums(P) + eps)

# M step
w <- colMeans(P)
```

Here, eps is a small number near zero (e.g., 1e-8).

Observe that individual EM iterations are fast but it takes many
iterations to converge to a solution.

```{r, em}
out <- system.time(fit.em <- mixopt.em(L,tol = 1e-4,verbose = FALSE))
cat(sprintf("Model fitting took %d iterations and %0.2f seconds.\n",
            length(fit.em$maxd),out["elapsed"]))
```

## Fit mixture model using IP solver---primal formulation

The primal-dual interior-point solver is based on the algorithm
described by [Armand *et al*](https://doi.org/10.1137/S1052623498344720).
It is substantially more complicated than the EM algorithm, and
individual iterations are more expensive, but it takes only a small
number of iterations to converge to a solution.

```{r, ip}
out <- system.time(fit.ip <- mixopt.ip(L))
cat(sprintf("Model fitting took %d iterations and %0.2f seconds.\n",
            length(fit.ip$maxd),out["elapsed"]))
```

## Fit mixture model using IP solver---dual formulation

Dual optimization problem

```{r, dualip}
out <- system.time(fit.dualip <- mixopt.dualip(L))
cat(sprintf("Model fitting took %d iterations and %0.2f seconds.\n",
            length(fit.dualip$maxd),out["elapsed"]))
```

The EM algorithm implements a very simple convergence criterion---the
maximum difference between the iterates must be small---whereas the
convergence in the IP method is based on how close we are to
satisfying the KKT optimality conditions.

*Note:* if you have the RMosek and REBayes packages installed, you can
compare the output of the IP method to running

```{r rebayes, echo=TRUE, eval=FALSE}
REBayes:KWDual(L,rep(1,k),rep(1,n))
```

## Compare the quality of the three solutions.

The (primal) IP method gives us the best solution:

```{r}
print(t(t(c(EM     = min(fit.em$obj),
            IP     = min(fit.ip$obj),
            dualIP = fit.dualip$obj))),
      digits = 12,right = FALSE)
```

## Plots showing improvement in EM & IP solutions over time

This first plot shows the maximum change in the solution against the
running time of the EM algorithm.

```{r plot-delta-vs-time-em}
plot.delta.vs.time <- function (timing, maxd, color, plot.title) {
  m  <- length(maxd)
  i  <- 2:(m-1)
  return(ggplot(data.frame(time = timing[i,"elapsed"],
                           maxd = maxd[i]),
                aes(x = time,y = maxd)) +
         geom_line(col = color,size = 0.5) +
         geom_point(col = color,shape = 20) +
         scale_y_continuous(breaks = 10^seq(-4,2),trans = "log10") +
         labs(x     = "elapsed time (seconds)",
              y     = "max. change in solution",
              title = plot.title))
}
p1 <- plot.delta.vs.time(fit.em$timing,fit.em$maxd,"darkorange","EM algorithm")
```

The next two plots also show the maximum change in the solution at
each iteration, but for the IP solvers.

```{r plot-delta-vs-time-ip}
p2 <- plot.delta.vs.time(fit.ip$timing,fit.ip$maxd,"royalblue",
                         "IP algorithm (primal)")
p3 <- plot.delta.vs.time(fit.dualip$timing,fit.dualip$ipsolver$maxd,
						 "darkblue","IP algorithm (dual)")
```

This plot shows the distance of the (primal) objective function to the
minimum against the running time of the EM algorithm. (Here, we take
the "minimum" to be the best solution among the three algorithms.)

```{r plot-objective-vs-time-em}
best.sol <- min(c(fit.em$obj,fit.ip$obj,fit.dualip$obj))
plot.obj.vs.time <- function (timing, obj, min.obj, color, plot.title) {
  m  <- length(obj)
  i  <- 2:(m-1)
  return(ggplot(data.frame(time = timing[i,"elapsed"],
                           y    = obj[i] - min.obj),
                aes(x = time,y = y)) +
  geom_line(col = color,size = 0.5) +
  geom_point(col = color,shape = 20) +
  scale_y_continuous(breaks = 10^(-6:3),trans = "log10") +
  labs(x     = "elapsed time (seconds)",
       y     = "dist. from min.",
       title = plot.title))
}
p4 <- plot.obj.vs.time(fit.em$timing,fit.em$obj,best.sol,
                       "darkorange","EM algorithm")
```

These two plots show the value of the objective against the running
time of the IP solver. For the dual formulation, the distance to the
dual objective is shown.

```{r plot-objective-vs-time-ip}
p5 <- plot.obj.vs.time(fit.ip$timing,fit.ip$obj,best.sol,"royalblue",
                       "IP algorithm (primal)")
p6 <- plot.obj.vs.time(fit.dualip$timing,fit.dualip$ipsolver$obj,
                       min(fit.dualip$ipsolver$obj),"darkblue",
                       "IP algorithm (dual)")
```
Arrange all six plots in a single figure.

```{r plot-grid, fig.width=7, fig.height=7}
adjust.plot <- function (p)
  p + theme_cowplot(font_size = 12) +
    theme(plot.title = element_text(face = "plain"))
print(plot_grid(adjust.plot(p1),adjust.plot(p4),
                adjust.plot(p2),adjust.plot(p5),
                adjust.plot(p3),adjust.plot(p6),
				nrow = 3))
```

Points in the plots indicate individual iterations.

I plotted the results in this way to show the *improvement in the
solution over time.* (With the caveat that the dual formulation of the
IP method shows the improvement in the dual objective.) In this way,
we can clearly see the differences in how the EM and IP methods
behave:

+ The IP method moves rapidly toward the solution, but each iteration
  is more expensive.

+ EM algorithm never quite reaches the solution, even after a large
  number of iterations; the panels in the top row both highlight the
  poor convergence properties of EM. 

+ We also see that the dual IP method is slower to converge than the
  primal IP, and the per-iteration cost is substantialy higher.

## Session information

This is the version of R and the packages that were used to generate
these results.

```{r session-info}
sessionInfo()
```
