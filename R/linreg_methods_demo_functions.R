# Compute the mean squared error.
mse <- function (x, y)
  mean((x - y)^2)

# Create a scatterplot showing true vs. estimated regression
# coefficients.
plot_coefs <- function (true, est)
  qplot(true,est) +
    geom_abline(intercept = 0,slope = 1,color = "deepskyblue",
                linetype = "dashed") +
    labs(x = "true coef",y = "estimated coef") +
    theme_cowplot(font_size = 12)	+
    theme(plot.title = element_text(face = "plain",size = 12))

# Create a scatterplot showing true Y vs. predicted Y.
plot_responses <- function (true, pred)
  qplot(true,pred) +
    geom_abline(intercept = 0,slope = 1,color = "deepskyblue",
                linetype = "dashed") +
    labs(x = "true Y",y = "predicted Y",
         title = sprintf("mse = %0.3f",mse(test$y,y))) +
    theme_cowplot(font_size = 12) +
    theme(plot.title = element_text(face = "plain",size = 12))
