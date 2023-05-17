# Create a scatterplot showing true vs. estimated regression
# coefficients.
plot_coefs <- function (true, est)
  qplot(true,est) +
  geom_abline(intercept = 0,slope = 1,color = "deepskyblue",
              linetype = "dashed") +
  labs(x = "true coef,y = "estimated coef") +
  theme_cowplot(font_size = 12)	 
