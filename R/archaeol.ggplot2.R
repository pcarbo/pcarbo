# Creating effective data visualizations in R with ggplot2
# Archaeological Data Sets (26900/46900) taught by Prof. Alice Yao
# Dept. of Anthropology
# University of Chicago
# November 1, 2017
#
# INSTRUCTOR NOTES
# ----------------
# * Use Terminal, not iTerm.
# * Change font so that commas are periods are distinguishable.
# * Bring R Graphics book.
#
# OUTLINE
# -------
# *Add outline here.*
#
# SETUP
# -----
#
# 1. Start up R or RStudio.
#
# 2. Download the data files if you haven't already done so, then
#    rename them to be "pompei_loom.csv" and "sw_ceramics_seriation.csv".
#
# 3. Set your working directory so that the data files are in the same
#    directory as your working directory.
#
# 4. Install ggplot2 and cowplot packages:
#    install.packages("ggplot2")
#    install.packages("cowplot")
#
# 5. Load the ggplot2 package.
#
library(ggplot2)

# LOAD POMPEI LOOM DATA
# ---------------------
# Use read.csv function to read in data from CSV file. For more info,
# type help(read.csv). Note that I added a comment to the top of the
# CSV file to help me remember what the data is.
pompei <- read.csv(file = "pompei_loom.csv",comment = "#")

# Quickly inspect the data set.
head(pompei)
tail(pompei)
summary(pompei)
nrow(pompei)

# CREATE POMPEII LOOM SCATTERPLOT
# -------------------------------
# Let's create our first plot using ggplot2.
p <- ggplot(data = pompei,aes(x = Weight,y = Height)) +
       geom_point()
print(p)

# Let's plot blue x's instead of circles.
# Use colors() to get list of all built-in colors.
# Use pchShow() to get a list of all available shapes.
p <- ggplot(data = pompei,aes(x = Weight,y = Height)) +
       geom_point(shape = 4,color = "darkblue")
print(p)

# Let's make some adjustments to the plot:
#
#   1. Use minimal theme.
#   2. Remove grid lines.
#   3. Add title.
#   4. Add units (are these the right units?
#   5. Add more numbers to X axis.
#   6. Add more numbers to Y axis.
#
p <- p + theme_minimal()
p <- p + theme(panel.grid.minor = element_blank(),
               panel.grid.major = element_blank())
p <- p + labs(title = "Excavated looms from Pompeii",
              x = "Weight (g)",y = "Height (cm)")
p <- p + scale_x_continuous(breaks = seq(100,700,100))
p <- p + scale_y_continuous(breaks = seq(60,130,10))
print(p)

# Demonstrate a different theme.
library(cowplot)
p <- ggplot(data = pompei,aes(x = Weight,y = Height)) +
       geom_point(shape = 4,color = "darkblue")
print(p)

# CREATE POMPEII LOOM CONTOUR PLOT
# --------------------------------
# Uses kernel density estimation.
p2 <- ggplot(data = pompei,aes(x = Weight,y = Height)) +
        geom_density_2d()
print(p2)

# Make a few adjustments.
p2 <- ggplot(data = pompei,aes(x = Weight,y = Height)) +
        geom_density_2d(color = "black",size = 0.5,bins = 8)

# Let's show both plots side-by-side.
plot_grid(p,p2,labels = c("A","B"))

# Things to mention at this stage:
#
#   1. Documentation: ggplot2.tidyverse.org
#   2. Show how to save the plot as PNG and PDF using ggsave.
#

# LOAD CERAMIC WARE DATA
# ----------------------
# This is a slightly more complex data set.
# Note that I changed the names of the columns slightly to avoid issues.
ceramics <- read.csv("sw_ceramics_seriation.csv",comment = "#")
ceramics

# CREATE CORRUGATED WARE BAR CHART
# --------------------------------
# Create the bar chart.
p3 <- ggplot(data = ceramics,aes(x = Level,y = Corrugated.ware)) +
        geom_col()
print(p3)

# Fix the Level column.
# ceramics <- transform(ceramics,Level = factor(Level))
p3 <- ggplot(data = ceramics,aes(x = Level,y = Corrugated.ware)) +
        geom_col()
print(p3)

# Add a line to the bar chart.
p3 <- p3 + geom_line()
print(p3)

# Fix the colors, change width of bars, remove the axis lines, add
# Levels 1–10 on the X axis, and add a title to the plot.
#
# Note: Try first without "fill".
p4 <- ggplot(data = ceramics,aes(x = Level,y = Corrugated.ware)) +
        geom_col(color = "dodgerblue",fill = "dodgerblue",width = 0.5) +
        geom_line(color = "darkorange",size = 1)
p4 <- p4 + theme(axis.line = element_blank())
p4 <- p4 + scale_x_continuous(breaks = seq(1,10))
p4 <- p4 + labs(title = "")
print(p4)
