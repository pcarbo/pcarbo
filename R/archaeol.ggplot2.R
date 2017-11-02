# CREATING EFFECTIVE DATA VISUALIZATIONS IN R WITH GGPLOT2
# Archaeological Data Sets (26900/46900) taught by Prof. Alice Yao
# Dept. of Anthropology
# University of Chicago
# Haskell room 121
# November 1, 2017
#
# INSTRUCTOR NOTES
# ----------------
# * Use Terminal, not iTerm.
# * Change font so that commas are periods are distinguishable.
# * Bring R Graphics book.
# * Write outline on white/blackboard, and keep referring to it.
#
# AIMS
# ----
# 1. Get cofmortable with basics of the ggplot2 interface.
# 2. Illustrate Best Practices for data analysis in R.
# 3. Create some fun data visualizations.
#
# OUTLINE
# -------
# 1. Set up R programming environment for data analysis.
# 2. Load a spreadsheet into R.
# 3. Create a simple scatterplot of loom weight vs. height.
# 4. Create a contour plot of loom weight vs. height.
# 5. Adjust plotting parameters for more effective plots.
# 6. Create a simple bar chart from ceramics data.
# 7. Create a more sophisticated plot by combining "layers".
# 8. Create an even more sophisticated multipanel plot using "facets". 
#
# SOME USEFUL RESOURCES
# ---------------------
# * Software Carpentry: http://software-carpentry.org/lessons
# * "R for Data Science" book: http://r4ds.had.co.nz
# * ggplot2 website: http://ggplot2.tidyverse.org/reference
# * My "Intro to R" workshop, using Divvy data:
#   https://rcc-uchicago.github.io/R-intro-divvy
#
# SETUP
# -----
#
# 1. Start up R or RStudio. (Make sure you have a fairly recent
#    version of R---ideally, version 3.3.0 or greater.)
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

# Quickly inspect the data frame.
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
p <- ggplot(data = pompei,
            mapping = aes(x = Weight,y = Height)) +
       geom_point(shape = 4,color = "darkblue")
print(p)

# Let's make some adjustments to the plot:
#
#   1. Use minimal theme.
#   2. Remove grid lines.
#   3. Add title.
#   4. Add units (are these the right units?
#   5. Add more numbers to x-axis.
#   6. Add more numbers to y-axis.
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
p <- ggplot(data = pompei,
            mapping = aes(x = Weight,y = Height)) +
       geom_point(shape = 4,color = "darkblue")
print(p)

# CREATE POMPEII LOOM CONTOUR PLOT
# --------------------------------
# Uses kernel density estimation.
p2 <- ggplot(data = pompei,
             mapping = aes(x = Weight,y = Height)) +
        geom_density_2d()
print(p2)

# Make a few adjustments.
p2 <- ggplot(data = pompei,aes(x = Weight,y = Height)) +
        geom_density_2d(color = "black",size = 0.5,bins = 8)

# EXERCISES: Combine the contour plot and scatterplot into a single
# plot, then try adjusting the colors, line widths, line styles and 

# NOTE: Show how to save the plot as PNG and PDF using ggsave, or by
# taking a screen shot, or by using the Export button in RStudio.

# LOAD CERAMIC WARE DATA
# ----------------------
# This is a slightly more complex data set. Note that I changed the
# names of the columns slightly to avoid possible issues.
ceramics <- read.csv("sw_ceramics_seriation.csv",comment = "#")
ceramics

# CREATE CORRUGATED WARE BAR CHART
# --------------------------------
# Create the bar chart.
p3 <- ggplot(data = ceramics,
             mapping = aes(x = Level,y = Corrugated.ware)) +
        geom_col()
print(p3)

# Add a line to the bar chart.
p3 <- p3 + geom_line()
print(p3)

# Fix the colors, change width of bars, remove the axis lines, add
# Levels 1–10 on the x-axis, and add a title to the plot.
#
# Note: Try first without "fill".
p4 <- ggplot(data = ceramics,
             mapping = aes(x = Level,y = Corrugated.ware)) +
        geom_col(color = "dodgerblue",fill = "dodgerblue",width = 0.5) +
        geom_line(color = "darkorange",size = 1)
p4 <- p4 + theme(axis.line = element_blank())
p4 <- p4 + scale_x_continuous(breaks = seq(1,10))
p4 <- p4 + labs(title = "Change in ceramic ware by level")
print(p4)

# CREATE BAR CHARTS FOR ALL CERAMIC TYPES
# ---------------------------------------
# Create a new data frame containing the same data, but with only 3
# columns. Notice that the "type" column is a *factor*.
#
# Note: This is the most complex code chunk in this lesson.
#
pdat <- data.frame(Level      = rep(ceramics$Level,times = 5),
                   delta.ware = do.call(c,ceramics[-1]),
                   type       = rep(colnames(ceramics)[-1],each = 10))
head(pdat)
tail(pdat)
nrow(pdat)
summary(pdat)
summary(pdat$type)

# Create the five bar charts using facets. Here, the scale on the
# y-axis is different.
p5 <- ggplot(data = pdat,aes(x = Level,y = delta.ware)) +
        geom_col() +
        geom_line() +
        facet_wrap("type",scales = "free",nrow = 1)
print(p5)

# Adjust the plotting parameters to make the plots look nicer.
p5 <- ggplot(data = pdat,aes(x = Level,y = delta.ware)) +
        geom_col(color = "dodgerblue",fill = "dodgerblue",width = 0.5) +
        geom_line(color = "darkorange",size = 1) +
        facet_wrap("type",scales = "free",nrow = 1)
p5 <- p5 + scale_x_continuous(breaks = 1:10)
p5 <- p5 + theme_cowplot(font_size = 11)
p5 <- p5 + theme(axis.line        = element_blank(),
                axis.ticks.x     = element_blank(),
                strip.background = element_blank())
print(p5)

