# Illustration of using plotly to create an interactive plot with a
# "tooltip" that shows data that aren't shown in the original
# (non-interactive) plot.
#
# NOTE: You should install the development version of ggplot2:
#
#   library(devtools)
#   install_github("hadley/ggplot2")
#
library(ggplot2)
library(maps)
library(plotly)
library(htmlwidgets)

# Read the airports data set.
# Source: https://old.datahub.io/dataset/global-airports
airports <- read.csv("../data/global_airports.csv",stringsAsFactors = FALSE)

p <- ggplot(airports,aes_string(city = "city",country = "country")) +
  geom_path(data = map_data("world"),
            aes(x = long,y = lat,group = group),
            color = "gray",size = 0.5,inherit.aes = FALSE) +
  coord_cartesian(xlim = c(-180,190),ylim = c(-80,80)) +
  geom_point(data = airports,mapping = aes(x = longitude,y = latitude),
             shape = 20,size = 0.75,color = "royalblue") +
  theme_minimal()

# Show the non-interactive plot.
print(p)

# Create the interactive plot in a webpage, airportsmap.html.
out <- ggplotly(p,tooltip = c("city","country"))
saveWidget(out,"airportsmap.html",selfcontained = TRUE)
