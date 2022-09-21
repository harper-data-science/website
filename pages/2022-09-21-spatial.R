## tut 1 ####
# https://trucvietle.me/r/tutorial/2017/01/18/spatial-heat-map-plotting-using-r.html 

library(ggplot2)
library(ggmap)
library(RColorBrewer) # for color selection

library(RCurl)
## Read the input spatiotemporal data

x <- 'https://raw.githubusercontent.com/vietexob/mobile-intelligence/master/data/drone/drone_strikes.csv'

drone.data <- read.csv(file=x)
## Break down by year
year <- vector()
for(i in 1:nrow(drone.data)) {
  dateStr <- toString(drone.data$Date[i])
  dateStrSplit <- strsplit(dateStr, "/")[[1]]
  year[i] <- as.numeric(dateStrSplit[3])
}
## Create a year attribute
drone.data$year <- year
## Subset the data by year
subset.drone.data <- subset(drone.data, year >= 2008)
## Convert year to factor
subset.drone.data$year <- as.factor(subset.drone.data$year)

## Specify a map with center at the center of all the coordinates
mean.longitude <- mean(subset.drone.data$Longitude)
mean.latitude <- mean(subset.drone.data$Latitude)

register_google(key='AIzaSyBkzvExzFoMI7ARV2mxuGZE215FK53rb_Y')

drone.map <- get_map(location = c(mean.longitude, mean.latitude), zoom = 9, scale = 2)
print(drone.map)

## Convert into ggmap object
drone.map <- ggmap(drone.map, extent="device", legend="none")
print(drone.map)


# Plot a heat map layer: Polygons with fill colors based on
## relative frequency of events
drone.map <- drone.map + stat_density2d(data=subset.drone.data,
                                        aes(x=Longitude, y=Latitude, fill=..level.., alpha=..level..), geom="polygon")
## Define the spectral colors to fill the density contours
drone.map <- drone.map + scale_fill_gradientn(colours=rev(brewer.pal(7, "Spectral")))


drone.map <- drone.map + geom_point(data=subset.drone.data,
                                    aes(x=Longitude, y=Latitude), fill="red", shape=21, alpha=0.8)
## Remove any legends
drone.map <- drone.map + guides(size=FALSE, alpha = FALSE)
## Give the map a title
drone.map <- drone.map + ggtitle("US Drone Strikes in Pakistan from 2008 to 2013")


drone.map <- drone.map + facet_wrap(~year)
print(drone.map) # this is necessary to display the plot


## tut 2 ####
# https://www.sharpsightlabs.com/blog/how-to-create-a-crime-heatmap-in-r/

## tut 3 ####
# https://towardsdatascience.com/spatial-regression-using-fabricated-data-bbdb35da4851

## tut 4 ####
# https://www.spatialanalysisonline.com/An%20Introduction%20to%20Spatial%20Data%20Analysis%20in%20R.pdf

# https://rspatial.org/terra/analysis/analysis.pdf

## tut 5 ####
# https://stackoverflow.com/questions/50533738/best-method-of-spatial-interpolation-for-geographic-heat-contour-maps