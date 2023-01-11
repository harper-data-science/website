## HEADER ####
## what: Plant Spatial ecology code
## what: Ch 01 modified
## who: Ed H
## when: last edited 2023-01-07

## CONTENTS ####
## Setup
## Fig 1.1 NOT WORKING cannot locate .tif
## Fig 1.2a
## Fig. 1.2b
## Fig. 1.2c

## Setup ####
## Unzip book data to folder /data in script working dir

library(gstat) 
library(sf)
library(terra)
library(stars)
library(starsExtra)
library(raster)
library(ggplot2)
library(lattice)
library(rasterVis)


setwd(r'(D:\Dropbox\git-harper-data-science\plant-spatial\code-and-data)')

## Fig. 1.1 ####
## NOT WORKING cannot locate .tif

# Display of 1/10 of points in Data Set 2

# Hillshade map downloaded from UCSB Biogeograph Lab
# http://www.biogeog.ucsb.edu/projects/gap/gap_data_state.html
# and gepregistered in ArcGIS
# Somewhat confusingly, the function rast() creates a terra object

# ca.elev <- rast("Auxiliary\\caelev.tif")
# crs(ca.elev) <- "EPSG:4326"
# greys <- grey(0:255 / 255)
# plot(x = ca.elev, y = 1, col = greys, axes = TRUE, legend = FALSE,
#    mar = c(4,4,4,4), xlab = "Longitude", ylab = "Latitude")
# # From Appendix B/Set2 Input
# data.Set2 <- read.csv("set2\\set2data.csv", header = TRUE)
# # Assign ID values and use the modulo function to select 1 in 10
# data.Set2$ID <- 1:nrow(data.Set2)
# Set2.plot <- data.Set2[-which(data.Set2$ID %% 10 != 0),]
# Set2.plot.sf <- st_as_sf(Set2.plot, coords = c(2,3))
# st_crs(Set2.plot.sf) <- 4326  # WGS 84
# # Use the ID data field as a dummy
# plot(Set2.plot.sf["ID"], pch = 16, cex = 0.5, add = TRUE,
#    col = "white") # Fig. 1.1
# title(main = "Wieslander Survey Locations", cex.main = 2)
# 
# col.terrain <- terrain.colors(10)[10:1]
# col.terrain[10] <- "white"
# plot(x = ca.elev, y = 1, col = col.terrain, axes = TRUE,
#    legend = FALSE, mar = c(4,4,4,4),
#     xlab = "Longitude", ylab = "Latitude") # Fig. 1.1
# plot(Set2.plot.sf["ID"], pch = 16, cex = 0.5, add = TRUE,
#    col = "red") 
# title(main = "Wieslander Survey Locations", cex.main = 2)
   
## Fig. 1.2a ####

### not.spplot() def ####
# Homemade function to call levelplot()
# basically takes a grip object of values and plots it 'like' a raster
not.spplot <- function(xyz, plot.main, plot.xlab, plot.ylab, 
  plot.col = FALSE){
grid.xy <- expand.grid(
   x = seq(xmin(xyz) + 0.5 * res(xyz)[1],
     xmax(xyz) - 0.5 * res(xyz)[1], res(xyz)[1]),
   y = seq(ymin(xyz)+ 0.5 * res(xyz)[2],
    ymax(xyz) - 0.5 * res(xyz)[2], res(xyz)[2]))
   M1 <- matrix(values(xyz), nrow = nrow(xyz),
     ncol = ncol(xyz), byrow = TRUE)
   M2 <- matrix(0, nrow = nrow(xyz), 
      ncol = ncol(xyz))
   for (i in 1:nrow(M1))
      for(j in 1:ncol(M1))
         M2[i,j] <- M1[nrow(M1)-i+1,j]
   grid.xy$z <- as.vector(t(M2))
   p <- levelplot(z ~ x + y, grid.xy, aspect = nrow(xyz) / ncol(xyz),
   main = plot.main, 
   xlab = plot.xlab, ylab = plot.ylab,
   col.regions = plot.col)
return(p)
}

# Kriging of Field 4-2 ECa to create Fig. 1.2a
data.Set4.2EC <- read.csv("data/Set4/Set4.2EC.csv", header = TRUE)
data.Set4.2EC$ID <- 1:nrow(data.Set4.2EC)

# Establish the coordinates of the rectangular boundary
N <- 4267868
S <- 4267513
E <- 592867
W <- 592082

# # Create the interpolation grid
# library(maptools)
# grid.xy <- expand.grid(x = seq(W, E, cell.size),  y = seq(N, S, -cell.size))
# coordinates(grid.xy) <- ~x + y
# gridded(grid.xy) = TRUE

# If cell size 5 chokes the computer, set cell.size to 10
#cell.size <- 5
cell.size <- 10

# Create the interpolation grid
# library(stars)
# library(starsExtra)

# Based on https://rdrr.io/cran/starsExtra/man/make_grid.html
SW <- st_point(c(W, S))
NE <- st_point(c(E, N))
xy.sfc <- st_sfc(SW, NE)
class(xy.sfc)

grid.xy <- make_grid(xy.sfc, res = cell.size, buffer = 0)
st_crs(grid.xy) <- "EPSG:32610" # UTM Zone 10N


# Select every (cell.size)th data value
# library(gstat)
data.vgm <- data.Set4.2EC[-which(data.Set4.2EC$ID %% cell.size != 0), ]
data.vgm.sf <- st_as_sf(data.vgm, 
                        coords = c("Easting", "Northing"),
                        crs = "EPSG:32610")

EC.vgm <- variogram(ECto30 ~ 1, data.vgm.sf)
EC.fit <- fit.variogram(EC.vgm, model = vgm(100000, "Sph", 700, 10000))

plot(EC.vgm, EC.fit, col = "goldenrod") # Fig.1.2a
EC.krig <- krige(ECto30 ~ 1, data.vgm.sf, grid.xy, model = EC.fit)

library(ggplot2)
ggplot() + 
    geom_stars(data = EC.krig, aes(fill = var1.pred,
       x = x, y = y)) +
    scale_fill_gradient(low = "black", high = "white") + 
    geom_sf(data = xy.sfc, cex = 0.5)

# Next to raster and terra
# First convert to a raster object
# library(raster)
EC.krig.ras <- as(EC.krig["var1.pred"], "Raster")

# Next convert to a terra object
# library(terra)
# library(lattice)
EC.krig.ter <- rast(EC.krig.ras)
greys <- grey(0:255 / 255)
not.spplot(EC.krig.ter, 
   plot.main = "Soil Apparent Electrical Conductivity (mS/m)",   
   plot.xlab = "Easting", plot.ylab = "Northing",
   plot.col = greys)

  
## Fig. 1.2b ####

# Plot of Field 4-2 NDVI to create Fig. 1.2b
#library(terra)
data.4.2.May.ras <- rast("data/set4/Set4.20596.tif")
crs(data.4.2.May.ras) <- "EPSG:32610" # UTM Zone 10N
greys <- grey(0:255 / 255)
par(mai = c(1,1,1,1))
N <- 4267868
S <- 4267513
E <- 592867
W <- 592082
ext(data.4.2.May.ras)
e <- ext(W, E, S, N)

# crop by the extent
display.ras <- crop(data.4.2.May.ras, e)
ext(display.ras)

# par.settings = GrTheme causes a greyscale print
# library(rasterVis)
rasterVis::levelplot(display.ras, layers = 1, margin = FALSE,
  par.settings = GrTheme, xlab = "Easting", ylab = "Northing",
  main = "Infrared Band Digital Number")

## Fig. 1.2c ####
  
# Construction of Fig. 1.2c
# Interpolate yield to a 10 meter grid
# library(sf)

# This file is created in Section 6.2.3 << no
# use existing yield map for wheat

data.Yield4.2 <- read.csv("data/Set4/Set4.296wheatyield.csv")
data.Yield4.2$ID <- 1:nrow(data.Yield4.2)
head(data.Yield4.2)

# Establish the coordinates of the rectangular boundary
N <- 4267868
S <- 4267513
E <- 592867
W <- 592082

# IF the computer chokes on this use cell.size <- 20
cell.size <- 20

# Remove yield trend prior to kriging
data.Yield4.2$x <- with(data.Yield4.2, Easting - min(Easting))
data.Yield4.2$y <- with(data.Yield4.2, Northing - min(Northing))
trend.lm <- lm(Yield ~ x + y + I(x^2) + I(y^2) +
   I(x*y), data = data.Yield4.2)
data.Yield4.2$trend <- fitted(trend.lm)
data.Yield4.2$YieldDT <- data.Yield4.2$Yield - fitted(trend.lm)

# Select every nth value
krig.dat <- data.Yield4.2[-which(data.Yield4.2$ID %% cell.size != 0),]
krig.dat.sf <- st_as_sf(krig.dat, 
  coords = c("Easting", "Northing"), crs = "EPSG:32610")

# Create the interpolation grid
# Based on https://rdrr.io/cran/starsExtra/man/make_grid.html
# library(stars)
# library(starsExtra)
SW <- st_point(c(W, S))
NE <- st_point(c(E, N))
xy <- st_sfc(SW, NE)
xy.sf <- st_sf(xy)
grid.xy <- make_grid(xy.sf, res = cell.size, buffer = 0)
st_crs(grid.xy) <- "EPSG:32610" # UTM Zone 10N

# Compute kriged detrended yield
# library(gstat)
Yield.vgm <- variogram(YieldDT ~ 1, krig.dat.sf)
Yield.fit <- fit.variogram(Yield.vgm, 
   model = vgm(100000, "Sph", 200, 10000))

# Check the fit visually and compute kriged interploation
plot(Yield.vgm, Yield.fit, col = "goldenrod")

# This may take a long time, move to a 20 grid if necessary
Yield.krig <- krige(YieldDT ~ 1, krig.dat.sf, grid.xy,
   model = Yield.fit)
plot(Yield.krig["var1.pred"]) # Check

# Add trend back
x <- st_coordinates(Yield.krig)[,1] - W
y <- st_coordinates(Yield.krig)[,2] - S
b <- coef(trend.lm)
Yield.trend <- b[1] + b[2]*x + b[3]*y + b[4]*x^2 + b[5]*y^2 + b[6]*x*y
M <- matrix(Yield.trend, nrow = nrow(Yield.krig[[1]]),
   ncol = ncol(Yield.krig[[1]]), byrow = TRUE)
Yield.krig[[1]] <- Yield.krig[[1]] + M

# Move to terra
# library(raster)
# library(terra)
Yield.krig.ras <- as(Yield.krig["var1.pred"], "Raster")
Yield.krig.ter <- rast(Yield.krig.ras)
plot(Yield.krig.ter) # Check

# library(rasterVis)
greys <- grey(255:0 / 255)

# par.settings = GrTheme causes a greyscale print
rasterVis::levelplot(Yield.krig.ter, layers = 1, margin = FALSE,
  par.settings = GrTheme, xlab = "Easting", ylab = "Northing",
  main = "Grain Yield (kg/ha)", col.regions = greys)


