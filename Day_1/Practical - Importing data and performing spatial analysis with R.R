# Importing data and performing spatial analysis with R

root <- "/Users/s2255815/Library/CloudStorage/GoogleDrive-ymutua@gmail.com/My Drive/Ruminant_feed_balance/Workshop_materials"

## # linux systems
## root <- "/home/Feed-balance-modeling-training"

## # for windows systems
## root <- "c:/Documents/Feed-balance-modeling-training"

# Install libraries
install.packages("sf")
install.packages("ggplot2")
install.packages("terra")

# Load libraries
library(sf)

# Path to directory that contains shpefile
indir <- paste0(root, "/Day_1/SpatialData/inputs/AdminBound")

# Path to shapefile
aoi1 <- read_sf(paste0(indir, "/gadm40_BFA_1.shp"))

# Look at the structure
str(aoi1) # note again the geometry column

# Visualise it
plot(aoi1)

# Visualise part
# In order to only plot the polygon boundaries we need to directly use the geometry column
plot(st_geometry(aoi1))

# Plot using a better library
# However, we can use the ggplot2 R package to make a better plot
library(ggplot2)
ggplot() + 
  geom_sf(data = aoi1, colour = "black", show.legend = F) + 
  coord_sf(xlim = c(2.1, 15.1), ylim = c(3.8, 14.3), expand = FALSE)

# Load library for reading raster
library(terra)
r <- rast(nrows=20, ncols=20, # number of cells in x and y dimension
          xmin=0, xmax=360) # min and max x coordinates (left-right borders)

# Look at the structure
r

# How many cells
ncell(r)

# How many layers
nlyr(r)

# What is the coordinate syatem
crs(r, proj=TRUE)

# Visualize the raster file
# However, it's empty! because the cells do not have any values
plot(r)

# Add some values
values(r) <- runif(ncell(r))

# Visualize it
r

# Plot teh new file with values
plot(r)
