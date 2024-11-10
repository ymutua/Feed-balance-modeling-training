# Prepare livelihoods data

#install.packages("terra")

#Libraries
library(terra)

# study area
region <- "BurkinaFaso"

# Paths, directories
root <- "/Users/s2255815/Library/CloudStorage/OneDrive-UniversityofEdinburgh/Work/Projects/AU-IBAR/Spatial modelling workshop/Workshop_materials/Day_4"
datadir <- paste0(root, "/src/3Balance-estimates/", region, "/SpatialData/inputs")
outdir <- paste0(root, "/src/3Balance-estimates/", region, "/SpatialData/intermediate"); dir.create(outdir, F, T)

# load livelihoods vectors
livelihood <- vect(paste0(datadir, '/BF_LHZ_2014.shp'))

# reference raster
r <- rast(ext(livelihood), resolution = 0.02635871, crs = crs(livelihood))

# rasterize the SpatVector
liveOut <- rasterize(livelihood, r, field = "LZCODE")

# Write output
writeRaster(liveOut, paste0(outdir, "/livelihoodZones.tif"), overwrite=TRUE)
