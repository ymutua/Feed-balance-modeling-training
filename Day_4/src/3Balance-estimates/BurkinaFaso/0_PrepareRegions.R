# Prepare livelihoods data

# install.packages("terra")
# install.packages("raster")
# install.packages("stars")
# install.packages("sf")
# install.packages("dplyr")
# install.packages("exactextractr")

#Libraries
library(terra)
library(raster)
library(stars)
library(sf)
library(dplyr)
library(exactextractr)

# study area
region <- "BurkinaFaso"

# Paths, directories
root <- "/Users/s2255815/Library/CloudStorage/OneDrive-UniversityofEdinburgh/Work/Projects/AU-IBAR/Spatial modelling workshop/Workshop_materials/Day_4"
datadir <- paste0(root, "/src/3Balance-estimates/", region, "/SpatialData/inputs")
outdir <- paste0(root, "/src/3Balance-estimates/", region, "/SpatialData/intermediate"); dir.create(outdir, F, T)

sf_use_s2(FALSE)
aoi_sudSah <-  st_read(paste0(datadir, '/BF_LHZ_2014.shp'))
aoi_sudSah$grouping <- ifelse(grepl("transhumant", aoi_sudSah$LZNAMEEN), "Sahel", "Sudanian")
aoi_sudSah <- aoi_sudSah %>%  group_by(grouping) %>% summarise()
st_write(aoi_sudSah, paste0(outdir, "/regions.gpkg"), append = F)
aoi_sudSah <- aoi_sudSah %>% vect()

#st_rasterize(sf = aoi_sudSah[, "grouping"], file = paste0(outdir, "/regions.tif"))

# reference raster
r <- rast(ext(aoi_sudSah), resolution = 0.02635871, crs = crs(aoi_sudSah))

# rasterize the SpatVector
regions <- rasterize(aoi_sudSah, r, field = "grouping")

# Write output
writeRaster(regions, paste0(outdir, "/regions.tif"), overwrite=TRUE)

sf_use_s2(FALSE)
aoi1 <-  st_read(paste0(datadir, '/BF_LHZ_2014.shp'))
aoi1$grouping <- NA
aoi1$grouping[1:2] <- "Central mixed"
aoi1$grouping[c(3)] <- "North mixed"
aoi1$grouping[4] <- "(Agro)pastoral Sahel"
aoi1$grouping[c(5,7,8,9)] <- "Cropping"
aoi1$grouping[c(6)] <- "South mixed"
#aoi1$grouping <- ifelse(grepl("transhumant", aoi1$LZNAMEEN), "Sahel", "Sudanian")
aoi1 <- aoi1 %>%  group_by(grouping) %>% summarise() #%>% ungroup()

#st_write(aoi1, 'SpatialData/intermediate/zones.gpkg', append = F)
st_write(aoi1, paste0(outdir, "/zones.gpkg"), append = F)

