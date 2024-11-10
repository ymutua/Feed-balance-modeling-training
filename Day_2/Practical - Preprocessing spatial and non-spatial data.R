# Preprocessing data

root <- "/Users/s2255815/Library/CloudStorage/OneDrive-UniversityofEdinburgh/Work/Projects/AU-IBAR/Spatial modelling workshop/Workshop_materials"

## # linux systems
## root <- "/home/Feed-balance-modeling-training"

## # for windows systems
## root <- "c:/Documents/Feed-balance-modeling-training"

library(ncdf4) # package for netcdf manipulation
library(raster) # package for raster manipulation
library(dplyr)
library(rgdal)
library(sf)
library(tidyterra)
library(ggplot2)
library(terra)
library(readr)

#-----------------------------------------------------
#-----------------------------------------------------
# Dry matter productivity
outdir <- paste0(root, "/Day_2/SpatialData/inputs/Feed_DrySeason/DMP"); dir.create(outdir, F, T)

# read AOI
aoi <- read_sf(paste0(root, "/Day_1/SpatialData/inputs/AdminBound/gadm40_BFA_0.shp"))

nc_files <- list.files(paste0(root, "/Day_1/SpatialData/inputs/Feed/DMP"), pattern = ".nc$", full.names = TRUE, recursive = TRUE)
nc_files

nc_file <- nc_open(paste0(root, "/Day_1/SpatialData/inputs/Feed/DMP/c_gls_DMP300-RT6_202001100000_GLOBE_OLCI_V1.1.2.nc"))
nc_file

nc_close(nc_file) 

lapply(nc_files, function(nc_file){
  
  nc_name <- gsub('.{3}$', '', basename(nc_file))
  
  iDMP <- raster::raster(nc_file, varname="DMP", ncdf=TRUE)
  iDMP <- crop(iDMP, extent(aoi))
  iDMP <- mask(iDMP, aoi)
  
  # save as GeoTIFF
  raster::writeRaster(iDMP, filename = paste0(outdir, "/", nc_name, ".tif"), overwrite=TRUE)
})

iDMP <- rast(paste0(root, "/Day_2/SpatialData/inputs/Feed_DrySeason/DMP/c_gls_DMP300-RT6_202001310000_GLOBE_OLCI_V1.1.2.tif"))

ggplot() + geom_sf(data = aoi, colour = "black", show.legend = F) +
  geom_spatraster(data = iDMP) +
  geom_sf(data = aoi, colour = "black", fill = NA, show.legend = F) +
  scale_fill_gradient(low = "#CDDF4A", high = "#0BAE1C", na.value = NA, name="DM (kg/hectare/day)")

#-----------------------------------------------------
#-----------------------------------------------------
# Crop type and area
outdir <- paste0(root, "/Day_2/SpatialData/inputs/SPAM2020"); dir.create(outdir, F, T)

dmpTemp <- rast(paste0(root, "/Day_2/SpatialData/inputs/Feed_DrySeason/DMP/c_gls_DMP300-RT6_202001100000_GLOBE_OLCI_V1.1.2.tif"))

sPamfiles <- list.files(paste0(root, "/Day_1/SpatialData/inputs/Feed/CropType/spam2020V0r1_global_physical_area"), pattern = "_A.tif$", full.names = TRUE, recursive = TRUE)

lapply(sPamfiles, function(sPamfile){
  sPamfile_name <- tolower(gsub('.{4}$', '', basename(sPamfile)))
  
  isPamFile <- rast(sPamfile)
  isPamFile <- crop(isPamFile, ext(dmpTemp))
  isPamFile <- resample(isPamFile, dmpTemp, method="bilinear")
  isPamFile <- mask(isPamFile, mask = dmpTemp)
  
  isPamFile[is.nan(values(isPamFile))] <- NA
  
  names(isPamFile) <- sPamfile_name
  varnames(isPamFile) <- sPamfile_name
  
  # save as GeoTIFF
  writeRaster(isPamFile, filename = paste0(outdir, "/", sPamfile_name, ".tif"), overwrite=TRUE)
})

#-----------------------------------------------------
#-----------------------------------------------------
# Harvest index
rasterOptions(maxmemory = 1e+60)
rasterOptions(todisk = TRUE)

cropHI <- read_csv(paste0(root, "/Day_1/Tables/crop_harvest index.csv"))

pathSPAM <- paste0(root, "/Day_2/SpatialData/inputs/SPAM2020")
pathSPAMInter <- paste0(root, "/Day_2/SpatialData/inputs/SPAM2020/intermediate"); dir.create(pathSPAMInter, F, T)

filesSPAM <- list.files(path = pathSPAM, pattern = "_a.tif$", full.names = T)
stSPAM <- rast(filesSPAM)

crops <- sub(".*_a_(.*?)_a\\.tif$", "\\1", filesSPAM)

iSPAMcropArea <- app(stSPAM, fun = sum, na.rm = TRUE)

for(i in 1: length(crops)){
  
   # Extract the relevant SPAM data for the crop
  tmpCropIndex <- grep(pattern = paste(crops[i], collapse = "|"), names(stSPAM))
  iSPAMtmpArea <- stSPAM[[tmpCropIndex]]
  
  # Adjust crop data based on the harvest index
  icrop <- stSPAM[[tmpCropIndex]]
  icrop[icrop >0] <- (1 - cropHI$harvest_index[cropHI$codeSPAM == crops[i]])
  
  # Write the adjusted crop data
  writeRaster(icrop, paste0(pathSPAMInter, "/", crops[i], "_res_frac.tif"), overwrite = T)
  
  # Calculate crop proportion and write the output
  stSPAMcropProp <- iSPAMtmpArea/iSPAMcropArea
  
  writeRaster(icrop, paste0(pathSPAMInter, "/", crops[i], "_prop.tif"), overwrite = T)
}
