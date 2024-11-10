# Process regional feed ME - min and max


# install.packages("raster")
# install.packages("stars")
# install.packages("sf")
# install.packages("dplyr")
# install.packages("exactextractr")
# install.packages("tidyr")

#.libPaths(c(.libPaths()[2], .libPaths()[3]))
library(raster)
#library(stars)
library(sf)
library(dplyr)
library(tidyr)
library(exactextractr)

rasterOptions(maxmemory = 1e+60)
rasterOptions(todisk=TRUE)
rasterOptions(tmpdir="/Users/s2255815/Downloads/AU_Temp")

# study area
region <- "BurkinaFaso"

# Paths, directories
root <- "/Users/s2255815/Library/CloudStorage/OneDrive-UniversityofEdinburgh/Work/Projects/AU-IBAR/Spatial modelling workshop/Workshop_materials/Day_4"
datadir <- paste0(root, "/src/3Balance-estimates/", region, "/SpatialData/inputs")
intdatadir <- paste0(root, "/src/3Balance-estimates/", region, "/SpatialData/intermediate")
cropParams_dir <- paste0(root, "/src/3Balance-estimates/", region, "/CropParams")
resultsDir <- paste0(root, "/src/3Balance-estimates/", region, "/Results"); dir.create(resultsDir, F, T)
spatialResultsDir <- paste0(root, "/src/3Balance-estimates/", region, "/SpatialData/outputs"); dir.create(spatialResultsDir, F, T)

zones <- st_read(paste0(intdatadir, "/zones.gpkg"))
regions <- st_read(paste0(intdatadir, "/regions.gpkg"))

cropME_HI_utilmin <- raster(paste0(intdatadir, "/cropME_HI_utilmin.tif"))
cropMEmin <- raster(paste0(intdatadir, "/cropMEmin.tif"))
feedQuality_item <- read.csv(paste0(cropParams_dir, "/feedQuality_item.csv"))
feedCropBurn <- raster(paste0(datadir, "/Burned/burnCropsDekads.tif"))

#croppingDays <- sum(raster('CropParams/phenoCroppingDays1.tif'), raster('CropParams/phenoCroppingDays2.tif'), na.rm = T)
croppingDays <- raster(paste0(cropParams_dir, "/croppingDays.tif"))
croppingDays <- reclassify(croppingDays, c(-Inf, 0, 0)) 
croppingDays <- raster::resample(croppingDays, feedCropBurn, method = "ngb")
croppingDays <- reclassify(croppingDays, c(220, Inf, 220)) 
dryDays <- 365 - croppingDays

region <- raster(paste0(intdatadir, "/regions.tif"))
grassMESud <- 6.2 #min = 6.2, max = 6.8
grassMESah <- 5.8 #min = 5.8, max = 6.4
grassME <- calc(region, fun = function(x){ifelse(x == 4, grassMESah, grassMESud)})
grassME <- raster::resample(grassME, feedCropBurn, method = "ngb")

browseMESud <- 5.8
browseMESah <- 8
browseME <- calc(region, fun = function(x){ifelse(x == 4, browseMESah, browseMESud)})
browseME <- raster::resample(browseME, feedCropBurn, method = "ngb")
#browseMEmean <- 5

grassFracDry <- 0.33 #max 0.55
grassFracWet <- 0.55 #max 0.55
browseFrac <- 0.05

tCrop <- stack(list.files(path = paste0(datadir, "/Feed_quantity/"), pattern="cropmean_2",full.names = T))
tCrop <- reclassify(tCrop, c(-Inf, 0, 0)) 
tCrop <- stack(tCrop)

croppingDays <- reclassify(croppingDays, c(0, 60, 60))

tCrop <- tCrop*croppingDays*cropME_HI_utilmin
for(i in 1:length(names(tCrop))){
  tCrop[[i]] <- overlay(tCrop[[i]], feedCropBurn, fun = function(DMP, burn){ifelse(burn > 0, 0, DMP) }) #, filename = 'SpatialData/outputs/Feed_crop_burn_MJ.tif', overwrite = T
}

tGrassWet <- stack(list.files(path = paste0(datadir, "/Feed_quantity/"),pattern="DMPgrassWetmean_2",full.names = T))
tGrassWet <- reclassify(tGrassWet, c(-Inf, 0, 0)) #Some negative DM in copernicus product
tGrassWet <- tGrassWet*grassFracWet*croppingDays * grassME 
tGrassWet <- stack(tGrassWet)

tGrassDry <- stack(list.files(path = paste0(datadir, "/Feed_quantity/"),pattern="DMPgrassDrymean_2",full.names = T))
tGrassDry <- reclassify(tGrassDry, c(-Inf, 0, 0)) 
tGrassDry <- tGrassDry*grassFracDry*dryDays * grassME
tGrassDry <- stack(tGrassDry)

tGrass <- tGrassWet + tGrassDry
tGrass <- stack(tGrass)
#rm(tGrassDry, tGrassWet)

tBrowse <- stack(list.files(path = paste0(datadir, "/Feed_quantity/"),pattern="DMPbrowsemean_2",full.names = T))
tBrowse <- reclassify(tBrowse, c(-Inf, 0, 0)) 
tBrowse <- stack(tBrowse)
tBrowseWet <- tBrowse*browseME*croppingDays * browseFrac
tBrowseWet <- stack(tBrowseWet)
tBrowseDry <- tBrowse*browseME*dryDays * browseFrac
tBrowseDry <- stack(tBrowseDry)

tBrowse <- tBrowseWet + tBrowseDry
tBrowse<- stack(tBrowse)

tAfter <- stack(list.files(path = paste0(datadir, "/Feed_quantity/"),pattern="DMPaftermean_2",full.names = T))
tAfter <- reclassify(tAfter, c(-Inf, 0, 0)) 
tAfter <- tAfter*grassFracDry*feedQuality_item$ME_min[feedQuality_item$codeSPAM == "natPast"]*dryDays
tAfter <- stack(tAfter)

rm(dryDays, croppingDays, feedCropBurn, cropME_HI_utilmin)

## Feed output
tsSum <- read.csv(paste0(resultsDir, "/disaggregated_timeseries.csv"))

cropME_min <- exact_extract(tCrop, zones, "sum")
tsSum$cropME_min <- as.numeric(c(cropME_min[1,], cropME_min[2,], cropME_min[3,], cropME_min[4,], cropME_min[5,]))

grassME_min <- exact_extract(tGrass, zones, "sum")
tsSum$grassME_min <- as.numeric(c(grassME_min[1,], grassME_min[2,], grassME_min[3,], grassME_min[4,], grassME_min[5,]))

browseME_min <- exact_extract(tBrowse, zones, "sum")
tsSum$browseME_min <- as.numeric(c(browseME_min[1,], browseME_min[2,], browseME_min[3,], browseME_min[4,], browseME_min[5,]))

afterME_min <- exact_extract(tAfter, zones, "sum")
tsSum$afterME_min <- as.numeric(c(afterME_min[1,], afterME_min[2,], afterME_min[3,], afterME_min[4,], afterME_min[5,]))

write.csv(tsSum, paste0(resultsDir, "/disaggregated_timeseries.csv"))

##Export total feed ME for adequacy estimates
tFeed <- tCrop + tGrass + tBrowse + tAfter
tFeed <- stack(tFeed)
writeRaster(tFeed$layer.1, paste0(spatialResultsDir, "/Feed_total_min_MJ2014.tif"), overwrite = T)
writeRaster(tFeed$layer.2, paste0(spatialResultsDir, "/Feed_total_min_MJ2015.tif"), overwrite = T)
writeRaster(tFeed$layer.3, paste0(spatialResultsDir, "/Feed_total_min_MJ2016.tif"), overwrite = T)
writeRaster(tFeed$layer.4, paste0(spatialResultsDir, "/Feed_total_min_MJ2017.tif"), overwrite = T)
writeRaster(tFeed$layer.5, paste0(spatialResultsDir, "/Feed_total_min_MJ2018.tif"), overwrite = T)
writeRaster(tFeed$layer.6, paste0(spatialResultsDir, "/Feed_total_min_MJ2019.tif"), overwrite = T)


##Calculate average ME
tsSum <- data.frame(region = c(rep("Sahel", 6), rep("Sudanian", 6)), year = c(2014:2019, 2014:2019))
#tsSum <- data.frame(region = c(rep("Highland (agro)pastoral", 6), rep("Highland mixed", 6), rep("Lowland (agro)pastoral", 6), rep("Lowland mixed", 6)), year = c(2014:2019, 2014:2019, 2014:2019, 2014:2019), lvstReq = NA, cropME_mean = NA, cropME_min = NA, cropME_max = NA, grassME_mean = NA, grassME_min = NA, grassME_max = NA, browseME_mean = NA, browseME_min = NA, browseME_max = NA, afterME_mean = NA, afterME_min = NA, afterME_max = NA, adeq_mean = NA, adeq_min = NA, adeq_max = NA)

cropME_min <- exact_extract(tCrop, regions, "sum")
tsSum$cropME_min <- as.numeric(c(cropME_min[1,], cropME_min[2,]))

grassME_min <- exact_extract(tGrass, regions, "sum")
tsSum$grassME_min <- as.numeric(c(grassME_min[1,], grassME_min[2,]))

browseME_min <- exact_extract(tBrowse, regions, "sum")
tsSum$browseME_min <- as.numeric(c(browseME_min[1,], browseME_min[2,]))

afterME_min <- exact_extract(tAfter, regions, "sum")
tsSum$afterME_min <- as.numeric(c(afterME_min[1,], afterME_min[2,]))

##Calculate ME of grass and browse by season
grassMEwet_min <- exact_extract(tGrassWet, regions, "sum")
tsSum$grassMEwet_min <- as.numeric(c(grassMEwet_min[1,], grassMEwet_min[2,]))

grassMEdry_min <- exact_extract(tGrassDry, regions, "sum")
tsSum$grassMEdry_min <- as.numeric(c(grassMEdry_min[1,], grassMEdry_min[2,]))

browseMEwet_min <- exact_extract(tBrowseWet, regions, "sum")
tsSum$browseMEwet_min <- as.numeric(c(browseMEwet_min[1,], browseMEwet_min[2,]))

browseMEdry_min <- exact_extract(tBrowseDry, regions, "sum")
tsSum$browseMEdry_min <- as.numeric(c(browseMEdry_min[1,], browseMEdry_min[2,]))

#Add DM

cropDM <- exact_extract(tCrop/cropMEmin, regions, "sum")
tsSum$cropDM <- as.numeric(c(cropDM[1,], cropDM[2,]))

grassDM <- exact_extract(tGrass/grassME, regions, "sum")
tsSum$grassDM <- as.numeric(c(grassDM[1,], grassDM[2,]))

grassDMwet <- exact_extract(tGrassWet/grassME, regions, "sum")
tsSum$grassDMwet <- as.numeric(c(grassDMwet[1,], grassDMwet[2,]))

grassDMdry <- exact_extract(tGrassDry/grassME, regions, "sum")
tsSum$grassDMdry <- as.numeric(c(grassDMdry[1,], grassDMdry[2,]))

browseDM_min <- exact_extract(tBrowse/browseME, regions, "sum")
tsSum$browseDM <- as.numeric(c(browseDM_min[1,], browseDM_min[2,]))

browseDMwet_min <- exact_extract(tBrowseWet/browseME, regions, "sum")
tsSum$browseDMwet <- as.numeric(c(browseDMwet_min[1,], browseDMwet_min[2,]))

browseDMdry_min <- exact_extract(tBrowseDry/browseME, regions, "sum")
tsSum$browseDMdry <- as.numeric(c(browseDMdry_min[1,], browseDMdry_min[2,]))

tsSum$afterDM <- tsSum$afterME_min / feedQuality_item$ME_min[feedQuality_item$codeSPAM == "natPast"]

outMEmean <- read.csv(paste0(resultsDir, "/cropME_region.csv"))

outMEmin <- tsSum[tsSum$year == 2019,] %>% rowwise() %>% mutate(ME_all_min = sum(cropME_min, grassME_min, browseME_min, afterME_min) / sum(cropDM, grassDM, browseDM, afterDM), ME_crop_min = cropME_min / cropDM, MEwet_all_min = sum(grassMEwet_min, browseMEwet_min) / sum(grassDMwet, browseDMwet), MEdry_all_min = sum(cropME_min, grassMEdry_min, browseMEdry_min) / sum(cropDM, grassDMdry, browseDMdry, afterDM)) 
#outMEmin <- tsSum[tsSum$year == 2019,] %>% rowwise() %>% mutate(ME_all_min = sum(cropME_min, grassME_min, browseME_min, afterME_min) / sum(cropDM, grassDM, browseDM, afterDM), ME_crop_min = cropME_min / cropDM) 
outMEmean <- bind_cols(outMEmean, select(outMEmin, ME_all_min, ME_crop_min, MEwet_all_min, MEdry_all_min))
write.csv(outMEmean, paste0(resultsDir, "/cropME_region.csv"))


#######
##Duplicate for maximum
zones <- st_read(paste0(intdatadir, "/zones.gpkg"))
regions <- st_read(paste0(intdatadir, "/regions.gpkg"))

cropME_HI_utilmax <- raster(paste0(intdatadir, "/cropME_HI_utilmax.tif"))
cropMEmax <- raster(paste0(intdatadir, "/cropMEmax.tif"))
feedQuality_item <- read.csv(paste0(cropParams_dir, "/feedQuality_item.csv"))
feedCropBurn <- raster(paste0(datadir, "/Burned/burnCropsDekads.tif"))

#croppingDays <- sum(raster('CropParams/phenoCroppingDays1.tif'), raster('CropParams/phenoCroppingDays2.tif'), na.rm = T)
croppingDays <- raster(paste0(cropParams_dir, "/croppingDays.tif"))
croppingDays <- reclassify(croppingDays, c(-Inf, 0, 0)) 
croppingDays <- raster::resample(croppingDays, feedCropBurn, method = "ngb")
croppingDays <- reclassify(croppingDays, c(220, Inf, 220)) 
dryDays <- 365 - croppingDays

region <- raster(paste0(intdatadir, "/regions.tif"))
grassMESud <- 6.8 #min = 6.2, max = 6.8 #!change max/min 
grassMESah <- 6.4 #min = 5.8, max = 6.4 #!change max/min
grassME <- calc(region, fun = function(x){ifelse(x == 4, grassMESah, grassMESud)})
grassME <- raster::resample(grassME, feedCropBurn, method = "ngb")

browseMESud <- 8.2
browseMESah <- 6.2
browseME <- calc(region, fun = function(x){ifelse(x == 4, browseMESah, browseMESud)})
browseME <- raster::resample(browseME, feedCropBurn, method = "ngb")
#browseMEmean <- 5

grassFracDry <- 0.33 #max 0.55
grassFracWet <- 0.55 #max 0.55
browseFrac <- 0.38

tCrop <- stack(list.files(path = paste0(datadir, "/Feed_quantity/"),pattern="cropmean_2",full.names = T))
tCrop <- reclassify(tCrop, c(-Inf, 0, 0)) 
tCrop <- stack(tCrop)
croppingDays <-reclassify(croppingDays, c(0, 60, 60))
tCrop <- tCrop*croppingDays*cropME_HI_utilmax
for(i in 1:length(names(tCrop))){
  tCrop[[i]] <- overlay(tCrop[[i]], feedCropBurn, fun = function(DMP, burn){ifelse(burn > 0, 0, DMP) }) #, filename = 'SpatialData/outputs/Feed_crop_burn_MJ.tif', overwrite = T
}

tGrassWet <- stack(list.files(path = paste0(datadir, "/Feed_quantity/"),pattern="DMPgrassWetmean_2",full.names = T))
tGrassWet <- reclassify(tGrassWet, c(-Inf, 0, 0)) #Some negative DM in copernicus product
tGrassWet <- tGrassWet*grassFracWet*croppingDays * grassME 
tGrassWet <- stack(tGrassWet)

tGrassDry <- stack(list.files(path = paste0(datadir, "/Feed_quantity/"),pattern="DMPgrassDrymean_2",full.names = T))
tGrassDry <- reclassify(tGrassDry, c(-Inf, 0, 0)) 
tGrassDry <- tGrassDry*grassFracDry*dryDays * grassME
tGrassDry <- stack(tGrassDry)

tGrass <- tGrassWet + tGrassDry
tGrass <- stack(tGrass)
#rm(tGrassDry, tGrassWet)

tBrowse <- stack(list.files(path = paste0(datadir, "/Feed_quantity/"),pattern="DMPbrowsemean_2",full.names = T))
tBrowse <- reclassify(tBrowse, c(-Inf, 0, 0)) 
tBrowseWet <- tBrowse*browseME*croppingDays * browseFrac
tBrowseWet <- stack(tBrowseWet)
tBrowseDry <- tBrowse*browseME*dryDays * browseFrac
tBrowseDry <- stack(tBrowseDry)

tBrowse <- tBrowseWet + tBrowseDry
tBrowse <- stack(tBrowse)

tAfter <- stack(list.files(path = paste0(datadir, "/Feed_quantity/"),pattern="DMPaftermean_2",full.names = T))
tAfter <- reclassify(tAfter, c(-Inf, 0, 0)) 
tAfter <- tAfter*grassFracDry*feedQuality_item$ME_min[feedQuality_item$codeSPAM == "natPast"]*dryDays
tAfter <- stack(tAfter)

gc()
rm(dryDays, croppingDays, feedCropBurn, cropME_HI_utilmax)

## Feed output
tsSum <- read.csv(paste0(resultsDir, "/disaggregated_timeseries.csv"))

cropME_max <- exact_extract(tCrop, zones, "sum")
tsSum$cropME_max <- as.numeric(c(cropME_max[1,], cropME_max[2,], cropME_max[3,], cropME_max[4,], cropME_max[5,]))

grassME_max <- exact_extract(tGrass, zones, "sum")
tsSum$grassME_max <- as.numeric(c(grassME_max[1,], grassME_max[2,], grassME_max[3,], grassME_max[4,], grassME_max[5,]))

browseME_max <- exact_extract(tBrowse, zones, "sum")
tsSum$browseME_max <- as.numeric(c(browseME_max[1,], browseME_max[2,], browseME_max[3,], browseME_max[4,], browseME_max[5,]))

afterME_max <- exact_extract(tAfter, zones, "sum")
tsSum$afterME_max <- as.numeric(c(afterME_max[1,], afterME_max[2,], afterME_max[3,], afterME_max[4,], afterME_max[5,]))

write.csv(tsSum, paste0(resultsDir, "/disaggregated_timeseries.csv"))

##Export total feed ME for adequacy estimates
tFeed <- tCrop + tGrass + tBrowse + tAfter
tFeed <- stack(tFeed)
gc()
writeRaster(tFeed$layer.1, paste0(spatialResultsDir, "/Feed_total_max_MJ2014.tif"), overwrite = T)
writeRaster(tFeed$layer.2, paste0(spatialResultsDir, "/Feed_total_max_MJ2015.tif"), overwrite = T)
writeRaster(tFeed$layer.3, paste0(spatialResultsDir, "/Feed_total_max_MJ2016.tif"), overwrite = T)
writeRaster(tFeed$layer.4, paste0(spatialResultsDir, "/Feed_total_max_MJ2017.tif"), overwrite = T)
writeRaster(tFeed$layer.5, paste0(spatialResultsDir, "/Feed_total_max_MJ2018.tif"), overwrite = T)
writeRaster(tFeed$layer.6, paste0(spatialResultsDir, "/Feed_total_max_MJ2019.tif"), overwrite = T)

##Calculate average ME
tsSum <- data.frame(region = c(rep("Sahel", 6), rep("Sudanian", 6)), year = c(2014:2019, 2014:2019))
#tsSum <- data.frame(region = c(rep("Highland (agro)pastoral", 6), rep("Highland mixed", 6), rep("Lowland (agro)pastoral", 6), rep("Lowland mixed", 6)), year = c(2014:2019, 2014:2019, 2014:2019, 2014:2019), lvstReq = NA, cropME_mean = NA, cropME_max = NA, cropME_max = NA, grassME_mean = NA, grassME_max = NA, grassME_max = NA, browseME_mean = NA, browseME_max = NA, browseME_max = NA, afterME_mean = NA, afterME_max = NA, afterME_max = NA, adeq_mean = NA, adeq_max = NA, adeq_max = NA)

cropME_max <- exact_extract(tCrop, regions, "sum")
tsSum$cropME_max <- as.numeric(c(cropME_max[1,], cropME_max[2,]))

grassME_max <- exact_extract(tGrass, regions, "sum")
tsSum$grassME_max <- as.numeric(c(grassME_max[1,], grassME_max[2,]))

browseME_max <- exact_extract(tBrowse, regions, "sum")
tsSum$browseME_max <- as.numeric(c(browseME_max[1,], browseME_max[2,]))

afterME_max <- exact_extract(tAfter, regions, "sum")
tsSum$afterME_max <- as.numeric(c(afterME_max[1,], afterME_max[2,]))

##Calculate ME of grass and browse by season
grassMEwet_max <- exact_extract(tGrassWet, regions, "sum")
tsSum$grassMEwet_max <- as.numeric(c(grassMEwet_max[1,], grassMEwet_max[2,]))

grassMEdry_max <- exact_extract(tGrassDry, regions, "sum")
tsSum$grassMEdry_max <- as.numeric(c(grassMEdry_max[1,], grassMEdry_max[2,]))

browseMEwet_max <- exact_extract(tBrowseWet, regions, "sum")
tsSum$browseMEwet_max <- as.numeric(c(browseMEwet_max[1,], browseMEwet_max[2,]))

browseMEdry_max <- exact_extract(tBrowseDry, regions, "sum")
tsSum$browseMEdry_max <- as.numeric(c(browseMEdry_max[1,], browseMEdry_max[2,]))

#Add DM
cropDM <- exact_extract(tCrop/cropMEmax, regions, "sum")
tsSum$cropDM <- as.numeric(c(cropDM[1,], cropDM[2,]))

grassDM <- exact_extract(tGrass/grassME, regions, "sum")
tsSum$grassDM <- as.numeric(c(grassDM[1,], grassDM[2,]))

grassDMwet <- exact_extract(tGrassWet/grassME, regions, "sum")
tsSum$grassDMwet <- as.numeric(c(grassDMwet[1,], grassDMwet[2,]))

grassDMdry <- exact_extract(tGrassDry/grassME, regions, "sum")
tsSum$grassDMdry <- as.numeric(c(grassDMdry[1,], grassDMdry[2,]))

browseDM_max <- exact_extract(tBrowse/browseME, regions, "sum")
tsSum$browseDM <- as.numeric(c(browseDM_max[1,], browseDM_max[2,]))

browseDMwet_max <- exact_extract(tBrowseWet/browseME, regions, "sum")
tsSum$browseDMwet <- as.numeric(c(browseDMwet_max[1,], browseDMwet_max[2,]))

browseDMdry_max <- exact_extract(tBrowseDry/browseME, regions, "sum")
tsSum$browseDMdry <- as.numeric(c(browseDMdry_max[1,], browseDMdry_max[2,]))

tsSum$afterDM <- tsSum$afterME_max / feedQuality_item$ME_min[feedQuality_item$codeSPAM == "natPast"]

outMEmean <- read.csv(paste0(resultsDir, "/cropME_region.csv"))

outMEmax <- tsSum[tsSum$year == 2019,] %>% rowwise() %>% mutate(ME_all_max = sum(cropME_max, grassME_max, browseME_max, afterME_max) / sum(cropDM, grassDM, browseDM, afterDM), ME_crop_max = cropME_max / cropDM, MEwet_all_max = sum(grassMEwet_max, browseMEwet_max) / sum(grassDMwet, browseDMwet), MEdry_all_max = sum(cropME_max, grassMEdry_max, browseMEdry_max) / sum(cropDM, grassDMdry, browseDMdry, afterDM)) 
#outMEmax <- tsSum[tsSum$year == 2019,] %>% rowwise() %>% mutate(ME_all_max = sum(cropME_max, grassME_max, browseME_max, afterME_max) / sum(cropDM, grassDM, browseDM, afterDM), ME_crop_max = cropME_max / cropDM) 
outMEmean <- bind_cols(outMEmean, select(outMEmax, ME_all_max, ME_crop_max, MEwet_all_max, MEdry_all_max))

outMEmean <- outMEmean %>% rowwise() %>% mutate(geomMean = sqrt(ME_crop_min*ME_crop_max))
outMEmean <- outMEmean %>% rowwise() %>% mutate(ME_crop_sd = (ME_crop/geomMean)*(sqrt((ME_crop^2)-(geomMean^2))))
write.csv(outMEmean, paste0(resultsDir, "/cropME_region.csv"))
