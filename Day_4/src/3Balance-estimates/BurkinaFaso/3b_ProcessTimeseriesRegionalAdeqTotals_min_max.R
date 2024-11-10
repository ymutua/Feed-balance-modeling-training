# Process timeseries - minimum and maximum

#.libPaths(c(.libPaths()[2], .libPaths()[3]))

# install.packages("terra")
# install.packages("sf")
# install.packages("dplyr")
# install.packages("tidyr")
# install.packages("exactextractr")

library(terra)
#library(stars)
library(sf)
library(dplyr)
library(tidyr)
library(exactextractr)

terraOptions(tempdir = "/Users/s2255815/Downloads/AU_Temp") # Process needs > 40GB of temporary disk space
terraOptions(memfrac=0.5)
terraOptions(todisk=TRUE)

# study area
region <- "BurkinaFaso"

# Paths, directories
root <- "/Users/s2255815/Library/CloudStorage/OneDrive-UniversityofEdinburgh/Work/Projects/AU-IBAR/Spatial modelling workshop/Workshop_materials/Day_4"
datadir <- paste0(root, "/src/3Balance-estimates/", region, "/SpatialData/inputs")
intdatadir <- paste0(root, "/src/3Balance-estimates/", region, "/SpatialData/intermediate")
livestockParams_dir <- paste0(root, "/src/3Balance-estimates/", region, "/LivestockParams")
cropParams_dir <- paste0(root, "/src/3Balance-estimates/", region, "/CropParams")
resultsDir <- paste0(root, "/src/3Balance-estimates/", region, "/Results")
spatialResultsDir <- paste0(root, "/src/3Balance-estimates/", region, "/SpatialData/outputs")

##Minimum adequacy
zones <- st_read(paste0(intdatadir, "/zones.gpkg"))

###Livestock requirements
cattleIntake_model_MJ_2015 <- rast(paste0(spatialResultsDir, "/cattleMER_MJ_2015_max.tif")) #Max feed requirement will give min adequacy
shoatsIntake_model_MJ_2015 <- rast(paste0(spatialResultsDir, "/shoatsMER_MJ_2015_max.tif")) #Max feed requirement will give min adequacy
horseDonkeyIntake_model_MJ_2015 <- rast(paste0(spatialResultsDir, "/horseDonkeyMER_MJ_2015.tif"))

FAOlvstPop <- read.csv(paste0(livestockParams_dir, "/FAOSTAT_livestock_data.csv"))

lv2014 <- sum(horseDonkeyIntake_model_MJ_2015, (cattleIntake_model_MJ_2015 + (cattleIntake_model_MJ_2015*FAOlvstPop$PcChange[FAOlvstPop$Year == 2014 & FAOlvstPop$Item == "Cattle"])), (shoatsIntake_model_MJ_2015 + (shoatsIntake_model_MJ_2015*FAOlvstPop$PcChange[FAOlvstPop$Year == 2014 & FAOlvstPop$Item == "Shoats"])), na.rm = T)
lv2015 <- sum(horseDonkeyIntake_model_MJ_2015, cattleIntake_model_MJ_2015, shoatsIntake_model_MJ_2015, na.rm = T)
lv2016 <- sum(horseDonkeyIntake_model_MJ_2015, cattleIntake_model_MJ_2015 + (cattleIntake_model_MJ_2015*FAOlvstPop$PcChange[FAOlvstPop$Year == 2016 & FAOlvstPop$Item == "Cattle"]), shoatsIntake_model_MJ_2015 + (shoatsIntake_model_MJ_2015*FAOlvstPop$PcChange[FAOlvstPop$Year == 2016 & FAOlvstPop$Item == "Shoats"]), na.rm = T)
lv2017 <- sum(horseDonkeyIntake_model_MJ_2015, cattleIntake_model_MJ_2015 + (cattleIntake_model_MJ_2015*FAOlvstPop$PcChange[FAOlvstPop$Year == 2017 & FAOlvstPop$Item == "Cattle"]), shoatsIntake_model_MJ_2015 + (shoatsIntake_model_MJ_2015*FAOlvstPop$PcChange[FAOlvstPop$Year == 2017 & FAOlvstPop$Item == "Shoats"]), na.rm = T)
lv2018 <- sum(horseDonkeyIntake_model_MJ_2015, cattleIntake_model_MJ_2015 + (cattleIntake_model_MJ_2015*FAOlvstPop$PcChange[FAOlvstPop$Year == 2018 & FAOlvstPop$Item == "Cattle"]), shoatsIntake_model_MJ_2015 + (shoatsIntake_model_MJ_2015*FAOlvstPop$PcChange[FAOlvstPop$Year == 2018 & FAOlvstPop$Item == "Shoats"]), na.rm = T)
lv2019 <- sum(horseDonkeyIntake_model_MJ_2015, cattleIntake_model_MJ_2015 + (cattleIntake_model_MJ_2015*FAOlvstPop$PcChange[FAOlvstPop$Year == 2019 & FAOlvstPop$Item == "Cattle"]), shoatsIntake_model_MJ_2015 + (shoatsIntake_model_MJ_2015*FAOlvstPop$PcChange[FAOlvstPop$Year == 2019 & FAOlvstPop$Item == "Shoats"]), na.rm = T)

tLv <- c(lv2014, lv2015, lv2016, lv2017, lv2018, lv2019)
names(tLv) <- c("lv2014", "lv2015", "lv2016", "lv2017", "lv2018", "lv2019")
rm(lv2014, lv2015, lv2016, lv2017, lv2018, lv2019, shoatsIntake_model_MJ_2015, cattleIntake_model_MJ_2015)

## Adequacy outputs

#totalDM_2019 <- (tCrop$Feed_crop_burn_MJ.6*croppingDays) + (tGrassWet$layer.6*croppingDays) + (tGrassDry$layer.6*dryDays) + (tBrowse$DMPbrowsemean_2019 * 365)
tFeed <- rast(list.files(path = paste0(spatialResultsDir),pattern="Feed_total_min_MJ",full.names = T))

zones <- bind_cols(dplyr::select(zones, grouping), exact_extract(tLv, zones, fun = "sum"))
zones <- bind_cols(zones, exact_extract(tFeed, zones, fun = "sum"))

st_geometry(zones) <- NULL

tsSum <- data.frame(zones)

colnames(tsSum) <- c("NAME_1", "lvstReqMEmax_2014", "lvstReqMEmax_2015", "lvstReqMEmax_2016", "lvstReqMEmax_2017", "lvstReqMEmax_2018", "lvstReqMEmax_2019", "feedME_min_2014", "feedME_min_2015", "feedME_min_2016", "feedME_min_2017", "feedME_min_2018", "feedME_min_2019")

tsSum$adeqMin_2014 <- tsSum$feedME_min_2014 / tsSum$lvstReqMEmax_2014
tsSum$adeqMin_2015 <- tsSum$feedME_min_2015 / tsSum$lvstReqMEmax_2015
tsSum$adeqMin_2016 <- tsSum$feedME_min_2016 / tsSum$lvstReqMEmax_2016
tsSum$adeqMin_2017 <- tsSum$feedME_min_2017 / tsSum$lvstReqMEmax_2017
tsSum$adeqMin_2018 <- tsSum$feedME_min_2018 / tsSum$lvstReqMEmax_2018
tsSum$adeqMin_2019 <- tsSum$feedME_min_2019 / tsSum$lvstReqMEmax_2019

tsSum_adeq <- read.csv(paste0(resultsDir, "/totals_timeseries_region.csv"))
tsSum_adeq$adeqMin_2014 <- tsSum$adeqMin_2014
tsSum_adeq$adeqMin_2015 <- tsSum$adeqMin_2015
tsSum_adeq$adeqMin_2016 <- tsSum$adeqMin_2016
tsSum_adeq$adeqMin_2017 <- tsSum$adeqMin_2017
tsSum_adeq$adeqMin_2018 <- tsSum$adeqMin_2018
tsSum_adeq$adeqMin_2019 <- tsSum$adeqMin_2019
#tsSum_adeq <- bind_cols(tsSum_adeq, select(tsSum, adeqMin_2014, adeqMin_2015, adeqMin_2016, adeqMin_2017, adeqMin_2018, adeqMin_2019))
write.csv(tsSum_adeq, paste0(resultsDir, "/totals_timeseries_region.csv"))


###Calculate admin zone level 1 totals for comparison
aoi1 <-  st_read(paste0(datadir, "/gadm40_BFA_1.shp"))
out <- read.csv(paste0(resultsDir, "/totals_compare_2019.csv"), stringsAsFactors = F)

aoi1 <- bind_cols(dplyr::select(aoi1, NAME_1), lvst = exact_extract(tLv$lv2019, aoi1, fun = "sum"))
aoi1 <- bind_cols(aoi1, feed = exact_extract(tFeed$Feed_total_min_MJ2019, aoi1, fun = "sum"))

out$FeedAdeq_ME_common_min <- aoi1$feed / aoi1$lvst

write.csv(out, paste0(resultsDir, "/totals_compare_2019.csv"))


######## 
#Duplicate for max adequacy
zones <- st_read(paste0(intdatadir, "/zones.gpkg"))


###Livestock requirements
cattleIntake_model_MJ_2015 <- rast(paste0(spatialResultsDir, "/cattleMER_MJ_2015_min.tif"))#Max feed requirement will give max adequacy
shoatsIntake_model_MJ_2015 <- rast(paste0(spatialResultsDir, "/shoatsMER_MJ_2015_min.tif"))#Max feed requirement will give max adequacy
horseDonkeyIntake_model_MJ_2015 <- rast(paste0(spatialResultsDir, "/horseDonkeyMER_MJ_2015.tif"))

FAOlvstPop <- read.csv(paste0(livestockParams_dir, "/FAOSTAT_livestock_data.csv"))

lv2014 <- sum(horseDonkeyIntake_model_MJ_2015, (cattleIntake_model_MJ_2015 + (cattleIntake_model_MJ_2015*FAOlvstPop$PcChange[FAOlvstPop$Year == 2014 & FAOlvstPop$Item == "Cattle"])), (shoatsIntake_model_MJ_2015 + (shoatsIntake_model_MJ_2015*FAOlvstPop$PcChange[FAOlvstPop$Year == 2014 & FAOlvstPop$Item == "Shoats"])), na.rm = T)
lv2015 <- sum(horseDonkeyIntake_model_MJ_2015, cattleIntake_model_MJ_2015, shoatsIntake_model_MJ_2015, na.rm = T)
lv2016 <- sum(horseDonkeyIntake_model_MJ_2015, cattleIntake_model_MJ_2015 + (cattleIntake_model_MJ_2015*FAOlvstPop$PcChange[FAOlvstPop$Year == 2016 & FAOlvstPop$Item == "Cattle"]), shoatsIntake_model_MJ_2015 + (shoatsIntake_model_MJ_2015*FAOlvstPop$PcChange[FAOlvstPop$Year == 2016 & FAOlvstPop$Item == "Shoats"]), na.rm = T)
lv2017 <- sum(horseDonkeyIntake_model_MJ_2015, cattleIntake_model_MJ_2015 + (cattleIntake_model_MJ_2015*FAOlvstPop$PcChange[FAOlvstPop$Year == 2017 & FAOlvstPop$Item == "Cattle"]), shoatsIntake_model_MJ_2015 + (shoatsIntake_model_MJ_2015*FAOlvstPop$PcChange[FAOlvstPop$Year == 2017 & FAOlvstPop$Item == "Shoats"]), na.rm = T)
lv2018 <- sum(horseDonkeyIntake_model_MJ_2015, cattleIntake_model_MJ_2015 + (cattleIntake_model_MJ_2015*FAOlvstPop$PcChange[FAOlvstPop$Year == 2018 & FAOlvstPop$Item == "Cattle"]), shoatsIntake_model_MJ_2015 + (shoatsIntake_model_MJ_2015*FAOlvstPop$PcChange[FAOlvstPop$Year == 2018 & FAOlvstPop$Item == "Shoats"]), na.rm = T)
lv2019 <- sum(horseDonkeyIntake_model_MJ_2015, cattleIntake_model_MJ_2015 + (cattleIntake_model_MJ_2015*FAOlvstPop$PcChange[FAOlvstPop$Year == 2019 & FAOlvstPop$Item == "Cattle"]), shoatsIntake_model_MJ_2015 + (shoatsIntake_model_MJ_2015*FAOlvstPop$PcChange[FAOlvstPop$Year == 2019 & FAOlvstPop$Item == "Shoats"]), na.rm = T)

tLv <- c(lv2014, lv2015, lv2016, lv2017, lv2018, lv2019)
names(tLv) <- c("lv2014", "lv2015", "lv2016", "lv2017", "lv2018", "lv2019")
rm(lv2014, lv2015, lv2016, lv2017, lv2018, lv2019, shoatsIntake_model_MJ_2015, cattleIntake_model_MJ_2015)

## Adequacy outputs

#totalDM_2019 <- (tCrop$Feed_crop_burn_MJ.6*croppingDays) + (tGrassWet$layer.6*croppingDays) + (tGrassDry$layer.6*dryDays) + (tBrowse$DMPbrowsemean_2019 * 365)
tFeed <- rast(list.files(path = paste0(spatialResultsDir),pattern="Feed_total_max_MJ",full.names = T))

zones <- bind_cols(dplyr::select(zones, grouping), exact_extract(tLv, zones, fun = "sum"))
zones <- bind_cols(zones, exact_extract(tFeed, zones, fun = "sum"))

st_geometry(zones) <- NULL

tsSum <- data.frame(zones)

colnames(tsSum) <- c("NAME_1", "lvstReqMEmin_2014", "lvstReqMEmin_2015", "lvstReqMEmin_2016", "lvstReqMEmin_2017", "lvstReqMEmin_2018", "lvstReqMEmin_2019", "feedME_max_2014", "feedME_max_2015", "feedME_max_2016", "feedME_max_2017", "feedME_max_2018", "feedME_max_2019")

tsSum$adeqMax_2014 <- tsSum$feedME_max_2014 / tsSum$lvstReqMEmin_2014
tsSum$adeqMax_2015 <- tsSum$feedME_max_2015 / tsSum$lvstReqMEmin_2015
tsSum$adeqMax_2016 <- tsSum$feedME_max_2016 / tsSum$lvstReqMEmin_2016
tsSum$adeqMax_2017 <- tsSum$feedME_max_2017 / tsSum$lvstReqMEmin_2017
tsSum$adeqMax_2018 <- tsSum$feedME_max_2018 / tsSum$lvstReqMEmin_2018
tsSum$adeqMax_2019 <- tsSum$feedME_max_2019 / tsSum$lvstReqMEmin_2019

tsSum_adeq <- read.csv(paste0(resultsDir, "/totals_timeseries_region.csv"))
tsSum_adeq$adeqMax_2014 <- tsSum$adeqMax_2014
tsSum_adeq$adeqMax_2015 <- tsSum$adeqMax_2015
tsSum_adeq$adeqMax_2016 <- tsSum$adeqMax_2016
tsSum_adeq$adeqMax_2017 <- tsSum$adeqMax_2017
tsSum_adeq$adeqMax_2018 <- tsSum$adeqMax_2018
tsSum_adeq$adeqMax_2019 <- tsSum$adeqMax_2019
#tsSum_adeq <- bind_cols(tsSum_adeq, select(tsSum, adeqMax_2014, adeqMax_2015, adeqMax_2016, adeqMax_2017, adeqMax_2018, adeqMax_2019))
write.csv(tsSum_adeq, paste0(resultsDir, "/totals_timeseries_region.csv"))


###Calculate admin zone level 1 totals for comparison
aoi1 <-  st_read(paste0(datadir, "/gadm40_BFA_1.shp"))
out <- read.csv(paste0(resultsDir, "/totals_compare_2019.csv"), stringsAsFactors = F)

aoi1 <- bind_cols(dplyr::select(aoi1, NAME_1), lvst = exact_extract(tLv$lv2019, aoi1, fun = "sum"))
aoi1 <- bind_cols(aoi1, feed = exact_extract(tFeed$Feed_total_max_MJ2019, aoi1, fun = "sum"))

out$FeedAdeq_ME_common_max <- aoi1$feed / aoi1$lvst

write.csv(out, paste0(resultsDir, "/totals_compare_2019.csv"))
