# Prepare livestock requirements

# install.packages("raster")
# install.packages("terra")
# install.packages("sf")
# install.packages("dplyr")
# install.packages("tidyr")

# .libPaths(c(.libPaths()[2], .libPaths()[3]))
library(raster)
library(terra)
library(sf)
library(dplyr)
library(tidyr)
#library(exactextractr) #For zonal statistics

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
resultsDir <- paste0(root, "/src/3Balance-estimates/", region, "/Results"); dir.create(resultsDir, F, T)
spatialResultsDir <- paste0(root, "/src/3Balance-estimates/", region, "/SpatialData/outputs"); dir.create(spatialResultsDir, F, T)

param_ME <- read.csv(paste0(livestockParams_dir, "/Livestock_energy_requirement.csv"), stringsAsFactors = F)
#param_ME <- pivot_longer(select(param_ME, -X), cols = c("Bull", "Steer", "Calf", "Heifer", "Cow", "Lamb", "Sheep", "Kid", "Goat"))
param_ME <- pivot_longer(param_ME, cols = c("Bull", "Steer", "Calf", "Heifer", "Cow", "Lamb", "Sheep", "Kid", "Goat"))

##CSIRO, 2007, Nutrient Requirements of Domesticated Ruminants 
#https://vdocuments.net/nutrient-requirements-of-domesticated-ruminants.html?page=1

###Spatial data
#Add level 3 admin boundaries
aoi1 <-  st_read(paste0(datadir, "/gadm40_BFA_1.shp"))

livelihood <- rast(paste0(intdatadir, "/livelihoodZones.tif"))
lvCattle <- rast(paste0(datadir, "/GLW4/Ct_2015_10k.tif"))
lvSheep <- rast(paste0(datadir, "/GLW4/Sh_2015_10k.tif"))
lvGoat <- rast(paste0(datadir, "/GLW4/Gt_2015_10k.tif"))
lvHorse <- rast(paste0(datadir, "/GLW4/Ho_2015_10k.tif"))
lvCattle <- terra::crop(lvCattle, aoi1, mask = T)
lvSheep <- terra::crop(lvSheep, aoi1, mask = T)
lvGoat <- terra::crop(lvGoat, aoi1, mask = T)
lvHorse <- terra::crop(lvHorse, aoi1, mask = T)
lvDonkey <- lvHorse*3

lv <- c(lvCattle, lvSheep, lvGoat) #Number per 10km pixel

livelihood <- terra::resample(livelihood, lv, method = 'near')
lv <- terra::crop(lv, livelihood)
lv <- c(lv, livelihood)

##Add production system layer
periurban <- rast(paste0(datadir, "/Lvst_system/w001001.adf")) #1-4 = rangelands; 5-8 = mixed systems; 9-14 = irrigated
periurban <- terra::resample(periurban, lv, method = 'near')
periurban <- terra::crop(periurban, livelihood, mask = T)
periurban <- periurban == 13
lv <- c(lv, periurban)

lvTLU <- lapp(lv[[1:3]], fun = function(cattle, sheep, goat){(cattle*1)+(sheep*0.15)+(goat*0.15)}, filename = paste0(datadir, "/GLW4/TLU.tif"), overwrite = T)

#Add season data
wetSSN <- rast(paste0(cropParams_dir, "/croppingDays.tif"))
wetSSN <- terra::crop(wetSSN, aoi1, mask = T)
wetSSN <- terra::resample(wetSSN, lv, method = 'near')
wetSSN[is.na(wetSSN)] <- global(wetSSN, 'mean', na.rm = T) #Fill NAs and 0s with global mean
wetSSN[wetSSN ==0] <- global(wetSSN, 'mean', na.rm = T)
drySSN <- 365 - wetSSN
lv <- c(lv, wetSSN)
lv <- c(lv, drySSN)

lv <- terra::crop(lv, aoi1)

###Admin level approach - comparing average daily requirements #! not used
#outIntake_TLU_DM <- lapp(lvTLU, fun = function(TLU){((TLU)*(250*0.03))}, filename = 'SpatialData/inputs/tmp/outIntake.tif', overwrite = T) #, filename = 'D:/outIntake.tif', overwrite = T

ECM <- 3.054 #energy content of milk MJ/kg (CSIRO,2007)
EC <- 20 #energy content of the tissue=MJ/kg


##Intermediate MER calculations
#################
#Sahelian zone
#MERm Sah cattle 
MERm_WS_Sah_bull <- (param_ME$value[param_ME$Variable=="K" & param_ME$name == "Bull"]*
                       param_ME$value[param_ME$Variable == "S" & param_ME$name == "Bull"]*
                       param_ME$value[param_ME$Variable == "M" & param_ME$name == "Bull"]*
                       ((0.26*(param_ME$value[param_ME$Variable =="MLW" & param_ME$name == "Bull" & param_ME$Season == "WS" & param_ME$Region == "Sah"]^0.75)*
                           exp(-0.008*param_ME$value[param_ME$Variable == "Age" & param_ME$name == "Bull"]))/
                          ((0.02*param_ME$value[param_ME$Variable =="M.D" & param_ME$name == "Bull" & param_ME$Season == "WS" & param_ME$Region == "Sah"])+0.5)))

MERm_WS_Sah_steer <- (param_ME$value[param_ME$Variable=="K" & param_ME$name == "Steer"]*
                        param_ME$value[param_ME$Variable == "S" & param_ME$name == "Steer"]*
                        param_ME$value[param_ME$Variable == "M" & param_ME$name == "Steer"]*
                        ((0.26*(param_ME$value[param_ME$Variable =="MLW" & param_ME$name == "Steer" & param_ME$Season == "WS" & param_ME$Region == "Sah"]^0.75)*
                            exp(-0.008*param_ME$value[param_ME$Variable == "Age" & param_ME$name == "Steer"]))/
                           ((0.02*param_ME$value[param_ME$Variable =="M.D" & param_ME$name == "Steer" & param_ME$Season == "WS" & param_ME$Region == "Sah"])+0.5)))

MERm_WS_Sah_calf <- (param_ME$value[param_ME$Variable=="K" & param_ME$name == "Calf"]*
                       param_ME$value[param_ME$Variable == "S" & param_ME$name == "Calf"]*
                       param_ME$value[param_ME$Variable == "M" & param_ME$name == "Calf"]*
                       ((0.26*(param_ME$value[param_ME$Variable =="MLW" & param_ME$name == "Calf" & param_ME$Season == "WS" & param_ME$Region == "Sah"]^0.75)*
                           exp(-0.008*param_ME$value[param_ME$Variable == "Age" & param_ME$name == "Calf"]))/
                          ((0.02*param_ME$value[param_ME$Variable =="M.D" & param_ME$name == "Calf" & param_ME$Season == "WS" & param_ME$Region == "Sah"])+0.5)))

MERm_WS_Sah_heifer <- (param_ME$value[param_ME$Variable=="K" & param_ME$name == "Heifer"]*
                         param_ME$value[param_ME$Variable == "S" & param_ME$name == "Heifer"]*
                         param_ME$value[param_ME$Variable == "M" & param_ME$name == "Heifer"]*
                         ((0.26*(param_ME$value[param_ME$Variable =="MLW" & param_ME$name == "Heifer" & param_ME$Season == "WS" & param_ME$Region == "Sah"]^0.75)*
                             exp(-0.008*param_ME$value[param_ME$Variable == "Age" & param_ME$name == "Heifer"]))/
                            ((0.02*param_ME$value[param_ME$Variable =="M.D" & param_ME$name == "Heifer" & param_ME$Season == "WS" & param_ME$Region == "Sah"])+0.5)))

MERm_WS_Sah_cow <- (param_ME$value[param_ME$Variable=="K" & param_ME$name == "Cow"]*
                      param_ME$value[param_ME$Variable == "S" & param_ME$name == "Cow"]*
                      param_ME$value[param_ME$Variable == "M" & param_ME$name == "Cow"]*
                      ((0.26*(param_ME$value[param_ME$Variable =="MLW" & param_ME$name == "Cow" & param_ME$Season == "WS" & param_ME$Region == "Sah"]^0.75)*
                          exp(-0.008*param_ME$value[param_ME$Variable == "Age" & param_ME$name == "Cow"]))/
                         ((0.02*param_ME$value[param_ME$Variable =="M.D" & param_ME$name == "Cow" & param_ME$Season == "WS" & param_ME$Region == "Sah"])+0.5)))

MERm_WS_Sah_lamb <- (param_ME$value[param_ME$Variable=="K" & param_ME$name == "Lamb"]*
                       param_ME$value[param_ME$Variable == "S" & param_ME$name == "Lamb"]*
                       param_ME$value[param_ME$Variable == "M" & param_ME$name == "Lamb"]*
                       ((0.26*(param_ME$value[param_ME$Variable =="MLW" & param_ME$name == "Lamb" & param_ME$Season == "WS" & param_ME$Region == "Sah"]^0.75)*
                           exp(-0.008*param_ME$value[param_ME$Variable == "Age" & param_ME$name == "Lamb"]))/
                          ((0.02*param_ME$value[param_ME$Variable =="M.D" & param_ME$name == "Lamb" & param_ME$Season == "WS" & param_ME$Region == "Sah"])+0.5)))

MERm_WS_Sah_sheep <- (param_ME$value[param_ME$Variable=="K" & param_ME$name == "Sheep"]*
                        param_ME$value[param_ME$Variable == "S" & param_ME$name == "Sheep"]*
                        param_ME$value[param_ME$Variable == "M" & param_ME$name == "Sheep"]*
                        ((0.26*(param_ME$value[param_ME$Variable =="MLW" & param_ME$name == "Sheep" & param_ME$Season == "WS" & param_ME$Region == "Sah"]^0.75)*
                            exp(-0.008*param_ME$value[param_ME$Variable == "Age" & param_ME$name == "Sheep"]))/
                           ((0.02*param_ME$value[param_ME$Variable =="M.D" & param_ME$name == "Sheep" & param_ME$Season == "WS" & param_ME$Region == "Sah"])+0.5)))

MERm_WS_Sah_kid <- (param_ME$value[param_ME$Variable=="K" & param_ME$name == "Kid"]*
                      param_ME$value[param_ME$Variable == "S" & param_ME$name == "Kid"]*
                      param_ME$value[param_ME$Variable == "M" & param_ME$name == "Kid"]*
                      ((0.26*(param_ME$value[param_ME$Variable =="MLW" & param_ME$name == "Kid" & param_ME$Season == "WS" & param_ME$Region == "Sah"]^0.75)*
                          exp(-0.008*param_ME$value[param_ME$Variable == "Age" & param_ME$name == "Kid"]))/
                         ((0.02*param_ME$value[param_ME$Variable =="M.D" & param_ME$name == "Kid" & param_ME$Season == "WS" & param_ME$Region == "Sah"])+0.5)))

MERm_WS_Sah_goat <- (param_ME$value[param_ME$Variable=="K" & param_ME$name == "Goat"]*
                       param_ME$value[param_ME$Variable == "S" & param_ME$name == "Goat"]*
                       param_ME$value[param_ME$Variable == "M" & param_ME$name == "Goat"]*
                       ((0.26*(param_ME$value[param_ME$Variable =="MLW" & param_ME$name == "Goat" & param_ME$Season == "WS" & param_ME$Region == "Sah"]^0.75)*
                           exp(-0.008*param_ME$value[param_ME$Variable == "Age" & param_ME$name == "Goat"]))/
                          ((0.02*param_ME$value[param_ME$Variable =="M.D" & param_ME$name == "Goat" & param_ME$Season == "WS" & param_ME$Region == "Sah"])+0.5)))

#dry season
MERm_DS_Sah_bull <- (param_ME$value[param_ME$Variable=="K" & param_ME$name == "Bull"]*
                       param_ME$value[param_ME$Variable == "S" & param_ME$name == "Bull"]*
                       param_ME$value[param_ME$Variable == "M" & param_ME$name == "Bull"]*
                       ((0.26*(param_ME$value[param_ME$Variable =="MLW" & param_ME$name == "Bull" & param_ME$Season == "DS" & param_ME$Region == "Sah"]^0.75)*
                           exp(-0.008*param_ME$value[param_ME$Variable == "Age" & param_ME$name == "Bull"]))/
                          ((0.02*param_ME$value[param_ME$Variable =="M.D" & param_ME$name == "Bull" & param_ME$Season == "DS" & param_ME$Region == "Sah"])+0.5)))

MERm_DS_Sah_steer <- (param_ME$value[param_ME$Variable=="K" & param_ME$name == "Steer"]*
                        param_ME$value[param_ME$Variable == "S" & param_ME$name == "Steer"]*
                        param_ME$value[param_ME$Variable == "M" & param_ME$name == "Steer"]*
                        ((0.26*(param_ME$value[param_ME$Variable =="MLW" & param_ME$name == "Steer" & param_ME$Season == "DS" & param_ME$Region == "Sah"]^0.75)*
                            exp(-0.008*param_ME$value[param_ME$Variable == "Age" & param_ME$name == "Steer"]))/
                           ((0.02*param_ME$value[param_ME$Variable =="M.D" & param_ME$name == "Steer" & param_ME$Season == "DS" & param_ME$Region == "Sah"])+0.5)))

MERm_DS_Sah_calf <- (param_ME$value[param_ME$Variable=="K" & param_ME$name == "Calf"]*
                       param_ME$value[param_ME$Variable == "S" & param_ME$name == "Calf"]*
                       param_ME$value[param_ME$Variable == "M" & param_ME$name == "Calf"]*
                       ((0.26*(param_ME$value[param_ME$Variable =="MLW" & param_ME$name == "Calf" & param_ME$Season == "DS" & param_ME$Region == "Sah"]^0.75)*
                           exp(-0.008*param_ME$value[param_ME$Variable == "Age" & param_ME$name == "Calf"]))/
                          ((0.02*param_ME$value[param_ME$Variable =="M.D" & param_ME$name == "Calf" & param_ME$Season == "DS" & param_ME$Region == "Sah"])+0.5)))

MERm_DS_Sah_heifer <- (param_ME$value[param_ME$Variable=="K" & param_ME$name == "Heifer"]*
                         param_ME$value[param_ME$Variable == "S" & param_ME$name == "Heifer"]*
                         param_ME$value[param_ME$Variable == "M" & param_ME$name == "Heifer"]*
                         ((0.26*(param_ME$value[param_ME$Variable =="MLW" & param_ME$name == "Heifer" & param_ME$Season == "DS" & param_ME$Region == "Sah"]^0.75)*
                             exp(-0.008*param_ME$value[param_ME$Variable == "Age" & param_ME$name == "Heifer"]))/
                            ((0.02*param_ME$value[param_ME$Variable =="M.D" & param_ME$name == "Heifer" & param_ME$Season == "DS" & param_ME$Region == "Sah"])+0.5)))

MERm_DS_Sah_cow <- (param_ME$value[param_ME$Variable=="K" & param_ME$name == "Cow"]*
                      param_ME$value[param_ME$Variable == "S" & param_ME$name == "Cow"]*
                      param_ME$value[param_ME$Variable == "M" & param_ME$name == "Cow"]*
                      ((0.26*(param_ME$value[param_ME$Variable =="MLW" & param_ME$name == "Cow" & param_ME$Season == "DS" & param_ME$Region == "Sah"]^0.75)*
                          exp(-0.008*param_ME$value[param_ME$Variable == "Age" & param_ME$name == "Cow"]))/
                         ((0.02*param_ME$value[param_ME$Variable =="M.D" & param_ME$name == "Cow" & param_ME$Season == "DS" & param_ME$Region == "Sah"])+0.5)))

MERm_DS_Sah_lamb <- (param_ME$value[param_ME$Variable=="K" & param_ME$name == "Lamb"]*
                       param_ME$value[param_ME$Variable == "S" & param_ME$name == "Lamb"]*
                       param_ME$value[param_ME$Variable == "M" & param_ME$name == "Lamb"]*
                       ((0.26*(param_ME$value[param_ME$Variable =="MLW" & param_ME$name == "Lamb" & param_ME$Season == "DS" & param_ME$Region == "Sah"]^0.75)*
                           exp(-0.008*param_ME$value[param_ME$Variable == "Age" & param_ME$name == "Lamb"]))/
                          ((0.02*param_ME$value[param_ME$Variable =="M.D" & param_ME$name == "Lamb" & param_ME$Season == "DS" & param_ME$Region == "Sah"])+0.5)))

MERm_DS_Sah_sheep <- (param_ME$value[param_ME$Variable=="K" & param_ME$name == "Sheep"]*
                        param_ME$value[param_ME$Variable == "S" & param_ME$name == "Sheep"]*
                        param_ME$value[param_ME$Variable == "M" & param_ME$name == "Sheep"]*
                        ((0.26*(param_ME$value[param_ME$Variable =="MLW" & param_ME$name == "Sheep" & param_ME$Season == "DS" & param_ME$Region == "Sah"]^0.75)*
                            exp(-0.008*param_ME$value[param_ME$Variable == "Age" & param_ME$name == "Sheep"]))/
                           ((0.02*param_ME$value[param_ME$Variable =="M.D" & param_ME$name == "Sheep" & param_ME$Season == "DS" & param_ME$Region == "Sah"])+0.5)))

MERm_DS_Sah_kid <- (param_ME$value[param_ME$Variable=="K" & param_ME$name == "Kid"]*
                      param_ME$value[param_ME$Variable == "S" & param_ME$name == "Kid"]*
                      param_ME$value[param_ME$Variable == "M" & param_ME$name == "Kid"]*
                      ((0.26*(param_ME$value[param_ME$Variable =="MLW" & param_ME$name == "Kid" & param_ME$Season == "DS" & param_ME$Region == "Sah"]^0.75)*
                          exp(-0.008*param_ME$value[param_ME$Variable == "Age" & param_ME$name == "Kid"]))/
                         ((0.02*param_ME$value[param_ME$Variable =="M.D" & param_ME$name == "Kid" & param_ME$Season == "DS" & param_ME$Region == "Sah"])+0.5)))

MERm_DS_Sah_goat <- (param_ME$value[param_ME$Variable=="K" & param_ME$name == "Goat"]*
                       param_ME$value[param_ME$Variable == "S" & param_ME$name == "Goat"]*
                       param_ME$value[param_ME$Variable == "M" & param_ME$name == "Goat"]*
                       ((0.26*(param_ME$value[param_ME$Variable =="MLW" & param_ME$name == "Goat" & param_ME$Season == "DS" & param_ME$Region == "Sah"]^0.75)*
                           exp(-0.008*param_ME$value[param_ME$Variable == "Age" & param_ME$name == "Goat"]))/
                          ((0.02*param_ME$value[param_ME$Variable =="M.D" & param_ME$name == "Goat" & param_ME$Season == "DS" & param_ME$Region == "Sah"])+0.5)))


#MERt <- WD*MLW*0.0026 #[MJ/KgLW/km]
#Wet season
MERt_WS_Sah_bull <- (param_ME$value[param_ME$Variable=="WD" & param_ME$name == "Bull" & param_ME$Season == "WS" & param_ME$Region == "Sah"]*
                       param_ME$value[param_ME$Variable == "MLW" & param_ME$name == "Bull" & param_ME$Season == "WS" & param_ME$Region == "Sah"] * 0.0026)

MERt_WS_Sah_steer <- (param_ME$value[param_ME$Variable=="WD" & param_ME$name == "Steer" & param_ME$Season == "WS" & param_ME$Region == "Sah"]*
                        param_ME$value[param_ME$Variable == "MLW" & param_ME$name == "Steer" & param_ME$Season == "WS" & param_ME$Region == "Sah"] * 0.0026)

MERt_WS_Sah_calf <- (param_ME$value[param_ME$Variable=="WD" & param_ME$name == "Calf" & param_ME$Season == "WS" & param_ME$Region == "Sah"]*
                       param_ME$value[param_ME$Variable == "MLW" & param_ME$name == "Calf" & param_ME$Season == "WS" & param_ME$Region == "Sah"] * 0.0026)

MERt_WS_Sah_heifer <- (param_ME$value[param_ME$Variable=="WD" & param_ME$name == "Heifer" & param_ME$Season == "WS" & param_ME$Region == "Sah"]*
                         param_ME$value[param_ME$Variable == "MLW" & param_ME$name == "Heifer" & param_ME$Season == "WS" & param_ME$Region == "Sah"] * 0.0026)

MERt_WS_Sah_cow <- (param_ME$value[param_ME$Variable=="WD" & param_ME$name == "Cow" & param_ME$Season == "WS" & param_ME$Region == "Sah"]*
                      param_ME$value[param_ME$Variable == "MLW" & param_ME$name == "Cow" & param_ME$Season == "WS" & param_ME$Region == "Sah"] * 0.0026)

MERt_WS_Sah_lamb <- (param_ME$value[param_ME$Variable=="WD" & param_ME$name == "Lamb" & param_ME$Season == "WS" & param_ME$Region == "Sah"]*
                       param_ME$value[param_ME$Variable == "MLW" & param_ME$name == "Lamb" & param_ME$Season == "WS" & param_ME$Region == "Sah"] * 0.0026)

MERt_WS_Sah_sheep <- (param_ME$value[param_ME$Variable=="WD" & param_ME$name == "Sheep" & param_ME$Season == "WS" & param_ME$Region == "Sah"]*
                        param_ME$value[param_ME$Variable == "MLW" & param_ME$name == "Sheep" & param_ME$Season == "WS" & param_ME$Region == "Sah"] * 0.0026)

MERt_WS_Sah_kid <- (param_ME$value[param_ME$Variable=="WD" & param_ME$name == "Kid" & param_ME$Season == "WS" & param_ME$Region == "Sah"]*
                      param_ME$value[param_ME$Variable == "MLW" & param_ME$name == "Kid" & param_ME$Season == "WS" & param_ME$Region == "Sah"] * 0.0026)

MERt_WS_Sah_goat <- (param_ME$value[param_ME$Variable=="WD" & param_ME$name == "Goat" & param_ME$Season == "WS" & param_ME$Region == "Sah"]*
                       param_ME$value[param_ME$Variable == "MLW" & param_ME$name == "Goat" & param_ME$Season == "WS" & param_ME$Region == "Sah"] * 0.0026)

#Dry season
MERt_DS_Sah_bull <- (param_ME$value[param_ME$Variable=="WD" & param_ME$name == "Bull" & param_ME$Season == "DS" & param_ME$Region == "Sah"]*
                       param_ME$value[param_ME$Variable == "MLW" & param_ME$name == "Bull" & param_ME$Season == "DS" & param_ME$Region == "Sah"] * 0.0026)

MERt_DS_Sah_steer <- (param_ME$value[param_ME$Variable=="WD" & param_ME$name == "Steer" & param_ME$Season == "DS" & param_ME$Region == "Sah"]*
                        param_ME$value[param_ME$Variable == "MLW" & param_ME$name == "Steer" & param_ME$Season == "DS" & param_ME$Region == "Sah"] * 0.0026)

MERt_DS_Sah_calf <- (param_ME$value[param_ME$Variable=="WD" & param_ME$name == "Calf" & param_ME$Season == "DS" & param_ME$Region == "Sah"]*
                       param_ME$value[param_ME$Variable == "MLW" & param_ME$name == "Calf" & param_ME$Season == "DS" & param_ME$Region == "Sah"] * 0.0026)

MERt_DS_Sah_heifer <- (param_ME$value[param_ME$Variable=="WD" & param_ME$name == "Heifer" & param_ME$Season == "DS" & param_ME$Region == "Sah"]*
                         param_ME$value[param_ME$Variable == "MLW" & param_ME$name == "Heifer" & param_ME$Season == "DS" & param_ME$Region == "Sah"] * 0.0026)

MERt_DS_Sah_cow <- (param_ME$value[param_ME$Variable=="WD" & param_ME$name == "Cow" & param_ME$Season == "DS" & param_ME$Region == "Sah"]*
                      param_ME$value[param_ME$Variable == "MLW" & param_ME$name == "Cow" & param_ME$Season == "DS" & param_ME$Region == "Sah"] * 0.0026)

MERt_DS_Sah_lamb <- (param_ME$value[param_ME$Variable=="WD" & param_ME$name == "Lamb" & param_ME$Season == "DS" & param_ME$Region == "Sah"]*
                       param_ME$value[param_ME$Variable == "MLW" & param_ME$name == "Lamb" & param_ME$Season == "DS" & param_ME$Region == "Sah"] * 0.0026)

MERt_DS_Sah_sheep <- (param_ME$value[param_ME$Variable=="WD" & param_ME$name == "Sheep" & param_ME$Season == "DS" & param_ME$Region == "Sah"]*
                        param_ME$value[param_ME$Variable == "MLW" & param_ME$name == "Sheep" & param_ME$Season == "DS" & param_ME$Region == "Sah"] * 0.0026)

MERt_DS_Sah_kid <- (param_ME$value[param_ME$Variable=="WD" & param_ME$name == "Kid" & param_ME$Season == "DS" & param_ME$Region == "Sah"]*
                      param_ME$value[param_ME$Variable == "MLW" & param_ME$name == "Kid" & param_ME$Season == "DS" & param_ME$Region == "Sah"] * 0.0026)

MERt_DS_Sah_goat <- (param_ME$value[param_ME$Variable=="WD" & param_ME$name == "Goat" & param_ME$Season == "DS" & param_ME$Region == "Sah"]*
                       param_ME$value[param_ME$Variable == "MLW" & param_ME$name == "Goat" & param_ME$Season == "DS" & param_ME$Region == "Sah"] * 0.0026)


#MERl <- (DMY*ECM)/((0.02*M.D)+0.04) #! These are a factor of 10 too high.
#wet season
MERl_WS_Sah_cow <- (param_ME$value[param_ME$Variable=="DMY" & param_ME$name == "Cow" & param_ME$Season == "WS" & param_ME$Region == "Sah"]*
                      ECM)/
  ((0.02*param_ME$value[param_ME$Variable == "M.D" & param_ME$name == "Cow" & param_ME$Season == "WS" & param_ME$Region == "Sah"])
   +0.04)

MERl_WS_Sah_sheep <- (param_ME$value[param_ME$Variable=="DMY" & param_ME$name == "Sheep" & param_ME$Season == "WS" & param_ME$Region == "Sah"]*
                        ECM)/
  ((0.02*param_ME$value[param_ME$Variable == "M.D" & param_ME$name == "Sheep" & param_ME$Season == "WS" & param_ME$Region == "Sah"])
   +0.04)

MERl_WS_Sah_goat <- (param_ME$value[param_ME$Variable=="DMY" & param_ME$name == "Goat" & param_ME$Season == "WS" & param_ME$Region == "Sah"]*
                       ECM)/
  ((0.02*param_ME$value[param_ME$Variable == "M.D" & param_ME$name == "Goat" & param_ME$Season == "WS" & param_ME$Region == "Sah"])
   +0.04)

#Dry season
MERl_DS_Sah_cow <- (param_ME$value[param_ME$Variable=="DMY" & param_ME$name == "Cow" & param_ME$Season == "DS" & param_ME$Region == "Sah"]*
                      ECM)/
  ((0.02*param_ME$value[param_ME$Variable == "M.D" & param_ME$name == "Cow" & param_ME$Season == "DS" & param_ME$Region == "Sah"])
   +0.04)

MERl_DS_Sah_sheep <- (param_ME$value[param_ME$Variable=="DMY" & param_ME$name == "Sheep" & param_ME$Season == "DS" & param_ME$Region == "Sah"]*
                        ECM)/
  ((0.02*param_ME$value[param_ME$Variable == "M.D" & param_ME$name == "Sheep" & param_ME$Season == "DS" & param_ME$Region == "Sah"])
   +0.04)

MERl_DS_Sah_goat <- (param_ME$value[param_ME$Variable=="DMY" & param_ME$name == "Goat" & param_ME$Season == "DS" & param_ME$Region == "Sah"]*
                       ECM)/
  ((0.02*param_ME$value[param_ME$Variable == "M.D" & param_ME$name == "Goat" & param_ME$Season == "DS" & param_ME$Region == "Sah"])
   +0.04)


#Pregnancy
#MEp = (BCexp(-Ct)*SBW*exp(A - B*exp(-Ct)))/0.133 #(Eq. 1.25 & 1.26)
#Cows
t <- 280 - (280-(12*7)) #last 12 weeks# #days since gravid uterus = 22
A <- 349.22 #Table 1.9 - undefined
B <- 349.16 #Table 1.9 - undefined
C <- (5.76*10^-5) #Table 1.9 - undefined
SBW <- 1 # Ratio of expected birth weight
Y <- (SBW*exp(A - B*exp(-C*t))) #Eq. 1.26
MERp_cow_daily_term <- B * C * exp(-C*t) * Y / 0.133 #Eq. 1.25. Daily requirement at one point in time - at term

t <- (280-(12*7)):280 #last 12 weeks #22:280
MEcow <- function(x){B * C * exp(-C*t) * (SBW*exp(A - B*exp(-C*t))) / 0.133}
y <- MEcow(t)
MERp_cow_fullPreg <- sum(y)
MERp_cow_fullPreg <- 0 #!Pregnancy is included in maintenance for Burkina faso
#f <- function(t,y) approxfun(y)(t)
#MERp_cow_fullPreg <- integrate(f,min(t),max(t),y)

#Shoats
t <- 147 - (147-(12*7)) #last 12 weeks #days since gravid uterus = 12 DOI: 10.1530/rep.1.00398 https://pubmed.ncbi.nlm.nih.gov/15579583/
A <- 7.64 #Table 1.9 - undefined
B <- 11.46 #Table 1.9 - undefined
C <- (6.43*10^-3) #Table 1.9 - undefined
SBW <- 1 # Ratio of expected birth weight
Y <- (SBW*exp(A - B*exp(-C*t))) #Eq. 1.26
MERp_shoat_daily_term <- ((B * C * exp(-C*t)*Y)/0.133) #Eq. 1.25. Daily requirement at one point in time - at term

t <- (147-(12*7)):147
MEshoat <- function(x){B * C * exp(-C*t) * (SBW*exp(A - B*exp(-C*t))) / 0.133}
y <- MEshoat(t)
MERp_shoat_fullPreg  <- sum(y)
MERp_shoat_fullPreg <- 0 #!Pregnancy is included in maintenance for Burkina faso
#MERp_shoat_fullPreg <- integrate(f,min(t),max(t),y)


#MERg 
#Wet season
#MERg=(DWG*0.92*EC)/(0.043*M.D)
#Negative
#MERg_dry=(DWG*0.92*EC)/(0.8)


MERg_WS_Sah_bull <- ifelse(param_ME$value[param_ME$Variable=="DWG" & param_ME$name == "Bull" & param_ME$Season == "WS" & param_ME$Region == "Sah"] >= 0,
                           (param_ME$value[param_ME$Variable=="DWG" & param_ME$name == "Bull" & param_ME$Season == "WS" & param_ME$Region == "Sah"]*
                              0.92*EC) / 
                             (0.043*param_ME$value[param_ME$Variable == "M.D" & param_ME$name == "Bull" & param_ME$Season == "WS" & param_ME$Region == "Sah"]),
                           (param_ME$value[param_ME$Variable=="DWG" & param_ME$name == "Bull" & param_ME$Season == "WS" & param_ME$Region == "Sah"]*
                              0.92*EC) / (0.8))

MERg_WS_Sah_steer <- ifelse(param_ME$value[param_ME$Variable=="DWG" & param_ME$name == "Steer" & param_ME$Season == "WS" & param_ME$Region == "Sah"] >=0,
                            (param_ME$value[param_ME$Variable=="DWG" & param_ME$name == "Steer" & param_ME$Season == "WS" & param_ME$Region == "Sah"]*
                               0.92*EC) / 
                              (0.043*param_ME$value[param_ME$Variable == "M.D" & param_ME$name == "Steer" & param_ME$Season == "WS" & param_ME$Region == "Sah"]),
                            (param_ME$value[param_ME$Variable=="DWG" & param_ME$name == "Steer" & param_ME$Season == "WS" & param_ME$Region == "Sah"]*
                               0.92*EC) / (0.8))

MERg_WS_Sah_calf <- ifelse(param_ME$value[param_ME$Variable=="DWG" & param_ME$name == "Calf" & param_ME$Season == "WS" & param_ME$Region == "Sah"] >= 0,
                           (param_ME$value[param_ME$Variable=="DWG" & param_ME$name == "Calf" & param_ME$Season == "WS" & param_ME$Region == "Sah"]*
                              0.92*EC) / 
                             (0.043*param_ME$value[param_ME$Variable == "M.D" & param_ME$name == "Calf" & param_ME$Season == "WS" & param_ME$Region == "Sah"]),
                           (param_ME$value[param_ME$Variable=="DWG" & param_ME$name == "Calf" & param_ME$Season == "WS" & param_ME$Region == "Sah"]*
                              0.92*EC) / 0.8)

MERg_WS_Sah_heifer <- ifelse(param_ME$value[param_ME$Variable=="DWG" & param_ME$name == "Heifer" & param_ME$Season == "WS" & param_ME$Region == "Sah"] >= 0,
                             (param_ME$value[param_ME$Variable=="DWG" & param_ME$name == "Heifer" & param_ME$Season == "WS" & param_ME$Region == "Sah"]*
                                0.92*EC) / 
                               (0.043*param_ME$value[param_ME$Variable == "M.D" & param_ME$name == "Heifer" & param_ME$Season == "WS" & param_ME$Region == "Sah"]),
                             (param_ME$value[param_ME$Variable=="DWG" & param_ME$name == "Heifer" & param_ME$Season == "WS" & param_ME$Region == "Sah"]*
                                0.92*EC) / 0.8)

MERg_WS_Sah_cow <- ifelse(param_ME$value[param_ME$Variable=="DWG" & param_ME$name == "Cow" & param_ME$Season == "WS" & param_ME$Region == "Sah"] >= 0,
                          (param_ME$value[param_ME$Variable=="DWG" & param_ME$name == "Cow" & param_ME$Season == "WS" & param_ME$Region == "Sah"]*
                             0.92*EC) / 
                            (0.043*param_ME$value[param_ME$Variable == "M.D" & param_ME$name == "Cow" & param_ME$Season == "WS" & param_ME$Region == "Sah"]),
                          (param_ME$value[param_ME$Variable=="DWG" & param_ME$name == "Cow" & param_ME$Season == "WS" & param_ME$Region == "Sah"]*
                             0.92*EC) / 0.8)

MERg_WS_Sah_lamb <- ifelse(param_ME$value[param_ME$Variable=="DWG" & param_ME$name == "Lamb" & param_ME$Season == "WS" & param_ME$Region == "Sah"] >= 0,
                           (param_ME$value[param_ME$Variable=="DWG" & param_ME$name == "Lamb" & param_ME$Season == "WS" & param_ME$Region == "Sah"]*
                              0.92*EC) / 
                             (0.043*param_ME$value[param_ME$Variable == "M.D" & param_ME$name == "Lamb" & param_ME$Season == "WS" & param_ME$Region == "Sah"]),
                           (param_ME$value[param_ME$Variable=="DWG" & param_ME$name == "Lamb" & param_ME$Season == "WS" & param_ME$Region == "Sah"]*
                              0.92*EC) / 0.8)

MERg_WS_Sah_sheep <- ifelse(param_ME$value[param_ME$Variable=="DWG" & param_ME$name == "Sheep" & param_ME$Season == "WS" & param_ME$Region == "Sah"] >= 0,
                            (param_ME$value[param_ME$Variable=="DWG" & param_ME$name == "Sheep" & param_ME$Season == "WS" & param_ME$Region == "Sah"]*
                               0.92*EC) / 
                              (0.043*param_ME$value[param_ME$Variable == "M.D" & param_ME$name == "Sheep" & param_ME$Season == "WS" & param_ME$Region == "Sah"]),
                            (param_ME$value[param_ME$Variable=="DWG" & param_ME$name == "Sheep" & param_ME$Season == "WS" & param_ME$Region == "Sah"]*
                               0.92*EC) / 0.8)

MERg_WS_Sah_kid <- ifelse(param_ME$value[param_ME$Variable=="DWG" & param_ME$name == "Kid" & param_ME$Season == "WS" & param_ME$Region == "Sah"] >= 0,
                          (param_ME$value[param_ME$Variable=="DWG" & param_ME$name == "Kid" & param_ME$Season == "WS" & param_ME$Region == "Sah"]*
                             0.92*EC) / 
                            (0.043*param_ME$value[param_ME$Variable == "M.D" & param_ME$name == "Kid" & param_ME$Season == "WS" & param_ME$Region == "Sah"]),
                          (param_ME$value[param_ME$Variable=="DWG" & param_ME$name == "Kid" & param_ME$Season == "WS" & param_ME$Region == "Sah"]*
                             0.92*EC) / 0.8)

MERg_WS_Sah_goat <- ifelse(param_ME$value[param_ME$Variable=="DWG" & param_ME$name == "Goat" & param_ME$Season == "WS" & param_ME$Region == "Sah"] >=0,
                           (param_ME$value[param_ME$Variable=="DWG" & param_ME$name == "Goat" & param_ME$Season == "WS" & param_ME$Region == "Sah"]*
                              0.92*EC) / 
                             (0.043*param_ME$value[param_ME$Variable == "M.D" & param_ME$name == "Goat" & param_ME$Season == "WS" & param_ME$Region == "Sah"]),
                           (param_ME$value[param_ME$Variable=="DWG" & param_ME$name == "Goat" & param_ME$Season == "WS" & param_ME$Region == "Sah"]*
                              0.92*EC) / 0.8)

#Dry season
MERg_DS_Sah_bull <- ifelse(param_ME$value[param_ME$Variable=="DWG" & param_ME$name == "Bull" & param_ME$Season == "DS" & param_ME$Region == "Sah"] >= 0,
                           (param_ME$value[param_ME$Variable=="DWG" & param_ME$name == "Bull" & param_ME$Season == "DS" & param_ME$Region == "Sah"]*
                              0.92*EC) / 
                             (0.043*param_ME$value[param_ME$Variable == "M.D" & param_ME$name == "Bull" & param_ME$Season == "DS" & param_ME$Region == "Sah"]),
                           (param_ME$value[param_ME$Variable=="DWG" & param_ME$name == "Bull" & param_ME$Season == "DS" & param_ME$Region == "Sah"]*
                              0.92*EC) / (0.8))

MERg_DS_Sah_steer <- ifelse(param_ME$value[param_ME$Variable=="DWG" & param_ME$name == "Steer" & param_ME$Season == "DS" & param_ME$Region == "Sah"] >=0,
                            (param_ME$value[param_ME$Variable=="DWG" & param_ME$name == "Steer" & param_ME$Season == "DS" & param_ME$Region == "Sah"]*
                               0.92*EC) / 
                              (0.043*param_ME$value[param_ME$Variable == "M.D" & param_ME$name == "Steer" & param_ME$Season == "DS" & param_ME$Region == "Sah"]),
                            (param_ME$value[param_ME$Variable=="DWG" & param_ME$name == "Steer" & param_ME$Season == "DS" & param_ME$Region == "Sah"]*
                               0.92*EC) / (0.8))

MERg_DS_Sah_calf <- ifelse(param_ME$value[param_ME$Variable=="DWG" & param_ME$name == "Calf" & param_ME$Season == "DS" & param_ME$Region == "Sah"] >= 0,
                           (param_ME$value[param_ME$Variable=="DWG" & param_ME$name == "Calf" & param_ME$Season == "DS" & param_ME$Region == "Sah"]*
                              0.92*EC) / 
                             (0.043*param_ME$value[param_ME$Variable == "M.D" & param_ME$name == "Calf" & param_ME$Season == "DS" & param_ME$Region == "Sah"]),
                           (param_ME$value[param_ME$Variable=="DWG" & param_ME$name == "Calf" & param_ME$Season == "DS" & param_ME$Region == "Sah"]*
                              0.92*EC) / 0.8)

MERg_DS_Sah_heifer <- ifelse(param_ME$value[param_ME$Variable=="DWG" & param_ME$name == "Heifer" & param_ME$Season == "DS" & param_ME$Region == "Sah"] >= 0,
                             (param_ME$value[param_ME$Variable=="DWG" & param_ME$name == "Heifer" & param_ME$Season == "DS" & param_ME$Region == "Sah"]*
                                0.92*EC) / 
                               (0.043*param_ME$value[param_ME$Variable == "M.D" & param_ME$name == "Heifer" & param_ME$Season == "DS" & param_ME$Region == "Sah"]),
                             (param_ME$value[param_ME$Variable=="DWG" & param_ME$name == "Heifer" & param_ME$Season == "DS" & param_ME$Region == "Sah"]*
                                0.92*EC) / 0.8)

MERg_DS_Sah_cow <- ifelse(param_ME$value[param_ME$Variable=="DWG" & param_ME$name == "Cow" & param_ME$Season == "DS" & param_ME$Region == "Sah"] >= 0,
                          (param_ME$value[param_ME$Variable=="DWG" & param_ME$name == "Cow" & param_ME$Season == "DS" & param_ME$Region == "Sah"]*
                             0.92*EC) / 
                            (0.043*param_ME$value[param_ME$Variable == "M.D" & param_ME$name == "Cow" & param_ME$Season == "DS" & param_ME$Region == "Sah"]),
                          (param_ME$value[param_ME$Variable=="DWG" & param_ME$name == "Cow" & param_ME$Season == "DS" & param_ME$Region == "Sah"]*
                             0.92*EC) / 0.8)

MERg_DS_Sah_lamb <- ifelse(param_ME$value[param_ME$Variable=="DWG" & param_ME$name == "Lamb" & param_ME$Season == "DS" & param_ME$Region == "Sah"] >= 0,
                           (param_ME$value[param_ME$Variable=="DWG" & param_ME$name == "Lamb" & param_ME$Season == "DS" & param_ME$Region == "Sah"]*
                              0.92*EC) / 
                             (0.043*param_ME$value[param_ME$Variable == "M.D" & param_ME$name == "Lamb" & param_ME$Season == "DS" & param_ME$Region == "Sah"]),
                           (param_ME$value[param_ME$Variable=="DWG" & param_ME$name == "Lamb" & param_ME$Season == "DS" & param_ME$Region == "Sah"]*
                              0.92*EC) / 0.8)

MERg_DS_Sah_sheep <- ifelse(param_ME$value[param_ME$Variable=="DWG" & param_ME$name == "Sheep" & param_ME$Season == "DS" & param_ME$Region == "Sah"] >= 0,
                            (param_ME$value[param_ME$Variable=="DWG" & param_ME$name == "Sheep" & param_ME$Season == "DS" & param_ME$Region == "Sah"]*
                               0.92*EC) / 
                              (0.043*param_ME$value[param_ME$Variable == "M.D" & param_ME$name == "Sheep" & param_ME$Season == "DS" & param_ME$Region == "Sah"]),
                            (param_ME$value[param_ME$Variable=="DWG" & param_ME$name == "Sheep" & param_ME$Season == "DS" & param_ME$Region == "Sah"]*
                               0.92*EC) / 0.8)

MERg_DS_Sah_kid <- ifelse(param_ME$value[param_ME$Variable=="DWG" & param_ME$name == "Kid" & param_ME$Season == "DS" & param_ME$Region == "Sah"] >= 0,
                          (param_ME$value[param_ME$Variable=="DWG" & param_ME$name == "Kid" & param_ME$Season == "DS" & param_ME$Region == "Sah"]*
                             0.92*EC) / 
                            (0.043*param_ME$value[param_ME$Variable == "M.D" & param_ME$name == "Kid" & param_ME$Season == "DS" & param_ME$Region == "Sah"]),
                          (param_ME$value[param_ME$Variable=="DWG" & param_ME$name == "Kid" & param_ME$Season == "DS" & param_ME$Region == "Sah"]*
                             0.92*EC) / 0.8)

MERg_DS_Sah_goat <- ifelse(param_ME$value[param_ME$Variable=="DWG" & param_ME$name == "Goat" & param_ME$Season == "DS" & param_ME$Region == "Sah"] >=0,
                           (param_ME$value[param_ME$Variable=="DWG" & param_ME$name == "Goat" & param_ME$Season == "DS" & param_ME$Region == "Sah"]*
                              0.92*EC) / 
                             (0.043*param_ME$value[param_ME$Variable == "M.D" & param_ME$name == "Goat" & param_ME$Season == "DS" & param_ME$Region == "Sah"]),
                           (param_ME$value[param_ME$Variable=="DWG" & param_ME$name == "Goat" & param_ME$Season == "DS" & param_ME$Region == "Sah"]*
                              0.92*EC) / 0.8)






##MERtotal by animal class
#Testing total
#MERtotal_WS_Sah_cow <- (MERm_WS_Sah_cow + MERg_WS_Sah_cow + MERl_WS_Sah_cow + MERt_WS_Sah_cow) * 110
#MERtotal_DS_Sah_cow <- (MERm_DS_Sah_cow + MERg_DS_Sah_cow + MERl_DS_Sah_cow + MERt_DS_Sah_cow) * 228
MERtotal_WS_Sah_bull <- (MERm_WS_Sah_bull + MERg_WS_Sah_bull + MERt_WS_Sah_bull)
MERtotal_WS_Sah_steer <- (MERm_WS_Sah_steer + MERg_WS_Sah_steer + MERt_WS_Sah_steer)
MERtotal_WS_Sah_calf <- (MERm_WS_Sah_calf + MERg_WS_Sah_calf + MERt_WS_Sah_calf)
MERtotal_WS_Sah_heifer <- (MERm_WS_Sah_heifer + MERg_WS_Sah_heifer + MERt_WS_Sah_heifer)
#MERtotal_WS_Sah_cow <- (MERm_WS_Sah_cow + MERg_WS_Sah_cow + MERl_WS_Sah_cow + MERt_WS_Sah_cow)
MERtotal_WS_Sah_lamb <- (MERm_WS_Sah_lamb + MERg_WS_Sah_lamb + MERt_WS_Sah_lamb)
#MERtotal_WS_Sah_sheep <- (MERm_WS_Sah_sheep + MERg_WS_Sah_sheep + MERl_WS_Sah_sheep + MERt_WS_Sah_sheep)
MERtotal_WS_Sah_kid <- (MERm_WS_Sah_kid + MERg_WS_Sah_kid + MERt_WS_Sah_kid)
#MERtotal_WS_Sah_goat <- (MERm_WS_Sah_goat + MERg_WS_Sah_goat + MERl_WS_Sah_goat + MERt_WS_Sah_goat)

MERtotal_DS_Sah_bull <- (MERm_DS_Sah_bull + MERg_DS_Sah_bull + MERt_DS_Sah_bull)
MERtotal_DS_Sah_steer <- (MERm_DS_Sah_steer + MERg_DS_Sah_steer + MERt_DS_Sah_steer)
MERtotal_DS_Sah_calf <- (MERm_DS_Sah_calf + MERg_DS_Sah_calf + MERt_DS_Sah_calf)
MERtotal_DS_Sah_heifer <- (MERm_DS_Sah_heifer + MERg_DS_Sah_heifer + MERt_DS_Sah_heifer)
#MERtotal_DS_Sah_cow <- (MERm_DS_Sah_cow + MERg_DS_Sah_cow + MERl_DS_Sah_cow + MERt_DS_Sah_cow)
MERtotal_DS_Sah_lamb <- (MERm_DS_Sah_lamb + MERg_DS_Sah_lamb + MERt_DS_Sah_lamb)
#MERtotal_DS_Sah_sheep <- (MERm_DS_Sah_sheep + MERg_DS_Sah_sheep + MERl_DS_Sah_sheep + MERt_DS_Sah_sheep)
MERtotal_DS_Sah_kid <- (MERm_DS_Sah_kid + MERg_DS_Sah_kid + MERt_DS_Sah_kid)
#MERtotal_DS_Sah_goat <- (MERm_DS_Sah_goat + MERg_DS_Sah_goat + MERl_DS_Sah_goat + MERt_DS_Sah_goat)

##Total pop requirement calcs
MERtotalYr_WS_Sah_cattle <- lapp(lv[[c(1,4,5,6)]], fun = function(cattle, livelihood, periurban, wetSSN) { 
  ifelse(livelihood ==8 & periurban ==0, #8 is the Sahel
         (MERtotal_WS_Sah_bull * cattle * param_ME$value[param_ME$Variable=="HS" & param_ME$name == "Bull" & param_ME$Region == "Sah"]*
            wetSSN) +
           (MERtotal_WS_Sah_steer * cattle * param_ME$value[param_ME$Variable=="HS" & param_ME$name == "Steer" & param_ME$Region == "Sah"]*
              wetSSN) +
           (MERtotal_WS_Sah_calf * cattle * param_ME$value[param_ME$Variable=="HS" & param_ME$name == "Calf" & param_ME$Region == "Sah"]*
              wetSSN) +
           (MERtotal_WS_Sah_heifer * cattle * param_ME$value[param_ME$Variable=="HS" & param_ME$name == "Heifer" & param_ME$Region == "Sah"]*
              wetSSN) +
           ((MERm_WS_Sah_cow + MERg_WS_Sah_cow + MERt_WS_Sah_cow) * cattle * param_ME$value[param_ME$Variable=="HS" & param_ME$name == "Cow" & param_ME$Region == "Sah"]*
              wetSSN) +
           (MERl_WS_Sah_cow * cattle * param_ME$value[param_ME$Variable=="HS" & param_ME$name == "Cow" & param_ME$Region == "Sah"]*
              param_ME$value[param_ME$Variable=="LL" & param_ME$name == "Cow"] * 
              param_ME$value[param_ME$Variable=="Fertility" & param_ME$name == "Cow"]/2) + # & param_ME$Region == "Sah"
           (MERp_cow_fullPreg * cattle * param_ME$value[param_ME$Variable=="HS" & param_ME$name == "Cow" & param_ME$Region == "Sah"]*
              param_ME$value[param_ME$Variable=="Fertility" & param_ME$name == "Cow"])
         ,NA)#Close if
})

MERtotalYr_WS_Sah_shoats <- lapp(lv[[c(2,3,4,5,6)]], fun = function(sheep, goats, livelihood, periurban, wetSSN) { 
  ifelse(livelihood ==8 & periurban ==0, #8 is the Sahel
         (MERtotal_WS_Sah_lamb * sheep * param_ME$value[param_ME$Variable=="HS" & param_ME$name == "Lamb" & param_ME$Region == "Sah"]*
            wetSSN) +
           ((MERm_WS_Sah_sheep + MERg_WS_Sah_sheep + MERt_WS_Sah_sheep) * sheep * param_ME$value[param_ME$Variable=="HS" & param_ME$name == "Sheep" & param_ME$Region == "Sah"]*
              wetSSN) +
           (MERl_WS_Sah_sheep* sheep * param_ME$value[param_ME$Variable=="HS" & param_ME$name == "Sheep" & param_ME$Region == "Sah"]*
              param_ME$value[param_ME$Variable=="LL" & param_ME$name == "Sheep"] *
              param_ME$value[param_ME$Variable=="Fertility" & param_ME$name == "Sheep"]/2) + # Weighted by fertility rate and assumed to be half in DS and half in WS
           (MERtotal_WS_Sah_kid * goats * param_ME$value[param_ME$Variable=="HS" & param_ME$name == "Kid" & param_ME$Region == "Sah"]*
              wetSSN) +
           ((MERm_WS_Sah_goat + MERg_WS_Sah_goat + MERt_WS_Sah_goat) * goats * param_ME$value[param_ME$Variable=="HS" & param_ME$name == "Goat" & param_ME$Region == "Sah"]*
              wetSSN) +
           (MERl_WS_Sah_goat * goats * param_ME$value[param_ME$Variable=="HS" & param_ME$name == "Goat" & param_ME$Region == "Sah"]*
              param_ME$value[param_ME$Variable=="LL" & param_ME$name == "Goat"] *
              param_ME$value[param_ME$Variable=="Fertility" & param_ME$name == "Goat"]/2) + # Weighted by fertility rate and assumed to be half in DS and half in WS
           (MERp_cow_fullPreg * sheep * param_ME$value[param_ME$Variable=="HS" & param_ME$name == "Sheep" & param_ME$Region == "Sah"]*
              param_ME$value[param_ME$Variable=="Fertility" & param_ME$name == "Sheep"]) +
           (MERp_cow_fullPreg * goats * param_ME$value[param_ME$Variable=="HS" & param_ME$name == "Goat" & param_ME$Region == "Sah"]*
              param_ME$value[param_ME$Variable=="Fertility" & param_ME$name == "Goat"])
         ,NA)#Close if
})

MERtotalYr_DS_Sah_cattle <- lapp(lv[[c(1,4,5,7)]], fun = function(cattle, livelihood, periurban, drySSN) { 
  ifelse(livelihood ==8 & periurban ==0, #8 is the Sahel
         (MERtotal_DS_Sah_bull * cattle * param_ME$value[param_ME$Variable=="HS" & param_ME$name == "Bull" & param_ME$Region == "Sah"]*
            drySSN) +
           (MERtotal_DS_Sah_steer * cattle * param_ME$value[param_ME$Variable=="HS" & param_ME$name == "Steer" & param_ME$Region == "Sah"]*
              drySSN) +
           (MERtotal_DS_Sah_calf * cattle * param_ME$value[param_ME$Variable=="HS" & param_ME$name == "Calf" & param_ME$Region == "Sah"]*
              drySSN) +
           (MERtotal_DS_Sah_heifer * cattle * param_ME$value[param_ME$Variable=="HS" & param_ME$name == "Heifer" & param_ME$Region == "Sah"]*
              drySSN) +
           ((MERm_DS_Sah_cow + MERg_DS_Sah_cow + MERt_DS_Sah_cow) * cattle * param_ME$value[param_ME$Variable=="HS" & param_ME$name == "Cow" & param_ME$Region == "Sah"]*
              drySSN) +
           (MERl_DS_Sah_cow * cattle * param_ME$value[param_ME$Variable=="HS" & param_ME$name == "Cow" & param_ME$Region == "Sah"]*
              param_ME$value[param_ME$Variable=="LL" & param_ME$name == "Cow"] * 
              param_ME$value[param_ME$Variable=="Fertility" & param_ME$name == "Cow"]/2) # Weighted by fertility rate and assumed to be half in DS and half in WS
         ,NA)#Close if
})

MERtotalYr_DS_Sah_shoats <- lapp(lv[[c(2,3,4,5,7)]], fun = function(sheep, goats, livelihood, periurban, drySSN) { 
  ifelse(livelihood ==8 & periurban == 0, #8 is the Sahel
         (MERtotal_DS_Sah_lamb * sheep * param_ME$value[param_ME$Variable=="HS" & param_ME$name == "Lamb" & param_ME$Region == "Sah"]*
            drySSN) +
           ((MERm_DS_Sah_sheep + MERg_DS_Sah_sheep + MERt_DS_Sah_sheep) * sheep * param_ME$value[param_ME$Variable=="HS" & param_ME$name == "Sheep" & param_ME$Region == "Sah"]*
              drySSN) +
           (MERl_DS_Sah_sheep* sheep * param_ME$value[param_ME$Variable=="HS" & param_ME$name == "Sheep" & param_ME$Region == "Sah"]*
              param_ME$value[param_ME$Variable=="LL" & param_ME$name == "Sheep"] *
              param_ME$value[param_ME$Variable=="Fertility" & param_ME$name == "Sheep"]/2) + # Weighted by fertility rate and assumed to be half in DS and half in WS
           (MERtotal_DS_Sah_kid * goats * param_ME$value[param_ME$Variable=="HS" & param_ME$name == "Kid" & param_ME$Region == "Sah"]*
              drySSN) +
           ((MERm_DS_Sah_goat + MERg_DS_Sah_goat + MERt_DS_Sah_goat) * goats * param_ME$value[param_ME$Variable=="HS" & param_ME$name == "Goat" & param_ME$Region == "Sah"]*
              drySSN) +
           (MERl_DS_Sah_goat * goats * param_ME$value[param_ME$Variable=="HS" & param_ME$name == "Goat" & param_ME$Region == "Sah"]*
              param_ME$value[param_ME$Variable=="LL" & param_ME$name == "Goat"] *
              param_ME$value[param_ME$Variable=="Fertility" & param_ME$name == "Goat"]/2) # Weighted by fertility rate and assumed to be half in DS and half in WS
         ,NA)#Close if
})


#######
#Sudanian zone  
#MERm Sud cattle

MERm_WS_Sud_bull <- (param_ME$value[param_ME$Variable=="K" & param_ME$name == "Bull"]*
                       param_ME$value[param_ME$Variable == "S" & param_ME$name == "Bull"]*
                       param_ME$value[param_ME$Variable == "M" & param_ME$name == "Bull"]*
                       ((0.26*(param_ME$value[param_ME$Variable =="MLW" & param_ME$name == "Bull" & param_ME$Season == "WS" & param_ME$Region == "Sud"]^0.75)*
                           exp(-0.008*param_ME$value[param_ME$Variable == "Age" & param_ME$name == "Bull"]))/
                          ((0.02*param_ME$value[param_ME$Variable =="M.D" & param_ME$name == "Bull" & param_ME$Season == "WS" & param_ME$Region == "Sud"])+0.5)))

MERm_WS_Sud_steer <- (param_ME$value[param_ME$Variable=="K" & param_ME$name == "Steer"]*
                        param_ME$value[param_ME$Variable == "S" & param_ME$name == "Steer"]*
                        param_ME$value[param_ME$Variable == "M" & param_ME$name == "Steer"]*
                        ((0.26*(param_ME$value[param_ME$Variable =="MLW" & param_ME$name == "Steer" & param_ME$Season == "WS" & param_ME$Region == "Sud"]^0.75)*
                            exp(-0.008*param_ME$value[param_ME$Variable == "Age" & param_ME$name == "Steer"]))/
                           ((0.02*param_ME$value[param_ME$Variable =="M.D" & param_ME$name == "Steer" & param_ME$Season == "WS" & param_ME$Region == "Sud"])+0.5)))

MERm_WS_Sud_calf <- (param_ME$value[param_ME$Variable=="K" & param_ME$name == "Calf"]*
                       param_ME$value[param_ME$Variable == "S" & param_ME$name == "Calf"]*
                       param_ME$value[param_ME$Variable == "M" & param_ME$name == "Calf"]*
                       ((0.26*(param_ME$value[param_ME$Variable =="MLW" & param_ME$name == "Calf" & param_ME$Season == "WS" & param_ME$Region == "Sud"]^0.75)*
                           exp(-0.008*param_ME$value[param_ME$Variable == "Age" & param_ME$name == "Calf"]))/
                          ((0.02*param_ME$value[param_ME$Variable =="M.D" & param_ME$name == "Calf" & param_ME$Season == "WS" & param_ME$Region == "Sud"])+0.5)))

MERm_WS_Sud_heifer <- (param_ME$value[param_ME$Variable=="K" & param_ME$name == "Heifer"]*
                         param_ME$value[param_ME$Variable == "S" & param_ME$name == "Heifer"]*
                         param_ME$value[param_ME$Variable == "M" & param_ME$name == "Heifer"]*
                         ((0.26*(param_ME$value[param_ME$Variable =="MLW" & param_ME$name == "Heifer" & param_ME$Season == "WS" & param_ME$Region == "Sud"]^0.75)*
                             exp(-0.008*param_ME$value[param_ME$Variable == "Age" & param_ME$name == "Heifer"]))/
                            ((0.02*param_ME$value[param_ME$Variable =="M.D" & param_ME$name == "Heifer" & param_ME$Season == "WS" & param_ME$Region == "Sud"])+0.5)))

MERm_WS_Sud_cow <- (param_ME$value[param_ME$Variable=="K" & param_ME$name == "Cow"]*
                      param_ME$value[param_ME$Variable == "S" & param_ME$name == "Cow"]*
                      param_ME$value[param_ME$Variable == "M" & param_ME$name == "Cow"]*
                      ((0.26*(param_ME$value[param_ME$Variable =="MLW" & param_ME$name == "Cow" & param_ME$Season == "WS" & param_ME$Region == "Sud"]^0.75)*
                          exp(-0.008*param_ME$value[param_ME$Variable == "Age" & param_ME$name == "Cow"]))/
                         ((0.02*param_ME$value[param_ME$Variable =="M.D" & param_ME$name == "Cow" & param_ME$Season == "WS" & param_ME$Region == "Sud"])+0.5)))

MERm_WS_Sud_lamb <- (param_ME$value[param_ME$Variable=="K" & param_ME$name == "Lamb"]*
                       param_ME$value[param_ME$Variable == "S" & param_ME$name == "Lamb"]*
                       param_ME$value[param_ME$Variable == "M" & param_ME$name == "Lamb"]*
                       ((0.26*(param_ME$value[param_ME$Variable =="MLW" & param_ME$name == "Lamb" & param_ME$Season == "WS" & param_ME$Region == "Sud"]^0.75)*
                           exp(-0.008*param_ME$value[param_ME$Variable == "Age" & param_ME$name == "Lamb"]))/
                          ((0.02*param_ME$value[param_ME$Variable =="M.D" & param_ME$name == "Lamb" & param_ME$Season == "WS" & param_ME$Region == "Sud"])+0.5)))

MERm_WS_Sud_sheep <- (param_ME$value[param_ME$Variable=="K" & param_ME$name == "Sheep"]*
                        param_ME$value[param_ME$Variable == "S" & param_ME$name == "Sheep"]*
                        param_ME$value[param_ME$Variable == "M" & param_ME$name == "Sheep"]*
                        ((0.26*(param_ME$value[param_ME$Variable =="MLW" & param_ME$name == "Sheep" & param_ME$Season == "WS" & param_ME$Region == "Sud"]^0.75)*
                            exp(-0.008*param_ME$value[param_ME$Variable == "Age" & param_ME$name == "Sheep"]))/
                           ((0.02*param_ME$value[param_ME$Variable =="M.D" & param_ME$name == "Sheep" & param_ME$Season == "WS" & param_ME$Region == "Sud"])+0.5)))

MERm_WS_Sud_kid <- (param_ME$value[param_ME$Variable=="K" & param_ME$name == "Kid"]*
                      param_ME$value[param_ME$Variable == "S" & param_ME$name == "Kid"]*
                      param_ME$value[param_ME$Variable == "M" & param_ME$name == "Kid"]*
                      ((0.26*(param_ME$value[param_ME$Variable =="MLW" & param_ME$name == "Kid" & param_ME$Season == "WS" & param_ME$Region == "Sud"]^0.75)*
                          exp(-0.008*param_ME$value[param_ME$Variable == "Age" & param_ME$name == "Kid"]))/
                         ((0.02*param_ME$value[param_ME$Variable =="M.D" & param_ME$name == "Kid" & param_ME$Season == "WS" & param_ME$Region == "Sud"])+0.5)))

MERm_WS_Sud_goat <- (param_ME$value[param_ME$Variable=="K" & param_ME$name == "Goat"]*
                       param_ME$value[param_ME$Variable == "S" & param_ME$name == "Goat"]*
                       param_ME$value[param_ME$Variable == "M" & param_ME$name == "Goat"]*
                       ((0.26*(param_ME$value[param_ME$Variable =="MLW" & param_ME$name == "Goat" & param_ME$Season == "WS" & param_ME$Region == "Sud"]^0.75)*
                           exp(-0.008*param_ME$value[param_ME$Variable == "Age" & param_ME$name == "Goat"]))/
                          ((0.02*param_ME$value[param_ME$Variable =="M.D" & param_ME$name == "Goat" & param_ME$Season == "WS" & param_ME$Region == "Sud"])+0.5)))

#dry season
MERm_DS_Sud_bull <- (param_ME$value[param_ME$Variable=="K" & param_ME$name == "Bull"]*
                       param_ME$value[param_ME$Variable == "S" & param_ME$name == "Bull"]*
                       param_ME$value[param_ME$Variable == "M" & param_ME$name == "Bull"]*
                       ((0.26*(param_ME$value[param_ME$Variable =="MLW" & param_ME$name == "Bull" & param_ME$Season == "DS" & param_ME$Region == "Sud"]^0.75)*
                           exp(-0.008*param_ME$value[param_ME$Variable == "Age" & param_ME$name == "Bull"]))/
                          ((0.02*param_ME$value[param_ME$Variable =="M.D" & param_ME$name == "Bull" & param_ME$Season == "DS" & param_ME$Region == "Sud"])+0.5)))

MERm_DS_Sud_steer <- (param_ME$value[param_ME$Variable=="K" & param_ME$name == "Steer"]*
                        param_ME$value[param_ME$Variable == "S" & param_ME$name == "Steer"]*
                        param_ME$value[param_ME$Variable == "M" & param_ME$name == "Steer"]*
                        ((0.26*(param_ME$value[param_ME$Variable =="MLW" & param_ME$name == "Steer" & param_ME$Season == "DS" & param_ME$Region == "Sud"]^0.75)*
                            exp(-0.008*param_ME$value[param_ME$Variable == "Age" & param_ME$name == "Steer"]))/
                           ((0.02*param_ME$value[param_ME$Variable =="M.D" & param_ME$name == "Steer" & param_ME$Season == "DS" & param_ME$Region == "Sud"])+0.5)))

MERm_DS_Sud_calf <- (param_ME$value[param_ME$Variable=="K" & param_ME$name == "Calf"]*
                       param_ME$value[param_ME$Variable == "S" & param_ME$name == "Calf"]*
                       param_ME$value[param_ME$Variable == "M" & param_ME$name == "Calf"]*
                       ((0.26*(param_ME$value[param_ME$Variable =="MLW" & param_ME$name == "Calf" & param_ME$Season == "DS" & param_ME$Region == "Sud"]^0.75)*
                           exp(-0.008*param_ME$value[param_ME$Variable == "Age" & param_ME$name == "Calf"]))/
                          ((0.02*param_ME$value[param_ME$Variable =="M.D" & param_ME$name == "Calf" & param_ME$Season == "DS" & param_ME$Region == "Sud"])+0.5)))

MERm_DS_Sud_heifer <- (param_ME$value[param_ME$Variable=="K" & param_ME$name == "Heifer"]*
                         param_ME$value[param_ME$Variable == "S" & param_ME$name == "Heifer"]*
                         param_ME$value[param_ME$Variable == "M" & param_ME$name == "Heifer"]*
                         ((0.26*(param_ME$value[param_ME$Variable =="MLW" & param_ME$name == "Heifer" & param_ME$Season == "DS" & param_ME$Region == "Sud"]^0.75)*
                             exp(-0.008*param_ME$value[param_ME$Variable == "Age" & param_ME$name == "Heifer"]))/
                            ((0.02*param_ME$value[param_ME$Variable =="M.D" & param_ME$name == "Heifer" & param_ME$Season == "DS" & param_ME$Region == "Sud"])+0.5)))

MERm_DS_Sud_cow <- (param_ME$value[param_ME$Variable=="K" & param_ME$name == "Cow"]*
                      param_ME$value[param_ME$Variable == "S" & param_ME$name == "Cow"]*
                      param_ME$value[param_ME$Variable == "M" & param_ME$name == "Cow"]*
                      ((0.26*(param_ME$value[param_ME$Variable =="MLW" & param_ME$name == "Cow" & param_ME$Season == "DS" & param_ME$Region == "Sud"]^0.75)*
                          exp(-0.008*param_ME$value[param_ME$Variable == "Age" & param_ME$name == "Cow"]))/
                         ((0.02*param_ME$value[param_ME$Variable =="M.D" & param_ME$name == "Cow" & param_ME$Season == "DS" & param_ME$Region == "Sud"])+0.5)))

MERm_DS_Sud_lamb <- (param_ME$value[param_ME$Variable=="K" & param_ME$name == "Lamb"]*
                       param_ME$value[param_ME$Variable == "S" & param_ME$name == "Lamb"]*
                       param_ME$value[param_ME$Variable == "M" & param_ME$name == "Lamb"]*
                       ((0.26*(param_ME$value[param_ME$Variable =="MLW" & param_ME$name == "Lamb" & param_ME$Season == "DS" & param_ME$Region == "Sud"]^0.75)*
                           exp(-0.008*param_ME$value[param_ME$Variable == "Age" & param_ME$name == "Lamb"]))/
                          ((0.02*param_ME$value[param_ME$Variable =="M.D" & param_ME$name == "Lamb" & param_ME$Season == "DS" & param_ME$Region == "Sud"])+0.5)))

MERm_DS_Sud_sheep <- (param_ME$value[param_ME$Variable=="K" & param_ME$name == "Sheep"]*
                        param_ME$value[param_ME$Variable == "S" & param_ME$name == "Sheep"]*
                        param_ME$value[param_ME$Variable == "M" & param_ME$name == "Sheep"]*
                        ((0.26*(param_ME$value[param_ME$Variable =="MLW" & param_ME$name == "Sheep" & param_ME$Season == "DS" & param_ME$Region == "Sud"]^0.75)*
                            exp(-0.008*param_ME$value[param_ME$Variable == "Age" & param_ME$name == "Sheep"]))/
                           ((0.02*param_ME$value[param_ME$Variable =="M.D" & param_ME$name == "Sheep" & param_ME$Season == "DS" & param_ME$Region == "Sud"])+0.5)))

MERm_DS_Sud_kid <- (param_ME$value[param_ME$Variable=="K" & param_ME$name == "Kid"]*
                      param_ME$value[param_ME$Variable == "S" & param_ME$name == "Kid"]*
                      param_ME$value[param_ME$Variable == "M" & param_ME$name == "Kid"]*
                      ((0.26*(param_ME$value[param_ME$Variable =="MLW" & param_ME$name == "Kid" & param_ME$Season == "DS" & param_ME$Region == "Sud"]^0.75)*
                          exp(-0.008*param_ME$value[param_ME$Variable == "Age" & param_ME$name == "Kid"]))/
                         ((0.02*param_ME$value[param_ME$Variable =="M.D" & param_ME$name == "Kid" & param_ME$Season == "DS" & param_ME$Region == "Sud"])+0.5)))

MERm_DS_Sud_goat <- (param_ME$value[param_ME$Variable=="K" & param_ME$name == "Goat"]*
                       param_ME$value[param_ME$Variable == "S" & param_ME$name == "Goat"]*
                       param_ME$value[param_ME$Variable == "M" & param_ME$name == "Goat"]*
                       ((0.26*(param_ME$value[param_ME$Variable =="MLW" & param_ME$name == "Goat" & param_ME$Season == "DS" & param_ME$Region == "Sud"]^0.75)*
                           exp(-0.008*param_ME$value[param_ME$Variable == "Age" & param_ME$name == "Goat"]))/
                          ((0.02*param_ME$value[param_ME$Variable =="M.D" & param_ME$name == "Goat" & param_ME$Season == "DS" & param_ME$Region == "Sud"])+0.5)))


#MERt <- WD*MLW*0.0026 #[MJ/KgLW/km]
#Wet season
MERt_WS_Sud_bull <- (param_ME$value[param_ME$Variable=="WD" & param_ME$name == "Bull" & param_ME$Season == "WS" & param_ME$Region == "Sud"]*
                       param_ME$value[param_ME$Variable == "MLW" & param_ME$name == "Bull" & param_ME$Season == "WS" & param_ME$Region == "Sud"] * 0.0026)

MERt_WS_Sud_steer <- (param_ME$value[param_ME$Variable=="WD" & param_ME$name == "Steer" & param_ME$Season == "WS" & param_ME$Region == "Sud"]*
                        param_ME$value[param_ME$Variable == "MLW" & param_ME$name == "Steer" & param_ME$Season == "WS" & param_ME$Region == "Sud"] * 0.0026)

MERt_WS_Sud_calf <- (param_ME$value[param_ME$Variable=="WD" & param_ME$name == "Calf" & param_ME$Season == "WS" & param_ME$Region == "Sud"]*
                       param_ME$value[param_ME$Variable == "MLW" & param_ME$name == "Calf" & param_ME$Season == "WS" & param_ME$Region == "Sud"] * 0.0026)

MERt_WS_Sud_heifer <- (param_ME$value[param_ME$Variable=="WD" & param_ME$name == "Heifer" & param_ME$Season == "WS" & param_ME$Region == "Sud"]*
                         param_ME$value[param_ME$Variable == "MLW" & param_ME$name == "Heifer" & param_ME$Season == "WS" & param_ME$Region == "Sud"] * 0.0026)

MERt_WS_Sud_cow <- (param_ME$value[param_ME$Variable=="WD" & param_ME$name == "Cow" & param_ME$Season == "WS" & param_ME$Region == "Sud"]*
                      param_ME$value[param_ME$Variable == "MLW" & param_ME$name == "Cow" & param_ME$Season == "WS" & param_ME$Region == "Sud"] * 0.0026)

MERt_WS_Sud_lamb <- (param_ME$value[param_ME$Variable=="WD" & param_ME$name == "Lamb" & param_ME$Season == "WS" & param_ME$Region == "Sud"]*
                       param_ME$value[param_ME$Variable == "MLW" & param_ME$name == "Lamb" & param_ME$Season == "WS" & param_ME$Region == "Sud"] * 0.0026)

MERt_WS_Sud_sheep <- (param_ME$value[param_ME$Variable=="WD" & param_ME$name == "Sheep" & param_ME$Season == "WS" & param_ME$Region == "Sud"]*
                        param_ME$value[param_ME$Variable == "MLW" & param_ME$name == "Sheep" & param_ME$Season == "WS" & param_ME$Region == "Sud"] * 0.0026)

MERt_WS_Sud_kid <- (param_ME$value[param_ME$Variable=="WD" & param_ME$name == "Kid" & param_ME$Season == "WS" & param_ME$Region == "Sud"]*
                      param_ME$value[param_ME$Variable == "MLW" & param_ME$name == "Kid" & param_ME$Season == "WS" & param_ME$Region == "Sud"] * 0.0026)

MERt_WS_Sud_goat <- (param_ME$value[param_ME$Variable=="WD" & param_ME$name == "Goat" & param_ME$Season == "WS" & param_ME$Region == "Sud"]*
                       param_ME$value[param_ME$Variable == "MLW" & param_ME$name == "Goat" & param_ME$Season == "WS" & param_ME$Region == "Sud"] * 0.0026)

#Dry season
MERt_DS_Sud_bull <- (param_ME$value[param_ME$Variable=="WD" & param_ME$name == "Bull" & param_ME$Season == "DS" & param_ME$Region == "Sud"]*
                       param_ME$value[param_ME$Variable == "MLW" & param_ME$name == "Bull" & param_ME$Season == "DS" & param_ME$Region == "Sud"] * 0.0026)

MERt_DS_Sud_steer <- (param_ME$value[param_ME$Variable=="WD" & param_ME$name == "Steer" & param_ME$Season == "DS" & param_ME$Region == "Sud"]*
                        param_ME$value[param_ME$Variable == "MLW" & param_ME$name == "Steer" & param_ME$Season == "DS" & param_ME$Region == "Sud"] * 0.0026)

MERt_DS_Sud_calf <- (param_ME$value[param_ME$Variable=="WD" & param_ME$name == "Calf" & param_ME$Season == "DS" & param_ME$Region == "Sud"]*
                       param_ME$value[param_ME$Variable == "MLW" & param_ME$name == "Calf" & param_ME$Season == "DS" & param_ME$Region == "Sud"] * 0.0026)

MERt_DS_Sud_heifer <- (param_ME$value[param_ME$Variable=="WD" & param_ME$name == "Heifer" & param_ME$Season == "DS" & param_ME$Region == "Sud"]*
                         param_ME$value[param_ME$Variable == "MLW" & param_ME$name == "Heifer" & param_ME$Season == "DS" & param_ME$Region == "Sud"] * 0.0026)

MERt_DS_Sud_cow <- (param_ME$value[param_ME$Variable=="WD" & param_ME$name == "Cow" & param_ME$Season == "DS" & param_ME$Region == "Sud"]*
                      param_ME$value[param_ME$Variable == "MLW" & param_ME$name == "Cow" & param_ME$Season == "DS" & param_ME$Region == "Sud"] * 0.0026)

MERt_DS_Sud_lamb <- (param_ME$value[param_ME$Variable=="WD" & param_ME$name == "Lamb" & param_ME$Season == "DS" & param_ME$Region == "Sud"]*
                       param_ME$value[param_ME$Variable == "MLW" & param_ME$name == "Lamb" & param_ME$Season == "DS" & param_ME$Region == "Sud"] * 0.0026)

MERt_DS_Sud_sheep <- (param_ME$value[param_ME$Variable=="WD" & param_ME$name == "Sheep" & param_ME$Season == "DS" & param_ME$Region == "Sud"]*
                        param_ME$value[param_ME$Variable == "MLW" & param_ME$name == "Sheep" & param_ME$Season == "DS" & param_ME$Region == "Sud"] * 0.0026)

MERt_DS_Sud_kid <- (param_ME$value[param_ME$Variable=="WD" & param_ME$name == "Kid" & param_ME$Season == "DS" & param_ME$Region == "Sud"]*
                      param_ME$value[param_ME$Variable == "MLW" & param_ME$name == "Kid" & param_ME$Season == "DS" & param_ME$Region == "Sud"] * 0.0026)

MERt_DS_Sud_goat <- (param_ME$value[param_ME$Variable=="WD" & param_ME$name == "Goat" & param_ME$Season == "DS" & param_ME$Region == "Sud"]*
                       param_ME$value[param_ME$Variable == "MLW" & param_ME$name == "Goat" & param_ME$Season == "DS" & param_ME$Region == "Sud"] * 0.0026)


#MERl <- (DMY*ECM)/((0.02*M.D)+0.04) #! These are a factor of 10 too high.
#wet season
MERl_WS_Sud_cow <- (param_ME$value[param_ME$Variable=="DMY" & param_ME$name == "Cow" & param_ME$Season == "WS" & param_ME$Region == "Sud"]*
                      ECM)/
  ((0.02*param_ME$value[param_ME$Variable == "M.D" & param_ME$name == "Cow" & param_ME$Season == "WS" & param_ME$Region == "Sud"])
   +0.04)

MERl_WS_Sud_sheep <- (param_ME$value[param_ME$Variable=="DMY" & param_ME$name == "Sheep" & param_ME$Season == "WS" & param_ME$Region == "Sud"]*
                        ECM)/
  ((0.02*param_ME$value[param_ME$Variable == "M.D" & param_ME$name == "Sheep" & param_ME$Season == "WS" & param_ME$Region == "Sud"])
   +0.04)

MERl_WS_Sud_goat <- (param_ME$value[param_ME$Variable=="DMY" & param_ME$name == "Goat" & param_ME$Season == "WS" & param_ME$Region == "Sud"]*
                       ECM)/
  ((0.02*param_ME$value[param_ME$Variable == "M.D" & param_ME$name == "Goat" & param_ME$Season == "WS" & param_ME$Region == "Sud"])
   +0.04)

#Dry season
MERl_DS_Sud_cow <- (param_ME$value[param_ME$Variable=="DMY" & param_ME$name == "Cow" & param_ME$Season == "DS" & param_ME$Region == "Sud"]*
                      ECM)/
  ((0.02*param_ME$value[param_ME$Variable == "M.D" & param_ME$name == "Cow" & param_ME$Season == "DS" & param_ME$Region == "Sud"])
   +0.04)

MERl_DS_Sud_sheep <- (param_ME$value[param_ME$Variable=="DMY" & param_ME$name == "Sheep" & param_ME$Season == "DS" & param_ME$Region == "Sud"]*
                        ECM)/
  ((0.02*param_ME$value[param_ME$Variable == "M.D" & param_ME$name == "Sheep" & param_ME$Season == "DS" & param_ME$Region == "Sud"])
   +0.04)

MERl_DS_Sud_goat <- (param_ME$value[param_ME$Variable=="DMY" & param_ME$name == "Goat" & param_ME$Season == "DS" & param_ME$Region == "Sud"]*
                       ECM)/
  ((0.02*param_ME$value[param_ME$Variable == "M.D" & param_ME$name == "Goat" & param_ME$Season == "DS" & param_ME$Region == "Sud"])
   +0.04)

#MERg #!x10 greater than Rahimi et al.
#Wet season
#MERg=(DWG*0.92*EC)/(0.043*M.D)
#Negative
#MERg_dry=(DWG*0.92*EC)/(0.8)


MERg_WS_Sud_bull <- ifelse(param_ME$value[param_ME$Variable=="DWG" & param_ME$name == "Bull" & param_ME$Season == "WS" & param_ME$Region == "Sud"] >= 0,
                           (param_ME$value[param_ME$Variable=="DWG" & param_ME$name == "Bull" & param_ME$Season == "WS" & param_ME$Region == "Sud"]*
                              0.92*EC) / 
                             (0.043*param_ME$value[param_ME$Variable == "M.D" & param_ME$name == "Bull" & param_ME$Season == "WS" & param_ME$Region == "Sud"]),
                           (param_ME$value[param_ME$Variable=="DWG" & param_ME$name == "Bull" & param_ME$Season == "WS" & param_ME$Region == "Sud"]*
                              0.92*EC) / (0.8))

MERg_WS_Sud_steer <- ifelse(param_ME$value[param_ME$Variable=="DWG" & param_ME$name == "Steer" & param_ME$Season == "WS" & param_ME$Region == "Sud"] >=0,
                            (param_ME$value[param_ME$Variable=="DWG" & param_ME$name == "Steer" & param_ME$Season == "WS" & param_ME$Region == "Sud"]*
                               0.92*EC) / 
                              (0.043*param_ME$value[param_ME$Variable == "M.D" & param_ME$name == "Steer" & param_ME$Season == "WS" & param_ME$Region == "Sud"]),
                            (param_ME$value[param_ME$Variable=="DWG" & param_ME$name == "Steer" & param_ME$Season == "WS" & param_ME$Region == "Sud"]*
                               0.92*EC) / (0.8))

MERg_WS_Sud_calf <- ifelse(param_ME$value[param_ME$Variable=="DWG" & param_ME$name == "Calf" & param_ME$Season == "WS" & param_ME$Region == "Sud"] >= 0,
                           (param_ME$value[param_ME$Variable=="DWG" & param_ME$name == "Calf" & param_ME$Season == "WS" & param_ME$Region == "Sud"]*
                              0.92*EC) / 
                             (0.043*param_ME$value[param_ME$Variable == "M.D" & param_ME$name == "Calf" & param_ME$Season == "WS" & param_ME$Region == "Sud"]),
                           (param_ME$value[param_ME$Variable=="DWG" & param_ME$name == "Calf" & param_ME$Season == "WS" & param_ME$Region == "Sud"]*
                              0.92*EC) / 0.8)

MERg_WS_Sud_heifer <- ifelse(param_ME$value[param_ME$Variable=="DWG" & param_ME$name == "Heifer" & param_ME$Season == "WS" & param_ME$Region == "Sud"] >= 0,
                             (param_ME$value[param_ME$Variable=="DWG" & param_ME$name == "Heifer" & param_ME$Season == "WS" & param_ME$Region == "Sud"]*
                                0.92*EC) / 
                               (0.043*param_ME$value[param_ME$Variable == "M.D" & param_ME$name == "Heifer" & param_ME$Season == "WS" & param_ME$Region == "Sud"]),
                             (param_ME$value[param_ME$Variable=="DWG" & param_ME$name == "Heifer" & param_ME$Season == "WS" & param_ME$Region == "Sud"]*
                                0.92*EC) / 0.8)

MERg_WS_Sud_cow <- ifelse(param_ME$value[param_ME$Variable=="DWG" & param_ME$name == "Cow" & param_ME$Season == "WS" & param_ME$Region == "Sud"] >= 0,
                          (param_ME$value[param_ME$Variable=="DWG" & param_ME$name == "Cow" & param_ME$Season == "WS" & param_ME$Region == "Sud"]*
                             0.92*EC) / 
                            (0.043*param_ME$value[param_ME$Variable == "M.D" & param_ME$name == "Cow" & param_ME$Season == "WS" & param_ME$Region == "Sud"]),
                          (param_ME$value[param_ME$Variable=="DWG" & param_ME$name == "Cow" & param_ME$Season == "WS" & param_ME$Region == "Sud"]*
                             0.92*EC) / 0.8)

MERg_WS_Sud_lamb <- ifelse(param_ME$value[param_ME$Variable=="DWG" & param_ME$name == "Lamb" & param_ME$Season == "WS" & param_ME$Region == "Sud"] >= 0,
                           (param_ME$value[param_ME$Variable=="DWG" & param_ME$name == "Lamb" & param_ME$Season == "WS" & param_ME$Region == "Sud"]*
                              0.92*EC) / 
                             (0.043*param_ME$value[param_ME$Variable == "M.D" & param_ME$name == "Lamb" & param_ME$Season == "WS" & param_ME$Region == "Sud"]),
                           (param_ME$value[param_ME$Variable=="DWG" & param_ME$name == "Lamb" & param_ME$Season == "WS" & param_ME$Region == "Sud"]*
                              0.92*EC) / 0.8)

MERg_WS_Sud_sheep <- ifelse(param_ME$value[param_ME$Variable=="DWG" & param_ME$name == "Sheep" & param_ME$Season == "WS" & param_ME$Region == "Sud"] >= 0,
                            (param_ME$value[param_ME$Variable=="DWG" & param_ME$name == "Sheep" & param_ME$Season == "WS" & param_ME$Region == "Sud"]*
                               0.92*EC) / 
                              (0.043*param_ME$value[param_ME$Variable == "M.D" & param_ME$name == "Sheep" & param_ME$Season == "WS" & param_ME$Region == "Sud"]),
                            (param_ME$value[param_ME$Variable=="DWG" & param_ME$name == "Sheep" & param_ME$Season == "WS" & param_ME$Region == "Sud"]*
                               0.92*EC) / 0.8)

MERg_WS_Sud_kid <- ifelse(param_ME$value[param_ME$Variable=="DWG" & param_ME$name == "Kid" & param_ME$Season == "WS" & param_ME$Region == "Sud"] >= 0,
                          (param_ME$value[param_ME$Variable=="DWG" & param_ME$name == "Kid" & param_ME$Season == "WS" & param_ME$Region == "Sud"]*
                             0.92*EC) / 
                            (0.043*param_ME$value[param_ME$Variable == "M.D" & param_ME$name == "Kid" & param_ME$Season == "WS" & param_ME$Region == "Sud"]),
                          (param_ME$value[param_ME$Variable=="DWG" & param_ME$name == "Kid" & param_ME$Season == "WS" & param_ME$Region == "Sud"]*
                             0.92*EC) / 0.8)

MERg_WS_Sud_goat <- ifelse(param_ME$value[param_ME$Variable=="DWG" & param_ME$name == "Goat" & param_ME$Season == "WS" & param_ME$Region == "Sud"] >=0,
                           (param_ME$value[param_ME$Variable=="DWG" & param_ME$name == "Goat" & param_ME$Season == "WS" & param_ME$Region == "Sud"]*
                              0.92*EC) / 
                             (0.043*param_ME$value[param_ME$Variable == "M.D" & param_ME$name == "Goat" & param_ME$Season == "WS" & param_ME$Region == "Sud"]),
                           (param_ME$value[param_ME$Variable=="DWG" & param_ME$name == "Goat" & param_ME$Season == "WS" & param_ME$Region == "Sud"]*
                              0.92*EC) / 0.8)

#Dry season
MERg_DS_Sud_bull <- ifelse(param_ME$value[param_ME$Variable=="DWG" & param_ME$name == "Bull" & param_ME$Season == "DS" & param_ME$Region == "Sud"] >= 0,
                           (param_ME$value[param_ME$Variable=="DWG" & param_ME$name == "Bull" & param_ME$Season == "DS" & param_ME$Region == "Sud"]*
                              0.92*EC) / 
                             (0.043*param_ME$value[param_ME$Variable == "M.D" & param_ME$name == "Bull" & param_ME$Season == "DS" & param_ME$Region == "Sud"]),
                           (param_ME$value[param_ME$Variable=="DWG" & param_ME$name == "Bull" & param_ME$Season == "DS" & param_ME$Region == "Sud"]*
                              0.92*EC) / (0.8))

MERg_DS_Sud_steer <- ifelse(param_ME$value[param_ME$Variable=="DWG" & param_ME$name == "Steer" & param_ME$Season == "DS" & param_ME$Region == "Sud"] >=0,
                            (param_ME$value[param_ME$Variable=="DWG" & param_ME$name == "Steer" & param_ME$Season == "DS" & param_ME$Region == "Sud"]*
                               0.92*EC) / 
                              (0.043*param_ME$value[param_ME$Variable == "M.D" & param_ME$name == "Steer" & param_ME$Season == "DS" & param_ME$Region == "Sud"]),
                            (param_ME$value[param_ME$Variable=="DWG" & param_ME$name == "Steer" & param_ME$Season == "DS" & param_ME$Region == "Sud"]*
                               0.92*EC) / (0.8))

MERg_DS_Sud_calf <- ifelse(param_ME$value[param_ME$Variable=="DWG" & param_ME$name == "Calf" & param_ME$Season == "DS" & param_ME$Region == "Sud"] >= 0,
                           (param_ME$value[param_ME$Variable=="DWG" & param_ME$name == "Calf" & param_ME$Season == "DS" & param_ME$Region == "Sud"]*
                              0.92*EC) / 
                             (0.043*param_ME$value[param_ME$Variable == "M.D" & param_ME$name == "Calf" & param_ME$Season == "DS" & param_ME$Region == "Sud"]),
                           (param_ME$value[param_ME$Variable=="DWG" & param_ME$name == "Calf" & param_ME$Season == "DS" & param_ME$Region == "Sud"]*
                              0.92*EC) / 0.8)

MERg_DS_Sud_heifer <- ifelse(param_ME$value[param_ME$Variable=="DWG" & param_ME$name == "Heifer" & param_ME$Season == "DS" & param_ME$Region == "Sud"] >= 0,
                             (param_ME$value[param_ME$Variable=="DWG" & param_ME$name == "Heifer" & param_ME$Season == "DS" & param_ME$Region == "Sud"]*
                                0.92*EC) / 
                               (0.043*param_ME$value[param_ME$Variable == "M.D" & param_ME$name == "Heifer" & param_ME$Season == "DS" & param_ME$Region == "Sud"]),
                             (param_ME$value[param_ME$Variable=="DWG" & param_ME$name == "Heifer" & param_ME$Season == "DS" & param_ME$Region == "Sud"]*
                                0.92*EC) / 0.8)

MERg_DS_Sud_cow <- ifelse(param_ME$value[param_ME$Variable=="DWG" & param_ME$name == "Cow" & param_ME$Season == "DS" & param_ME$Region == "Sud"] >= 0,
                          (param_ME$value[param_ME$Variable=="DWG" & param_ME$name == "Cow" & param_ME$Season == "DS" & param_ME$Region == "Sud"]*
                             0.92*EC) / 
                            (0.043*param_ME$value[param_ME$Variable == "M.D" & param_ME$name == "Cow" & param_ME$Season == "DS" & param_ME$Region == "Sud"]),
                          (param_ME$value[param_ME$Variable=="DWG" & param_ME$name == "Cow" & param_ME$Season == "DS" & param_ME$Region == "Sud"]*
                             0.92*EC) / 0.8)

MERg_DS_Sud_lamb <- ifelse(param_ME$value[param_ME$Variable=="DWG" & param_ME$name == "Lamb" & param_ME$Season == "DS" & param_ME$Region == "Sud"] >= 0,
                           (param_ME$value[param_ME$Variable=="DWG" & param_ME$name == "Lamb" & param_ME$Season == "DS" & param_ME$Region == "Sud"]*
                              0.92*EC) / 
                             (0.043*param_ME$value[param_ME$Variable == "M.D" & param_ME$name == "Lamb" & param_ME$Season == "DS" & param_ME$Region == "Sud"]),
                           (param_ME$value[param_ME$Variable=="DWG" & param_ME$name == "Lamb" & param_ME$Season == "DS" & param_ME$Region == "Sud"]*
                              0.92*EC) / 0.8)

MERg_DS_Sud_sheep <- ifelse(param_ME$value[param_ME$Variable=="DWG" & param_ME$name == "Sheep" & param_ME$Season == "DS" & param_ME$Region == "Sud"] >= 0,
                            (param_ME$value[param_ME$Variable=="DWG" & param_ME$name == "Sheep" & param_ME$Season == "DS" & param_ME$Region == "Sud"]*
                               0.92*EC) / 
                              (0.043*param_ME$value[param_ME$Variable == "M.D" & param_ME$name == "Sheep" & param_ME$Season == "DS" & param_ME$Region == "Sud"]),
                            (param_ME$value[param_ME$Variable=="DWG" & param_ME$name == "Sheep" & param_ME$Season == "DS" & param_ME$Region == "Sud"]*
                               0.92*EC) / 0.8)

MERg_DS_Sud_kid <- ifelse(param_ME$value[param_ME$Variable=="DWG" & param_ME$name == "Kid" & param_ME$Season == "DS" & param_ME$Region == "Sud"] >= 0,
                          (param_ME$value[param_ME$Variable=="DWG" & param_ME$name == "Kid" & param_ME$Season == "DS" & param_ME$Region == "Sud"]*
                             0.92*EC) / 
                            (0.043*param_ME$value[param_ME$Variable == "M.D" & param_ME$name == "Kid" & param_ME$Season == "DS" & param_ME$Region == "Sud"]),
                          (param_ME$value[param_ME$Variable=="DWG" & param_ME$name == "Kid" & param_ME$Season == "DS" & param_ME$Region == "Sud"]*
                             0.92*EC) / 0.8)

MERg_DS_Sud_goat <- ifelse(param_ME$value[param_ME$Variable=="DWG" & param_ME$name == "Goat" & param_ME$Season == "DS" & param_ME$Region == "Sud"] >=0,
                           (param_ME$value[param_ME$Variable=="DWG" & param_ME$name == "Goat" & param_ME$Season == "DS" & param_ME$Region == "Sud"]*
                              0.92*EC) / 
                             (0.043*param_ME$value[param_ME$Variable == "M.D" & param_ME$name == "Goat" & param_ME$Season == "DS" & param_ME$Region == "Sud"]),
                           (param_ME$value[param_ME$Variable=="DWG" & param_ME$name == "Goat" & param_ME$Season == "DS" & param_ME$Region == "Sud"]*
                              0.92*EC) / 0.8)






##MERtotal by animal class
#Testing total
#MERtotal_WS_Sud_cow <- (MERm_WS_Sud_cow + MERg_WS_Sud_cow + MERl_WS_Sud_cow + MERt_WS_Sud_cow) * 110
#MERtotal_DS_Sud_cow <- (MERm_DS_Sud_cow + MERg_DS_Sud_cow + MERl_DS_Sud_cow + MERt_DS_Sud_cow) * 228
MERtotal_WS_Sud_bull <- (MERm_WS_Sud_bull + MERg_WS_Sud_bull + MERt_WS_Sud_bull)
MERtotal_WS_Sud_steer <- (MERm_WS_Sud_steer + MERg_WS_Sud_steer + MERt_WS_Sud_steer)
MERtotal_WS_Sud_calf <- (MERm_WS_Sud_calf + MERg_WS_Sud_calf + MERt_WS_Sud_calf)
MERtotal_WS_Sud_heifer <- (MERm_WS_Sud_heifer + MERg_WS_Sud_heifer + MERt_WS_Sud_heifer)
#MERtotal_WS_Sud_cow <- (MERm_WS_Sud_cow + MERg_WS_Sud_cow + MERl_WS_Sud_cow + MERt_WS_Sud_cow)
MERtotal_WS_Sud_lamb <- (MERm_WS_Sud_lamb + MERg_WS_Sud_lamb + MERt_WS_Sud_lamb)
#MERtotal_WS_Sud_sheep <- (MERm_WS_Sud_sheep + MERg_WS_Sud_sheep + MERl_WS_Sud_sheep + MERt_WS_Sud_sheep)
MERtotal_WS_Sud_kid <- (MERm_WS_Sud_kid + MERg_WS_Sud_kid + MERt_WS_Sud_kid)
#MERtotal_WS_Sud_goat <- (MERm_WS_Sud_goat + MERg_WS_Sud_goat + MERl_WS_Sud_goat + MERt_WS_Sud_goat)

MERtotal_DS_Sud_bull <- (MERm_DS_Sud_bull + MERg_DS_Sud_bull + MERt_DS_Sud_bull)
MERtotal_DS_Sud_steer <- (MERm_DS_Sud_steer + MERg_DS_Sud_steer + MERt_DS_Sud_steer)
MERtotal_DS_Sud_calf <- (MERm_DS_Sud_calf + MERg_DS_Sud_calf + MERt_DS_Sud_calf)
MERtotal_DS_Sud_heifer <- (MERm_DS_Sud_heifer + MERg_DS_Sud_heifer + MERt_DS_Sud_heifer)
#MERtotal_DS_Sud_cow <- (MERm_DS_Sud_cow + MERg_DS_Sud_cow + MERl_DS_Sud_cow + MERt_DS_Sud_cow)
MERtotal_DS_Sud_lamb <- (MERm_DS_Sud_lamb + MERg_DS_Sud_lamb + MERt_DS_Sud_lamb)
#MERtotal_DS_Sud_sheep <- (MERm_DS_Sud_sheep + MERg_DS_Sud_sheep + MERl_DS_Sud_sheep + MERt_DS_Sud_sheep)
MERtotal_DS_Sud_kid <- (MERm_DS_Sud_kid + MERg_DS_Sud_kid + MERt_DS_Sud_kid)
#MERtotal_DS_Sud_goat <- (MERm_DS_Sud_goat + MERg_DS_Sud_goat + MERl_DS_Sud_goat + MERt_DS_Sud_goat)

##Total pop requirement calcs
MERtotalYr_WS_Sud_cattle <- lapp(lv[[c(1,4,5,6)]], fun = function(cattle, livelihood, periurban, wetSSN) { #!Including pregnancy
  ifelse(livelihood !=8 & periurban == 0, #8 is the Sudanian
         (MERtotal_WS_Sud_bull * cattle * param_ME$value[param_ME$Variable=="HS" & param_ME$name == "Bull" & param_ME$Region == "Sud"]*
            wetSSN) +
           #Add work only for WS. FAO, 2018 assume work is generally an additional 40% on top of maintenance
           (MERm_WS_Sud_bull * 0.4 * cattle * param_ME$value[param_ME$Variable=="HS" & param_ME$name == "Bull" & param_ME$Region == "Sud"]*
              param_ME$value[param_ME$Variable=="Work" & param_ME$name == "Bull" & param_ME$Region == "Sud"] *
              wetSSN) +
           (MERtotal_WS_Sud_steer * cattle * param_ME$value[param_ME$Variable=="HS" & param_ME$name == "Steer" & param_ME$Region == "Sud"]*
              wetSSN) +
           (MERtotal_WS_Sud_calf * cattle * param_ME$value[param_ME$Variable=="HS" & param_ME$name == "Calf" & param_ME$Region == "Sud"]*
              wetSSN) +
           (MERtotal_WS_Sud_heifer * cattle * param_ME$value[param_ME$Variable=="HS" & param_ME$name == "Heifer" & param_ME$Region == "Sud"]*
              wetSSN) +
           ((MERm_WS_Sud_cow + MERg_WS_Sud_cow + MERt_WS_Sud_cow) * cattle * param_ME$value[param_ME$Variable=="HS" & param_ME$name == "Cow" & param_ME$Region == "Sud"]*
              wetSSN) +
           (MERl_WS_Sud_cow * cattle * param_ME$value[param_ME$Variable=="HS" & param_ME$name == "Cow" & param_ME$Region == "Sud"]*
              param_ME$value[param_ME$Variable=="LL" & param_ME$name == "Cow"]*
              param_ME$value[param_ME$Variable=="Fertility" & param_ME$name == "Cow"]/2) + # Weighted by fertility rate and assumed to be half in DS and half in WS
           (MERp_cow_fullPreg * cattle * param_ME$value[param_ME$Variable=="HS" & param_ME$name == "Cow" & param_ME$Region == "Sud"]*
              param_ME$value[param_ME$Variable=="Fertility" & param_ME$name == "Cow"])
         ,NA)#Close if
})

MERtotalYr_WS_Sud_shoats <- lapp(lv[[c(2,3,4,5,6)]], fun = function(sheep, goats, livelihood, periurban, wetSSN) { #! including pregnancy
  ifelse(livelihood !=8 & periurban == 0, #8 is the Sudanian
         (MERtotal_WS_Sud_lamb * sheep * param_ME$value[param_ME$Variable=="HS" & param_ME$name == "Lamb" & param_ME$Region == "Sud"]*
            wetSSN) +
           ((MERm_WS_Sud_sheep + MERg_WS_Sud_sheep + MERt_WS_Sud_sheep) * sheep * param_ME$value[param_ME$Variable=="HS" & param_ME$name == "Sheep" & param_ME$Region == "Sud"]*
              wetSSN) +
           (MERl_WS_Sud_sheep* sheep * param_ME$value[param_ME$Variable=="HS" & param_ME$name == "Sheep" & param_ME$Region == "Sud"]*
              param_ME$value[param_ME$Variable=="LL" & param_ME$name == "Sheep"] *
              param_ME$value[param_ME$Variable=="Fertility" & param_ME$name == "Sheep"]/2) + # Weighted by fertility rate and assumed to be half in DS and half in WS
           (MERtotal_WS_Sud_kid * goats * param_ME$value[param_ME$Variable=="HS" & param_ME$name == "Kid" & param_ME$Region == "Sud"]*
              wetSSN) +
           ((MERm_WS_Sud_goat + MERg_WS_Sud_goat + MERt_WS_Sud_goat) * goats * param_ME$value[param_ME$Variable=="HS" & param_ME$name == "Goat" & param_ME$Region == "Sud"]*
              wetSSN) +
           (MERl_WS_Sud_goat * goats * param_ME$value[param_ME$Variable=="HS" & param_ME$name == "Goat" & param_ME$Region == "Sud"]*
              param_ME$value[param_ME$Variable=="LL" & param_ME$name == "Goat"]*
              param_ME$value[param_ME$Variable=="Fertility" & param_ME$name == "Goat"]/2) + # & param_ME$Region == "Sud"
           (MERp_shoat_fullPreg * sheep * param_ME$value[param_ME$Variable=="HS" & param_ME$name == "Sheep" & param_ME$Region == "Sud"]*
              param_ME$value[param_ME$Variable=="Fertility" & param_ME$name == "Sheep"]) +
           (MERp_shoat_fullPreg * goats * param_ME$value[param_ME$Variable=="HS" & param_ME$name == "Goat" & param_ME$Region == "Sud"]*
              param_ME$value[param_ME$Variable=="Fertility" & param_ME$name == "Goat"])
         ,NA)#Close if
})

MERtotalYr_DS_Sud_cattle <- lapp(lv[[c(1,4,5,7)]], fun = function(cattle, livelihood, periurban, drySSN) { 
  ifelse(livelihood !=8 & periurban == 0, #8 is the Sudanian
         (MERtotal_DS_Sud_bull * cattle * param_ME$value[param_ME$Variable=="HS" & param_ME$name == "Bull" & param_ME$Region == "Sud"]*
            drySSN) +
           (MERtotal_DS_Sud_steer * cattle * param_ME$value[param_ME$Variable=="HS" & param_ME$name == "Steer" & param_ME$Region == "Sud"]*
              drySSN) +
           (MERtotal_DS_Sud_calf * cattle * param_ME$value[param_ME$Variable=="HS" & param_ME$name == "Calf" & param_ME$Region == "Sud"]*
              drySSN) +
           (MERtotal_DS_Sud_heifer * cattle * param_ME$value[param_ME$Variable=="HS" & param_ME$name == "Heifer" & param_ME$Region == "Sud"]*
              drySSN) +
           ((MERm_DS_Sud_cow + MERg_DS_Sud_cow + MERt_DS_Sud_cow) * cattle * param_ME$value[param_ME$Variable=="HS" & param_ME$name == "Cow" & param_ME$Region == "Sud"]*
              drySSN) +
           (MERl_DS_Sud_cow * cattle * param_ME$value[param_ME$Variable=="HS" & param_ME$name == "Cow" & param_ME$Region == "Sud"]*
              param_ME$value[param_ME$Variable=="LL" & param_ME$name == "Cow"] *
              param_ME$value[param_ME$Variable=="Fertility" & param_ME$name == "Cow"]/2) # # Weighted by fertility rate and assumed to be half in DS and half in WS
         ,NA)#Close if
})

MERtotalYr_DS_Sud_shoats <- lapp(lv[[c(2,3,4,5,7)]], fun = function(sheep, goats, livelihood, periurban, drySSN) { 
  ifelse(livelihood !=8 & periurban == 0, #8 is the Sudanian
         (MERtotal_DS_Sud_lamb * sheep * param_ME$value[param_ME$Variable=="HS" & param_ME$name == "Lamb" & param_ME$Region == "Sud"]*
            drySSN) +
           ((MERm_DS_Sud_sheep + MERg_DS_Sud_sheep + MERt_DS_Sud_sheep) * sheep * param_ME$value[param_ME$Variable=="HS" & param_ME$name == "Sheep" & param_ME$Region == "Sud"]*
              drySSN) +
           (MERl_DS_Sud_sheep* sheep * param_ME$value[param_ME$Variable=="HS" & param_ME$name == "Sheep" & param_ME$Region == "Sud"]*
              param_ME$value[param_ME$Variable=="LL" & param_ME$name == "Sheep"]*
              param_ME$value[param_ME$Variable=="Fertility" & param_ME$name == "Sheep"]/2) + # Weighted by fertility rate and assumed to be half in DS and half in WS
           (MERtotal_DS_Sud_kid * goats * param_ME$value[param_ME$Variable=="HS" & param_ME$name == "Kid" & param_ME$Region == "Sud"]*
              drySSN) +
           ((MERm_DS_Sud_goat + MERg_DS_Sud_goat + MERt_DS_Sud_goat) * goats * param_ME$value[param_ME$Variable=="HS" & param_ME$name == "Goat" & param_ME$Region == "Sud"]*
              drySSN) +
           (MERl_DS_Sud_goat * goats * param_ME$value[param_ME$Variable=="HS" & param_ME$name == "Goat" & param_ME$Region == "Sud"]*
              param_ME$value[param_ME$Variable=="LL" & param_ME$name == "Goat"]*
              param_ME$value[param_ME$Variable=="Fertility" & param_ME$name == "Goat"]/2) # Weighted by fertility rate and assumed to be half in DS and half in WS
         ,NA)#Close if
})


##################
##Periurban
#

#MERt <- WD*MLW*0.0026 #[MJ/KgLW/km]
#Wet season
MERt_WS_Periurban_bull <- (param_ME$value[param_ME$Variable=="WD" & param_ME$name == "Bull" & param_ME$Season == "WS" & param_ME$Region == "Periurban"]*
                       param_ME$value[param_ME$Variable == "MLW" & param_ME$name == "Bull" & param_ME$Season == "WS" & param_ME$Region == "Sud"] * 0.0026)

MERt_WS_Periurban_steer <- (param_ME$value[param_ME$Variable=="WD" & param_ME$name == "Steer" & param_ME$Season == "WS" & param_ME$Region == "Periurban"]*
                        param_ME$value[param_ME$Variable == "MLW" & param_ME$name == "Steer" & param_ME$Season == "WS" & param_ME$Region == "Sud"] * 0.0026)

MERt_WS_Periurban_calf <- (param_ME$value[param_ME$Variable=="WD" & param_ME$name == "Calf" & param_ME$Season == "WS" & param_ME$Region == "Periurban"]*
                       param_ME$value[param_ME$Variable == "MLW" & param_ME$name == "Calf" & param_ME$Season == "WS" & param_ME$Region == "Sud"] * 0.0026)

MERt_WS_Periurban_heifer <- (param_ME$value[param_ME$Variable=="WD" & param_ME$name == "Heifer" & param_ME$Season == "WS" & param_ME$Region == "Periurban"]*
                         param_ME$value[param_ME$Variable == "MLW" & param_ME$name == "Heifer" & param_ME$Season == "WS" & param_ME$Region == "Sud"] * 0.0026)

MERt_WS_Periurban_cow <- (param_ME$value[param_ME$Variable=="WD" & param_ME$name == "Cow" & param_ME$Season == "WS" & param_ME$Region == "Periurban"]*
                      param_ME$value[param_ME$Variable == "MLW" & param_ME$name == "Cow" & param_ME$Season == "WS" & param_ME$Region == "Sud"] * 0.0026)

MERt_WS_Periurban_lamb <- (param_ME$value[param_ME$Variable=="WD" & param_ME$name == "Lamb" & param_ME$Season == "WS" & param_ME$Region == "Periurban"]*
                       param_ME$value[param_ME$Variable == "MLW" & param_ME$name == "Lamb" & param_ME$Season == "WS" & param_ME$Region == "Sud"] * 0.0026)

MERt_WS_Periurban_sheep <- (param_ME$value[param_ME$Variable=="WD" & param_ME$name == "Sheep" & param_ME$Season == "WS" & param_ME$Region == "Periurban"]*
                        param_ME$value[param_ME$Variable == "MLW" & param_ME$name == "Sheep" & param_ME$Season == "WS" & param_ME$Region == "Sud"] * 0.0026)

MERt_WS_Periurban_kid <- (param_ME$value[param_ME$Variable=="WD" & param_ME$name == "Kid" & param_ME$Season == "WS" & param_ME$Region == "Periurban"]*
                      param_ME$value[param_ME$Variable == "MLW" & param_ME$name == "Kid" & param_ME$Season == "WS" & param_ME$Region == "Sud"] * 0.0026)

MERt_WS_Periurban_goat <- (param_ME$value[param_ME$Variable=="WD" & param_ME$name == "Goat" & param_ME$Season == "WS" & param_ME$Region == "Periurban"]*
                       param_ME$value[param_ME$Variable == "MLW" & param_ME$name == "Goat" & param_ME$Season == "WS" & param_ME$Region == "Sud"] * 0.0026)

#Dry season
MERt_DS_Periurban_bull <- (param_ME$value[param_ME$Variable=="WD" & param_ME$name == "Bull" & param_ME$Season == "DS" & param_ME$Region == "Periurban"]*
                       param_ME$value[param_ME$Variable == "MLW" & param_ME$name == "Bull" & param_ME$Season == "DS" & param_ME$Region == "Sud"] * 0.0026)

MERt_DS_Periurban_steer <- (param_ME$value[param_ME$Variable=="WD" & param_ME$name == "Steer" & param_ME$Season == "DS" & param_ME$Region == "Periurban"]*
                        param_ME$value[param_ME$Variable == "MLW" & param_ME$name == "Steer" & param_ME$Season == "DS" & param_ME$Region == "Sud"] * 0.0026)

MERt_DS_Periurban_calf <- (param_ME$value[param_ME$Variable=="WD" & param_ME$name == "Calf" & param_ME$Season == "DS" & param_ME$Region == "Periurban"]*
                       param_ME$value[param_ME$Variable == "MLW" & param_ME$name == "Calf" & param_ME$Season == "DS" & param_ME$Region == "Sud"] * 0.0026)

MERt_DS_Periurban_heifer <- (param_ME$value[param_ME$Variable=="WD" & param_ME$name == "Heifer" & param_ME$Season == "DS" & param_ME$Region == "Periurban"]*
                         param_ME$value[param_ME$Variable == "MLW" & param_ME$name == "Heifer" & param_ME$Season == "DS" & param_ME$Region == "Sud"] * 0.0026)

MERt_DS_Periurban_cow <- (param_ME$value[param_ME$Variable=="WD" & param_ME$name == "Cow" & param_ME$Season == "DS" & param_ME$Region == "Periurban"]*
                      param_ME$value[param_ME$Variable == "MLW" & param_ME$name == "Cow" & param_ME$Season == "DS" & param_ME$Region == "Sud"] * 0.0026)

MERt_DS_Periurban_lamb <- (param_ME$value[param_ME$Variable=="WD" & param_ME$name == "Lamb" & param_ME$Season == "DS" & param_ME$Region == "Periurban"]*
                       param_ME$value[param_ME$Variable == "MLW" & param_ME$name == "Lamb" & param_ME$Season == "DS" & param_ME$Region == "Sud"] * 0.0026)

MERt_DS_Periurban_sheep <- (param_ME$value[param_ME$Variable=="WD" & param_ME$name == "Sheep" & param_ME$Season == "DS" & param_ME$Region == "Periurban"]*
                        param_ME$value[param_ME$Variable == "MLW" & param_ME$name == "Sheep" & param_ME$Season == "DS" & param_ME$Region == "Sud"] * 0.0026)

MERt_DS_Periurban_kid <- (param_ME$value[param_ME$Variable=="WD" & param_ME$name == "Kid" & param_ME$Season == "DS" & param_ME$Region == "Periurban"]*
                      param_ME$value[param_ME$Variable == "MLW" & param_ME$name == "Kid" & param_ME$Season == "DS" & param_ME$Region == "Sud"] * 0.0026)

MERt_DS_Periurban_goat <- (param_ME$value[param_ME$Variable=="WD" & param_ME$name == "Goat" & param_ME$Season == "DS" & param_ME$Region == "Periurban"]*
                       param_ME$value[param_ME$Variable == "MLW" & param_ME$name == "Goat" & param_ME$Season == "DS" & param_ME$Region == "Sud"] * 0.0026)


#MERl <- (DMY*ECM)/((0.02*M.D)+0.04) #! These are a factor of 10 too high.
#wet season
MERl_WS_Periurban_cow <- (param_ME$value[param_ME$Variable=="DMY" & param_ME$name == "Cow" & param_ME$Season == "WS" & param_ME$Region == "Periurban"]*
                      ECM)/
  ((0.02*param_ME$value[param_ME$Variable == "M.D" & param_ME$name == "Cow" & param_ME$Season == "WS" & param_ME$Region == "Sud"])
   +0.04)

MERl_WS_Periurban_sheep <- (param_ME$value[param_ME$Variable=="DMY" & param_ME$name == "Sheep" & param_ME$Season == "WS" & param_ME$Region == "Periurban"]*
                        ECM)/
  ((0.02*param_ME$value[param_ME$Variable == "M.D" & param_ME$name == "Sheep" & param_ME$Season == "WS" & param_ME$Region == "Sud"])
   +0.04)

MERl_WS_Periurban_goat <- (param_ME$value[param_ME$Variable=="DMY" & param_ME$name == "Goat" & param_ME$Season == "WS" & param_ME$Region == "Periurban"]*
                       ECM)/
  ((0.02*param_ME$value[param_ME$Variable == "M.D" & param_ME$name == "Goat" & param_ME$Season == "WS" & param_ME$Region == "Sud"])
   +0.04)

#Dry season
MERl_DS_Periurban_cow <- (param_ME$value[param_ME$Variable=="DMY" & param_ME$name == "Cow" & param_ME$Season == "DS" & param_ME$Region == "Periurban"]*
                      ECM)/
  ((0.02*param_ME$value[param_ME$Variable == "M.D" & param_ME$name == "Cow" & param_ME$Season == "DS" & param_ME$Region == "Sud"])
   +0.04)

MERl_DS_Periurban_sheep <- (param_ME$value[param_ME$Variable=="DMY" & param_ME$name == "Sheep" & param_ME$Season == "DS" & param_ME$Region == "Periurban"]*
                        ECM)/
  ((0.02*param_ME$value[param_ME$Variable == "M.D" & param_ME$name == "Sheep" & param_ME$Season == "DS" & param_ME$Region == "Sud"])
   +0.04)

MERl_DS_Periurban_goat <- (param_ME$value[param_ME$Variable=="DMY" & param_ME$name == "Goat" & param_ME$Season == "DS" & param_ME$Region == "Periurban"]*
                       ECM)/
  ((0.02*param_ME$value[param_ME$Variable == "M.D" & param_ME$name == "Goat" & param_ME$Season == "DS" & param_ME$Region == "Sud"])
   +0.04)

#Totals for non adult females
MERtotal_WS_Periurban_bull <- (MERm_WS_Sud_bull + MERg_WS_Sud_bull + MERt_WS_Periurban_bull)
MERtotal_WS_Periurban_steer <- (MERm_WS_Sud_steer + MERg_WS_Sud_steer + MERt_WS_Periurban_steer)
MERtotal_WS_Periurban_calf <- (MERm_WS_Sud_calf + MERg_WS_Sud_calf + MERt_WS_Periurban_calf)
MERtotal_WS_Periurban_heifer <- (MERm_WS_Sud_heifer + MERg_WS_Sud_heifer + MERt_WS_Periurban_heifer)
#MERtotal_WS_Sud_cow <- (MERm_WS_Sud_cow + MERg_WS_Sud_cow + MERl_WS_Sud_cow + MERt_WS_Sud_cow)
MERtotal_WS_Periurban_lamb <- (MERm_WS_Sud_lamb + MERg_WS_Sud_lamb + MERt_WS_Periurban_lamb)
#MERtotal_WS_Sud_sheep <- (MERm_WS_Sud_sheep + MERg_WS_Sud_sheep + MERl_WS_Sud_sheep + MERt_WS_Sud_sheep)
MERtotal_WS_Periurban_kid <- (MERm_WS_Sud_kid + MERg_WS_Sud_kid + MERt_WS_Periurban_kid)
#MERtotal_WS_Sud_goat <- (MERm_WS_Sud_goat + MERg_WS_Sud_goat + MERl_WS_Sud_goat + MERt_WS_Sud_goat)

MERtotal_DS_Periurban_bull <- (MERm_DS_Sud_bull + MERg_DS_Sud_bull + MERt_DS_Periurban_bull)
MERtotal_DS_Periurban_steer <- (MERm_DS_Sud_steer + MERg_DS_Sud_steer + MERt_DS_Periurban_steer)
MERtotal_DS_Periurban_calf <- (MERm_DS_Sud_calf + MERg_DS_Sud_calf + MERt_DS_Periurban_calf)
MERtotal_DS_Periurban_heifer <- (MERm_DS_Sud_heifer + MERg_DS_Sud_heifer + MERt_DS_Periurban_heifer)
#MERtotal_DS_Sud_cow <- (MERm_DS_Sud_cow + MERg_DS_Sud_cow + MERl_DS_Sud_cow + MERt_DS_Sud_cow)
MERtotal_DS_Periurban_lamb <- (MERm_DS_Sud_lamb + MERg_DS_Sud_lamb + MERt_DS_Periurban_lamb)
#MERtotal_DS_Sud_sheep <- (MERm_DS_Sud_sheep + MERg_DS_Sud_sheep + MERl_DS_Sud_sheep + MERt_DS_Sud_sheep)
MERtotal_DS_Periurban_kid <- (MERm_DS_Sud_kid + MERg_DS_Sud_kid + MERt_DS_Periurban_kid)

MERtotalYr_WS_Periurban_cattle <- lapp(lv[[c(1,4,5,6)]], fun = function(cattle, livelihood, periurban, wetSSN) { #!Including pregnancy
  ifelse(periurban == 1, 
         (MERtotal_WS_Periurban_bull * cattle * param_ME$value[param_ME$Variable=="HS" & param_ME$name == "Bull" & param_ME$Region == "Periurban"]*
            wetSSN) +
           (MERtotal_WS_Periurban_steer * cattle * param_ME$value[param_ME$Variable=="HS" & param_ME$name == "Steer" & param_ME$Region == "Periurban"]*
              wetSSN) +
           (MERtotal_WS_Periurban_calf * cattle * param_ME$value[param_ME$Variable=="HS" & param_ME$name == "Calf" & param_ME$Region == "Periurban"]*
              wetSSN) +
           (MERtotal_WS_Periurban_heifer * cattle * param_ME$value[param_ME$Variable=="HS" & param_ME$name == "Heifer" & param_ME$Region == "Periurban"]*
              wetSSN) +
           ((MERm_WS_Sud_cow + MERg_WS_Sud_cow + MERt_WS_Periurban_cow) * cattle * param_ME$value[param_ME$Variable=="HS" & param_ME$name == "Cow" & param_ME$Region == "Periurban"]*
              wetSSN) +
           (MERl_WS_Periurban_cow * cattle * param_ME$value[param_ME$Variable=="HS" & param_ME$name == "Cow" & param_ME$Region == "Periurban"]*
              param_ME$value[param_ME$Variable=="LL" & param_ME$name == "Cow"]*
              param_ME$value[param_ME$Variable=="Fertility" & param_ME$name == "Cow"]/2) + # Weighted by fertility rate and assumed to be half in DS and half in WS
           (MERp_cow_fullPreg * cattle * param_ME$value[param_ME$Variable=="HS" & param_ME$name == "Cow" & param_ME$Region == "Periurban"]*
              param_ME$value[param_ME$Variable=="Fertility" & param_ME$name == "Cow"])
         ,NA)#Close if
})

MERtotalYr_WS_Periurban_shoats <- lapp(lv[[c(2,3,4,5,6)]], fun = function(sheep, goats, livelihood, periurban, wetSSN) { #! including pregnancy
  ifelse(periurban == 1, 
         (MERtotal_WS_Periurban_lamb * sheep * param_ME$value[param_ME$Variable=="HS" & param_ME$name == "Lamb" & param_ME$Region == "Periurban"]*
            wetSSN) +
           ((MERm_WS_Sud_sheep + MERg_WS_Sud_sheep + MERt_WS_Periurban_sheep) * sheep * param_ME$value[param_ME$Variable=="HS" & param_ME$name == "Sheep" & param_ME$Region == "Periurban"]*
              wetSSN) +
           (MERl_WS_Periurban_sheep* sheep * param_ME$value[param_ME$Variable=="HS" & param_ME$name == "Sheep" & param_ME$Region == "Periurban"]*
              param_ME$value[param_ME$Variable=="LL" & param_ME$name == "Sheep"] *
              param_ME$value[param_ME$Variable=="Fertility" & param_ME$name == "Sheep"]/2) + # Weighted by fertility rate and assumed to be half in DS and half in WS
           (MERtotal_WS_Periurban_kid * goats * param_ME$value[param_ME$Variable=="HS" & param_ME$name == "Kid" & param_ME$Region == "Periurban"]*
              wetSSN) +
           ((MERm_WS_Sud_goat + MERg_WS_Sud_goat + MERt_WS_Periurban_goat) * goats * param_ME$value[param_ME$Variable=="HS" & param_ME$name == "Goat" & param_ME$Region == "Periurban"]*
              wetSSN) +
           (MERl_WS_Periurban_goat * goats * param_ME$value[param_ME$Variable=="HS" & param_ME$name == "Goat" & param_ME$Region == "Periurban"]*
              param_ME$value[param_ME$Variable=="LL" & param_ME$name == "Goat"]*
              param_ME$value[param_ME$Variable=="Fertility" & param_ME$name == "Goat"]/2) + # & param_ME$Region == "Periurban"
           (MERp_shoat_fullPreg * sheep * param_ME$value[param_ME$Variable=="HS" & param_ME$name == "Sheep" & param_ME$Region == "Periurban"]*
              param_ME$value[param_ME$Variable=="Fertility" & param_ME$name == "Sheep"]) +
           (MERp_shoat_fullPreg * goats * param_ME$value[param_ME$Variable=="HS" & param_ME$name == "Goat" & param_ME$Region == "Periurban"]*
              param_ME$value[param_ME$Variable=="Fertility" & param_ME$name == "Goat"])
         ,NA)#Close if
})

MERtotalYr_DS_Periurban_cattle <- lapp(lv[[c(1,4,5,7)]], fun = function(cattle, livelihood, periurban, drySSN) { 
  ifelse(periurban == 1, 
         (MERtotal_DS_Periurban_bull * cattle * param_ME$value[param_ME$Variable=="HS" & param_ME$name == "Bull" & param_ME$Region == "Periurban"]*
            drySSN) +
           (MERtotal_DS_Periurban_steer * cattle * param_ME$value[param_ME$Variable=="HS" & param_ME$name == "Steer" & param_ME$Region == "Periurban"]*
              drySSN) +
           (MERtotal_DS_Periurban_calf * cattle * param_ME$value[param_ME$Variable=="HS" & param_ME$name == "Calf" & param_ME$Region == "Periurban"]*
              drySSN) +
           (MERtotal_DS_Periurban_heifer * cattle * param_ME$value[param_ME$Variable=="HS" & param_ME$name == "Heifer" & param_ME$Region == "Periurban"]*
              drySSN) +
           ((MERm_DS_Sud_cow + MERg_DS_Sud_cow + MERt_DS_Periurban_cow) * cattle * param_ME$value[param_ME$Variable=="HS" & param_ME$name == "Cow" & param_ME$Region == "Periurban"]*
              drySSN) +
           (MERl_DS_Periurban_cow * cattle * param_ME$value[param_ME$Variable=="HS" & param_ME$name == "Cow" & param_ME$Region == "Periurban"]*
              param_ME$value[param_ME$Variable=="LL" & param_ME$name == "Cow"] *
              param_ME$value[param_ME$Variable=="Fertility" & param_ME$name == "Cow"]/2) # # Weighted by fertility rate and assumed to be half in DS and half in WS
         ,NA)#Close if
})

MERtotalYr_DS_Periurban_shoats <- lapp(lv[[c(2,3,4,5,7)]], fun = function(sheep, goats, livelihood, periurban, drySSN) { 
  ifelse(periurban == 1,
         (MERtotal_DS_Periurban_lamb * sheep * param_ME$value[param_ME$Variable=="HS" & param_ME$name == "Lamb" & param_ME$Region == "Periurban"]*
            drySSN) +
           ((MERm_DS_Sud_sheep + MERg_DS_Sud_sheep + MERt_DS_Periurban_sheep) * sheep * param_ME$value[param_ME$Variable=="HS" & param_ME$name == "Sheep" & param_ME$Region == "Periurban"]*
              drySSN) +
           (MERl_DS_Periurban_sheep* sheep * param_ME$value[param_ME$Variable=="HS" & param_ME$name == "Sheep" & param_ME$Region == "Periurban"]*
              param_ME$value[param_ME$Variable=="LL" & param_ME$name == "Sheep"]*
              param_ME$value[param_ME$Variable=="Fertility" & param_ME$name == "Sheep"]/2) + # Weighted by fertility rate and assumed to be half in DS and half in WS
           (MERtotal_DS_Periurban_kid * goats * param_ME$value[param_ME$Variable=="HS" & param_ME$name == "Kid" & param_ME$Region == "Periurban"]*
              drySSN) +
           ((MERm_DS_Sud_goat + MERg_DS_Sud_goat + MERt_DS_Periurban_goat) * goats * param_ME$value[param_ME$Variable=="HS" & param_ME$name == "Goat" & param_ME$Region == "Periurban"]*
              drySSN) +
           (MERl_DS_Periurban_goat * goats * param_ME$value[param_ME$Variable=="HS" & param_ME$name == "Goat" & param_ME$Region == "Periurban"]*
              param_ME$value[param_ME$Variable=="LL" & param_ME$name == "Goat"]*
              param_ME$value[param_ME$Variable=="Fertility" & param_ME$name == "Goat"]/2) # Weighted by fertility rate and assumed to be half in DS and half in WS
         ,NA)#Close if
})




##############
#Total requirements in MJ
#MERall <- sum(eval(parse(text = ls(pattern = "MERtotalYr"))), na.rm = T)
#8.3 is a conversion from DM to ME
#365 = annualised. 180 = assuming only working half of the year
MERhorse <- (((87/1000) * (350^0.75)) * 8.3 * lvHorse * 365) +  (0.9*(((87/1000) * (350^0.75)) * 8.3 * lvHorse * 180))

#Intake calculation from https://books.google.co.uk/books?hl=en&lr=&id=rlBfYgLiqtwC&oi=fnd&pg=PA64&dq=horse+feed+requirements+ME+DM&ots=SjcNMBUJ_o&sig=MABvL3RGWr6J-TZMw8MpCwmwLwU&redir_esc=y#v=onepage&q=horse%20feed%20requirements%20ME%20DM&f=false
#liveweight of African Donkeys from Nininahazwe et al., 2017 https://www.ncbi.nlm.nih.gov/pmc/articles/PMC5682267/
MERdonkey <- (((87/1000) * (126^0.75)) * 8.3 * lvDonkey * 365) +  (0.9*(((87/1000) * (126^0.75)) * 8.3 * lvDonkey * 180))

terra::writeRaster(sum(MERhorse, MERdonkey, na.rm = T), paste0(spatialResultsDir, "/horseDonkeyMER_MJ_2015.tif"), overwrite = T)

MERcattle <- sum(MERtotalYr_DS_Sah_cattle, MERtotalYr_DS_Sud_cattle, MERtotalYr_DS_Periurban_cattle, MERtotalYr_WS_Sah_cattle, MERtotalYr_WS_Sud_cattle, MERtotalYr_WS_Periurban_cattle, na.rm = T)
terra::writeRaster(MERcattle, paste0(spatialResultsDir, "/cattleMER_MJ_2015.tif"), overwrite = T)

MERshoats <- sum(MERtotalYr_DS_Sah_shoats, MERtotalYr_DS_Sud_shoats, MERtotalYr_DS_Periurban_shoats, MERtotalYr_WS_Sah_shoats, MERtotalYr_WS_Sud_shoats, MERtotalYr_WS_Periurban_shoats, na.rm = T)
terra::writeRaster(MERshoats, paste0(spatialResultsDir, "/shoatsMER_MJ_2015.tif"), overwrite = T)

MERall <- sum(MERhorse, MERdonkey, MERtotalYr_DS_Sah_cattle, MERtotalYr_DS_Sah_shoats, MERtotalYr_DS_Sud_cattle, MERtotalYr_DS_Sud_shoats, MERtotalYr_DS_Periurban_cattle, MERtotalYr_DS_Periurban_shoats, MERtotalYr_WS_Sah_cattle, MERtotalYr_WS_Sah_shoats, MERtotalYr_WS_Sud_cattle, MERtotalYr_WS_Sud_shoats, MERtotalYr_WS_Periurban_cattle, MERtotalYr_WS_Periurban_shoats, na.rm = T)
terra::writeRaster(MERall, paste0(spatialResultsDir, "/livestockMER_MJ_2015.tif"), overwrite = T)



###
#Weighted mean requirements
#Bull total
weighted.mean(c(MERm_DS_Sah_bull, MERm_WS_Sah_bull), c(as.numeric(global(lv[[7]]*(lv[[4]]!=8)*(lv[[5]]==0), 'mean', na.rm = T)), as.numeric(global(lv[[6]]*(lv[[4]]!=8)*(lv[[5]]==0), 'mean', na.rm = T))))
weighted.mean(c(MERg_DS_Sah_bull, MERg_WS_Sah_bull), c(as.numeric(global(lv[[7]]*(lv[[4]]!=8)*(lv[[5]]==0), 'mean', na.rm = T)), as.numeric(global(lv[[6]]*(lv[[4]]!=8)*(lv[[5]]==0), 'mean', na.rm = T))))
weighted.mean(c(MERt_DS_Sah_bull, MERt_WS_Sah_bull), c(as.numeric(global(lv[[7]]*(lv[[4]]!=8)*(lv[[5]]==0), 'mean', na.rm = T)), as.numeric(global(lv[[6]]*(lv[[4]]!=8)*(lv[[5]]==0), 'mean', na.rm = T))))
#Work/draught power
#Add work only for WS. FAO, 2018 assume work is generally an additional 40% on top of maintenance
#(MERm_WS_Sah_bull * 0.4)
#total = 63.1

weighted.mean(c(MERm_DS_Sud_bull, MERm_WS_Sud_bull), c(as.numeric(global(lv[[7]]*(lv[[4]]!=8)*(lv[[5]]==0), 'mean', na.rm = T)), as.numeric(global(lv[[6]]*(lv[[4]]!=8)*(lv[[5]]==0), 'mean', na.rm = T))))
weighted.mean(c(MERg_DS_Sud_bull, MERg_WS_Sud_bull), c(as.numeric(global(lv[[7]]*(lv[[4]]!=8)*(lv[[5]]==0), 'mean', na.rm = T)), as.numeric(global(lv[[6]]*(lv[[4]]!=8)*(lv[[5]]==0), 'mean', na.rm = T))))
weighted.mean(c(MERt_DS_Sud_bull, MERt_WS_Sud_bull), c(as.numeric(global(lv[[7]]*(lv[[4]]!=8)*(lv[[5]]==0), 'mean', na.rm = T)), as.numeric(global(lv[[6]]*(lv[[4]]!=8)*(lv[[5]]==0), 'mean', na.rm = T))))
#Work/draught power
#Add work only for WS. FAO, 2018 assume work is generally an additional 40% on top of maintenance
(MERm_WS_Sud_bull* 0.4)
#total = 67.9

#Maintenance
weighted.mean(c(MERm_DS_Sah_cow, MERm_WS_Sah_cow), c(as.numeric(global(lv[[7]]*(lv[[4]]!=8)*(lv[[5]]==0), 'mean', na.rm = T)), as.numeric(global(lv[[6]]*(lv[[4]]!=8)*(lv[[5]]==0), 'mean', na.rm = T))))
weighted.mean(c(MERm_DS_Sah_sheep, MERm_WS_Sah_sheep), c(as.numeric(global(lv[[7]]*(lv[[4]]!=8)*(lv[[5]]==0), 'mean', na.rm = T)), as.numeric(global(lv[[6]]*(lv[[4]]!=8)*(lv[[5]]==0), 'mean', na.rm = T))))
weighted.mean(c(MERm_DS_Sah_goat, MERm_WS_Sah_goat), c(as.numeric(global(lv[[7]]*(lv[[4]]!=8)*(lv[[5]]==0), 'mean', na.rm = T)), as.numeric(global(lv[[6]]*(lv[[4]]!=8)*(lv[[5]]==0), 'mean', na.rm = T))))

weighted.mean(c(MERm_DS_Sud_cow, MERm_WS_Sud_cow), c(as.numeric(global(lv[[7]]*(lv[[4]]==8), 'mean', na.rm = T)), as.numeric(global(lv[[6]]*(lv[[4]]==8), 'mean', na.rm = T))))
weighted.mean(c(MERm_DS_Sud_sheep, MERm_WS_Sud_sheep), c(as.numeric(global(lv[[7]]*(lv[[4]]==8), 'mean', na.rm = T)), as.numeric(global(lv[[6]]*(lv[[4]]==8), 'mean', na.rm = T))))
weighted.mean(c(MERm_DS_Sud_goat, MERm_WS_Sud_goat), c(as.numeric(global(lv[[7]]*(lv[[4]]==8), 'mean', na.rm = T)), as.numeric(global(lv[[6]]*(lv[[4]]==8), 'mean', na.rm = T))))


#Growth - !check sheep and goat inputs
weighted.mean(c(MERg_DS_Sah_cow, MERg_WS_Sah_cow), c(as.numeric(global(lv[[7]]*(lv[[4]]!=8)*(lv[[5]]==0), 'mean', na.rm = T)), as.numeric(global(lv[[6]]*(lv[[4]]!=8)*(lv[[5]]==0), 'mean', na.rm = T))))
weighted.mean(c(MERg_DS_Sah_sheep, MERg_WS_Sah_sheep), c(as.numeric(global(lv[[7]]*(lv[[4]]!=8)*(lv[[5]]==0), 'mean', na.rm = T)), as.numeric(global(lv[[6]]*(lv[[4]]!=8)*(lv[[5]]==0), 'mean', na.rm = T))))
weighted.mean(c(MERg_DS_Sah_goat, MERg_WS_Sah_goat), c(as.numeric(global(lv[[7]]*(lv[[4]]!=8)*(lv[[5]]==0), 'mean', na.rm = T)), as.numeric(global(lv[[6]]*(lv[[4]]!=8)*(lv[[5]]==0), 'mean', na.rm = T))))

weighted.mean(c(MERg_DS_Sud_cow, MERg_WS_Sud_cow), c(as.numeric(global(lv[[7]]*(lv[[4]]==8), 'mean', na.rm = T)), as.numeric(global(lv[[6]]*(lv[[4]]==8), 'mean', na.rm = T))))
weighted.mean(c(MERg_DS_Sud_sheep, MERg_WS_Sud_sheep), c(as.numeric(global(lv[[7]]*(lv[[4]]==8), 'mean', na.rm = T)), as.numeric(global(lv[[6]]*(lv[[4]]==8), 'mean', na.rm = T))))
weighted.mean(c(MERg_DS_Sud_goat, MERg_WS_Sud_goat), c(as.numeric(global(lv[[7]]*(lv[[4]]==8), 'mean', na.rm = T)), as.numeric(global(lv[[6]]*(lv[[4]]==8), 'mean', na.rm = T))))

#Lactation
weighted.mean(c(MERl_DS_Sah_cow, MERl_WS_Sah_cow), c(as.numeric(global(lv[[7]]*(lv[[4]]!=8)*(lv[[5]]==0), 'mean', na.rm = T)), as.numeric(global(lv[[6]]*(lv[[4]]!=8)*(lv[[5]]==0), 'mean', na.rm = T))))*param_ME$value[param_ME$Variable=="LL" & param_ME$name == "Cow"]/365
weighted.mean(c(MERl_DS_Sah_sheep, MERl_WS_Sah_sheep), c(as.numeric(global(lv[[7]]*(lv[[4]]!=8)*(lv[[5]]==0), 'mean', na.rm = T)), as.numeric(global(lv[[6]]*(lv[[4]]!=8)*(lv[[5]]==0), 'mean', na.rm = T))))*param_ME$value[param_ME$Variable=="LL" & param_ME$name == "Sheep"]/365
weighted.mean(c(MERl_DS_Sah_goat, MERl_WS_Sah_goat), c(as.numeric(global(lv[[7]]*(lv[[4]]!=8)*(lv[[5]]==0), 'mean', na.rm = T)), as.numeric(global(lv[[6]]*(lv[[4]]!=8)*(lv[[5]]==0), 'mean', na.rm = T))))*param_ME$value[param_ME$Variable=="LL" & param_ME$name == "Goat"]/365

weighted.mean(c(MERl_DS_Sud_cow, MERl_WS_Sud_cow), c(as.numeric(global(lv[[7]]*(lv[[4]]==8), 'mean', na.rm = T)), as.numeric(global(lv[[6]]*(lv[[4]]==8), 'mean', na.rm = T))))*param_ME$value[param_ME$Variable=="LL" & param_ME$name == "Cow"]/365
weighted.mean(c(MERl_DS_Sud_sheep, MERl_WS_Sud_sheep), c(as.numeric(global(lv[[7]]*(lv[[4]]==8), 'mean', na.rm = T)), as.numeric(global(lv[[6]]*(lv[[4]]==8), 'mean', na.rm = T))))*param_ME$value[param_ME$Variable=="LL" & param_ME$name == "Sheep"]/365
weighted.mean(c(MERl_DS_Sud_goat, MERl_WS_Sud_goat), c(as.numeric(global(lv[[7]]*(lv[[4]]==8), 'mean', na.rm = T)), as.numeric(global(lv[[6]]*(lv[[4]]==8), 'mean', na.rm = T))))*param_ME$value[param_ME$Variable=="LL" & param_ME$name == "Goat"]/365

weighted.mean(c(MERl_DS_Periurban_cow, MERl_WS_Periurban_cow), c(as.numeric(global(lv[[7]]*(lv[[5]]==1), 'mean', na.rm = T)), as.numeric(global(lv[[6]]*(lv[[5]]==1), 'mean', na.rm = T))))*param_ME$value[param_ME$Variable=="LL" & param_ME$name == "Cow"]/365


#Locomotion
weighted.mean(c(MERt_DS_Sah_cow, MERt_WS_Sah_cow), c(as.numeric(global(lv[[7]]*(lv[[4]]!=8)*(lv[[5]]==0), 'mean', na.rm = T)), as.numeric(global(lv[[6]]*(lv[[4]]!=8)*(lv[[5]]==0), 'mean', na.rm = T))))
weighted.mean(c(MERt_DS_Sah_sheep, MERt_WS_Sah_sheep), c(as.numeric(global(lv[[7]]*(lv[[4]]!=8)*(lv[[5]]==0), 'mean', na.rm = T)), as.numeric(global(lv[[6]]*(lv[[4]]!=8)*(lv[[5]]==0), 'mean', na.rm = T))))
weighted.mean(c(MERt_DS_Sah_goat, MERt_WS_Sah_goat), c(as.numeric(global(lv[[7]]*(lv[[4]]!=8)*(lv[[5]]==0), 'mean', na.rm = T)), as.numeric(global(lv[[6]]*(lv[[4]]!=8)*(lv[[5]]==0), 'mean', na.rm = T))))

weighted.mean(c(MERt_DS_Sud_cow, MERt_WS_Sud_cow), c(as.numeric(global(lv[[7]]*(lv[[4]]==8), 'mean', na.rm = T)), as.numeric(global(lv[[6]]*(lv[[4]]==8), 'mean', na.rm = T))))
weighted.mean(c(MERt_DS_Sud_sheep, MERt_WS_Sud_sheep), c(as.numeric(global(lv[[7]]*(lv[[4]]==8), 'mean', na.rm = T)), as.numeric(global(lv[[6]]*(lv[[4]]==8), 'mean', na.rm = T))))
weighted.mean(c(MERt_DS_Sud_goat, MERt_WS_Sud_goat), c(as.numeric(global(lv[[7]]*(lv[[4]]==8), 'mean', na.rm = T)), as.numeric(global(lv[[6]]*(lv[[4]]==8), 'mean', na.rm = T))))

weighted.mean(c(MERt_DS_Periurban_cow, MERt_WS_Periurban_cow), c(as.numeric(global(lv[[7]]*(lv[[5]]==1), 'mean', na.rm = T)), as.numeric(global(lv[[6]]*(lv[[5]]==1), 'mean', na.rm = T))))*param_ME$value[param_ME$Variable=="LL" & param_ME$name == "Cow"]/365


#Gestation
#mean(MERp_cow_fullPreg)
MERp_cow_fullPreg*param_ME$value[param_ME$Variable=="Fertility" & param_ME$name == "Cow"]/365
MERp_shoat_fullPreg*param_ME$value[param_ME$Variable=="Fertility" & param_ME$name == "Sheep"]/365
MERp_shoat_fullPreg*param_ME$value[param_ME$Variable=="Fertility" & param_ME$name == "Goat"]/365

