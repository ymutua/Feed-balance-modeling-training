# Estimating crop ME

# install.packages("raster")
# install.packages("stars")
# install.packages("sf")
# install.packages("dplyr")
# install.packages("exactextractr")

library(raster)
library(stars)
library(sf)
library(dplyr)
library(exactextractr)

rasterOptions(maxmemory = 1e+60)
rasterOptions(todisk=TRUE)
rasterOptions(tmpdir="/Users/s2255815/Downloads/AU_Temp")

# study area
region <- "BurkinaFaso"

# Paths, directories
root <- "/Users/s2255815/Library/CloudStorage/OneDrive-UniversityofEdinburgh/Work/Projects/AU-IBAR/Spatial modelling workshop/Workshop_materials/Day_4"
datadir <- paste0(root, "/src/3Balance-estimates/", region, "/SpatialData/inputs")
CropParams_dir <- paste0(root, "/src/3Balance-estimates/", region, "/CropParams")
outdir <- paste0(root, "/src/3Balance-estimates/", region, "/SpatialData/intermediate"); dir.create(outdir, F, T)

aoi3 <- st_read(paste0(datadir, '/gadm40_BFA_3.shp'))

cropHI <- read.csv(paste0(CropParams_dir, '/crop_harvest index.csv'), stringsAsFactors = F)
cropHI$utilMax <- ifelse(cropHI$codeSPAM %in% c("swpo", "grou"), 1, ifelse(cropHI$codeSPAM %in% c("bana", "sugc"), 0.3, 0.6))

feedQuality_SSA <- read.csv(paste0(CropParams_dir, '/feedQuality_SSAdb.csv'), stringsAsFactors = F)
#If ME is NA then estimate DE and ME from IVDMD & OM #NRC (2001) using IVDMD as a proxy for IVOMD
feedQuality_SSA <- feedQuality_SSA[!is.na(feedQuality_SSA$IVDMD),]
DMI <- 5 #DMI has minimal effect on ME_DE. Set at a reasonable value
OM <- 0.9
feedQuality_SSA$DE_ILRI <- feedQuality_SSA$ME * (1/0.82)
feedQuality_SSA$DE <- ifelse(is.na(feedQuality_SSA$OM), 0.01*OM*(feedQuality_SSA$IVDMD + 12.9)*4.4 - 0.3, 0.01*(feedQuality_SSA$OM/100)*(feedQuality_SSA$IVDMD + 12.9)*4.4 - 0.3)*4.1868 #NRC (2001) using IVDMD as a proxy for IVOMD
feedQuality_SSA$ME_DE <- 0.9410+0.0042*DMI-0.0017*feedQuality_SSA$NDF-0.0022*feedQuality_SSA$CP 
feedQuality_SSA$MEseo <- feedQuality_SSA$DE * feedQuality_SSA$ME_DE # Seo et al., 2021 https://doi.org/10.1093/jas/skab182 
feedQuality_item <- group_by(feedQuality_SSA, codeSPAM)
feedQuality_item <- summarise(feedQuality_item, ME_sd = sd(MEseo, na.rm = T), ME_min = min(MEseo, na.rm = T), ME_max = max(MEseo, na.rm = T), ME = mean(MEseo, na.rm = T), CP = mean(CP, na.rm = T), NDF = mean(NDF, na.rm =T), IVDMD = mean(IVDMD, na.rm = T), n = n())

feedQuality_EQUIP <- read.csv(paste0(CropParams_dir, '/feedQuality_EQUIP.csv'), stringsAsFactors = F)
feedQuality_EQUIP <- feedQuality_EQUIP[feedQuality_EQUIP$IVOMD > 10,]
DMI <- 5 #DMI has minimal effect on ME_DE. Set at a reasonable value
OM <- 0.9
#feedQuality_EQUIP$DE <- feedQuality_EQUIP$ME * (1/0.82) #ifelse(is.na(feedQuality_EQUIP$OM), 0.01*OM*(feedQuality_EQUIP$IVDMD + 12.9)*4.4 - 0.3, 0.01*feedQuality_EQUIP$OM*(feedQuality_EQUIP$IVDMD + 12.9)*4.4 - 0.3)*4.1868 #NRC (2001) using IVDMD as a proxy for IVOMD. converted from Mcal to MJ
#feedQuality_EQUIP$DE_min <- feedQuality_EQUIP$ME_min * (1/0.82) #ifelse(is.na(feedQuality_EQUIP$OM), 0.01*OM*(feedQuality_EQUIP$IVDMD + 12.9)*4.4 - 0.3, 0.01*feedQuality_EQUIP$OM*(feedQuality_EQUIP$IVDMD + 12.9)*4.4 - 0.3)*4.1868 #NRC (2001) using IVDMD as a proxy for IVOMD. converted from Mcal to MJ
#feedQuality_EQUIP$DE_max <- feedQuality_EQUIP$ME_max  * (1/0.82) #ifelse(is.na(feedQuality_EQUIP$OM), 0.01*OM*(feedQuality_EQUIP$IVDMD + 12.9)*4.4 - 0.3, 0.01*feedQuality_EQUIP$OM*(feedQuality_EQUIP$IVDMD + 12.9)*4.4 - 0.3)*4.1868 #NRC (2001) using IVDMD as a proxy for IVOMD. converted from Mcal to MJ
feedQuality_EQUIP$DE <- (0.01*OM*(feedQuality_EQUIP$IVOMD + 12.9)*4.4 - 0.3)*4.1868 #NRC (2001) using IVDMD as a proxy for IVOMD. converted from Mcal to MJ
feedQuality_EQUIP$DE_min <- (0.01*OM*(feedQuality_EQUIP$IVOMD_min + 12.9)*4.4 - 0.3)*4.1868 #NRC (2001) using IVDMD as a proxy for IVOMD. converted from Mcal to MJ
feedQuality_EQUIP$DE_max <- (0.01*OM*(feedQuality_EQUIP$IVOMD_max + 12.9)*4.4 - 0.3)*4.1868 #NRC (2001) using IVDMD as a proxy for IVOMD. converted from Mcal to MJ
feedQuality_EQUIP$ME_DE <- 0.9410+0.0042*DMI-0.0017*feedQuality_EQUIP$NDF-0.0022*feedQuality_EQUIP$CP 
feedQuality_EQUIP$MEseo <- feedQuality_EQUIP$DE * feedQuality_EQUIP$ME_DE
feedQuality_EQUIP$MEseo_min <- feedQuality_EQUIP$DE_min * feedQuality_EQUIP$ME_DE
feedQuality_EQUIP$MEseo_max <- feedQuality_EQUIP$DE_max * feedQuality_EQUIP$ME_DE
feedQuality_EQUIP_item <- group_by(feedQuality_EQUIP, codeSPAM)
feedQuality_EQUIP_item <- summarise(feedQuality_EQUIP_item, ME_sd = NA, ME_min = min(MEseo_min, na.rm = T), ME_max = max(MEseo_max, na.rm = T), ME = weighted.mean(MEseo, nSample, na.rm = T), CP = mean(CP, na.rm = T), NDF = mean(NDF, na.rm =T), IVDMD = mean(IVOMD, na.rm =T))
feedQuality_EQUIP_item <- feedQuality_EQUIP_item[feedQuality_EQUIP_item$codeSPAM != "",]

feedQuality_item <- bind_rows(feedQuality_item, feedQuality_EQUIP_item)

feedQuality_item <- rbind(feedQuality_item, c("smil", feedQuality_item$ME_sd[feedQuality_item$codeSPAM == "pmil"], feedQuality_item$ME_min[feedQuality_item$codeSPAM == "pmil"], feedQuality_item$ME_max[feedQuality_item$codeSPAM == "pmil"], feedQuality_item$ME[feedQuality_item$codeSPAM == "pmil"], feedQuality_item$CP[feedQuality_item$codeSPAM == "pmil"], feedQuality_item$NDF[feedQuality_item$codeSPAM == "pmil"], NA))
feedQuality_item <- rbind(feedQuality_item, c("ocer", feedQuality_item$ME_sd[feedQuality_item$codeSPAM == "pmil"], feedQuality_item$ME_min[feedQuality_item$codeSPAM == "pmil"], feedQuality_item$ME_max[feedQuality_item$codeSPAM == "pmil"], feedQuality_item$ME[feedQuality_item$codeSPAM == "pmil"], feedQuality_item$CP[feedQuality_item$codeSPAM == "pmil"], feedQuality_item$NDF[feedQuality_item$codeSPAM == "pmil"], NA))
feedQuality_item <- rbind(feedQuality_item, c("oleg", feedQuality_item$ME_sd[feedQuality_item$codeSPAM == "cowp"], feedQuality_item$ME_min[feedQuality_item$codeSPAM == "cowp"], feedQuality_item$ME_max[feedQuality_item$codeSPAM == "cowp"], feedQuality_item$ME[feedQuality_item$codeSPAM == "cowp"], feedQuality_item$CP[feedQuality_item$codeSPAM == "pmil"], feedQuality_item$NDF[feedQuality_item$codeSPAM == "pmil"], NA))
feedQuality_item$ME_min[feedQuality_item$codeSPAM == "rice"] <- 5.91 #From SSA feed DB - excluding outlier Gambia https://feedsdatabase.ilri.org/search/Rice%20straw?title=rice&field_scientific_name_value=&field_feed_type_tid=All&field_country_tid=All&combine=
feedQuality_item$ME_max[feedQuality_item$codeSPAM == "rice"] <- 8.42 #From SSA feed DB - excluding outlier Gambia
feedQuality_item$ME[feedQuality_item$codeSPAM == "rice"] <- 6.76 #From SSA feed DB - excluding outlier Gambia
feedQuality_item <- rbind(feedQuality_item, c("cass", NA, 4.18, 4.18, 4.18, NA, NA, NA, NA)) #Kiendrebeogo, et al 2019
feedQuality_item <- rbind(feedQuality_item, c("yams", NA, 4.18, 4.18, 4.18, NA, NA, NA, NA)) #!Cassava
feedQuality_item <- rbind(feedQuality_item, c("orts", NA, 4.18, 4.18, 4.18, NA, NA, NA, NA)) #!Cassava
feedQuality_item <- rbind(feedQuality_item, c("grou", 1.58, 7.2, 9.3, 8.5, NA, NA, NA, NA)) #Rahimi et al., 2021
feedQuality_item$codeSPAM[feedQuality_item$codeSPAM == "beetWhole"] <- "sugb"
feedQuality_item <- rbind(feedQuality_item, c("natPast", NA, 6.2, 6.8, 6.5, NA, NA, NA, NA)) #Rahimi et al., 2021 - Sudanian
feedQuality_item$ME <- as.numeric(feedQuality_item$ME)
feedQuality_item$ME_min <- as.numeric(feedQuality_item$ME_min)
feedQuality_item$ME_max <- as.numeric(feedQuality_item$ME_max)
#!'sunf' and other oil crops not included

write.csv(feedQuality_item, paste0(CropParams_dir, '/feedQuality_item.csv'))

stSPAM <- stack(list.files(path = paste0(datadir, '/SPAM2017/'), pattern = "_A_clip.tif$", full.names = T))
iSPAMcropArea <- sum(stSPAM, na.rm = T)

#Crop specific harvest index recipricol, utilisation and ME
crops <- toupper(c('bana','barl','bean','cass','chic','cowp','grou','lent','maiz','ocer','opul','orts','pmil','pige','plnt','pota','rape','rice','sesa','smil','sorg','soyb','sugb','sugc','sunf','swpo','temf','trof','vege','whea','yams'))
tmpCropIndex <- grep(pattern = paste(crops, collapse = "|"), names(stSPAM))
stCropMEmean <- stack()
stCropMEmin <- stack()
stCropMEmax <- stack()
stCropME_HI_utilmean <- stack()
stCropME_HI_utilmin <- stack()
stCropME_HI_utilmax <- stack()
stSPAMcropProp <- stack()
for(i in 1: length(crops)){
  tmpCropIndex <- grep(pattern = paste(crops[i], collapse = "|"), names(stSPAM))
  iSPAMtmpArea <- overlay(stSPAM[[tmpCropIndex]], fun = sum)
  #Create raster of ME values weighted by harvest index and utilisation for each cell where the crop is grown
  icrop <- stSPAM[[tmpCropIndex]]
  icrop[icrop >0] <- 1
  
  icropMean <- icrop * max(0,feedQuality_item$ME[feedQuality_item$codeSPAM == tolower(crops[i])])
  stCropMEmean <- stack(stCropMEmean, icropMean)
  
  icropMin <- icrop * max(0,feedQuality_item$ME_min[feedQuality_item$codeSPAM == tolower(crops[i])]) 
  stCropMEmin <- stack(stCropMEmin, icropMin)
  
  icropMax <- icrop * max(0,feedQuality_item$ME_max[feedQuality_item$codeSPAM == tolower(crops[i])])
  stCropMEmax <- stack(stCropMEmax, icropMax)
  
  icropMean <- icrop * max(0,feedQuality_item$ME[feedQuality_item$codeSPAM == tolower(crops[i])]) *(1 - cropHI$harvest_index[cropHI$codeSPAM == tolower(crops[i])]) * cropHI$utilMax[cropHI$codeSPAM == tolower(crops[i])]
  stCropME_HI_utilmean <- stack(stCropME_HI_utilmean, icropMean)
  
  icropMin <- icrop * max(0,feedQuality_item$ME_min[feedQuality_item$codeSPAM == tolower(crops[i])]) * (1 - cropHI$harvest_index[cropHI$codeSPAM == tolower(crops[i])]) * cropHI$utilMax[cropHI$codeSPAM == tolower(crops[i])]
  stCropME_HI_utilmin <- stack(stCropME_HI_utilmin, icropMin)
  
  icropMax <- icrop * max(0,feedQuality_item$ME_max[feedQuality_item$codeSPAM == tolower(crops[i])]) * (1 - cropHI$harvest_index[cropHI$codeSPAM == tolower(crops[i])]) * cropHI$utilMax[cropHI$codeSPAM == tolower(crops[i])]
  stCropME_HI_utilmax <- stack(stCropME_HI_utilmax, icropMax)
  
  stSPAMcropProp <- stack(stSPAMcropProp, iSPAMtmpArea/iSPAMcropArea)
  
  print(paste("Crop species", i))
}

stCropMEmean <- reclassify(stCropMEmean, c(-Inf, 0, NA))
stCropMEmin <- reclassify(stCropMEmin, c(-Inf, 0, NA))
stCropMEmax <- reclassify(stCropMEmax, c(-Inf, 0, NA))
stSPAMcropProp <- reclassify(stSPAMcropProp, c(-Inf, 0, NA))
cropMEmean <- weighted.mean(stCropMEmean, stSPAMcropProp, na.rm = T)
cropMEsd <- weighted.mean( (stCropMEmean-cropMEmean)^2, stSPAMcropProp, na.rm = T)
cropMEmin <- weighted.mean(stCropMEmin, stSPAMcropProp, na.rm = T)
cropMEmax <- weighted.mean(stCropMEmax, stSPAMcropProp, na.rm = T)

#Aggregate at the 3rd admin level and then export raster
aoi3$cropMEmean <- exact_extract(cropMEmean, aoi3, fun = "mean")
aoi3$cropMEmin <- exact_extract(cropMEmin, aoi3, fun = "mean")
aoi3$cropMEmax <- exact_extract(cropMEmax, aoi3, fun = "mean")

feedCropBurn <- stars::read_stars(paste0(datadir, '/Burned/burnCropsDekads.tif'))
st_rasterize(sf = aoi3[, "cropMEmean"], template = feedCropBurn, file = paste0(outdir, "/cropMEmean.tif"))
st_rasterize(sf = aoi3[, "cropMEmin"], template = feedCropBurn, file = paste0(outdir, "/cropMEmin.tif"))
st_rasterize(sf = aoi3[, "cropMEmax"], template = feedCropBurn, file = paste0(outdir, "/cropMEmax.tif"))

stCropME_HI_utilmean <- reclassify(stCropME_HI_utilmean, c(-Inf, 0, NA))
stCropME_HI_utilmin <- reclassify(stCropME_HI_utilmin, c(-Inf, 0, NA))
stCropME_HI_utilmax <- reclassify(stCropME_HI_utilmax, c(-Inf, 0, NA))
cropME_HI_utilmean <- weighted.mean(stCropME_HI_utilmean, stSPAMcropProp, na.rm = T)
cropME_HI_utilsd <- weighted.mean( (stCropME_HI_utilmean-cropME_HI_utilmean)^2, stSPAMcropProp, na.rm = T)
cropME_HI_utilmin <- weighted.mean(stCropME_HI_utilmin, stSPAMcropProp, na.rm = T)
cropME_HI_utilmax <- weighted.mean(stCropME_HI_utilmax, stSPAMcropProp, na.rm = T)

#Aggregate at the 3rd admin level and then export raster
aoi3$cropME_HI_utilmean <- exact_extract(cropME_HI_utilmean, aoi3, fun = "mean")
aoi3$cropME_HI_utilmin <- exact_extract(cropME_HI_utilmin, aoi3, fun = "mean")
aoi3$cropME_HI_utilmax <- exact_extract(cropME_HI_utilmax, aoi3, fun = "mean")

st_rasterize(sf = aoi3[, "cropME_HI_utilmean"], template = feedCropBurn, file = paste0(outdir, "/cropME_HI_utilmean.tif"))
st_rasterize(sf = aoi3[, "cropME_HI_utilmin"], template = feedCropBurn, file = paste0(outdir, "/cropME_HI_utilmin.tif"))
st_rasterize(sf = aoi3[, "cropME_HI_utilmax"], template = feedCropBurn, file = paste0(outdir, "/cropME_HI_utilmax.tif"))