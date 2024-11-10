# Data download

root <- "/Users/s2255815/Library/CloudStorage/OneDrive-UniversityofEdinburgh/Work/Projects/AU-IBAR/Spatial modelling workshop/Workshop_materials"

## # linux systems
## root <- "/home/Feed-balance-modeling-training"

## # for windows systems
## root <- "c:/Documents/Feed-balance-modeling-training"

# Install useful packages
install.packages("geodata")
install.packages("sf")
install.packages("RCurl")
install.packages("googledrive")
install.packages("httr")
install.packages("rvest")

library(geodata)
library(sf)

outdir <- paste0(root, "/Day_1/SpatialData/inputs/AdminBound"); dir.create(outdir, F, T)

admin_levels <- c("0", "1", "2")
for(admin_level in admin_levels){
  aoi <- geodata::gadm(country = "NGA", level=admin_level, path = paste0(outdir), version="latest") %>% sf::st_as_sf()
  write_sf(aoi, paste0(outdir, "/gadm40_NGA_", admin_level, ".shp"), append = FALSE)
}

#----------------------------------------------------
#----------------------------------------------------
# Aggregation zones
library(RCurl)
library(sf)

outdir <- paste0(root, "/Day_1//SpatialData/inputs/AggregationZones"); dir.create(outdir, F, T)

download.file(
  url = paste0("https://shapefiles.fews.net/LHZ/NG_LHZ_2018.zip"),
  destfile = paste0(outdir, "/NG_LHZ_2018.zip"), quiet = TRUE)

unzip(zipfile = paste0(outdir, "/NG_LHZ_2018.zip"), exdir = paste0(outdir, "/"))

# Other zonations maps
library(googledrive)
drive_deauth()
drive_user()
public_file <-  drive_get(as_id("10HsGspftDNgq-fjAjeUwB8QsmHZWUJyT"))
drive_download(public_file, path = paste0(outdir, "/Ecological_and_Feed_Distribution.zip"), overwrite = TRUE)
unzip(zipfile = paste0(outdir, "/Ecological_and_Feed_Distribution.zip"), exdir = paste0(outdir, "/"))

#----------------------------------------------------
#----------------------------------------------------
# Land use
library(RCurl)

outdir <- paste0(root, "/Day_1/SpatialData/inputs/Feed/LandUse"); dir.create(outdir, F, T)

# Land use classes of interest
land_use_classes <- c("Tree", "Grass", "Crops", "Shrub")

# Download the file
lapply(land_use_classes, function(land_use_class){

  if (!file.exists(paste0(outdir, "/PROBAV_LC100_global_v3.0.1_2019-nrt_", land_use_class, "-CoverFraction-layer_EPSG-4326.tif"))){

    cat("Downloading: ", land_use_class, "\n")

    download.file(
      url = paste0("https://zenodo.org/records/3939050/files/PROBAV_LC100_global_v3.0.1_2019-nrt_", land_use_class, "-CoverFraction-layer_EPSG-4326.tif"),
      destfile = paste0(outdir, "/PROBAV_LC100_global_v3.0.1_2019-nrt_", land_use_class, "-CoverFraction-layer_EPSG-4326.tif"), quiet = TRUE)

  }else{
    cat("File already exists: ", land_use_class, "\n")
  }
})


#----------------------------------------------------
#----------------------------------------------------
# Tree cover
library(RCurl)

outdir <- paste0(root, "/Day_1/SpatialData/inputs/TreeCover"); dir.create(outdir, F, T)

if (!file.exists(paste0(outdir, "/ps_africa_treecover_2019_100m_v1.0.tif"))){
  cat("Downloading ", "tree cover", "\n")
  download.file(
    url = paste0("https://zenodo.org/records/7764460/files/ps_africa_treecover_2019_100m_v1.0.tif"),
    destfile = paste0(outdir, "/ps_africa_treecover_2019_100m_v1.0.tif"), quiet = TRUE)
  }else{
    cat("File already exists: ", "tree cover", "\n")
  }

#----------------------------------------------------
#----------------------------------------------------
# Crop type
library(RCurl)

outdir <- paste0(root, "/Day_1/SpatialData/inputs/Feed/CropType"); dir.create(outdir, F, T)

download.file(
  url = paste0("https://www.dropbox.com/scl/fo/808qb807xw4olh8z5pagd/APYE4A4ApAbKhlcfWspRxcg/Global_Geotiff?e=1&file_subpath=%2Fspam2020V0r1_global_harvested_area&preview=spam2020V0r1_global_harvested_area.zip&rlkey=mkmj10j7ub49dzrqo4qec25gp&subfolder_nav_tracking=1&st=34q63fux&dl=1"),
  destfile = paste0(outdir, "/spam2020V1r0_global.zip"), quiet = TRUE)

# Unzip the downloaded file (only the specific zip inside the archive)
unzip(zipfile = paste0(outdir, "/spam2020V1r0_global.zip"),
      files = "spam2020V0r1_global_physical_area.zip",
      exdir = paste0(outdir))

# List all files in the folder
all_files <- list.files(paste0(outdir), full.names = TRUE)

# Identify files to remove (all files except the one to keep)
files_to_remove <- all_files[!basename(all_files) %in% "spam2020V0r1_global_physical_area.zip"]

# Remove the files
file.remove(files_to_remove)

# List all files in the folder
all_files <- list.files(paste0(outdir), full.names = TRUE)

# Unzip the second archive
unzip(zipfile = paste0(outdir, "/spam2020V0r1_global_physical_area.zip"),
      exdir = paste0(outdir, "/"))

# List all files in the folder
cropPhysicalArea <- list.files(paste0(outdir, "/spam2020V0r1_global_physical_area"), full.names = TRUE)

# Remove all files except beans and maize
cropPhysicalArea_to_remove <- grep("BEAN|MAIZ", cropPhysicalArea, invert = TRUE, value = TRUE)

file.remove(cropPhysicalArea_to_remove)

#----------------------------------------------------
#----------------------------------------------------
# Protected areas
#Libraries
library(RCurl)

options(timeout = 3600)

outdir <- paste0(root, "/Day_1/SpatialData/inputs/ProtectedAreas"); dir.create(outdir, F, T)

# Download the file
download.file(
  url = "https://d1gam3xoknrgr2.cloudfront.net/current/WDPA_WDOECM_Oct2024_Public_NGA_shp.zip",
  destfile = paste0(outdir, "/WDPA_WDOECM_Oct2024_Public_NGA_shp.zip"))

zipped_files <- c("WDPA_WDOECM_Oct2024_Public_NGA_shp_0.zip", "WDPA_WDOECM_Oct2024_Public_NGA_shp_1.zip", "WDPA_WDOECM_Oct2024_Public_NGA_shp_2.zip")

for (zipped_file in zipped_files){
  # Unzip the downloaded file (only the specific zip inside the archive)
  unzip(
    zipfile = paste0(outdir, "/WDPA_WDOECM_Oct2024_Public_NGA_shp.zip"),
    files = paste0(zipped_file),
    exdir = paste0(outdir))}

# List all files in the folder
all_files <- list.files(paste0(outdir), full.names = TRUE)

# Identify files to remove (all files except the one to keep)
files_to_remove <- all_files[!basename(all_files) %in% zipped_files]

# Remove the files
file.remove(files_to_remove)

# List all files in the folder
all_files <- list.files(paste0(outdir), full.names = TRUE)

for(zipped_file in zipped_files){

  file_name = basename(paste0(outdir, "/", zipped_file))
  folder_name <- sub("\\.zip$", "", file_name)

  # Unzip the second archive
  unzip(
    zipfile = paste0(outdir, "/", file_name),
    exdir = paste0(outdir, "/", folder_name))
}

# Use list.files() to search for files that end with "NGA_shp-polygons.shp"
shp_files <- list.files(outdir, pattern = "NGA_shp-polygons\\.shp$", recursive = TRUE, full.names = TRUE)

# Read all shapefiles into a list of sf objects
shp_files <- lapply(shp_files, sf::st_read)

# Combine all shapefiles into one
WDPA_WDOECM_Oct2024_Public_NGA <- do.call(rbind, shp_files)

# Write combined file
sf::st_write(WDPA_WDOECM_Oct2024_Public_NGA, paste0(outdir, "/WDPA_WDOECM_Oct2024_Public_NGA.shp"))

#----------------------------------------------------
#----------------------------------------------------
# Dry matter productivity
library(RCurl)

options(timeout=3600)
outdir <- paste0(root, "/Day_1/SpatialData/inputs/Feed/DMP"); dir.create(outdir, F, T)

# Download manifest
download.file(
  url <- "https://globalland.vito.be/download/manifest/dmp_300m_v1_10daily_netcdf/manifest_clms_global_dmp_300m_v1_10daily_netcdf_latest.txt",
  destfile = paste0(outdir, "/manifest_clms_global_dmp_300m_v1_10daily_netcdf_latest.txt"))

# Read in manifest
dmp_manifest <- readLines(paste0(outdir, "/manifest_clms_global_dmp_300m_v1_10daily_netcdf_latest.txt"))

# Select files of interest
#dmp_manifest_list <- grep("RT6_2023", dmp_manifest, fixed=TRUE, value=TRUE) #select a file for each day

# Combine the patterns to search for
patterns <- "RT5_2020|RT6_2020|RT6_2021|RT2_202211100000|RT2_202211200000|RT2_202211300000|RT2_202212100000|RT2_202212200000|RT2_202212310000|RT6_2022|RT6_2023"

# Use grep to search for any of the patterns in dmp_manifest
dmp_manifest_list <- grep(patterns, dmp_manifest, value=TRUE) #select a file for each day

# Define files to exclude
exclude_patterns <- "RT5_202007100000|RT5_202007200000|RT5_202007310000|RT5_202008100000|RT5_202008200000|RT5_202008310000|RT6_202211100000|RT6_202211200000"

# Exclude the specific files from the results
dmp_manifest_list <- grep(exclude_patterns, dmp_manifest_list, invert=TRUE, value=TRUE)


dmp_selected <- grep("202001100000|202001200000|202001310000", dmp_manifest_list, value=TRUE)

for (i in dmp_selected){
  # Extract the file name
  #file_name <- basename(sub("OLCI_V1.*", "OLCI_V1", i))
  
  file_name_base <- basename(i)
  
  filenamep1 <- substr(file_name_base, 1, 13)
  filenamep2 <- substr(file_name_base, 18, 29)
  
  file_name <- paste0(filenamep1, "RT6_", filenamep2, "_GLOBE_OLCI_V1.1.2.nc")
  
  if(!file.exists(paste0(outdir, "/", file_name))){
    cat("Downloading: ", file_name, "\n")
    download.file(url = i, destfile = paste0(outdir, "/", file_name))
  }else {
    cat("File already exists: ", file_name, "\n")
  }
}

#----------------------------------------------------
#----------------------------------------------------
# Phenology
library(httr)
library(rvest)

yearList <- c("2020", "2021", "2022", "2023")

for(year in yearList){

  outdir <- paste0(root, "/Day_1/SpatialData/inputs/PhenologyModis/", year); dir.create(outdir, F, T)

  # Modis URL
  url <- paste0("https://e4ftl01.cr.usgs.gov/MOTA/MCD12Q2.061/", year, ".01.01/")

  # Get the HTML content of the URL
  page <- read_html(url)

  # Extract href links
  links <- page %>% html_nodes("a") %>% html_attr("href")

  # Filter the links based on the specified patterns
  #pattern <- "h20v07|h20v08|h20v09|h21v07|h21v08|h21v09|h21v10|h22v07|h22v08|h22v09"
  pattern <- "h18v07|h18v08|h19v07|h19v08"
  hdf_links <- links[grep(pattern, links)]
  hdf_links <- hdf_links[grep("\\.hdf$", hdf_links)]

  #paste0(url, hdf_links[1])

  for (file in hdf_links){
  if (!file.exists(paste0(outdir, "/", file))) {

    # Define your Earthdata credentials
    username <- Sys.getenv("EARTHDATA_USERNAME") # replace with your Earthdata username
    password <- Sys.getenv("EARTHDATA_PASSWORD") # replace with your Earthdata password

    cat("Downloading: ", file, "\n")
    download.file(url = paste0("https://", username, ":", password, "@", url, file),
                  destfile = paste0(outdir, "/", file), quiet = TRUE)
  }else {
    cat("File already exists: ", file, "\n")
    }
  }
}

#----------------------------------------------------
#----------------------------------------------------
# Burned areas
library(RCurl)

options(timeout=3600)

# Doanload manifest
download.file(
  url <- "https://globalland.vito.be/download/manifest/ba_300m_v3_monthly_netcdf/manifest_clms_global_ba_300m_v3_monthly_netcdf_latest.txt",
  destfile = paste0(root, "/Day_1/SpatialData/inputs/Burned/manifest_clms_global_ba_300m_v3_monthly_netcdf_latest.txt"))

# Read in manifest
dmp_manifest <- readLines(paste0(root, "/Day_1/SpatialData/inputs/Burned/manifest_clms_global_ba_300m_v3_monthly_netcdf_latest.txt"))

yearList <- c("2020", "2021", "2022", "2023")

for(year in yearList){

  outdir <- paste0(root, "/Day_1/SpatialData/inputs/Burned/", year); dir.create(outdir, F, T)

  # Select files of interest
  dmp_manifest_list <- grep(paste0("NTC_", year), dmp_manifest, fixed=TRUE, value=TRUE) #select a file for each day
  for (i in dmp_manifest_list){
    # Extract the file name
    file_name <- basename(i)
    if(!file.exists(paste0(outdir, "/", file_name))){
      download.file(url = i, destfile = paste0(outdir, "/", file_name))

    }else{
      cat("File already exists:", file_name, "\n")
    }
  }
}

#----------------------------------------------------
#----------------------------------------------------
# Feed parameters
library(googledrive)

outdir <- paste0(root, "/Day_1/Tables/inputs/CropParams"); dir.create(outdir, F, T)

drive_deauth()
drive_user()

#folder link to id
public_folder = "https://drive.google.com/drive/folders/1SpB1p9i4MGU1gMahF4M3Uc-HZr8FGoqd"
folder_id = drive_get(as_id(public_folder))

#find files in folder
public_files = drive_ls(folder_id)

for(i in seq_along(public_files)){
  public_file <- public_files[i, ]
  file_name <- public_file$name
  drive_download(public_file, path = paste0(outdir, "/", file_name), overwrite = TRUE)
}

#----------------------------------------------------
#----------------------------------------------------
# Livestock population
library(RCurl)

outdir <- paste0(root, "/Day_1/SpatialData/inputs/GLW4"); dir.create(outdir, F, T)

speciesCategories <- c("CTL", "GTS", "SHP", "PGS")

for (speciesCategory in speciesCategories){

  producLink = paste0("https://storage.googleapis.com/fao-gismgr-glw4-2020-data/DATA/GLW4-2020/MAPSET/D-DA/GLW4-2020.D-DA.",speciesCategory,".tif")

  file_name <- basename(producLink)

  if (!file.exists(paste0(outdir, "/", file_name))){
    download.file(
    url = paste0("https://storage.googleapis.com/fao-gismgr-glw4-2020-data/DATA/GLW4-2020/MAPSET/D-DA/GLW4-2020.D-DA.",speciesCategory,".tif"),
    destfile = paste0(outdir, "/", file_name), quiet = TRUE)
  }else {
    cat("File already exists:", file_name, "\n")
  }
}

#----------------------------------------------------
#----------------------------------------------------
# Livestock parameters
library(googledrive)

outdir <- paste0(root, "/Day_1/Tables/inputs/LivestockParams"); dir.create(outdir, F, T)

drive_deauth()
drive_user()
  
#folder link to id
public_folder = "https://drive.google.com/drive/folders/1-3N_kmMgcHr_tayylSxlMJAr-2PBGFXd"
folder_id = drive_get(as_id(public_folder))

#find files in folder
public_files = drive_ls(folder_id)

for(i in seq_along(public_files)){
  public_file <- public_files[i, ]
  file_name <- public_file$name
  drive_download(public_file, path = paste0(outdir, "/", file_name), overwrite = TRUE)
}
