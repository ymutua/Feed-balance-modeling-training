# Exploring feed quality

# install.packages("dplyr")
# install.packages("rvest")
# install.packages("stringr")
# install.packages("readr")

# Load libraries
library(dplyr)
library(rvest)
library(stringr)
library(readr)

# study area
region <- "BurkinaFaso"

# Paths, directories
root <- "/Users/s2255815/Library/CloudStorage/OneDrive-UniversityofEdinburgh/Work/Projects/AU-IBAR/Spatial modelling workshop/Workshop_materials/Day_4"
datadir <- paste0(root, "/src/3Balance-estimates/", region, "/CropParams")
outdir <- paste0(root, "/src/3Balance-estimates/", region, "/CropParams"); dir.create(outdir, F, T)

feedQuality <- read_csv(paste0(datadir, "/feedQuality_SSAdb.csv"))

plot(feedQuality$ME ~ feedQuality$IVDMD)
summary(lm(feedQuality$ME ~ feedQuality$IVDMD))

feedQuality <- group_by(feedQuality, Feed_item, codeSPAM)
feedQuality_sum <- summarise(feedQuality, ME_SD = sd(ME, na.rm = T), ME = mean(ME, na.rm = T), n = n())

feedQuality <- group_by(feedQuality, codeBasket_grouped)
ssa_cat_quality_sum <- summarise(feedQuality, ME_SD = sd(ME, na.rm = T), ME = mean(ME, na.rm = T), n = n())
