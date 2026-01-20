## SENTINEL Project Manuscript R Script for Analyses
## Abbie Chapman, 20th April 2024 (edited Dec 2025)
## TOR calculations on updated SPAM data and crops identified in 0_Trade_Matrices
## This script was repeated for birds and mammals only and for threatened species.
## The scripts are identical apart from the species range inputs.

## Read in output of 0_Trade_Data_Read_In.R first.
## Trade data read in:
rm(list = ls())
library(dplyr)
options(scipen = 999)

setwd("~/Data/")
outdir = "Overlay Analysis/April_2024/"
cropdir = "SHEFS/0_RawData_Harry/Corrected_trade_wide/"
form_1_2 = paste0(outdir, "trade_data_all_sentinel_countries_summarised_1997_to_2017_15thAugust2023.csv") # note - still using these years
# as more recent years are unreliable at this stage
tradedata = read.csv(file = form_1_2)
tradedata = tradedata[-1]
head(tradedata)
# cols = crop, domestic_tonnes, country, mean_export_tonnes_all_reporters
# AKA = crop, domestic, focal country, exports

#### Bringing in the data required to compute trade-off risk:
library(raster); library(maptools); library(sp); library(readbulk);
library(rgdal); library(rgeos); library(ggplot2); library(snow); library(dplyr); library(viridis);
library(gridExtra); library(reshape2)

outdir
cropdata.dir = "MapSPAM/2020_version/spam2020V1r0_global_physical_area/" # using physical area data from MapSPAM for 2020
species.dir = "Species Richness/From_Adrienne/"

## 1. Reading in the data:
## 1a. Crop data:
cropfilelist = list.files(paste0(cropdata.dir), pattern = "*.tif", all.files = TRUE, full.names = FALSE)
# to ensure we have physical area, all technologies (irrigated/inputs/rainfed), this needs filtering
cropfilelist1 = cropfilelist[grepl("_A_*", cropfilelist)] # physical area
cropfilelist2 = cropfilelist1[grepl("A.tif", cropfilelist1)] # all technologies together (irrigated and rainfed)
# 46 crops (up from 42 in 2017 SSA specific SPAM)
cropfiles = paste0(cropdata.dir,dir(path = cropdata.dir, pattern = "*_A.tif", recursive = TRUE),sep="")

## Cropping to the SENTINEL project focal countries - this is slow to run so is commented out once run and saved:
#########
GlobalExtent <- extent(-180,180,-90,90)
outprj = CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs") # updated code for the proj6 transition
# 
# reference_GHA = readOGR("Country Outlines/gadm36_GHA_0.shp")
# extent_GHA <- extent(reference_GHA)
# reference_ETH = readOGR("Country Outlines/gadm36_ETH_0.shp")
# extent_ETH <- extent(reference_ETH)
# reference_ZMB = readOGR("Country Outlines/gadm36_ZMB_0.shp")
# extent_ZMB <- extent(reference_ZMB)
# 
# outname_GHA = "GHA_"
# out.dir_GHA = "MapSPAM/2020_version/"
# 
# cropping_production_GHA <- function(x){
#   r <- raster::raster(x)
#   raster::crs(r) = outprj
#   r = raster::extend(r, GlobalExtent)
#   r[is.na(r[])]<-0
#   r <- (raster::crop(r,extent_GHA))
#   r <- (raster::mask(x = r, mask = reference_GHA))
#   if (class(r) != "try-error"){
#     return(r)
#   }
# }
# 
# crop_area_GHA = raster::stack(lapply(1:46,function(i){
#     cropfile = cropfiles[i]
#     cropfile_raster <- cropping_production_GHA(cropfile)
#     outPath <- paste0(out.dir_GHA, outname_GHA, basename(cropfile))
#     writeRaster(x=cropfile_raster, filename = outPath, format = "GTiff", overwrite = TRUE)
#   }))
# 
# outname_ETH = "ETH_"
# out.dir_ETH = "MapSPAM/2020_version/"
# 
# cropping_production_ETH <- function(x){
#   r <- raster::raster(x)
#   raster::crs(r) = outprj
#   r = raster::extend(r, GlobalExtent)
#   r[is.na(r[])]<-0
#   r <- (raster::crop(r,extent_ETH))
#   r <- (raster::mask(x = r, mask = reference_ETH))
#   if (class(r) != "try-error"){
#     return(r)
#   }
# }
# 
# crop_area_ETH = stack(lapply(1:46,function(i){
#   cropfile = cropfiles[i]
#   cropfile_raster <- cropping_production_ETH(cropfile)
#   outPath <- paste0(out.dir_ETH, outname_ETH, basename(cropfile))
#   writeRaster(x=cropfile_raster, filename = outPath, format = "GTiff", overwrite = TRUE)
# }))
# 
# outname_ZMB = "ZMB_"
# out.dir_ZMB = "MapSPAM/2020_version/"
# 
# cropping_production_ZMB <- function(x){
#   r <- raster::raster(x)
#   raster::crs(r) = outprj
#   r = raster::extend(r, GlobalExtent)
#   r[is.na(r[])]<-0
#   r <- (raster::crop(r,extent_ZMB))
#   r <- (raster::mask(x = r, mask = reference_ZMB))
#   if (class(r) != "try-error"){
#     return(r)
#   }
# }
# 
# crop_area_ZMB = stack(lapply(1:46,function(i){
#   cropfile = cropfiles[i]
#   cropfile_raster <- cropping_production_ZMB(cropfile)
#   outPath <- paste0(out.dir_ZMB, outname_ZMB, basename(cropfile))
#   writeRaster(x=cropfile_raster, filename = outPath, format = "GTiff", overwrite = TRUE)
# }))
#####

# Physical area data read in:
cropdata.dir_2020 = "MapSPAM/2020_version/"
cropfiles_2020 <- paste(cropdata.dir_2020,dir(path = cropdata.dir_2020, pattern = "*.tif",recursive = TRUE),sep="")
CropFiles_2020_GHA <- cropfiles_2020[grepl("GHA_",cropfiles_2020)]
CropFiles_2020_GHA <- CropFiles_2020_GHA[!grepl("BM",CropFiles_2020_GHA)]
CropFiles_2020_GHA <- CropFiles_2020_GHA[!grepl("threatened",CropFiles_2020_GHA)]
CropFiles_2020_GHA <- CropFiles_2020_GHA[!grepl("aux",CropFiles_2020_GHA)]
CropFiles_2020_GHA <- CropFiles_2020_GHA[!grepl("all_crops",CropFiles_2020_GHA)]
CropFiles_2020_GHA <- CropFiles_2020_GHA[!grepl("traded",CropFiles_2020_GHA)]
CropFiles_2020_GHA <- CropFiles_2020_GHA[!grepl("domestic",CropFiles_2020_GHA)]
CropFiles_2020_GHA <- CropFiles_2020_GHA[!grepl("top10filtered",CropFiles_2020_GHA)]
CropFiles_2020_ETH <- cropfiles_2020[grepl("ETH_",cropfiles_2020)]
CropFiles_2020_ETH <- CropFiles_2020_ETH[!grepl("BM",CropFiles_2020_ETH)]
CropFiles_2020_ETH <- CropFiles_2020_ETH[!grepl("threatened",CropFiles_2020_ETH)]
CropFiles_2020_ETH <- CropFiles_2020_ETH[!grepl("aux",CropFiles_2020_ETH)]
CropFiles_2020_ETH <- CropFiles_2020_ETH[!grepl("all_crops",CropFiles_2020_ETH)]
CropFiles_2020_ETH <- CropFiles_2020_ETH[!grepl("traded",CropFiles_2020_ETH)]
CropFiles_2020_ETH <- CropFiles_2020_ETH[!grepl("domestic",CropFiles_2020_ETH)]
CropFiles_2020_ETH <- CropFiles_2020_ETH[!grepl("top10filtered",CropFiles_2020_ETH)]
CropFiles_2020_ZMB <- cropfiles_2020[grepl("ZMB_",cropfiles_2020)]
CropFiles_2020_ZMB <- CropFiles_2020_ZMB[!grepl("BM",CropFiles_2020_ZMB)]
CropFiles_2020_ZMB <- CropFiles_2020_ZMB[!grepl("threatened",CropFiles_2020_ZMB)]
CropFiles_2020_ZMB <- CropFiles_2020_ZMB[!grepl("aux",CropFiles_2020_ZMB)]
CropFiles_2020_ZMB <- CropFiles_2020_ZMB[!grepl("all_crops",CropFiles_2020_ZMB)]
CropFiles_2020_ZMB <- CropFiles_2020_ZMB[!grepl("traded",CropFiles_2020_ZMB)]
CropFiles_2020_ZMB <- CropFiles_2020_ZMB[!grepl("domestic",CropFiles_2020_ZMB)]
CropFiles_2020_ZMB <- CropFiles_2020_ZMB[!grepl("top10filtered",CropFiles_2020_ZMB)]
Stack_2020_GHA = stack(CropFiles_2020_GHA)
plot(Stack_2020_GHA)
all_GHA = sum(Stack_2020_GHA)
plot(all_GHA)
outname_GHA = "GHA_"
outPath = paste0(cropdata.dir_2020, outname_GHA, "all_crops")
# writeRaster(all_GHA, filename = outPath, format = "GTiff", overwrite = TRUE)
Stack_2020_ETH = stack(CropFiles_2020_ETH)
plot(Stack_2020_ETH)
all_ETH = sum(Stack_2020_ETH)
plot(all_ETH)
outname_ETH = "ETH_"
outPath = paste0(cropdata.dir_2020, outname_ETH, "all_crops")
# writeRaster(all_ETH, filename = outPath, format = "GTiff", overwrite = TRUE)
Stack_2020_ZMB = stack(CropFiles_2020_ZMB)
plot(Stack_2020_ZMB)
all_ZMB = sum(Stack_2020_ZMB)
plot(all_ZMB)
outname_ZMB = "ZMB_"
outPath = paste0(cropdata.dir_2020, outname_ZMB, "all_crops")
# writeRaster(all_ZMB, filename = outPath, format = "GTiff", overwrite = TRUE)

# Summing the crop data to get the total areas used to grow each crop in hectares
# for supporting information:
cropdat_GHA = Stack_2020_GHA
results_area_GHA = NULL
for (b in 1:nlayers(cropdat_GHA)) {
  r1 <- sum((values(cropdat_GHA[[b]])), na.rm = TRUE)
  results_area_GHA = rbind(results_area_GHA, r1, names(cropdat_GHA[[b]]))
}#}
# write.csv(results_area_GHA, paste0(outdir, "area_crops_GHA.csv"))
cropdat_ETH = Stack_2020_ETH
results_area_ETH = NULL
for (b in 1:nlayers(cropdat_ETH)) {
  r1 <- sum((values(cropdat_ETH[[b]])), na.rm = TRUE)
  results_area_ETH = rbind(results_area_ETH, r1, names(cropdat_ETH[[b]]))
}#}
# write.csv(results_area_ETH, paste0(outdir, "area_crops_ETH.csv"))
cropdat_ZMB = Stack_2020_ZMB
results_area_ZMB = NULL
for (b in 1:nlayers(cropdat_ZMB)) {
  r1 <- sum((values(cropdat_ZMB[[b]])), na.rm = TRUE)
  results_area_ZMB = rbind(results_area_ZMB, r1, names(cropdat_ZMB[[b]]))
}#}
 # write.csv(results_area_ZMB, paste0(outdir, "area_crops_ZMB.csv"))

#####
## 1b. Species richness data (from Adrienne Etard, lead author of Etard et al. citation in manuscript):
species.dir = "Species Richness/From_Adrienne/"
species.files = paste(species.dir, dir(path = species.dir, recursive = TRUE), sep = "")
head(species.files)
species.files = species.files[!grepl("code_shared", species.files)]
species.files = species.files[!grepl("READ_ME", species.files)]
species.files = species.files[!grepl("50k", species.files)]
species.files
species.files = species.files[!grepl("Sentinel_Countries", species.files)]

amphibians = readRDS(species.files[1])
raster::plot(amphibians)
res(amphibians)
unique(values(amphibians), na.rm = TRUE) # this has the unusual value in it
check = as.data.frame(amphibians)
check
(check1 = check %>% 
    arrange(desc(layer)) %>% 
    slice(1:5))
amphibians[amphibians >= 1440541056] <- NA # removing this erroneous value (checked with creator
# Adrienne Etard and this is an error but is in the ocean so does not affect this analysis
# - removed for completeness)

birds = readRDS(species.files[2])
raster::plot(birds)
unique(values(birds), na.rm = TRUE) # this has the weird value in it
check = as.data.frame(birds)
check
(check1 = check %>% 
    arrange(desc(layer)) %>% 
    slice(1:5))


mammals = readRDS(species.files[3])
raster::plot(mammals)
unique(values(mammals), na.rm = TRUE) # this has the weird value in it
check = as.data.frame(mammals)
check
(check1 = check %>% 
    arrange(desc(layer)) %>% 
    slice(1:5))

reptiles = readRDS(species.files[4])
raster::plot(reptiles)
unique(values(reptiles), na.rm = TRUE) # this has the weird value in it
check = as.data.frame(reptiles)
check
(check1 = check %>% 
    arrange(desc(layer)) %>% 
    slice(1:5))


vertstack = raster::stack(amphibians, birds, mammals, reptiles)
vertstack
raster::plot(vertstack)
vertebrates = raster::calc(vertstack, sum)
vertebrates
crs(vertebrates) = CRS('+proj=cea +lon_0=0 +lat_ts=30 +x_0=0 +y_0=0 +datum=WGS84 +ellps=WGS84 +units=m +no_defs')
vertebrates
str(vertebrates)
extent(vertebrates) 
raster::plot(vertebrates) # terra package kept crashing so sticking with raster as using WGS84 anyway so proj4 deprecation not yet an issue

# clipping the extent as otherwise the reprojection won't work
GlobalExtent <- extent(-17357530,17357530,-7332230,7332230)
vertebrates_extent_changed = crop(vertebrates, GlobalExtent)
plot(vertebrates_extent_changed)

# Clipping the vertebrate richness for the three focal countries:
# Reference  refers to name of the layer with the desired extent: 
reference_GHA = readOGR("Country Outlines/gadm36_GHA_0.shp")
# OutName represents the intended prefix for output files: 
outname_GHA = "GHA_"
# OutPrj represents the desired output projection:
outprj = CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs") # updated code for the proj6 transition
# GlobalExtent <- extent(-180,180,-90,90) - needed in metres - see above
extent_GHA <- extent(reference_GHA)
out.dir = paste0(species.dir, "Sentinel_Countries/")
vertebrate_richness_projected = raster::projectRaster(vertebrates_extent_changed, crs = outprj) # ok as proj4 has been deprecated but using wgs84 so 'safe' - see https://rdrr.io/github/rspatial/terra/man/project.html
vertebrate_richness_cropped = crop(vertebrate_richness_projected, extent_GHA)
vertebrate_richness_mask = mask(vertebrate_richness_cropped, reference_GHA)
plot(vertebrate_richness_mask)
outPath = paste0(out.dir, outname_GHA, "vertebrate_richness")
#writeRaster(vertebrate_richness_mask, filename = outPath, format = "GTiff", overwrite = TRUE)

# Reference  refers to name of the layer with the desired extent: 
reference_ETH = readOGR("Country Outlines/gadm36_ETH_0.shp")
# OutName represents the intended prefix for output files: 
outname_ETH = "ETH_"
# OutPrj represents the desired output projection:
outprj = CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs") # updated code for the proj6 transition
# GlobalExtent <- extent(-180,180,-90,90) - needed in metres - see above
extent_ETH <- extent(reference_ETH)
out.dir = paste0(species.dir, "Sentinel_Countries/")

vertebrate_richness_projected = raster::projectRaster(vertebrates_extent_changed, crs = outprj) # ok as proj4 has been deprecated but using wgs84 so 'safe' - see https://rdrr.io/github/rspatial/terra/man/project.html
vertebrate_richness_cropped_ETH = crop(vertebrate_richness_projected, extent_ETH)
vertebrate_richness_mask_ETH = mask(vertebrate_richness_cropped_ETH, reference_ETH)
plot(vertebrate_richness_mask_ETH)
outPath = paste0(out.dir, outname_ETH, "vertebrate_richness")
#writeRaster(vertebrate_richness_mask_ETH, filename = outPath, format = "GTiff", overwrite = TRUE)

# Reference  refers to name of the layer with the desired extent: 
reference_ZMB = readOGR("Country Outlines/gadm36_ZMB_0.shp")
# OutName represents the intended prefix for output files: 
outname_ZMB = "ZMB_"
# OutPrj represents the desired output projection:
outprj = CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs") # updated code for the proj6 transition
# GlobalExtent <- extent(-180,180,-90,90) - needed in metres - see above
extent_ZMB <- extent(reference_ZMB)
out.dir = paste0(species.dir, "Sentinel_Countries/")

vertebrate_richness_projected = raster::projectRaster(vertebrates_extent_changed, crs = outprj) # ok as proj4 has been deprecated but using wgs84 so 'safe' - see https://rdrr.io/github/rspatial/terra/man/project.html
vertebrate_richness_cropped_ZMB = crop(vertebrate_richness_projected, extent_ZMB)
vertebrate_richness_mask_ZMB = mask(vertebrate_richness_cropped_ZMB, reference_ZMB)
plot(vertebrate_richness_mask_ZMB)
outPath = paste0(out.dir, outname_ZMB, "vertebrate_richness")
#writeRaster(vertebrate_richness_mask_ZMB, filename = outPath, format = "GTiff", overwrite = TRUE)

#####
## 1c. Resampling the crop data for compatibility with the species richness data:

Stack_2020_GHA
Stack_2020_ETH
Stack_2020_ZMB

resampled_crops_GHA = resample(Stack_2020_GHA, method = "bilinear", y = vertebrate_richness_mask)
resampled_crops_summed_GHA = sum(resampled_crops_GHA) # summing for a crops combined maps

resampled_crops_ETH = resample(Stack_2020_ETH, method = "bilinear", y = vertebrate_richness_mask_ETH)
resampled_crops_summed_ETH = sum(resampled_crops_ETH) # summing for a crops combined maps

resampled_crops_ZMB = resample(Stack_2020_ZMB, method = "bilinear", y = vertebrate_richness_mask_ZMB)
resampled_crops_summed_ZMB = sum(resampled_crops_ZMB) # summing for a crops combined maps

#####
## 2. Computing the trade-off risk. Thanks to Dr Charlie Outhwaite for her help fixing the function
## which was being stopped by NaN.

ras_stack1 = resampled_crops_GHA
results_GHA = NULL
for (m in 1:nlayers(ras_stack1)) {
  r1 <- sum((values(vertebrate_richness_mask)) * (values((ras_stack1[[m]])/(area(ras_stack1[[m]])))), na.rm = TRUE) # if this gives me layers which now have the multiplied values, perhaps I can do the summing later
  r2 <- r1/sum(values(ras_stack1[[m]]), na.rm = TRUE) 
  print(r2)# and this should give me layers which I can now do the sum of
  if (is.nan(r2)) r2 = NA
  # if r2 is NaN call it NA
  results_GHA = rbind(results_GHA, r2)
  #return(results_GHA)
}#}

tor_GHA = data.frame(results_GHA)  
tor_GHA$crop = names(ras_stack1)
tor_GHA$crop = gsub("GHA_spam2020V2r1_SSA_A_", "", tor_GHA$crop, fixed = TRUE)
tor_GHA$crop = gsub("_A", "", tor_GHA$crop, fixed = TRUE)

head(tor_GHA) # per-crop values of TOR 

# To represent these in a way that is more easy to interpret (e.g. a value greater than one
# means one thing, and below one means another), we want to run a different version of this.

# First, I need to get the median species richness across all cropped areas in the country:
median_SR = median(values(vertebrate_richness_mask), na.rm = T)
# Next, I need the median area of the crops for this country (checked with ChatGPT whether this is 
# OK to do on the sum of the rasterstack or needing another approach - confirmed I need the total
# area per crop first and then the median)
out1 = numeric(nlayers(ras_stack1))
for (n in 1:nlayers(ras_stack1)) {
  out1[n] = sum(values(ras_stack1[[n]]), na.rm = T)}
median_area = median(out1, na.rm = T)
cell_area_raster <- area(ras_stack1) # varies from 98.7 to 100.3
median_cell_area <- median(values(cell_area_raster), na.rm = TRUE) # around 100 (99.7)

# Now, I want to use these values to get the TOR average expectation:
average_expectation_GHA = (median_area*(median_SR/median_cell_area))/(median_area)

# Now, what's are the values relative to average expectation?
head(tor_GHA)
tor_GHA$tor_GHA_relativised = tor_GHA$results_GHA/average_expectation_GHA
# Here, greater than one means above the average expectation and below one means below the average
# expectation.
# Above 1 for Ghana are: plantain, coconut, other oils, other roots, citrus, oil palm, rubber,
# cassava, banana, tropical fruits, and maize.

newdir = "~Documents/Manuscripts/Overlay Analysis/Revision/"
write.csv(tor_GHA, paste0(newdir, "tor_with_relative_GHA.csv"))

# For relative risk potential, I could take the risk potential again and then divide by the average expectation
# but with the average expectation calculated from just the top part of the equation too
average_expectation_TRP_GHA = (median_area*(median_SR/median_cell_area))
# (can add this to the risk potential calculated further down later)

## And now repeating all of the above for Ethiopia and Zambia (apologies - I would now put this in a function
## but was relatively new to coding when I started this work a few years ago now!):
ras_stack1 = resampled_crops_ETH
results_ETH = NULL
for (m in 1:nlayers(ras_stack1)) {
  r1 <- sum((values(vertebrate_richness_mask_ETH)) * (values((ras_stack1[[m]])/(area(ras_stack1[[m]])))), na.rm = TRUE) # if this gives me layers which now have the multiplied values, perhaps I can do the summing later
  r2 <- r1/sum(values(ras_stack1[[m]]), na.rm = TRUE) 
  print(r2)# and this should give me layers which I can now do the sum of
  if (is.nan(r2)) r2 = NA
  # if r2 is NaN call it NA
  results_ETH = rbind(results_ETH, r2)
  #return(results_ETH)
}#}

tor_ETH = data.frame(results_ETH)  
tor_ETH$crop = names(ras_stack1)
tor_ETH$crop = gsub("ETH_spam2020V2r1_SSA_A_", "", tor_ETH$crop, fixed = TRUE)
tor_ETH$crop = gsub("_A", "", tor_ETH$crop, fixed = TRUE)

# To represent these in a way that is more easy to interpret (e.g. a value greater than one
# means one thing, and below one means another), we want to run a different version of this.

# First, I need to get the median species richness across all cropped areas in the country:
median_SR = median(values(vertebrate_richness_mask_ETH), na.rm = T)
# Next, I need the median area of the crops for this country (checked with ChatGPT whether this is 
# OK to do on the sum of the rasterstack or needing another approach - confirmed I need the total
# area per crop first and then the median)
out1 = numeric(nlayers(ras_stack1))
for (n in 1:nlayers(ras_stack1)) {
  out1[n] = sum(values(ras_stack1[[n]]), na.rm = T)}
median_area = median(out1, na.rm = T)
cell_area_raster <- area(ras_stack1) # varies from 97.3 to 100.4
median_cell_area <- median(values(cell_area_raster), na.rm = TRUE) # around 100 (99.4)

# Now, I want to use these values to get the TOR average expectation:
average_expectation_ETH = (median_area*(median_SR/median_cell_area))/(median_area)

# Now, what's are the values relative to average expectation?
head(tor_ETH)
tor_ETH$tor_ETH_relativised = tor_ETH$results_ETH/average_expectation_ETH
# Here, greater than one means above the average expectation and below one means below the average
# expectation.
# For Ethiopia, the majority of crops are above the average expectation. Only
# sesame, plantain, pearl millet, cassava, cowpea, and coconut are below the average expectation.
write.csv(tor_ETH, paste0(newdir, "tor_with_relative_ETH.csv"))

# write.csv(tor_ETH, paste0(outdir, "tor_values_per_crop_noweights_ETH_20042024.csv"))

# for relative risk potential, I could take the risk potential again and then divide by the average expectation
# but with the average expectation calculated from just the top part of the equation too
average_expectation_TRP_ETH = (median_area*(median_SR/median_cell_area))
# can add this to the risk potential calculated further down later


ras_stack1 = resampled_crops_ZMB
results_ZMB = NULL
for (m in 1:nlayers(ras_stack1)) {
  r1 <- sum((values(vertebrate_richness_mask_ZMB)) * (values((ras_stack1[[m]])/(area(ras_stack1[[m]])))), na.rm = TRUE) # if this gives me layers which now have the multiplied values, perhaps I can do the summing later
  r2 <- r1/sum(values(ras_stack1[[m]]), na.rm = TRUE) 
  print(r2)# and this should give me layers which I can now do the sum of
  if (is.nan(r2)) r2 = NA
  # if r2 is NaN call it NA
  results_ZMB = rbind(results_ZMB, r2)
  #return(results_ZMB)
}#}

tor_ZMB = data.frame(results_ZMB)  
tor_ZMB$crop = names(ras_stack1)
tor_ZMB$crop = gsub("ZMB_spam2020V2r1_SSA_A_", "", tor_ZMB$crop, fixed = TRUE)
tor_ZMB$crop = gsub("_A", "", tor_ZMB$crop, fixed = TRUE)

ras_stack1 = resampled_crops_ZMB
results_ZMB = NULL
for (m in 1:nlayers(ras_stack1)) {
  r1 <- sum((values(vertebrate_richness_mask_ZMB)) * (values((ras_stack1[[m]])/(area(ras_stack1[[m]])))), na.rm = TRUE) # if this gives me layers which now have the multiplied values, perhaps I can do the summing later
  r2 <- r1/sum(values(ras_stack1[[m]]), na.rm = TRUE) 
  print(r2)# and this should give me layers which I can now do the sum of
  if (is.nan(r2)) r2 = NA
  # if r2 is NaN call it NA
  results_ZMB = rbind(results_ZMB, r2)
  #return(results_ETH)
}#}

tor_ZMB = data.frame(results_ZMB)  
tor_ZMB$crop = names(ras_stack1)
tor_ZMB$crop = gsub("ZMB_spam2020V2r1_SSA_A_", "", tor_ZMB$crop, fixed = TRUE)
tor_ZMB$crop = gsub("_A", "", tor_ZMB$crop, fixed = TRUE)

# To represent these in a way that is more easy to interpret (e.g. a value greater than one
# means one thing, and below one means another), we want to run a different version of this.

# First, I need to get the median species richness across all cropped areas in the country:
median_SR = median(values(vertebrate_richness_mask_ZMB), na.rm = T)
# Next, I need the median area of the crops for this country (checked with ChatGPT whether this is 
# OK to do on the sum of the rasterstack or needing another approach - confirmed I need the total
# area per crop first and then the median)
out1 = numeric(nlayers(ras_stack1))
for (n in 1:nlayers(ras_stack1)) {
  out1[n] = sum(values(ras_stack1[[n]]), na.rm = T)}
median_area = median(out1, na.rm = T)
cell_area_raster <- area(ras_stack1) # varies from 95.7 to 99.6
median_cell_area <- median(values(cell_area_raster), na.rm = TRUE) # around 100 (97.998)

# Now, I want to use these values to get the TOR average expectation:
average_expectation_ZMB = (median_area*(median_SR/median_cell_area))/(median_area)

# Now, what's are the values relative to average expectation?
head(tor_ZMB)
tor_ZMB$tor_ZMB_relativised = tor_ZMB$results_ZMB/average_expectation_ZMB
# Here, greater than one means above the average expectation and below one means below the average
# expectation.
# For Zambia, above expectation are: tea, coffee, wheat, banana, barley
write.csv(tor_ZMB, paste0(newdir, "tor_with_relative_ZMB.csv"))
# write.csv(tor_ZMB, paste0(outdir, "tor_values_per_crop_noweights_ZMB_20042024.csv"))

# for relative risk potential, I could take the risk potential again and then divide by the average expectation
# but with the average expectation calculated from just the top part of the equation too
average_expectation_TRP_ZMB = (median_area*(median_SR/median_cell_area))
# can add this to the risk potential calculated further down later

#####
## Tidying the trade-off risk data:

head(tor_GHA)
tor_GHA$crop
longcrop = c("banana", "barley", "bean", "cassava", "chickpea", "citrus", "coconut", "cocoa", "coffeearabica", "cotton",
             "cowpea", "groundnut", "lentil", "maize", "smallmillet", "cerealnes", "other fibre", "oilpalm", "onion",
             "other oil", "pulsenes", "other roots", "pigeonpea", "plantain", "pearlmillet",
             "potato", "rapeseed", "coffeerobusta", "rest of crops", "rice", "rubber", "sesame",
             "sorghum", "soybean", "sugarbeet", "sugarcane", "sunflower", "sweetpotato", "tea", "fruitnes",
             "tobacco", "tomato", "tropicalnes", "vegetablenes", "wheat", "yam")
str(longcrop)
tor_GHA$longcrop= longcrop
tor_ETH$longcrop= longcrop
tor_ZMB$longcrop= longcrop

tor_GHA$crop = gsub("GHA_spam2020_v1r0_global_", "", tor_GHA$crop, fixed = TRUE)
tor_ETH$crop = gsub("ETH_spam2020_v1r0_global_", "", tor_ETH$crop, fixed = TRUE)
tor_ZMB$crop = gsub("ZMB_spam2020_v1r0_global_", "", tor_ZMB$crop, fixed = TRUE)

# tor_GHA = tor_GHA[-2]
# tor_ETH = tor_ETH[-2]
# tor_ZMB = tor_ZMB[-2]

tor_GHA$country = "Ghana"
tor_ETH$country = "Ethiopia"
tor_ZMB$country = "Zambia"
colnames(tor_GHA) = c("tor", "crop_code", "relative_tor", "crop", "country")
colnames(tor_ETH) = c("tor", "crop_code", "relative_tor", "crop", "country")
colnames(tor_ZMB) = c("tor", "crop_code", "relative_tor", "crop", "country")

head(tor_GHA);head(tor_ETH); head(tor_ZMB)
tordata = rbind(tor_GHA, tor_ETH, tor_ZMB)

## 2a. Total risk potential (numerator only).

ras_stack1 = resampled_crops_GHA
results_GHA1 = NULL
for (n in 1:nlayers(ras_stack1)) {
  r1 <- sum((values(vertebrate_richness_mask)) * (values((ras_stack1[[n]])/(area(ras_stack1[[n]])))), na.rm = TRUE) # if this gives me layers which now have the multiplied values, perhaps I can do the summing later
  print(r1)# 
  if (is.nan(r1)) r1 = NA
  # if r2 is NaN call it NA
  results_GHA1 = rbind(results_GHA1, r1)
  #return(results_GHA)
}#}

riskpotential_GHA = data.frame(results_GHA1)  
riskpotential_GHA$crop = names(ras_stack1)
riskpotential_GHA$crop = gsub("GHA_spam2020V2r1_SSA_A_", "", riskpotential_GHA$crop, fixed = TRUE)
riskpotential_GHA$crop = gsub("_A", "", riskpotential_GHA$crop, fixed = TRUE)

head(riskpotential_GHA)
riskpotential_GHA$TRP_relativised = riskpotential_GHA$results_GHA1/average_expectation_TRP_GHA
write.csv(riskpotential_GHA, paste0(newdir, "total_risk_potential_GHA.csv"))

# write.csv(riskpotential_GHA, paste0(outdir, "total_risk_potential_GHA_20042024.csv"))

ras_stack1 = resampled_crops_ETH
results_ETH1 = NULL
for (n in 1:nlayers(ras_stack1)) {
  r1 <- sum((values(vertebrate_richness_mask_ETH)) * (values((ras_stack1[[n]])/(area(ras_stack1[[n]])))), na.rm = TRUE) # if this gives me layers which now have the multiplied values, perhaps I can do the summing later
  print(r1)# and this should give me layers which I can now do the sum of
  if (is.nan(r1)) r1 = NA
  results_ETH1 = rbind(results_ETH1, r1)
  #return(results_ETH)
}#}

riskpotential_ETH = data.frame(results_ETH1)  
riskpotential_ETH$crop = names(ras_stack1)
riskpotential_ETH$crop = gsub("ETH_spam2020V2r1_SSA_A_", "", riskpotential_ETH$crop, fixed = TRUE)
riskpotential_ETH$crop = gsub("_A", "", riskpotential_ETH$crop, fixed = TRUE)

head(riskpotential_ETH)
riskpotential_ETH$TRP_relativised = riskpotential_ETH$results_ETH1/average_expectation_TRP_ETH
write.csv(riskpotential_ETH, paste0(newdir, "total_risk_potential_ETH.csv"))

# write.csv(riskpotential_ETH, paste0(outdir, "total_risk_potential_ETH_20042024.csv"))

ras_stack1 = resampled_crops_ZMB
results_ZMB1 = NULL
for (n in 1:nlayers(ras_stack1)) {
  r1 <- sum((values(vertebrate_richness_mask_ZMB)) * (values((ras_stack1[[n]])/(area(ras_stack1[[n]])))), na.rm = TRUE) # if this gives me layers which now have the multiplied values, perhaps I can do the summing later
  print(r1)# and this should give me layers which I can now do the sum of
  if (is.nan(r1)) r1 = NA
  results_ZMB1 = rbind(results_ZMB1, r1)
  #return(results_ZMB)
}#}

riskpotential_ZMB = data.frame(results_ZMB1)  
riskpotential_ZMB$crop = names(ras_stack1)
riskpotential_ZMB$crop = gsub("ZMB_spam2020V2r1_SSA_A_", "", riskpotential_ZMB$crop, fixed = TRUE)
riskpotential_ZMB$crop = gsub("_A", "", riskpotential_ZMB$crop, fixed = TRUE)

head(riskpotential_ZMB)
riskpotential_ZMB$TRP_relativised = riskpotential_ZMB$results_ZMB1/average_expectation_TRP_ZMB
write.csv(riskpotential_ZMB, paste0(newdir, "total_risk_potential_ZMB.csv"))

# write.csv(riskpotential_ZMB, paste0(outdir, "total_risk_potential_ZMB_20042024.csv"))

head(riskpotential_GHA)
riskpotential_GHA$crop
longcrop
str(longcrop)
riskpotential_GHA$longcrop= longcrop
riskpotential_ETH$longcrop= longcrop
riskpotential_ZMB$longcrop= longcrop

riskpotential_GHA$crop = gsub("GHA_spam2020_v1r0_global_", "", riskpotential_GHA$crop, fixed = TRUE)
riskpotential_ETH$crop = gsub("ETH_spam2020_v1r0_global_", "", riskpotential_ETH$crop, fixed = TRUE)
riskpotential_ZMB$crop = gsub("ZMB_spam2020_v1r0_global_", "", riskpotential_ZMB$crop, fixed = TRUE)

riskpotential_GHA$country = "Ghana"
riskpotential_ETH$country = "Ethiopia"
riskpotential_ZMB$country = "Zambia"
colnames(riskpotential_GHA) = c("tor", "crop_code","relative_TRP", "crop", "country")
colnames(riskpotential_ETH) = c("tor", "crop_code","relative_TRP", "crop", "country")
colnames(riskpotential_ZMB) = c("tor", "crop_code", "relative_TRP","crop", "country")

head(riskpotential_GHA);head(riskpotential_ETH); head(riskpotential_ZMB)
riskpotentialdata = rbind(riskpotential_GHA, riskpotential_ETH, riskpotential_ZMB)

#####
## 3. Figures and data:

# First some tidying is needed:
# need to drop the 'other' crops as these are unfairly adding the areas of multiple crops
tor_GHA2 = filter(tor_GHA, longcrop != "cerealnes" & longcrop != "pulsenes" & longcrop != "fruitnes" &
                    longcrop != "tropicalnes" & longcrop != "vegetablenes")
tor_ETH2 = filter(tor_ETH, longcrop != "cerealnes" & longcrop != "pulsenes" & longcrop != "fruitnes" &
                    longcrop != "tropicalnes" & longcrop != "vegetablenes")
tor_ZMB2 = filter(tor_ZMB, longcrop != "cerealnes" & longcrop != "pulsenes" & longcrop != "fruitnes" &
                    longcrop != "tropicalnes" & longcrop != "vegetablenes")

tor_GHA3 = na.omit(tor_GHA2); str(tor_GHA3) # 28 crops remain
tor_ETH3 = na.omit(tor_ETH2); str(tor_ETH3) # 35 crops remain
tor_ZMB3 = na.omit(tor_ZMB2); str(tor_ZMB3) # 25 crops remain
# The NAs are crops which are not showing as produced in these countries (e.g. see maps plotted earlier)

library(ggplot2)

head(tordata)
head(tradedata)
alldata = full_join(tordata, tradedata, by = c("crop", "country"))
# checked examples using alldata vs tordata vs tradedata viewers and all ok

head(alldata)

## how much does each country produce/export?
(ghanasum = alldata %>% 
  filter(country == "Ghana") %>% 
  summarise(sum_exports = sum(total_export_tonnes_all_reporters, na.rm = TRUE), 
  sum_domestic = sum(domestic_tonnes, na.rm = TRUE)))

#  sum_exports sum_domestic
#1     1215850     36347274

(ethiopiasum = alldata %>% 
    filter(country == "Ethiopia") %>% 
    summarise(sum_exports = sum(total_export_tonnes_all_reporters, na.rm = TRUE), 
              sum_domestic = sum(domestic_tonnes, na.rm = TRUE)))

# sum_exports sum_domestic
#    870339.4     36085450

(zambiasum = alldata %>% 
    filter(country == "Zambia") %>% 
    summarise(sum_exports = sum(total_export_tonnes_all_reporters, na.rm = TRUE), 
              sum_domestic = sum(domestic_tonnes, na.rm = TRUE)))

#sum_exports sum_domestic
#1    814130.7     10627544

#####
# Setting up the domestic vs traded (threshold testing in another script):

head(alldata)
alldata$domtraded = "domestic"
alldata$domtraded[alldata$proportion_traded >10] <- "traded"
unique(alldata$crop)
alldata = filter(alldata, crop != "cerealnes" & crop != "other fibre" &
                   crop != "other oil" & crop != "pulsenes" & crop != "other roots"
                  & crop != "rest of crops" 
                 & crop != "fruitnes" & crop != "tropicalnes" & crop != "vegetablenes"
                 & crop != "rootnes" & crop != "broadbean" & crop != "buckwheat" 
                 & crop != "greenbean" & crop != "greenbroadbean" & crop != "millet" & crop != "stringbean")
unique(alldata$crop)
# Ghana  - robusta, 4.518038, relative 0.8921729
# Ethiopia - arabica, 3.986856, relative 1.246856
# Zambia - arabica, 6.139300, relative 1.033378

### Testing an idea to then proceed with full dataset:
# alldata_coffee = filter(alldata, crop == "coffee" | crop == "coffeearabica" | crop == "coffeerobusta")
# alldata_coffee
# alldata_coffee[7,]$tor = 4.518038
### Worked so doing on full dataset:
alldata1 = alldata
alldata1[112,]$tor = 4.518038
alldata1[112,]$relative_tor = 0.8921729
alldata1[112,]$crop_code = "COFF"
alldata1[113,]$tor = 3.986856
alldata1[113,]$relative_tor = 1.246856
alldata1[113,]$crop_code = "COFF"
alldata1[114,]$tor = 6.139300
alldata1[114,]$relative_tor = 1.033378
alldata1[114,]$crop_code = "COFF"

alldata1 = filter(alldata1, crop != "coffeearabica" & crop!= "coffeerobusta")
alldata = alldata1

# Quick checks on data for manuscript info:
ghanadat1 = filter(alldata, country == "Ghana")
ghanadat1 = na.omit(ghanadat1)
str(ghanadat1)
ghanadat1

ethiopiadat1 = filter(alldata, country == "Ethiopia")
ethiopiadat1 = na.omit(ethiopiadat1)
str(ethiopiadat1)
ethiopiadat1

zambiadat1 = filter(alldata, country == "Zambia")
zambiadat1 = na.omit(zambiadat1)
str(zambiadat1)
zambiadat1

alldata$label = paste(alldata$crop, ",", alldata$country)

ghanadata = filter(alldata, country == "Ghana")
ethiopiadata = filter(alldata, country == "Ethiopia")
zambiadata = filter(alldata, country == "Zambia")

# Setting out which of the crops are domestic vs traded:
ghanadata_dom = filter(ghanadata, domtraded == "domestic")
ghanadata_traded = filter(ghanadata, domtraded == "traded")
ghanadata_dom$crop
names(Stack_2020_GHA)
domcrops_GHA = subset(Stack_2020_GHA, c("GHA_spam2020_v1r0_global_A_BARL_A", "GHA_spam2020_v1r0_global_A_BEAN_A",
                                         "GHA_spam2020_v1r0_global_A_CASS_A", "GHA_spam2020_v1r0_global_A_CHIC_A",
                                        "GHA_spam2020_v1r0_global_A_CITR_A",
                                         "GHA_spam2020_v1r0_global_A_CNUT_A", "GHA_spam2020_v1r0_global_A_COWP_A",
                                        "GHA_spam2020_v1r0_global_A_GROU_A", "GHA_spam2020_v1r0_global_A_MAIZ_A",
                                        "GHA_spam2020_v1r0_global_A_OILP_A", "GHA_spam2020_v1r0_global_A_ONIO_A", 
                                        "GHA_spam2020_v1r0_global_A_PIGE_A", "GHA_spam2020_v1r0_global_A_PMIL_A",
                                        "GHA_spam2020_v1r0_global_A_PLNT_A", "GHA_spam2020_v1r0_global_A_MILL_A",
                                        "GHA_spam2020_v1r0_global_A_POTA_A", "GHA_spam2020_v1r0_global_A_RAPE_A",
                                         "GHA_spam2020_v1r0_global_A_RICE_A", "GHA_spam2020_v1r0_global_A_RUBB_A",
                                         "GHA_spam2020_v1r0_global_A_SORG_A", "GHA_spam2020_v1r0_global_A_SOYB_A",
                                        "GHA_spam2020_v1r0_global_A_SUGB_A", "GHA_spam2020_v1r0_global_A_SUGC_A",
                                         "GHA_spam2020_v1r0_global_A_SUNF_A", "GHA_spam2020_v1r0_global_A_SWPO_A",
                                        "GHA_spam2020_v1r0_global_A_TOBA_A","GHA_spam2020_v1r0_global_A_TOMA_A",
                                        "GHA_spam2020_v1r0_global_A_TEAS_A", "GHA_spam2020_v1r0_global_A_YAMS_A"))

domcrops_GHA1 = sum(domcrops_GHA)
plot(domcrops_GHA1)
outPath1 = paste0(cropdata.dir_2020, outname_GHA, "domestic_crops")
# writeRaster(domcrops_GHA1, filename = outPath1, format = "GTiff", overwrite = TRUE)
ghanadata_traded$crop
tradedcrops_GHA = subset(Stack_2020_GHA, c("GHA_spam2020_v1r0_global_A_BANA_A", "GHA_spam2020_v1r0_global_A_COCO_A",
                                           "GHA_spam2020_v1r0_global_A_COTT_A", "GHA_spam2020_v1r0_global_A_LENT_A",
                                           "GHA_spam2020_v1r0_global_A_SESA_A", "GHA_spam2020_v1r0_global_A_WHEA_A",
                                           "GHA_spam2020_v1r0_global_A_RCOF_A"))

tradedcrops_GHA1 = sum(tradedcrops_GHA)
plot(tradedcrops_GHA1)
outPath2 = paste0(cropdata.dir_2020, outname_GHA, "traded_crops")
# writeRaster(tradedcrops_GHA1, filename = outPath2, format = "GTiff", overwrite = TRUE)

ethiopiadata_dom = filter(ethiopiadata, domtraded == "domestic")
ethiopiadata_traded = filter(ethiopiadata, domtraded == "traded")
ethiopiadata_dom$crop
names(Stack_2020_ETH)
domcrops_ETH = subset(Stack_2020_ETH, c("ETH_spam2020_v1r0_global_A_BANA_A", "ETH_spam2020_v1r0_global_A_BARL_A",
                                        "ETH_spam2020_v1r0_global_A_CASS_A", "ETH_spam2020_v1r0_global_A_CHIC_A",
                                        "ETH_spam2020_v1r0_global_A_CITR_A",
                                        "ETH_spam2020_v1r0_global_A_CNUT_A", "ETH_spam2020_v1r0_global_A_COCO_A",
                                        "ETH_spam2020_v1r0_global_A_COTT_A", "ETH_spam2020_v1r0_global_A_COWP_A",
                                        "ETH_spam2020_v1r0_global_A_GROU_A", "ETH_spam2020_v1r0_global_A_LENT_A",
                                        "ETH_spam2020_v1r0_global_A_MAIZ_A", "ETH_spam2020_v1r0_global_A_MILL_A",
                                        "ETH_spam2020_v1r0_global_A_OILP_A", "ETH_spam2020_v1r0_global_A_ONIO_A",
                                        "ETH_spam2020_v1r0_global_A_PIGE_A",
                                        "ETH_spam2020_v1r0_global_A_PLNT_A", "ETH_spam2020_v1r0_global_A_PMIL_A",
                                        "ETH_spam2020_v1r0_global_A_POTA_A", "ETH_spam2020_v1r0_global_A_RAPE_A",
                                        "ETH_spam2020_v1r0_global_A_RICE_A", "ETH_spam2020_v1r0_global_A_RUBB_A",
                                        "ETH_spam2020_v1r0_global_A_SORG_A", "ETH_spam2020_v1r0_global_A_SUGB_A",
                                        "ETH_spam2020_v1r0_global_A_SUGC_A", "ETH_spam2020_v1r0_global_A_SUNF_A",
                                        "ETH_spam2020_v1r0_global_A_SWPO_A", "ETH_spam2020_v1r0_global_A_TOBA_A",
                                        "ETH_spam2020_v1r0_global_A_TOMA_A",
                                        "ETH_spam2020_v1r0_global_A_WHEA_A", "ETH_spam2020_v1r0_global_A_YAMS_A"))

domcrops_ETH1 = sum(domcrops_ETH)
plot(domcrops_ETH1)
outPath1 = paste0(cropdata.dir_2020, outname_ETH, "domestic_crops")
# writeRaster(domcrops_ETH1, filename = outPath1, format = "GTiff", overwrite = TRUE)
ethiopiadata_traded$crop
tradedcrops_ETH = subset(Stack_2020_ETH, c("ETH_spam2020_v1r0_global_A_BEAN_A", "ETH_spam2020_v1r0_global_A_SESA_A",
                                           "ETH_spam2020_v1r0_global_A_SOYB_A", 
                                           "ETH_spam2020_v1r0_global_A_TEAS_A", "ETH_spam2020_v1r0_global_A_COFF_A"))

tradedcrops_ETH1 = sum(tradedcrops_ETH)
plot(tradedcrops_ETH1)
outPath2 = paste0(cropdata.dir_2020, outname_ETH, "traded_crops")
# writeRaster(tradedcrops_ETH1, filename = outPath2, format = "GTiff", overwrite = TRUE)

zambiadata_dom = filter(zambiadata, domtraded == "domestic")
zambiadata_traded = filter(zambiadata, domtraded == "traded")
zambiadata_dom$crop
names(Stack_2020_ZMB)
domcrops_ZMB = subset(Stack_2020_ZMB, c("ZMB_spam2020_v1r0_global_A_BANA_A", "ZMB_spam2020_v1r0_global_A_BEAN_A",
                                        "ZMB_spam2020_v1r0_global_A_CASS_A", "ZMB_spam2020_v1r0_global_A_CHIC_A",
                                        "ZMB_spam2020_v1r0_global_A_CITR_A", "ZMB_spam2020_v1r0_global_A_CNUT_A",
                                        "ZMB_spam2020_v1r0_global_A_COWP_A", "ZMB_spam2020_v1r0_global_A_GROU_A",
                                        "ZMB_spam2020_v1r0_global_A_MILL_A", "ZMB_spam2020_v1r0_global_A_ONIO_A",
                                        "ZMB_spam2020_v1r0_global_A_PIGE_A", "ZMB_spam2020_v1r0_global_A_PLNT_A",
                                        "ZMB_spam2020_v1r0_global_A_PMIL_A", "ZMB_spam2020_v1r0_global_A_POTA_A",
                                        "ZMB_spam2020_v1r0_global_A_RAPE_A", "ZMB_spam2020_v1r0_global_A_RICE_A",
                                        "ZMB_spam2020_v1r0_global_A_RUBB_A", "ZMB_spam2020_v1r0_global_A_SESA_A",
                                        "ZMB_spam2020_v1r0_global_A_SORG_A", "ZMB_spam2020_v1r0_global_A_SUGC_A",
                                        "ZMB_spam2020_v1r0_global_A_SUNF_A", "ZMB_spam2020_v1r0_global_A_SWPO_A",
                                        "ZMB_spam2020_v1r0_global_A_TOBA_A", "ZMB_spam2020_v1r0_global_A_TOMA_A",
                                        "ZMB_spam2020_v1r0_global_A_WHEA_A", "ZMB_spam2020_v1r0_global_A_YAMS_A",
                                        "ZMB_spam2020_v1r0_global_A_COFF_A"))

domcrops_ZMB1 = sum(domcrops_ZMB)
plot(domcrops_ZMB1)
outPath1 = paste0(cropdata.dir_2020, outname_ZMB, "domestic_crops")
# writeRaster(domcrops_ZMB1, filename = outPath1, format = "GTiff", overwrite = TRUE)
zambiadata_traded$crop
tradedcrops_ZMB = subset(Stack_2020_ZMB, c("ZMB_spam2020_v1r0_global_A_BARL_A", "ZMB_spam2020_v1r0_global_A_COCO_A",
                                           "ZMB_spam2020_v1r0_global_A_COTT_A", "ZMB_spam2020_v1r0_global_A_LENT_A",
                                           "ZMB_spam2020_v1r0_global_A_MAIZ_A", "ZMB_spam2020_v1r0_global_A_OILP_A",
                                           "ZMB_spam2020_v1r0_global_A_SOYB_A", "ZMB_spam2020_v1r0_global_A_SUGB_A",
                                           "ZMB_spam2020_v1r0_global_A_TEAS_A"))

tradedcrops_ZMB1 = sum(tradedcrops_ZMB)
plot(tradedcrops_ZMB1)
outPath2 = paste0(cropdata.dir_2020, outname_ZMB, "traded_crops")
# writeRaster(tradedcrops_ZMB1, filename = outPath2, format = "GTiff", overwrite = TRUE)

#### 
# Figure scripting was originally here but is now in separate code, so the figures below aren't the final ones. 

# Figure: Panel - trade-off risk:

# dir.2025 = "Overlay Analysis/Jan2025/"
#write.csv(alldata, paste0(dir.2025, "alldata_tor_verts.csv"))
# write.csv(alldata, paste0(newdir, "alldata_tor_verts.csv"))

# get the max and min crops
(alldata_summary = alldata %>% 
  group_by(country, domtraded) %>% 
  filter(tor == max(tor, na.rm = TRUE)))
(alldata_summary_min = alldata %>% 
    group_by(country, domtraded) %>% 
    filter(tor == min(tor, na.rm = TRUE)))

(plot1_all_tor = alldata %>% 
    ggplot(aes(x=domtraded,y=relative_tor, label = crop))+
    geom_boxplot(aes(width=.5, fill = domtraded), notch = TRUE, notchwidth = 0.9,
                 coef = NULL)+ # coef = NULL sets the whiskers to the min and max values
    # notching adds a way of comparing the medians between plots
    scale_fill_futurama() +
    facet_wrap(~country) +
    theme_bw()+
    labs(x = "Crop Group", y = "Trade-Off Risk Relative to Median Expectation") +
    geom_hline(yintercept = 1, linetype = "dotted") +
    theme(legend.position="none",
          text = element_text(family="sans"), # sans serif
          axis.title.y = element_text(size = 12, vjust = 2, face = "bold"), # ensuring position of axis label is away from the ticks
          axis.title.x = element_text(size = 12, vjust = -2, face = "bold"),
          axis.text.y = element_text(size = 12),
          axis.ticks.length.x = unit(-0.1, "cm"), # length of tick marks - negative sign places ticks inwards
          axis.ticks = element_blank(),
          axis.text.x = element_text(size = 12),
          panel.background = element_blank(),
          panel.grid.major = element_line(color = "gray93"), # adding the lines to show the log scale
          axis.line = element_line(color = "black"),
          plot.margin = margin(10,10,10,20),
          strip.text.x = element_text(margin = margin(2,0,2,0), size = 12), strip.background = element_rect(colour = "black", fill = "gray93"),
          panel.border = element_rect(colour = "black", fill = NA)))

head(alldata)
# want a Welch two sample t-test to compare two treatments: domestic and traded
# assumes that the variance of two groups is unequal so no changes needed first (https://www.datacamp.com/tutorial/t-tests-r-tutorial)
# want the numeric first then category column
t.test(tor~domtraded, data = alldata)
# separating the countries:
t.test(tor~domtraded, data = ghanadata)
t.test(tor~domtraded, data = ethiopiadata)
t.test(tor~domtraded, data = zambiadata)

# what is the effect size of the crops?
(ghanaeffect = ghanadata %>% 
  group_by(domtraded) %>% 
  summarise(mean = mean(tor, na.rm = TRUE)))
(ghana_effect = 4.95-4.78) # 0.17
# crops of interest seem to vary more though so how can we summarise the variance?
(ghana_summary_crops = ghanadata %>% 
    summarise(mean = mean(tor, na.rm = TRUE), sd = sd(tor, na.rm = TRUE), 
              median = median(tor, na.rm = TRUE),
              iqr = IQR(tor, na.rm = TRUE), range = (max(tor, na.rm = TRUE)-min(tor, na.rm = TRUE))))
# 29 crops,      mean        sd   median       iqr    range
#             4.91631 0.3777798 4.908125 0.7340241 1.142483
(ghana_summary_bygroup = ghanadata %>% 
    group_by(domtraded) %>% 
    summarise(mean = mean(tor, na.rm = TRUE), sd = sd(tor, na.rm = TRUE), 
              median = median(tor, na.rm = TRUE),
              iqr = IQR(tor, na.rm = TRUE), range = (max(tor, na.rm = TRUE)-min(tor, na.rm = TRUE))))

# what is the effect size of the crops?
(ethiopiaeffect = ethiopiadata %>% 
    group_by(domtraded) %>% 
    summarise(mean = mean(tor, na.rm = TRUE)))
(ethiopia_effect = 3.41-3.78) # -0.37
# crops of interest seem to vary more though so how can we summarise the variance?
(ethiopia_summary_crops = ethiopiadata %>% 
    summarise(mean = mean(tor, na.rm = TRUE), sd = sd(tor, na.rm = TRUE), 
              median = median(tor, na.rm = TRUE),
              iqr = IQR(tor, na.rm = TRUE), range = (max(tor, na.rm = TRUE)-min(tor, na.rm = TRUE)) 
              ))
# 29 crops,       mean        sd   median       iqr    range
#              3.468721 0.8993117 3.664672 0.5227026 3.802547
(ethiopia_summary_bygroup = ethiopiadata %>% 
    group_by(domtraded) %>% 
    summarise(mean = mean(tor, na.rm = TRUE), sd = sd(tor, na.rm = TRUE), 
              median = median(tor, na.rm = TRUE),
              iqr = IQR(tor, na.rm = TRUE), range = (max(tor, na.rm = TRUE)-min(tor, na.rm = TRUE))))

# what is the effect size of the crops?
(zambiaeffect = zambiadata %>% 
    group_by(domtraded) %>% 
    summarise(mean = mean(tor, na.rm = TRUE)))
(zambia_effect = 5.76-5.88) # -0.12
# crops of interest seem to vary more though so how can we summarise the variance?
(zambia_summary_crops = zambiadata %>% 
    summarise(mean = mean(tor, na.rm = TRUE), sd = sd(tor, na.rm = TRUE), 
              median = median(tor, na.rm = TRUE),
              iqr = IQR(tor, na.rm = TRUE), range = (max(tor, na.rm = TRUE)-min(tor, na.rm = TRUE)) 
              ))
# 28 crops,       mean        sd   median       iqr     range
#               5.788319 0.2227215 5.792967 0.2533674 0.7980017
(zambia_summary_bygroup = zambiadata %>% 
    group_by(domtraded) %>% 
    summarise(mean = mean(tor, na.rm = TRUE), sd = sd(tor, na.rm = TRUE), 
              median = median(tor, na.rm = TRUE),
              iqr = IQR(tor, na.rm = TRUE), range = (max(tor, na.rm = TRUE)-min(tor, na.rm = TRUE))))


#####
# Preparing data ready for mapping in ArcGIS:
## sort by richness
## map proportions (scaled symbology) for top 10% of richness

sprich_GHA = as.data.frame(vertebrate_richness_mask, row.names = NULL, optional = FALSE, xy = TRUE, na.rm = FALSE, long = FALSE)
str(sprich_GHA)
colnames(sprich_GHA) = c("x", "y", "sprich")
crop_GHA = as.data.frame(resampled_crops_GHA, row.names = NULL, optional = FALSE, xy = TRUE, na.rm = FALSE, long = FALSE)
str(crop_GHA)

ghanadata1 = cbind(sprich_GHA, crop_GHA)
## order by richness
ghanadataorder = ghanadata1[order(ghanadata1$sprich, na.last = TRUE, decreasing = TRUE),] # ordering by richness
head(ghanadataorder)

## cut off the data at top 10% of richness and then rasterise it and export ready for mapping in Arc
ghanatop10 = ghanadataorder[ghanadataorder$sprich > quantile(ghanadataorder$sprich,prob=1-10/100, na.rm = TRUE),]
summary(ghanatop10$sprich)
summary(ghanadataorder$sprich)
# write.csv(ghanatop10, file = paste0(outdir, "ghana_top10percentsprich_updateapril2024.csv"))
head(ghanatop10) # x and y columns repeated
colnames(ghanatop10)
ghanatop10a = ghanatop10[, !duplicated(colnames(ghanatop10), fromLast = FALSE)] 
head(ghanatop10a)
summary(ghanatop10a)
ghanatop10b = ghanatop10a[!is.na(ghanatop10a$x),]
summary(ghanatop10b$x) 
str(ghanatop10a)
str(ghanatop10b)

ghanarasttest = rasterFromXYZ(ghanatop10b, res = c(0.1040,0.0786), crs = " +proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0 " , digits = 5)
plot(ghanarasttest)
#writeRaster(ghanarasttest, file = paste0(outdir, "raster_ghanatop10_updateapril2024", names(ghanarasttest)), format = "GTiff", bylayer = TRUE)
ghana_stack = stack(ghanarasttest)
ghana_stack

domcrops_GHA_top10 = subset(ghana_stack, c("GHA_spam2020_v1r0_global_A_BARL_A", "GHA_spam2020_v1r0_global_A_BEAN_A",
                                           "GHA_spam2020_v1r0_global_A_CASS_A", "GHA_spam2020_v1r0_global_A_CHIC_A",
                                           "GHA_spam2020_v1r0_global_A_CITR_A",
                                           "GHA_spam2020_v1r0_global_A_CNUT_A", "GHA_spam2020_v1r0_global_A_COWP_A",
                                           "GHA_spam2020_v1r0_global_A_GROU_A", "GHA_spam2020_v1r0_global_A_MAIZ_A",
                                           "GHA_spam2020_v1r0_global_A_OILP_A", "GHA_spam2020_v1r0_global_A_ONIO_A", 
                                           "GHA_spam2020_v1r0_global_A_PIGE_A", "GHA_spam2020_v1r0_global_A_PMIL_A",
                                           "GHA_spam2020_v1r0_global_A_PLNT_A", "GHA_spam2020_v1r0_global_A_MILL_A",
                                           "GHA_spam2020_v1r0_global_A_POTA_A", "GHA_spam2020_v1r0_global_A_RAPE_A",
                                           "GHA_spam2020_v1r0_global_A_RICE_A", "GHA_spam2020_v1r0_global_A_RUBB_A",
                                           "GHA_spam2020_v1r0_global_A_SORG_A", "GHA_spam2020_v1r0_global_A_SOYB_A",
                                           "GHA_spam2020_v1r0_global_A_SUGB_A", "GHA_spam2020_v1r0_global_A_SUGC_A",
                                           "GHA_spam2020_v1r0_global_A_SUNF_A", "GHA_spam2020_v1r0_global_A_SWPO_A",
                                           "GHA_spam2020_v1r0_global_A_TOBA_A","GHA_spam2020_v1r0_global_A_TOMA_A",
                                           "GHA_spam2020_v1r0_global_A_TEAS_A", "GHA_spam2020_v1r0_global_A_YAMS_A"))

domcrops_GHA_top10 = sum(domcrops_GHA_top10)
plot(domcrops_GHA_top10)
outPath1 = paste0(cropdata.dir_2020, outname_GHA, "domestic_crops_top10filtered")
# writeRaster(domcrops_GHA_top10, filename = outPath1, format = "GTiff", overwrite = TRUE)

tradedcrops_GHA_top10 = subset(ghana_stack, c("GHA_spam2020_v1r0_global_A_BANA_A", "GHA_spam2020_v1r0_global_A_COCO_A",
                                              "GHA_spam2020_v1r0_global_A_COTT_A", "GHA_spam2020_v1r0_global_A_LENT_A",
                                              "GHA_spam2020_v1r0_global_A_SESA_A", "GHA_spam2020_v1r0_global_A_WHEA_A",
                                              "GHA_spam2020_v1r0_global_A_RCOF_A"))

tradedcrops_GHA_top10 = sum(tradedcrops_GHA_top10)
plot(tradedcrops_GHA_top10)
outPath2 = paste0(cropdata.dir_2020, outname_GHA, "traded_crops_top10filtered")
 # writeRaster(tradedcrops_GHA_top10, filename = outPath2, format = "GTiff", overwrite = TRUE)

#writeRaster(ghanarasttest, file = paste0(outdir, "raster_ghanatop10_updateaug2023", names(ghanarasttest)), format = "GTiff", bylayer = TRUE)
# should be able to plot these in Arc now 

## For mapping in Arc, need to know the full range for a customised scale
(min_GHA = min(ghanatop10b, na.rm = TRUE)) # -2.212347
(max_GHA = max(ghanatop10b, na.rm = TRUE)) # 3076.036
(quant_GHA = quantile(ghanadata1, na.rm = TRUE))
#0%          25%          50%          75%         100% 
#-39.9840677    0.0000000    0.3876531   14.2230424 5052.4899316 

sprich_ETH = as.data.frame(vertebrate_richness_mask_ETH, row.names = NULL, optional = FALSE, xy = TRUE, na.rm = FALSE, long = FALSE)
str(sprich_ETH)
colnames(sprich_ETH) = c("x", "y", "sprich")
crop_ETH = as.data.frame(resampled_crops_ETH, row.names = NULL, optional = FALSE, xy = TRUE, na.rm = FALSE, long = FALSE)
str(crop_ETH)

ethiopiadata1 = cbind(sprich_ETH, crop_ETH)
## order by richness
ethiopiadataorder = ethiopiadata1[order(ethiopiadata1$sprich, na.last = TRUE, decreasing = TRUE),] # ordering by richness
head(ethiopiadataorder)

## Cut off the data at top 10% of richness and then rasterise it and export ready for mapping in Arc
ethiopiatop10 = ethiopiadataorder[ethiopiadataorder$sprich > quantile(ethiopiadataorder$sprich,prob=1-10/100, na.rm = TRUE),]
summary(ethiopiatop10$sprich)
summary(ethiopiadataorder$sprich)
# write.csv(ethiopiatop10, file = paste0(outdir, "ethiopia_top10percentsprich_updateapr2024.csv"))
head(ethiopiatop10) # x and y columns repeated
colnames(ethiopiatop10)
ethiopiatop10a = ethiopiatop10[, !duplicated(colnames(ethiopiatop10), fromLast = FALSE)] 
head(ethiopiatop10a)
summary(ethiopiatop10a)
ethiopiatop10b = ethiopiatop10a[!is.na(ethiopiatop10a$x),]
summary(ethiopiatop10b$x) 

ethiopiarasttest = rasterFromXYZ(ethiopiatop10b, res = c(0.1040,0.0786), crs = " +proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0 " , digits = 5)
plot(ethiopiarasttest)
# writeRaster(ethiopiarasttest, file = paste0(outdir, "raster_ethiopiatop10_updateapril2024", names(ethiopiarasttest)), format = "GTiff", bylayer = TRUE)
# should be able to plot these in Arc now 
plot(ethiopiarasttest)
ethiopia_stack = stack(ethiopiarasttest)
ethiopia_stack

domcrops_ETH_top10 = subset(ethiopia_stack, c("ETH_spam2020_v1r0_global_A_BANA_A", "ETH_spam2020_v1r0_global_A_BARL_A",
                                              "ETH_spam2020_v1r0_global_A_CASS_A", "ETH_spam2020_v1r0_global_A_CHIC_A",
                                              "ETH_spam2020_v1r0_global_A_CITR_A",
                                              "ETH_spam2020_v1r0_global_A_CNUT_A", "ETH_spam2020_v1r0_global_A_COCO_A",
                                              "ETH_spam2020_v1r0_global_A_COTT_A", "ETH_spam2020_v1r0_global_A_COWP_A",
                                              "ETH_spam2020_v1r0_global_A_GROU_A", "ETH_spam2020_v1r0_global_A_LENT_A",
                                              "ETH_spam2020_v1r0_global_A_MAIZ_A", "ETH_spam2020_v1r0_global_A_MILL_A",
                                              "ETH_spam2020_v1r0_global_A_OILP_A", "ETH_spam2020_v1r0_global_A_ONIO_A",
                                              "ETH_spam2020_v1r0_global_A_PIGE_A",
                                              "ETH_spam2020_v1r0_global_A_PLNT_A", "ETH_spam2020_v1r0_global_A_PMIL_A",
                                              "ETH_spam2020_v1r0_global_A_POTA_A", "ETH_spam2020_v1r0_global_A_RAPE_A",
                                              "ETH_spam2020_v1r0_global_A_RICE_A", "ETH_spam2020_v1r0_global_A_RUBB_A",
                                              "ETH_spam2020_v1r0_global_A_SORG_A", "ETH_spam2020_v1r0_global_A_SUGB_A",
                                              "ETH_spam2020_v1r0_global_A_SUGC_A", "ETH_spam2020_v1r0_global_A_SUNF_A",
                                              "ETH_spam2020_v1r0_global_A_SWPO_A", "ETH_spam2020_v1r0_global_A_TOBA_A",
                                              "ETH_spam2020_v1r0_global_A_TOMA_A",
                                              "ETH_spam2020_v1r0_global_A_WHEA_A", "ETH_spam2020_v1r0_global_A_YAMS_A"))

domcrops_ETH_top10 = sum(domcrops_ETH_top10)
plot(domcrops_ETH_top10)
outPath1 = paste0(cropdata.dir_2020, outname_ETH, "domestic_crops_top10filtered")
# writeRaster(domcrops_ETH_top10, filename = outPath1, format = "GTiff", overwrite = TRUE)

tradedcrops_ETH_top10 = subset(ethiopia_stack, c("ETH_spam2020_v1r0_global_A_BEAN_A", "ETH_spam2020_v1r0_global_A_SESA_A",
                                                 "ETH_spam2020_v1r0_global_A_SOYB_A", 
                                                 "ETH_spam2020_v1r0_global_A_TEAS_A", "ETH_spam2020_v1r0_global_A_COFF_A"))

tradedcrops_ETH_top10 = sum(tradedcrops_ETH_top10)
plot(tradedcrops_ETH_top10)
outPath2 = paste0(cropdata.dir_2020, outname_ETH, "traded_crops_top10filtered")
# writeRaster(tradedcrops_ETH_top10, filename = outPath2, format = "GTiff", overwrite = TRUE)

## For mapping in Arc, need to know the full range for a customised scale
(min_ETH = min(ethiopiatop10b, na.rm = TRUE)) # 0
(max_ETH = max(ethiopiatop10b, na.rm = TRUE)) # 6736.817
(quant_ETH = quantile(ethiopiadata1, na.rm = TRUE))
#           0%         25%         50%         75%        100% 
# -94.995155    0.000000    0.000000    2.250895 6736.817437

sprich_ZMB = as.data.frame(vertebrate_richness_mask_ZMB, row.names = NULL, optional = FALSE, xy = TRUE, na.rm = FALSE, long = FALSE)
str(sprich_ZMB)
colnames(sprich_ZMB) = c("x", "y", "sprich")
crop_ZMB = as.data.frame(resampled_crops_ZMB, row.names = NULL, optional = FALSE, xy = TRUE, na.rm = FALSE, long = FALSE)
str(crop_ZMB)

zambiadata1 = cbind(sprich_ZMB, crop_ZMB)
## order by richness
zambiadataorder = zambiadata1[order(zambiadata1$sprich, na.last = TRUE, decreasing = TRUE),] # ordering by richness
head(zambiadataorder)

## cut off the data at top 10% of richness and then rasterise it and export ready for mapping in Arc
zambiatop10 = zambiadataorder[zambiadataorder$sprich > quantile(zambiadataorder$sprich,prob=1-10/100, na.rm = TRUE),]
summary(zambiatop10$sprich)
summary(zambiadataorder$sprich)
# write.csv(zambiatop10, file = paste0(outdir, "zambia_top10percentsprich_updatesapril2024.csv"))
head(zambiatop10) # x and y columns repeated
colnames(zambiatop10)
zambiatop10a = zambiatop10[, !duplicated(colnames(zambiatop10), fromLast = FALSE)] 
head(zambiatop10a)
summary(zambiatop10a)
zambiatop10b = zambiatop10a[!is.na(zambiatop10a$x),]
summary(zambiatop10b$x) 

zambiarasttest = rasterFromXYZ(zambiatop10b, res = c(0.1040,0.0786), crs = " +proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0 " , digits = 5)
plot(zambiarasttest)
# writeRaster(zambiarasttest, file = paste0(outdir, "raster_zambiatop10_updateapril2024", names(zambiarasttest)), format = "GTiff", bylayer = TRUE)
# should be able to plot these in Arc now 
plot(zambiarasttest)
zambia_stack = stack(zambiarasttest)
zambia_stack

domcrops_ZMB_top10 = subset(zambia_stack, c("ZMB_spam2020_v1r0_global_A_BANA_A", "ZMB_spam2020_v1r0_global_A_BEAN_A",
                                            "ZMB_spam2020_v1r0_global_A_CASS_A", "ZMB_spam2020_v1r0_global_A_CHIC_A",
                                            "ZMB_spam2020_v1r0_global_A_CITR_A", "ZMB_spam2020_v1r0_global_A_CNUT_A",
                                            "ZMB_spam2020_v1r0_global_A_COWP_A", "ZMB_spam2020_v1r0_global_A_GROU_A",
                                            "ZMB_spam2020_v1r0_global_A_MILL_A", "ZMB_spam2020_v1r0_global_A_ONIO_A",
                                            "ZMB_spam2020_v1r0_global_A_PIGE_A", "ZMB_spam2020_v1r0_global_A_PLNT_A",
                                            "ZMB_spam2020_v1r0_global_A_PMIL_A", "ZMB_spam2020_v1r0_global_A_POTA_A",
                                            "ZMB_spam2020_v1r0_global_A_RAPE_A", "ZMB_spam2020_v1r0_global_A_RICE_A",
                                            "ZMB_spam2020_v1r0_global_A_RUBB_A", "ZMB_spam2020_v1r0_global_A_SESA_A",
                                            "ZMB_spam2020_v1r0_global_A_SORG_A", "ZMB_spam2020_v1r0_global_A_SUGC_A",
                                            "ZMB_spam2020_v1r0_global_A_SUNF_A", "ZMB_spam2020_v1r0_global_A_SWPO_A",
                                            "ZMB_spam2020_v1r0_global_A_TOBA_A", "ZMB_spam2020_v1r0_global_A_TOMA_A",
                                            "ZMB_spam2020_v1r0_global_A_WHEA_A", "ZMB_spam2020_v1r0_global_A_YAMS_A",
                                            "ZMB_spam2020_v1r0_global_A_COFF_A"))

domcrops_ZMB_top10 = sum(domcrops_ZMB_top10)
plot(domcrops_ZMB_top10)
outPath1 = paste0(cropdata.dir_2020, outname_ZMB, "domestic_crops_top10filtered")
# writeRaster(domcrops_ZMB_top10, filename = outPath1, format = "GTiff", overwrite = TRUE)

tradedcrops_ZMB_top10 = subset(zambia_stack, c("ZMB_spam2020_v1r0_global_A_BARL_A", "ZMB_spam2020_v1r0_global_A_COCO_A",
                                               "ZMB_spam2020_v1r0_global_A_COTT_A", "ZMB_spam2020_v1r0_global_A_LENT_A",
                                               "ZMB_spam2020_v1r0_global_A_MAIZ_A", "ZMB_spam2020_v1r0_global_A_OILP_A",
                                               "ZMB_spam2020_v1r0_global_A_SOYB_A", "ZMB_spam2020_v1r0_global_A_SUGB_A",
                                               "ZMB_spam2020_v1r0_global_A_TEAS_A"))

tradedcrops_ZMB_top10 = sum(tradedcrops_ZMB_top10)
plot(tradedcrops_ZMB_top10)
outPath2 = paste0(cropdata.dir_2020, outname_ZMB, "traded_crops_top10filtered")
# writeRaster(tradedcrops_ZMB_top10, filename = outPath2, format = "GTiff", overwrite = TRUE)

## For mapping in Arc, need to know the full range for a customised scale
(min_ZMB = min(zambiatop10b, na.rm = TRUE)) # -16.13534
(max_ZMB = max(zambiatop10b, na.rm = TRUE)) # 2412.985
(quant_ZMB = quantile(zambiadata1, na.rm = TRUE))
#             0%            25%            50%            75%           100% 
#  -126.3995    0.0000    0.0000    0.0000 5524.1721

#####
### Need the proportion of harvested area of domestic v traded overlapping with biodiversity priority regions (% inside and % outside)

# Read in the KBA and Global 200 files:
#####
KBA.dir = "KBAs/"
G200.dir = "Ecoregions/Global 200/"

KBAfiles_GHA = list.files(paste0(KBA.dir), pattern = "GHA*", all.files = TRUE, full.names = FALSE)
KBAfiles_GHA <- KBAfiles_GHA[grepl(".shp",KBAfiles_GHA)]
KBAfiles_GHA <- KBAfiles_GHA[!grepl(".xml",KBAfiles_GHA)]
KBAfiles_GHA
KBA_GHA <- readOGR(paste0(KBA.dir, "KBA_GHA.shp"))
plot(KBA_GHA)
summary(KBA_GHA)

KBAfiles_ETH = list.files(paste0(KBA.dir), pattern = "ETH*", all.files = TRUE, full.names = FALSE)
KBAfiles_ETH <- KBAfiles_ETH[grepl(".shp",KBAfiles_ETH)]
KBAfiles_ETH <- KBAfiles_ETH[!grepl(".xml",KBAfiles_ETH)]
KBAfiles_ETH
KBA_ETH <- readOGR(paste0(KBA.dir, "KBA_ETH.shp"))
plot(KBA_ETH)
summary(KBA_ETH)

KBAfiles_ZMB = list.files(paste0(KBA.dir), pattern = "ZMB*", all.files = TRUE, full.names = FALSE)
KBAfiles_ZMB <- KBAfiles_ZMB[grepl(".shp",KBAfiles_ZMB)]
KBAfiles_ZMB <- KBAfiles_ZMB[!grepl(".xml",KBAfiles_ZMB)]
KBAfiles_ZMB
KBA_ZMB <- readOGR(paste0(KBA.dir, "KBA_ZMB.shp"))
plot(KBA_ZMB)
summary(KBA_ZMB)

G200files_GHA = list.files(paste0(G200.dir), pattern = "GHA*", all.files = TRUE, full.names = FALSE)
G200files_GHA <- G200files_GHA[grepl(".shp",G200files_GHA)]
G200files_GHA <- G200files_GHA[!grepl(".xml",G200files_GHA)]
G200files_GHA
G200_GHA <- readOGR(paste0(G200.dir, "g200_terr_GHA.shp"))
plot(G200_GHA)
summary(G200_GHA) # [+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0]
# cash crops +proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0 

G200files_ETH = list.files(paste0(G200.dir), pattern = "ETH*", all.files = TRUE, full.names = FALSE)
G200files_ETH <- G200files_ETH[grepl(".shp",G200files_ETH)]
G200files_ETH <- G200files_ETH[!grepl(".xml",G200files_ETH)]
G200files_ETH
G200_ETH <- readOGR(paste0(G200.dir, "g200_terr_ETH.shp"))
plot(G200_ETH)
summary(G200_ETH)

G200files_ZMB = list.files(paste0(G200.dir), pattern = "ZMB*", all.files = TRUE, full.names = FALSE)
G200files_ZMB <- G200files_ZMB[grepl(".shp",G200files_ZMB)]
G200files_ZMB <- G200files_ZMB[!grepl(".xml",G200files_ZMB)]
G200files_ZMB
G200_ZMB <- readOGR(paste0(G200.dir, "g200_terr_ZMB.shp"))
plot(G200_ZMB)
summary(G200_ZMB)

# 1. cash within G200
# a) area in Ghana with cash crops
cash_area_GHA = sum(values(tradedcrops_GHA), na.rm = TRUE)
G200_GHA
(G200_area_GHA = sum((G200_GHA$Shape_Area)))
(G200_area_ETH = sum((G200_ETH$Shape_Area)))
(G200_area_ZMB = sum((G200_ZMB$Shape_Area)))
# b) area in Global 200 with crop
cash_G200_GHA = mask(tradedcrops_GHA, G200_GHA)
cash_G200_GHA
plot(cash_G200_GHA)
cash_G200_area_GHA = sum(values(cash_G200_GHA), na.rm = TRUE)
cash_G200_area_GHA
(prop_cash_G200_GHA = cash_G200_area_GHA/cash_area_GHA)*100 # 0.171331

# 1i. cash within KBAs
# a) area in Ghana with cash crops
cash_area_GHA
(KBA_area_GHA = sum((KBA_GHA$Shape_Area)))
(KBA_area_ETH = sum((KBA_ETH$Shape_Area)))
(KBA_area_ZMB = sum((KBA_ZMB$Shape_Area)))
# b) area in KBAs with cash crops
cash_KBA_GHA = mask(tradedcrops_GHA, KBA_GHA)
cash_KBA_GHA
plot(cash_KBA_GHA)
cash_KBA_area_GHA = sum(values(cash_KBA_GHA), na.rm = TRUE)
cash_KBA_area_GHA
(prop_cash_KBA_GHA = cash_KBA_area_GHA/cash_area_GHA)*100 # 0.02361567

# 2. staple within Global 200
# a) area in Ghana with staple crops
staple_area_GHA = sum(values(domcrops_GHA), na.rm = TRUE)
# b) area in Global 200 with crop
staple_G200_GHA = mask(domcrops_GHA, G200_GHA)
staple_G200_GHA
plot(staple_G200_GHA)
staple_G200_area_GHA = sum(values(staple_G200_GHA), na.rm = TRUE)
staple_G200_area_GHA
(prop_staple_G200_GHA = staple_G200_area_GHA/staple_area_GHA)*100 # 0.3841378

# 1i. staple within KBAs
# a) area in Ghana with staple crops
staple_area_GHA
# b) area in KBAs with staple crops
staple_KBA_GHA = mask(domcrops_GHA, KBA_GHA)
staple_KBA_GHA
plot(staple_KBA_GHA)
staple_KBA_area_GHA = sum(values(staple_KBA_GHA), na.rm = TRUE)
staple_KBA_area_GHA
(prop_staple_KBA_GHA = staple_KBA_area_GHA/staple_area_GHA)*100 # 0.0291514

# 1. cash within G200
# a) area in ETHna with cash crops
cash_area_ETH = sum(values(tradedcrops_ETH), na.rm = TRUE)
# b) area in Global 200 with crop
cash_G200_ETH = mask(tradedcrops_ETH, G200_ETH)
cash_G200_ETH
plot(cash_G200_ETH)
cash_G200_area_ETH = sum(values(cash_G200_ETH), na.rm = TRUE)
cash_G200_area_ETH
(prop_cash_G200_ETH = cash_G200_area_ETH/cash_area_ETH)*100 # 0.6464094

# 1i. cash within KBAs
# a) area in ETHna with cash crops
cash_area_ETH
# b) area in KBAs with cash crops
cash_KBA_ETH = mask(tradedcrops_ETH, KBA_ETH)
cash_KBA_ETH
plot(cash_KBA_ETH)
cash_KBA_area_ETH = sum(values(cash_KBA_ETH), na.rm = TRUE)
cash_KBA_area_ETH
(prop_cash_KBA_ETH = cash_KBA_area_ETH/cash_area_ETH)*100 # 0.1308185

# 2. staple within Global 200
# a) area in ETHna with staple crops
staple_area_ETH = sum(values(domcrops_ETH), na.rm = TRUE)
# b) area in Global 200 with crop
staple_G200_ETH = mask(domcrops_ETH, G200_ETH)
staple_G200_ETH
plot(staple_G200_ETH)
staple_G200_area_ETH = sum(values(staple_G200_ETH), na.rm = TRUE)
staple_G200_area_ETH
(prop_staple_G200_ETH = staple_G200_area_ETH/staple_area_ETH)*100 # 0.7240814

# 1i. staple within KBAs
# a) area in ETHna with staple crops
staple_area_ETH
# b) area in KBAs with staple crops
staple_KBA_ETH = mask(domcrops_ETH, KBA_ETH)
staple_KBA_ETH
plot(staple_KBA_ETH)
staple_KBA_area_ETH = sum(values(staple_KBA_ETH), na.rm = TRUE)
staple_KBA_area_ETH
(prop_staple_KBA_ETH = staple_KBA_area_ETH/staple_area_ETH)*100 # 0.1673301


# 1. cash within G200
# a) area in ZMBna with cash crops
cash_area_ZMB = sum(values(tradedcrops_ZMB), na.rm = TRUE)
# b) area in Global 200 with crop
cash_G200_ZMB = mask(tradedcrops_ZMB, G200_ZMB)
cash_G200_ZMB
plot(cash_G200_ZMB)
cash_G200_area_ZMB = sum(values(cash_G200_ZMB), na.rm = TRUE)
cash_G200_area_ZMB
(prop_cash_G200_ZMB = cash_G200_area_ZMB/cash_area_ZMB)*100 # 0.5553696

# 1i. cash within KBAs
# a) area in ZMBna with cash crops
cash_area_ZMB
# b) area in KBAs with cash crops
cash_KBA_ZMB = mask(tradedcrops_ZMB, KBA_ZMB)
cash_KBA_ZMB
plot(cash_KBA_ZMB)
cash_KBA_area_ZMB = sum(values(cash_KBA_ZMB), na.rm = TRUE)
cash_KBA_area_ZMB
(prop_cash_KBA_ZMB = cash_KBA_area_ZMB/cash_area_ZMB)*100 # 0.02882867

# 2. staple within Global 200
# a) area in ZMBna with staple crops
staple_area_ZMB = sum(values(domcrops_ZMB), na.rm = TRUE)
# b) area in Global 200 with crop
staple_G200_ZMB = mask(domcrops_ZMB, G200_ZMB)
staple_G200_ZMB
plot(staple_G200_ZMB)
staple_G200_area_ZMB = sum(values(staple_G200_ZMB), na.rm = TRUE)
staple_G200_area_ZMB
(prop_staple_G200_ZMB = staple_G200_area_ZMB/staple_area_ZMB)*100 #0.6454732

# 1i. staple within KBAs
# a) area in ZMBna with staple crops
staple_area_ZMB
# b) area in KBAs with staple crops
staple_KBA_ZMB = mask(domcrops_ZMB, KBA_ZMB)
staple_KBA_ZMB
plot(staple_KBA_ZMB)
staple_KBA_area_ZMB = sum(values(staple_KBA_ZMB), na.rm = TRUE)
staple_KBA_area_ZMB
(prop_staple_KBA_ZMB = staple_KBA_area_ZMB/staple_area_ZMB)*100 # 0.05067313

#### 
## Pulling out some crops of interest:

names(Stack_2020_GHA)
names(Stack_2020_ETH)
names(Stack_2020_ZMB)
oilpalm_GHA = subset(Stack_2020_GHA, "GHA_spam2020_v1r0_global_A_OILP_A")
plantain_GHA = subset(Stack_2020_GHA, "GHA_spam2020_v1r0_global_A_PLNT_A")
banana_GHA = subset(Stack_2020_GHA, "GHA_spam2020_v1r0_global_A_BANA_A")
cocoa_GHA = subset(Stack_2020_GHA, "GHA_spam2020_v1r0_global_A_COCO_A")
coffee_GHA = subset(Stack_2020_GHA, "GHA_spam2020_v1r0_global_A_RCOF_A")
yam_ETH = subset(Stack_2020_ETH, "ETH_spam2020_v1r0_global_A_YAMS_A")
banana_ETH = subset(Stack_2020_ETH, "ETH_spam2020_v1r0_global_A_BANA_A")
tea_ETH = subset(Stack_2020_ETH, "ETH_spam2020_v1r0_global_A_TEAS_A")
coffee_ETH = subset(Stack_2020_ETH, "ETH_spam2020_v1r0_global_A_COFF_A")
wheat_ETH = subset(Stack_2020_ETH, "ETH_spam2020_v1r0_global_A_WHEA_A")
wheat_ZMB = subset(Stack_2020_ZMB, "ZMB_spam2020_v1r0_global_A_WHEA_A")
coffee_ZMB = subset(Stack_2020_ZMB, "ZMB_spam2020_v1r0_global_A_COFF_A")
tea_ZMB = subset(Stack_2020_ZMB, "ZMB_spam2020_v1r0_global_A_TEAS_A")
barley_ZMB = subset(Stack_2020_ZMB, "ZMB_spam2020_v1r0_global_A_BARL_A")
maize_ZMB = subset(Stack_2020_ZMB, "ZMB_spam2020_v1r0_global_A_MAIZ_A")

oilpalmarea_GHA = sum(values(oilpalm_GHA), na.rm = TRUE)
oilpalm_G200_GHA = mask(oilpalm_GHA, G200_GHA)
oilpalm_G200_GHA
plot(oilpalm_G200_GHA)
oilpalm_G200_area_GHA = sum(values(oilpalm_G200_GHA), na.rm = TRUE)
oilpalm_G200_area_GHA
(prop_oilpalm_G200_GHA = oilpalm_G200_area_GHA/oilpalmarea_GHA) # 0.7488451

oilpalm_KBA_GHA = mask(oilpalm_GHA, KBA_GHA)
oilpalm_KBA_GHA
plot(oilpalm_KBA_GHA)
oilpalm_KBA_area_GHA = sum(values(oilpalm_KBA_GHA), na.rm = TRUE)
oilpalm_KBA_area_GHA
(prop_oilpalm_KBA_GHA = oilpalm_KBA_area_GHA/oilpalmarea_GHA) # 0.03320706

coffeearea_GHA = sum(values(coffee_GHA), na.rm = TRUE)
coffee_G200_GHA = mask(coffee_GHA, G200_GHA)
coffee_G200_GHA
plot(coffee_G200_GHA)
coffee_G200_area_GHA = sum(values(coffee_G200_GHA), na.rm = TRUE)
coffee_G200_area_GHA
(prop_coffee_G200_GHA = coffee_G200_area_GHA/coffeearea_GHA) # 

coffee_KBA_GHA = mask(coffee_GHA, KBA_GHA)
coffee_KBA_GHA
plot(coffee_KBA_GHA)
coffee_KBA_area_GHA = sum(values(coffee_KBA_GHA), na.rm = TRUE)
coffee_KBA_area_GHA
(prop_coffee_KBA_GHA = coffee_KBA_area_GHA/coffeearea_GHA) # 

plantainarea_GHA = sum(values(plantain_GHA), na.rm = TRUE)
plantain_G200_GHA = mask(plantain_GHA, G200_GHA)
plantain_G200_GHA
plot(plantain_G200_GHA)
plantain_G200_area_GHA = sum(values(plantain_G200_GHA), na.rm = TRUE)
plantain_G200_area_GHA
(prop_plantain_G200_GHA = plantain_G200_area_GHA/plantainarea_GHA)*100 # 0.6944662

plantain_KBA_GHA = mask(plantain_GHA, KBA_GHA)
plantain_KBA_GHA
plot(plantain_KBA_GHA)
plantain_KBA_area_GHA = sum(values(plantain_KBA_GHA), na.rm = TRUE)
plantain_KBA_area_GHA
(prop_plantain_KBA_GHA = plantain_KBA_area_GHA/plantainarea_GHA)*100 #  0.03500847

bananaarea_GHA = sum(values(banana_GHA), na.rm = TRUE)
banana_G200_GHA = mask(banana_GHA, G200_GHA)
banana_G200_GHA
plot(banana_G200_GHA)
banana_G200_area_GHA = sum(values(banana_G200_GHA), na.rm = TRUE)
banana_G200_area_GHA
(prop_banana_G200_GHA = banana_G200_area_GHA/bananaarea_GHA)*100 # 0.5860988

banana_KBA_GHA = mask(banana_GHA, KBA_GHA)
banana_KBA_GHA
plot(banana_KBA_GHA)
banana_KBA_area_GHA = sum(values(banana_KBA_GHA), na.rm = TRUE)
banana_KBA_area_GHA
(prop_banana_KBA_GHA = banana_KBA_area_GHA/bananaarea_GHA)*100 #  0.04086978

cocoaarea_GHA = sum(values(cocoa_GHA), na.rm = TRUE)
cocoa_G200_GHA = mask(cocoa_GHA, G200_GHA)
cocoa_G200_GHA
plot(cocoa_G200_GHA)
cocoa_G200_area_GHA = sum(values(cocoa_G200_GHA), na.rm = TRUE)
cocoa_G200_area_GHA
(prop_cocoa_G200_GHA = cocoa_G200_area_GHA/cocoaarea_GHA) # 0.168306

cocoa_KBA_GHA = mask(cocoa_GHA, KBA_GHA)
cocoa_KBA_GHA
plot(cocoa_KBA_GHA)
cocoa_KBA_area_GHA = sum(values(cocoa_KBA_GHA), na.rm = TRUE)
cocoa_KBA_area_GHA
(prop_cocoa_KBA_GHA = cocoa_KBA_area_GHA/cocoaarea_GHA) #  0.02354008

yamarea_ETH = sum(values(yam_ETH), na.rm = TRUE)
yam_G200_ETH = mask(yam_ETH, G200_ETH)
yam_G200_ETH
plot(yam_G200_ETH)
yam_G200_area_ETH = sum(values(yam_G200_ETH), na.rm = TRUE)
yam_G200_area_ETH
(prop_yam_G200_ETH = yam_G200_area_ETH/yamarea_ETH)*100 # 0.4475193

yam_KBA_ETH = mask(yam_ETH, KBA_ETH)
yam_KBA_ETH
plot(yam_KBA_ETH)
yam_KBA_area_ETH = sum(values(yam_KBA_ETH), na.rm = TRUE)
yam_KBA_area_ETH
(prop_yam_KBA_ETH = yam_KBA_area_ETH/yamarea_ETH)*100 #  0.02963484

bananaarea_ETH = sum(values(banana_ETH), na.rm = TRUE)
banana_G200_ETH = mask(banana_ETH, G200_ETH)
banana_G200_ETH
plot(banana_G200_ETH)
banana_G200_area_ETH = sum(values(banana_G200_ETH), na.rm = TRUE)
banana_G200_area_ETH
(prop_banana_G200_ETH = banana_G200_area_ETH/bananaarea_ETH) # 0.4689733

banana_KBA_ETH = mask(banana_ETH, KBA_ETH)
banana_KBA_ETH
plot(banana_KBA_ETH)
banana_KBA_area_ETH = sum(values(banana_KBA_ETH), na.rm = TRUE)
banana_KBA_area_ETH
(prop_banana_KBA_ETH = banana_KBA_area_ETH/bananaarea_ETH) #   0.07721647

teaarea_ETH = sum(values(tea_ETH), na.rm = TRUE)
tea_G200_ETH = mask(tea_ETH, G200_ETH)
tea_G200_ETH
plot(tea_G200_ETH)
tea_G200_area_ETH = sum(values(tea_G200_ETH), na.rm = TRUE)
tea_G200_area_ETH
(prop_tea_G200_ETH = tea_G200_area_ETH/teaarea_ETH)*100 # 0.7726245

tea_KBA_ETH = mask(tea_ETH, KBA_ETH)
tea_KBA_ETH
plot(tea_KBA_ETH)
tea_KBA_area_ETH = sum(values(tea_KBA_ETH), na.rm = TRUE)
tea_KBA_area_ETH
(prop_tea_KBA_ETH = tea_KBA_area_ETH/teaarea_ETH)*100 #  0.3019868

coffeearea_ETH = sum(values(coffee_ETH), na.rm = TRUE)
coffee_G200_ETH = mask(coffee_ETH, G200_ETH)
coffee_G200_ETH
plot(coffee_G200_ETH)
coffee_G200_area_ETH = sum(values(coffee_G200_ETH), na.rm = TRUE)
coffee_G200_area_ETH
(prop_coffee_G200_ETH = coffee_G200_area_ETH/coffeearea_ETH) # 0.5970837

coffee_KBA_ETH = mask(coffee_ETH, KBA_ETH)
coffee_KBA_ETH
plot(coffee_KBA_ETH)
coffee_KBA_area_ETH = sum(values(coffee_KBA_ETH), na.rm = TRUE)
coffee_KBA_area_ETH
(prop_coffee_KBA_ETH = coffee_KBA_area_ETH/coffeearea_ETH) # 0.1209794

wheatarea_ETH = sum(values(wheat_ETH), na.rm = TRUE)
wheat_G200_ETH = mask(wheat_ETH, G200_ETH)
wheat_G200_ETH
plot(wheat_G200_ETH)
wheat_G200_area_ETH = sum(values(wheat_G200_ETH), na.rm = TRUE)
wheat_G200_area_ETH
(prop_wheat_G200_ETH = wheat_G200_area_ETH/wheatarea_ETH) # 

wheat_KBA_ETH = mask(wheat_ETH, KBA_ETH)
wheat_KBA_ETH
plot(wheat_KBA_ETH)
wheat_KBA_area_ETH = sum(values(wheat_KBA_ETH), na.rm = TRUE)
wheat_KBA_area_ETH
(prop_wheat_KBA_ETH = wheat_KBA_area_ETH/wheatarea_ETH) # 

wheatarea_ZMB = sum(values(wheat_ZMB), na.rm = TRUE)
wheat_G200_ZMB = mask(wheat_ZMB, G200_ZMB)
wheat_G200_ZMB
plot(wheat_G200_ZMB)
wheat_G200_area_ZMB = sum(values(wheat_G200_ZMB), na.rm = TRUE)
wheat_G200_area_ZMB
(prop_wheat_G200_ZMB = wheat_G200_area_ZMB/wheatarea_ZMB) # 0.696972

wheat_KBA_ZMB = mask(wheat_ZMB, KBA_ZMB)
wheat_KBA_ZMB
plot(wheat_KBA_ZMB)
wheat_KBA_area_ZMB = sum(values(wheat_KBA_ZMB), na.rm = TRUE)
wheat_KBA_area_ZMB
(prop_wheat_KBA_ZMB = wheat_KBA_area_ZMB/wheatarea_ZMB) #  0.007432404

coffeearea_ZMB = sum(values(coffee_ZMB), na.rm = TRUE)
coffee_G200_ZMB = mask(coffee_ZMB, G200_ZMB)
coffee_G200_ZMB
plot(coffee_G200_ZMB)
coffee_G200_area_ZMB = sum(values(coffee_G200_ZMB), na.rm = TRUE)
coffee_G200_area_ZMB
(prop_coffee_G200_ZMB = coffee_G200_area_ZMB/coffeearea_ZMB)*100 # 0.463367

coffee_KBA_ZMB = mask(coffee_ZMB, KBA_ZMB)
coffee_KBA_ZMB
plot(coffee_KBA_ZMB)
coffee_KBA_area_ZMB = sum(values(coffee_KBA_ZMB), na.rm = TRUE)
coffee_KBA_area_ZMB
(prop_coffee_KBA_ZMB = coffee_KBA_area_ZMB/coffeearea_ZMB)*100 #  0.04791

teaarea_ZMB = sum(values(tea_ZMB), na.rm = TRUE)
tea_G200_ZMB = mask(tea_ZMB, G200_ZMB)
tea_G200_ZMB
plot(tea_G200_ZMB)
tea_G200_area_ZMB = sum(values(tea_G200_ZMB), na.rm = TRUE)
tea_G200_area_ZMB
(prop_tea_G200_ZMB = tea_G200_area_ZMB/teaarea_ZMB)*100 # 1

tea_KBA_ZMB = mask(tea_ZMB, KBA_ZMB)
tea_KBA_ZMB
plot(tea_KBA_ZMB)
tea_KBA_area_ZMB = sum(values(tea_KBA_ZMB), na.rm = TRUE)
tea_KBA_area_ZMB
(prop_tea_KBA_ZMB = tea_KBA_area_ZMB/teaarea_ZMB)*100 #  0

barleyarea_ZMB = sum(values(barley_ZMB), na.rm = TRUE)
barley_G200_ZMB = mask(barley_ZMB, G200_ZMB)
barley_G200_ZMB
plot(barley_G200_ZMB)
barley_G200_area_ZMB = sum(values(barley_G200_ZMB), na.rm = TRUE)
barley_G200_area_ZMB
(prop_barley_G200_ZMB = barley_G200_area_ZMB/barleyarea_ZMB) # 0.3900418

barley_KBA_ZMB = mask(barley_ZMB, KBA_ZMB)
barley_KBA_ZMB
plot(barley_KBA_ZMB)
barley_KBA_area_ZMB = sum(values(barley_KBA_ZMB), na.rm = TRUE)
barley_KBA_area_ZMB
(prop_barley_KBA_ZMB = barley_KBA_area_ZMB/barleyarea_ZMB) #  0

maizearea_ZMB = sum(values(maize_ZMB), na.rm = TRUE)
maize_G200_ZMB = mask(maize_ZMB, G200_ZMB)
maize_G200_ZMB
plot(maize_G200_ZMB)
maize_G200_area_ZMB = sum(values(maize_G200_ZMB), na.rm = TRUE)
maize_G200_area_ZMB
(prop_maize_G200_ZMB = maize_G200_area_ZMB/maizearea_ZMB) # 

maize_KBA_ZMB = mask(maize_ZMB, KBA_ZMB)
maize_KBA_ZMB
plot(maize_KBA_ZMB)
maize_KBA_area_ZMB = sum(values(maize_KBA_ZMB), na.rm = TRUE)
maize_KBA_area_ZMB
(prop_maize_KBA_ZMB = maize_KBA_area_ZMB/maizearea_ZMB) #  


#####
# Repeat main figs for total risk potential:

head(riskpotentialdata)
head(tradedata)
alldata_risk = full_join(riskpotentialdata, tradedata, by = c("crop", "country"))
# checked examples using alldata vs tordata vs tradedata viewers 

head(alldata_risk)

alldata_risk$proportion_domestic = (alldata_risk$domestic_tonnes/(alldata_risk$domestic_tonnes+alldata_risk$total_export_tonnes_all_reporters))*100
alldata_risk$proportion_traded = (alldata_risk$total_export_tonnes_all_reporters/(alldata_risk$domestic_tonnes+alldata_risk$total_export_tonnes_all_reporters))*100
# If 60% or more is traded, we classify as a predominantly export crop
predom_traded = filter(alldata_risk, proportion_traded >= 60) # we only get 4 crops - coffee, banana, cocoa (Ghana) and sesame (Ethiopia)
predom_traded = filter(alldata_risk, proportion_traded >= 50) # we still only get one crop for Zambia
predom_traded = filter(alldata_risk, proportion_traded >= 20)
# Instead of a threshold, maybe it makes more sense to say if more is exported than kept?
predom_traded = filter(alldata_risk, total_export_tonnes_all_reporters > domestic_tonnes)
# still not many
View(alldata_risk)
zambiadata = filter(alldata_risk, country == "Zambia")
# how many crops have exports?
zambiadata_exported = filter(zambiadata, total_export_tonnes_all_reporters >0)
zambiadata_exported # 25 crops have some level of export
# more than 10% exported seems to be quite high for Zambia, as most crops are majority kept
zambiadata_traded = filter(zambiadata_exported, proportion_traded>10) # 9 crops

ethiopiadata = filter(alldata_risk, country == "Ethiopia")
# how many crops have exports?
ethiopiadata_exported = filter(ethiopiadata, total_export_tonnes_all_reporters >0)
ethiopiadata_exported # 28 crops have some level of export
# more than 10% exported also seems to be quite high for Ethiopia, as most crops are majority kept
ethiopiadata_traded = filter(ethiopiadata_exported, proportion_traded>10) # 7 crops

ghanadata = filter(alldata_risk, country == "Ghana")
# how many crops have exports?
ghanadata_exported = filter(ghanadata, total_export_tonnes_all_reporters >0)
ghanadata_exported # 28 crops have some level of export
# more than 10% exported also seems to be quite high for Ghana, as most crops are majority kept
ghanadata_traded = filter(ghanadata_exported, proportion_traded>10) # 8 crops

head(alldata_risk)
alldata_risk$domtraded = "domestic"
alldata_risk$domtraded[alldata_risk$proportion_traded >10] <- "traded"
alldata_risk = filter(alldata_risk, crop != "cerealnes" & crop != "other fibre" &
                   crop != "other oil" & crop != "pulsenes" & crop != "other roots"
                 & crop != "rest of crops" 
                 & crop != "fruitnes" & crop != "tropicalnes" & crop != "vegetablenes"
                 & crop != "rootnes" & crop != "broadbean" & crop != "buckwheat" 
                 & crop != "greenbean" & crop != "greenbroadbean" & crop != "millet" & crop != "stringbean")

# # which crops are the outliers, so I can label them? - commented out as going to label max and min instead
# View(alldata_risk)
# findoutlier <- function(x) {
#   return(x < quantile(x, .25) - 1.5*IQR(x) | x > quantile(x, .75) + 1.5*IQR(x))
# }
# 
# df <- na.omit(alldata_risk) %>%
#   group_by(domtraded) %>%
#   mutate(outlier = ifelse(findoutlier(tor), tor, NA))

alldata_risk$label = paste(alldata_risk$crop, ",", alldata_risk$country)
ghanadata = filter(alldata_risk, country == "Ghana")
ethiopiadata = filter(alldata_risk, country == "Ethiopia")
zambiadata = filter(alldata_risk, country == "Zambia")

# which of the crops are domestic vs traded?
ghanadata_dom = filter(ghanadata, domtraded == "domestic")
ghanadata_traded = filter(ghanadata, domtraded == "traded")

library(ggsci)

# Figure: Panel - total risk potential:

# dir.2025 = "Overlay Analysis/Jan2025/"
#write.csv(alldata_risk, paste0(dir.2025, "alldata_riskpotential_verts.csv"))
# write.csv(alldata_risk, paste0(newdir, "alldata_riskpotential_verts.csv"))

# get the max and min crops
(alldata_risk_summary = alldata_risk %>% 
    group_by(country, domtraded) %>% 
    filter(tor == max(tor, na.rm = TRUE)))
(alldata_risk_summary_min = alldata_risk %>% 
    group_by(country, domtraded) %>% 
    filter(tor == min(tor, na.rm = TRUE)))

(plot1_all_riskpotential = alldata_risk %>% 
    ggplot(aes(x=domtraded,y=relative_TRP, label = crop))+
    geom_boxplot(aes(width=.5, fill = domtraded), notch = TRUE, notchwidth = 0.9,
                 coef = NULL)+ # coef = NULL sets the whiskers to the min and max values
    # notching adds a way of comparing the medians between plots
    scale_fill_futurama() +
    facet_wrap(~country) +
    # jittered text with geom_text
    theme_bw()+
    geom_hline(yintercept = 1, linetype = "dotted") +
    coord_cartesian(ylim = c(0, 25)) +
    labs(x = "Crop Group", y = "Total Risk Potential Relative to Median Expectation
         (excluding values > 25 for visualisation)") +
    theme(legend.position="none",
          text = element_text(family="sans"), # sans serif
          axis.title.y = element_text(size = 12, vjust = 2, face = "bold"), # ensuring position of axis label is away from the ticks
          axis.title.x = element_text(size = 12, vjust = -2, face = "bold"),
          axis.text.y = element_text(size = 12),
          axis.ticks.length.x = unit(-0.1, "cm"), # length of tick marks - negative sign places ticks inwards
          axis.ticks = element_blank(),
          axis.text.x = element_text(size = 12),
          panel.background = element_blank(),
          panel.grid.major = element_line(color = "gray93"), # adding the lines to show the log scale
          axis.line = element_line(color = "black"),
          plot.margin = margin(10,10,10,20),
          strip.text.x = element_text(margin = margin(2,0,2,0), size = 12), strip.background = element_rect(colour = "black", fill = "gray93"),
          panel.border = element_rect(colour = "black", fill = NA)))

# t test
t.test(tor~domtraded, data = alldata_risk)
# separating the countries:
t.test(tor~domtraded, data = ghanadata)
t.test(tor~domtraded, data = ethiopiadata)
t.test(tor~domtraded, data = zambiadata)


# what is the effect size of the crops?
(ghanaeffect = ghanadata %>% 
    group_by(domtraded) %>% 
    summarise(mean = mean(tor, na.rm = TRUE)))
(ghana_effect = 753632-1129572) # -375940
# crops of interest seem to vary more though so how can we summarise the variance?
(ghana_summary_crops = ghanadata %>% 
    summarise(mean = mean(tor, na.rm = TRUE), sd = sd(tor, na.rm = TRUE), 
              median = median(tor, na.rm = TRUE),
              iqr = IQR(tor, na.rm = TRUE), range = (max(tor, na.rm = TRUE)-min(tor, na.rm = TRUE))))
#mean      sd   median      iqr   range
#1 814595.2 1567319 63544.12 632661.1 6673683

(ghana_summary_crops_domtraded = ghanadata %>% 
    group_by(domtraded) %>% 
    summarise(mean = mean(tor, na.rm = TRUE), sd = sd(tor, na.rm = TRUE), 
              median = median(tor, na.rm = TRUE),
              iqr = IQR(tor, na.rm = TRUE), range = (max(tor, na.rm = TRUE)-min(tor, na.rm = TRUE))))

# what is the effect size of the crops?
(ethiopiaeffect = ethiopiadata %>% 
    group_by(domtraded) %>% 
    summarise(mean = mean(tor, na.rm = TRUE)))
(ethiopia_effect = 684153-453016) # 231137
# crops of interest seem to vary more though so how can we summarise the variance?
(ethiopia_summary_crops = ethiopiadata %>% 
    summarise(mean = mean(tor, na.rm = TRUE), sd = sd(tor, na.rm = TRUE), 
              median = median(tor, na.rm = TRUE),
              iqr = IQR(tor, na.rm = TRUE), range = (max(tor, na.rm = TRUE)-min(tor, na.rm = TRUE)) 
    ))
#       mean      sd   median      iqr   range
#1 659165.7 1389916 70399.11 316300.2 5451823

(ethiopia_summary_crops = ethiopiadata %>% 
    group_by(domtraded) %>% 
    summarise(mean = mean(tor, na.rm = TRUE), sd = sd(tor, na.rm = TRUE), 
              median = median(tor, na.rm = TRUE),
              iqr = IQR(tor, na.rm = TRUE), range = (max(tor, na.rm = TRUE)-min(tor, na.rm = TRUE))))

# what is the effect size of the crops?
(zambiaeffect = zambiadata %>% 
    group_by(domtraded) %>% 
    summarise(mean = mean(tor, na.rm = TRUE)))
(zambia_effect = 143026-839877) # -696851
# crops of interest seem to vary more though so how can we summarise the variance?
(zambia_summary_crops = zambiadata %>% 
    summarise(mean = mean(tor, na.rm = TRUE), sd = sd(tor, na.rm = TRUE), 
              median = median(tor, na.rm = TRUE),
              iqr = IQR(tor, na.rm = TRUE), range = (max(tor, na.rm = TRUE)-min(tor, na.rm = TRUE)) 
    ))
#       mean      sd   median      iqr   range
# 1 312530.3 1002844 12877.48 234628.5 5997975

(zambia_summary_crops = zambiadata %>% 
    group_by(domtraded) %>% 
    summarise(mean = mean(tor, na.rm = TRUE), sd = sd(tor, na.rm = TRUE), 
              median = median(tor, na.rm = TRUE),
              iqr = IQR(tor, na.rm = TRUE), range = (max(tor, na.rm = TRUE)-min(tor, na.rm = TRUE))))

