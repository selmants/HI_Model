## Rasterize shapefile of 1999-2019 
## annual fire perimeters in Hawaii 
## and calculate annual area burned 
## by Island, Moisture Zone, 
## and State Class 
## Paul C. Selmants
## 2020-03-17

# Set working directory to "Fire" folder of HI_Model
# GitHub repository (https://github.com/selmants/HI_Model)

# load required packages into R
library(raster)
library(rgdal)
library(sf)
library(fasterize)
library(dplyr)
library(tidyr)
library(stringr)

			#### Read spatial data into R ####

# read Island ID GeoTIFF into R from Build_STsim directory, where:
# 1 = Hawaii		5 = Molokai
# 2 = Kahoolawe		6 = Oahu
# 3 = Lanai			7 = Kauai
# 4 = Maui
Islands <- raster("./InputData/spatial_data/initial_conditions/Island_250m.tif")
# read Moisture Zone GeoTIFF into R, where:
# 1 = "Dry"
# 2 = "Mesic"
# 3 = "Wet"
MZ <- raster("./InputData/spatial_data/initial_conditions/MZ_3_250m.tif")
# read State Class GeoTIFF into R, where: 
# 1 = Water 		6 = Forest
# 2 = Urban 		7 = Grassland
# 3 = Plantation	8 = Agriculture
# 4 = WoodyCrop		9 = Wetland
# 5 = Barren	   10 = Shrubland
StateClass <- raster("./InputData/spatial_data/intital_conditions/StateClassNew_250m.tif")
# read in Hawaii statewide annual fire perimeter shapefile (1999-2019) 
fires <- st_read("./data/base/2019_1999_Hawaii_Fire_Perimeters.shp") %>%
	dplyr::select(UH_ID, Year, geometry)

			### Reclassify and customize raster data ### 

# reclassification matrix to make all islands values = 0
islands_recl <- c(0, NA, 1, 0, 2, 0, 3, 0, 4, 0, 5, 0, 6, 0, 
	7, 0, 255, NA) %>%
	matrix(., ncol = 2, byrow = TRUE)
# reclassify islands raster to all zero values
zero_islands <- reclassify(Islands, islands_recl)
# reclassification matrices to isolate indvidual islands 
bigisland_recl <- c(1, 0, 2, NA, 3, NA, 4, NA, 5, NA, 6, NA, 
	7, NA, 255, NA) %>%
	matrix(., ncol = 2, byrow = TRUE)
Kahoolawe_recl <- c(1, NA, 2, 0, 3, NA, 4, NA, 5, NA, 6, NA, 
	7, NA, 255, NA) %>%
	matrix(., ncol = 2, byrow = TRUE)
Lanai_recl <- c(1, NA, 2, NA, 3, 0, 4, NA, 5, NA, 6, NA, 
	7, NA, 255, NA) %>%
	matrix(., ncol = 2, byrow = TRUE)
Maui_recl <- c(1, NA, 2, NA, 3, NA, 4, 0, 5, NA, 6, NA, 
	7, NA, 255, NA) %>%
	matrix(., ncol = 2, byrow = TRUE)
Molokai_recl <- c(1, NA, 2, NA, 3, NA, 4, NA, 5, 0, 6, NA, 
	7, NA, 255, NA) %>%
	matrix(., ncol = 2, byrow = TRUE)
Oahu_recl <- c(1, NA, 2, NA, 3, NA, 4, NA, 5, NA, 6, 0, 
	7, NA, 255, NA) %>%
	matrix(., ncol = 2, byrow = TRUE)
Kauai_recl <- c(1, NA, 2, NA, 3, NA, 4, NA, 5, NA, 6, NA, 
	7, 0, 255, NA) %>%
	matrix(., ncol = 2, byrow = TRUE)
# reclassify islands raster to make 7 indvidual island rasters
Hawaii <- reclassify(Islands, bigisland_recl)
Kahoolawe <- reclassify(Islands, Kahoolawe_recl)
Lanai <- reclassify(Islands, Lanai_recl)
Maui <- reclassify(Islands, Maui_recl)
Molokai <- reclassify(Islands, Molokai_recl)
Oahu <- reclassify(Islands, Oahu_recl)
Kauai <- reclassify(Islands, Kauai_recl)
#make raster stack of invidual island rasters
islandstack <- stack(Hawaii, Kahoolawe, Lanai, Maui, Molokai, Oahu, Kauai)
# make vector of island names
islandnames <- c("Hawaii", "Kahoolawe", "Lanai", "Maui", "Molokai", "Oahu", "Kauai")
# apply island names to island raster stack 
names(islandstack) <- islandnames

# reclassification matrices to subset Moisture Zones into 
# individual rasters and set both '0' and '255' to 'NA'
dry_recl <- c(0, NA, 1, 1, 2, NA, 3, NA, 255, NA) %>%
	matrix(., ncol = 2, byrow = TRUE)
mesic_recl <- c(0, NA, 1, NA, 2, 1, 3, NA, 255, NA) %>%
	matrix(., ncol = 2, byrow = TRUE)
wet_recl <- c(0, NA, 1, NA, 2, NA, 3, 1, 255, NA) %>%
	matrix(., ncol = 2, byrow = TRUE)
#reclassify MZ into individual zones 
dry <- reclassify(MZ, dry_recl)
mesic <- reclassify(MZ, mesic_recl)
wet <- reclassify(MZ, wet_recl) 

# reclassification matrix for State Classes to set 'Water', 'Urban', 
# 'Barren' to zero, '255' to 'NA' and:
# 1 = Forest		4 = Plantation
# 2 = Grassland		5 = Agriculture
# 3 = Shrubland		6 = WoodyCrop
sc_recl <- c(0, NA, 1, NA, 2, NA, 3, 4, 4, 6, 5, NA, 6, 1, 7, 2, 8, 5, 
	9, NA, 10, 3, 255, NA) %>%
	matrix(., ncol = 2, byrow = TRUE)
# reclassify StateClass raster
sc_mod <- reclassify(StateClass, sc_recl)

# mask modified State Class raster by individual Moisture Zones
drysc <- mask(sc_mod, dry)
mesicsc <- mask(sc_mod, mesic)
wetsc <- mask(sc_mod, wet)

# reclassification matrix for mesicsc to set: 
# 7 = Forest		10 = Plantation
# 8 = Grassland		11 = Agriculture
# 9 = Shrubland		12 = WoodyCrop
mesicsc_recl <- c(1,7,2,8,3,9,4,10,5,11,6,12) %>%
	matrix(., ncol = 2, byrow = TRUE)
# reclassify mesicsc
mesicSC <- reclassify(mesicsc, mesicsc_recl)
# reclassification matrix for wetsc to set: 
# 13 = Forest		16 = Plantation
# 14 = Grassland	17 = Agriculture
# 15 = Shrubland	18 = WoodyCrop
wetsc_recl <- c(1,13,2,14,3,15,4,16,5,17,6,18) %>%
	matrix(., ncol = 2, byrow = TRUE)
# reclassify wetsc
wetSC <- reclassify(wetsc, wetsc_recl)

# merge MZ-specific StateClass rasters into single MZSC raster
MZSC <- merge(drysc, mesicSC, wetSC)

# mask MZ-specific SC raster by raster stack of islands
mzsc_island <- mask(MZSC, islandstack) 
names(mzsc_island) <- islandnames

# create dataframe of MZ-specific State Class area by Island 
mzsc_area <- zonal(mzsc_island, MZSC, fun = 'count') %>%
	as.data.frame() %>%
	pivot_longer(-zone, names_to = "SecondaryStratumID", values_to = "pixels") %>%
	arrange(SecondaryStratumID) %>%
	mutate(scarea_km2 = pixels*0.0625, 
		StratumID = rep(c(rep(c("Dry", "Mesic", "Wet"), each = 6)), 7), 
		StateClassID = rep(c("Forest", "Grassland", "Shrubland", "Plantation",
			"Agriculture", "WoodyCrop"), 21)) %>%
	select(StratumID, SecondaryStratumID, zone, StateClassID, pixels, scarea_km2) %>%
	as.data.frame()

			### Convert Fire shapefile to annual rasters ### 

# change projection of fires to match other rasters
fires_utm <- st_transform(fires, crs = st_crs(MZ))
# create new bounding box (extent) for fires_utm to match other rasters
new_bb = c(418501, 2089853, 941251, 2465103)
names(new_bb) = c("xmin", "ymin", "xmax", "ymax")
attr(new_bb, "class") = "bbox"
# change bounding box of fires_utm 
attr(st_geometry(fires_utm), "bbox") = new_bb 
# create function to rasterize fire perimeters by year at 250m resolution
fireyear <- function(x) {
	fires_utm %>%
	filter(Year == x) %>%
	dplyr::select(UH_ID, geometry) %>%
	fasterize(., Islands, field = NULL, background = NA)
}

# rasterize fire perimeters by year
fire_1999 <- fireyear(1999)
fire_2000 <- fireyear(2000)
fire_2001 <- fireyear(2001)
fire_2002 <- fireyear(2002)
fire_2003 <- fireyear(2003)
fire_2004 <- fireyear(2004)
fire_2005 <- fireyear(2005)
fire_2006 <- fireyear(2006)
fire_2007 <- fireyear(2007)
fire_2008 <- fireyear(2008)
fire_2009 <- fireyear(2009)
fire_2010 <- fireyear(2010)
fire_2011 <- fireyear(2011)
fire_2012 <- fireyear(2012)
fire_2013 <- fireyear(2013)
fire_2014 <- fireyear(2014)
fire_2015 <- fireyear(2015)
fire_2016 <- fireyear(2016)
fire_2017 <- fireyear(2017)
fire_2018 <- fireyear(2018)
fire_2019 <- fireyear(2019)
# create vector of fire raster names
firelayers <- mget(ls(pattern = "fire_"))
# create stack of fire rasters by year 
firestack <- stack(firelayers)
# make vector of layer names in firestack
names <- names(firestack)
# mosaic firestack_UTM and zero_islands
firebrick <- mosaic(firestack, zero_islands, fun = max)
# apply layer names with year to brick
names(firebrick) <- names 

# make reclassification matrix to set all burnable State Class values to 1
scmod_rcl <- c(3, 1, 4, 1, 6, 1, 7, 1, 8, 1, 10, 1) %>%
	matrix(., ncol = 2, byrow = TRUE)
# reclassify sc_mod raster as fire spatial multiplier for 2020
fire_2020 <- reclassify(sc_mod, scmod_rcl)

			### Calculate burn areas and probabilities by ###
			  ### Island, Moisture Zone and State Class ###

# calculate total area burned per year by island
burnarea_islands <- zonal(firebrick, Islands, fun = 'sum') %>%
	data.frame(.) %>%
	pivot_longer(-zone, names_to = "Year", values_to = "cells") %>%
	mutate(island = rep(islandnames, each = 21),
		burnarea_km2 = cells*0.0625,
		year = as.integer(str_replace(Year, "fire_", ""))) %>% 
	select(island, year, burnarea_km2) %>%
	as.data.frame()

# sum by island to get total area burned statewide by year
burnarea_state <- burnarea_islands %>%
	group_by(year) %>%
	summarize(totalarea_km2 = sum(burnarea_km2)) %>%
	as.data.frame()
# calculate quantiles of statewide burn area 
quantile(burnarea_state$totalarea_km2) 
# filter statewide area burned to years less than median
burnarea_low <- burnarea_state %>%
	filter(totalarea_km2 < 28.75)
# filter statewide area burned to years greater than median
burnarea_high <- burnarea_state %>%
	filter(totalarea_km2 >= 28.75)

# create function to calculate annual burn areas by MZ and State Class for 
# each island 
burnarea <- function(x,y) {
	zonal(firebrick, x, fun = 'sum', na.rm = TRUE) %>%
	data.frame(.) %>%
	pivot_longer(-zone, names_to = "year", values_to = "burnpixels") %>%
	mutate(burnarea_km2 = burnpixels*0.0625, 
		SecondaryStratumID = y, 
		year = as.integer(str_replace(year, "fire_", "")))  %>%
	select(zone, SecondaryStratumID, year, burnpixels, burnarea_km2)
}

#calculate annual area burned by MZ and State Class for each island 
burnarea_Hawaii <- burnarea(mzsc_island[[1]], "Hawai'i")
burnarea_Kahoolawe <- burnarea(mzsc_island[[2]], "Kaho'olawe")
burnarea_Lanai <- burnarea(mzsc_island[[3]], "Lana'i")
burnarea_Maui <- burnarea(mzsc_island[[4]], "Maui")
burnarea_Molokai <- burnarea(mzsc_island[[5]], "Moloka'i")
burnarea_Oahu <- burnarea(mzsc_island[[6]], "O'ahu")
burnarea_Kauai <- burnarea(mzsc_island[[7]], "Kaua'i")

# filter out State Classes with zero area from mzsc_area 
mzsc_nzarea <- filter(mzsc_area, pixels > 0)

# create statewide fire distribution dataframe for all years (1999-2019)
firedistribution <- bind_rows(burnarea_Hawaii, burnarea_Kahoolawe, burnarea_Lanai, 
	burnarea_Maui, burnarea_Molokai, burnarea_Oahu, burnarea_Kauai) %>%
	arrange(year, SecondaryStratumID) %>%
	mutate(StratumID = rep(mzsc_nzarea$Stratum, 21),
		StateClass = rep(mzsc_nzarea$StateClassID, 21),
		totalpixels = rep(mzsc_nzarea$pixels, 21), 
		Value = burnpixels/totalpixels, 
		ExternalVariableTypeID = "Historical Year: Fire",
		DistributionTypeID = str_c("Historical Rate: Fire ", StateClass)) %>%
	select(StratumID, SecondaryStratumID, DistributionTypeID, ExternalVariableTypeID,
		year, Value)
# Filter out years greater than or equal to median annual area burned (n = 10)
low_fire <- firedistribution %>%
	filter(year %in% c(2000, 2001, 2002, 2004, 
		2008, 2010, 2011, 2013, 2014, 2017)) %>%
	mutate(ExternalVariableMin = rep(c(1:10), each = 91), 
		ExternalVariableMax = ExternalVariableMin) %>%
	select(StratumID, SecondaryStratumID, DistributionTypeID, ExternalVariableTypeID,
		ExternalVariableMin, ExternalVariableMax, Value) %>%
	as.data.frame()
# Filter out years less than median annual area burned (n = 11)
high_fire <- firedistribution %>%
	filter(year %in% c(1999, 2003, 2005, 2006, 2007, 2009, 
		2012, 2015, 2016, 2018, 2019)) %>%
	mutate(ExternalVariableMin = rep(c(1:11), each = 91), 
		ExternalVariableMax = ExternalVariableMin) %>%
	select(StratumID, SecondaryStratumID, DistributionTypeID, ExternalVariableTypeID,
		ExternalVariableMin, ExternalVariableMax, Value) %>%
	as.data.frame()
	
	### Write data to HI_Model/InputData/spatial_data/spatial_multipliers ###

# write individual fire rasters to GeoTIFF files 
writeRaster(firebrick, names(firebrick), 
	bylayer = TRUE, format = 'GTiff', overwrite = TRUE)
# write fire_2020 raster to GeoTIFF file
writeRaster(fire_2020, "fire_2020.tif", overwrite = TRUE)

## Write data to HI_Model/InputData ##

# write firedistribution to .csv file
write.csv(firedistribution, "FireDistributions.csv", row.names = FALSE)
# write low_fire to .csv file
write.csv(low_fire, "FireDistributions_LOW.csv", row.names = FALSE)
# write high_fire to .csv file
write.csv(high_fire, "FireDistributions_HIGH.csv", row.names = FALSE)

## Write data to HI_Model/Fire/data/processed ##

# write burnarea_islands to .csv file
write.csv(burnarea_islands, "../Fire/data/processed/AnnualFire.csv", row.names = FALSE)

