## Create raster of
## merged Moisture Zones 
## and State Classes 
## Paul C. Selmants
## 2019-10-25

# read required packages into R
library(raster)
library(rgdal)
library(dplyr)

## set working directory at highest level of repo: "HI_Model" ##

# read Island ID TIFF into R, where:
# 1 = Hawaii		5 = Molokai
# 2 = Kahoolawe		6 = Oahu
# 3 = Lanai			7 = Kauai
# 4 = Maui
Islands <- raster("./Build_STsim/data/spatial_data/Island_250m.tif")
# read Moisture Zone TIFF into R, where:
# 1 = "Dry", 2 = "Mesic", 3 = "Wet"
MZ <- raster('./Build_STsim/data/spatial_data/MZ_3_250m.tif')
# read State Class TIFF into R, where: 
# 1 = Water 		6 = Forest
# 2 = Urban 		7 = Grassland
# 3 = Plantation	8 = Agriculture
# 4 = WoodyCrop		9 = Wetland
# 5 = Barren	   10 = Shrubland
StateClass <- raster('./Build_STsim/data/spatial_data/StateClassNew_250m.tif')

# make reclassification matrix to make all islands values = 0
islands_recl <- c(0, NA, 1, 0, 2, 0, 3, 0, 4, 0, 5, 0, 6, 0, 
	7, 0, 255, NA) %>%
	matrix(., ncol = 2, byrow = TRUE)
# reclassify islands raster
zero_islands <- reclassify(Islands, islands_recl)

# make reclassification matrices to subset moisture zones into 
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

# make reclassification matrix to set 'Water', 'Urban', 
# 'Barren', '0' and '255' to 'NA'
sc_recl <- c(0, NA, 1, NA, 2, NA, 3, 3, 4, 4, 5, NA, 6, 6, 7, 7, 8, 8, 
	9, NA, 10, 10, 255, NA) %>%
	matrix(., ncol = 2, byrow = TRUE)
# reclassify StateClass raster
sc_mod <- reclassify(StateClass, sc_recl)

# mask sc_mod with dry, mesic, and wet moisture zone rasters
drysc <- mask(sc_mod, dry)
mesicsc <- mask(sc_mod, mesic)
wetsc <- mask(sc_mod, wet)

# Reclassify drysc, mesicsc, and wetsc so that:
# 1 = Dry Forest	 	 7 = Mesic Forest			13 = Wet Forest
# 2 = Dry Grass		 	 8 = Mesic Grass			14 = Wet Grass
# 3 = Dry Shrub		 	 9 = Mesic Shrub			15 = Wet Shrub 
# 4 = Dry Plantation	10 = Mesic Plantation		16 = Wet Plantation
# 5 = Dry Ag			11 = Mesic Ag				17 = Wet Ag
# 6 = Dry WoodyCrop		12 = Mesic WoodyCrop		18 = Wet WoodyCrop



