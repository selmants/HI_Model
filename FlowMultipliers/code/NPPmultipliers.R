## Calculate NPP spatial multiplier raster
## and NPP climate flow multiplier datasheets
## using Del Grosso NCEAS NPP equations and 
## statistically downscaled CMIP5
## projections of rainfall and temperature
## for RCP 4.5 and RCP 8.5. 
## Paul C. Selmants
## 2020-03-19 (ISO 8601)

# load required packages into R
library(raster)
library(rgdal)
library(tidyr, warn.conflicts = FALSE)
library(dplyr, warn.conflicts = FALSE)

## Set working directory at highest level in HI_Model
## GitHub repository, https://github.com/selmants/HI_Model 

# read in State Class raster, where: 
# 1 = Water 		 6 = Forest		
# 2 = Urban 		 7 = Grassland		
# 3 = Plantation 	 8 = Agriculture			 
# 4 = WoodyCrop 	 9 = Wetland		 
# 5 = Barren 		10 = Shrubland		
SC <- raster("./Model_InputData/spatial_data/StateClassNew_250m.tif")

# reclassification matrix for SC to set Water, Urban, 
# Barren, & Wetland to 1 and set other classes to NA
sc_rcl <- c(0,NA,1,1,2,1,3,NA,4,NA,5,1,6,NA,7,NA,8,NA,9,1,10,NA,255,NA) %>%
	matrix(., ncol = 2, byrow = TRUE)
# reclassify SC raster 
SCnoveg <- reclassify(SC, sc_rcl)

# read in raster of combined Moisture Zones and State Classes, where:
# 1 = Dry Forest	 	 7 = Mesic Forest			13 = Wet Forest
# 2 = Dry Grass		 	 8 = Mesic Grass			14 = Wet Grass
# 3 = Dry Shrub		 	 9 = Mesic Shrub			15 = Wet Shrub 
# 4 = Dry Plantation	10 = Mesic Plantation		16 = Wet Plantation
# 5 = Dry Ag			11 = Mesic Ag				17 = Wet Ag
# 6 = Dry WoodyCrop		12 = Mesic WoodyCrop		18 = Wet WoodyCrop
MZSC <- raster("./Fire/data/processed/MZSC.tif")

# reclassification matrix to create forest MZSC raster
for_rcl <- c(1,1,2,NA,3,NA,4,4,5,NA,6,6,7,7,8,NA,9,NA,10,10,
	11,NA,12,12,13,13,14,NA,15,NA,16,16,17,NA,18,18) %>%
	matrix(., ncol = 2, byrow = TRUE)
# reclassify MZSC to make forest only raster
forMZSC <- reclassify(MZSC, for_rcl)
# reclassification matrix to create nonforest MZSC raster
shrubgrass_rcl <- c(1,NA,2,2,3,3,4,NA,5,5,6,NA,7,NA,8,8,9,9,10,NA,
	11,11,12,NA,13,NA,14,14,15,15,16,NA,17,17,18,NA) %>%
	matrix(., ncol = 2, byrow = TRUE)
# reclassify MZSC to make nonforest raster
shrubgrassMZSC <- reclassify(MZSC, shrubgrass_rcl)

# create list of individual island .tif files
islandlist <- list.files('./FlowMultipliers/data/islands', full.names = TRUE)
#create raster stack of individual island .tif files
islandstack <- stack(islandlist)

# mask MZ-specific SC raster with raster stack of islands
mzsc_island <- mask(MZSC, islandstack) 
names(mzsc_island) <- names(islandstack)

## Create rasters of future annual rainfall based on 
## 30-year (1983-2012) mean annual rainfall (mm) and mid- to 
## late-century rainfall anomalies for RCP 4.5 and RCP 8.5

# create list of annual rainfall .tif files
rainlist <- list.files('./FlowMultipliers/data/rainfall_tiff_annual', full.names = TRUE)
#create raster stack of annual rainfall (mm) .tif files
rainstack <- stack(rainlist)
# make raster layer of 30-year mean annual rainfall and re-sample
# to change projection to UTM (this takes ~30 seconds)
rain30ymean <- calc(rainstack, mean) %>%
	projectRaster(., MZSC, method = 'bilinear')
# create list of rainfall anomaly .tif files
rfanomlist <- list.files(pattern = 'sd_rfanom', full.names = TRUE)
# create raster stack of rainfall anomaly rasters and re-sample
# to change projection to UTM
rfanomstack <- stack(rfanomlist) %>%
	projectRaster(., MZSC, method = 'bilinear')
# create raster brick of mid- & late-century rainfall under RCPs 4.5 & 8.5
rainfuture <- rain30ymean + rain30ymean*(rfanomstack/100)
# create vector of rcp names 
rcpnames <- c('rcp45late', 'rcp45mid', 'rcp85late', 'rcp85mid')
# rename layers in rainfuture raster brick
names(rainfuture) <- rcpnames

## Create rasters of future mean annual temperature for RCPs 4.5 & 8.5
## based on current MAT and mid- to late-century temperature deltas 

#read present day (2001-2010) mean annual temperature (deg C) 
#TIFF into R and match projection to MZ 
MATpresent <- raster('./FlowMultipliers/data/MAT_annual.tif') %>%
	projectRaster(., MZSC, method = 'bilinear')
# create list of temperature delta .tif files
tdeltalist <- list.files(pattern = 'sd_Tdeltas', full.names = TRUE)
# create raster stack of temperature delta rasters and match projection
# to MAT_present
tdeltastack <- stack(tdeltalist) %>%
	projectRaster(., MATpresent, method = 'bilinear') 
# rename layers in Tdeltastack
names(tdeltastack) <- rcpnames
# calculate future MATs
MATfuture <- MATpresent + tdeltastack 
# rename layers in MATfuture
names(MATfuture) <- rcpnames

## Create current and future rainfall and temperature rasters masked 
## to Forest land cover classes 

# forest current rainfall
for_rain30ymean <- mask(rain30ymean, forMZSC)
# forest current MAT 
for_MATpresent <- mask(MATpresent, forMZSC)
# forest future rainfall
for_rainfuture <- mask(rainfuture, forMZSC)
# forest future temperature
for_MATfuture <- mask(MATfuture, forMZSC)

## create dry and wet rainfall rasters (dry <= 2500mm, wet > 2500mm) 
## of non-forest land cover (shrub & grass) for calculating non-forest NPP

# raster of current rainfall < 2500 mm masked with nonforest land cover
drynonforpresent <- reclassify(rain30ymean, 
	cbind(2500.01, 10000, NA), left=FALSE) %>% 
	mask(., shrubgrassMZSC)
# raster of current rainfall > 2500 mm masked with nonforest land cover
wetnonforpresent <- reclassify(rain30ymean, cbind(0, 2500, NA), right=FALSE) %>%
	mask(., shrubgrassMZSC)
# raster stack of future rainfall < 2500 mm masked with nonforest land cover
drynonforfuture <- reclassify(rainfuture, 
	cbind(2500.001, 10000, NA), left=FALSE) %>%
	mask(., shrubgrassMZSC)
# raster stack of future rainfall > 2500 mm masked with nonforest land cover
wetnonforfuture <- reclassify(rainfuture, cbind(0, 2500, NA), right=FALSE) %>%
	mask(., shrubgrassMZSC)

# create function to calculate forest NPP in gC/m^2 as function of annual rainfall  
# using MAR portion of NCEAS forest NPP equation (Del Grosso et al. 2008)
fMAR_for <- function(MAR){
	(0.551*MAR^1.055)/exp(3.06e-04*MAR)
}
# create function to calculate forest NPP in gC/m^2 as function of mean annual 
# temperature using MAT portion of NCEAS forest NPP equation (Del Grosso et al. 2008)
fMAT_for <- function(MAT){
	2540/(1+exp(1.584-0.0622*MAT))
}
#Define conditional function to apply forest NCEAS NPP equation based on lesser
#value of NPP in gC/m^2 derived from rainfall or temperature (Del Gross et al. 2008)
Con <- function(condition, trueValue, falseValue){
  return(condition * trueValue + (!condition)*falseValue)
}
#create function to calculate nonforest NPP in gC/m^2 as function of annual rainfall
#less than 2500 mm using NCEAS non-forest NPP equation (Del Grosso et al. 2008)
fMAR_DRYnonfor <- function(MAR){
	6116*(1-exp(-6.05e-05*MAR))
}
#create function to calculate nonforest NPP in gC/m^2 as a function of annual 
#rainfall greater than 2500 mm using Schuur rainfall NPP equation (Schuur 2003). 
#Schuur equation result multiplied by 100 to match NCEAS output in gC/m^2 
fMAR_WETnonfor <- function(MAR){
	((5.212e-03*MAR^1.12363)/exp(4.59532e-04*MAR))*100
}

# calculate present day forest NPP in gC/m^2 based on current rainfall 
forNPPpresent_rain <- fMAR_for(for_rain30ymean)
# calculate future forest NPP in gC/m^2 based on projected future rainfall 
forNPPfuture_rain <- fMAR_for(for_rainfuture)
# calculate present day forest NPP in gC/m^2 based on current temperature
forNPPpresent_MAT <- fMAT_for(for_MATpresent)
# calculate future forest NPP in gC/m^2 based on projected future temperature
forNPPfuture_MAT <- fMAT_for(for_MATfuture)

#calculate present day forest NPP in gC/m^2 based on lesser value of 
#estimates derived from rainfall vs. temperature
forestNPPpresent <- 
	Con(forNPPpresent_rain < forNPPpresent_MAT, 
		forNPPpresent_rain, forNPPpresent_MAT)
#calculate future forest NPP in gC/m^2 based on lesser value of 
#estimates based on projected rainfall vs. projected temperature
forestNPPfuture <- 
	Con(forNPPfuture_rain < forNPPfuture_MAT, 
		forNPPfuture_rain, forNPPfuture_MAT)	
#calculate present day nonforest NPP in gC/m^2 for areas with rainfall
#less than 2500 mm y^-1 using NCEAS non-forest equation
nonfordryNPPpresent <- fMAR_DRYnonfor(drynonforpresent)
#calculate future nonforest NPP in gC/m^2 for areas with projected 
#future rainfall less than 2500 mm y^-1 using NCEAS non-forest equation
nonfordryNPPfuture <- fMAR_DRYnonfor(drynonforfuture)
#calculate present day nonforest NPP in gC/m^2 for areas with rainfall
#greater than 2500 mm y^-1 using modified Schuur (2003) equation
nonforwetNPPpresent <- fMAR_WETnonfor(wetnonforpresent)
#calculate future nonforest NPP in gC/m^2 for areas with rainfall
#greater than 2500 mm y^-1 using modified Schuur (2003) equation
nonforwetNPPfuture <- fMAR_WETnonfor(wetnonforfuture)

#make a list of present day NPP (gC/m^2) rasters
presentNPPlist <- list(forestNPPpresent, nonfordryNPPpresent, nonforwetNPPpresent)
#merge present day NPP rasters into one
presentNPP <- do.call(merge, presentNPPlist)

#make a list of future NPP (gC/m^2) raster stacks
futureNPPlist <- list(forestNPPfuture, nonfordryNPPfuture, nonforwetNPPfuture)
#merge future NPP raster stacks into one
futureNPP <- do.call(merge, futureNPPlist)
#re-name layers in raster stack 
names(futureNPP) <- rcpnames

## Create spatial flow multiplier raster for NPP 
## based on deviation from mean NPP by MZSC zone 

# calculate zonal mean NPP based on MZSC raster (18 zones)
zonalmeanNPP <- zonal(presentNPP, MZSC, fun = "mean", na.rm = TRUE) %>%
	data.frame()
# re-classification matrix for MZSC using mean NPP values for each zone
recl_meanNPP <- c(1,zonalmeanNPP[1,2],2,zonalmeanNPP[2,2],3,zonalmeanNPP[3,2],4,
	zonalmeanNPP[4,2],5,zonalmeanNPP[5,2],6,zonalmeanNPP[6,2],7,zonalmeanNPP[7,2],8,
	zonalmeanNPP[8,2],9,zonalmeanNPP[9,2],10,zonalmeanNPP[10,2],11,zonalmeanNPP[11,2],
	12,zonalmeanNPP[12,2],13,zonalmeanNPP[13,2],14,zonalmeanNPP[14,2], 15,zonalmeanNPP[15,2],
	16,zonalmeanNPP[16,2],17,zonalmeanNPP[17,2],18,zonalmeanNPP[18,2]) %>%
	matrix(., ncol = 2, byrow = TRUE) 
# reclassify MZSC to make zonal mean NPP raster
NPPMZSC <- reclassify(MZSC, recl_meanNPP)
# Divide zonal mean NPP by NCEAS NPP estimates to create
# spatial flow multiplier raster for NPP 
growthmultipliers <- overlay(presentNPP, NPPMZSC,
	fun = function(x,y) {return(x/y)} ) 
# merge growthmultipliers with non-veg State Class raster
gm <- merge(growthmultipliers, SCnoveg)
# save NPP multipliers raster as NCEAS_NPP_sm
writeRaster(gm, "../processed/NCEAS_NPP_sm.tif", overwrite = TRUE)

## Create future NPP flow multiplier tables for RCPs 4.5 & 8.5 ##

# Divide future NPP estimates by current 30-year mean NPP estimates
# to calculate NPP multipliers for mid- and late-century RCPs 4.5 & 8.5 
growthmultipliers <- overlay(futureNPP, presentNPP,
	fun = function(x,y) {return(x/y)} ) 
# re-name layers in raster stack
names(growthmultipliers) <- rcpnames

# create function to mask growthmultipliers stack with individual rasters
# from islandstack
islandmask <- function(x) {
	mask(growthmultipliers, islandstack[[x]])
}

# mask growthmultipliers stack with island rasters, rename layers
Hawaii_gm <- islandmask(1)
names(Hawaii_gm) <- paste(rcpnames, "Hawaii", sep = "_")
Kahoolawe_gm <- islandmask(2)
names(Kahoolawe_gm) <- paste(rcpnames, "Kahoolawe", sep = "_")
Lanai_gm <- islandmask(4)
names(Lanai_gm) <- paste(rcpnames, "Lanai", sep = "_")
Maui_gm <- islandmask(5)
names(Maui_gm) <- paste(rcpnames, "Maui", sep = "_")
Molokai_gm <- islandmask(6)
names(Molokai_gm) <- paste(rcpnames, "Molokai", sep = "_")
Oahu_gm <- islandmask(7)
names(Oahu_gm) <- paste(rcpnames, "Oahu", sep = "_")
Kauai_gm <- islandmask(3)
names(Kauai_gm) <- paste(rcpnames, "Kauai", sep = "_")

# create list of gm island raster stacks
gmlayers <- mget(ls(pattern = "_gm"))
# create brick of gm island raster stacks
gmislandbrick <- brick(gmlayers) 

# calculate zonal mean for growth multipliers
zonalmean <- zonal(gmislandbrick, MZSC, fun = "mean", na.rm = TRUE) %>%
	data.frame()
# calculate zonal sd for growth multipliers and re-format
zonalsd <- zonal(gmislandbrick, MZSC, fun = "sd", na.rm = TRUE) %>%
	data.frame() %>%
	mutate(MZ = rep(c("Dry", "Mesic", "Wet"), each = 6)) %>%
	pivot_longer(-c(zone,MZ), names_to = "rcp_island", values_to = "gm_sd") %>%
	group_by(MZ, rcp_island) %>%
	summarize(sd_mean = mean(gm_sd, na.rm = TRUE)) %>%
	separate(rcp_island, c("RCPtime", "Island")) %>%
	arrange(Island) %>%
	filter(sd_mean > 0)

# filter zonalsd by RCP
gmsd45 <- filter(zonalsd, grepl('rcp45', RCPtime)) %>%
	data.frame()
gmsd85 <- filter(zonalsd, grepl('rcp85', RCPtime)) %>%
	data.frame()

### create functions to construct annual increment
### of climate flow multiplier for NPP 

# Island of Hawaii annual NPP multiplier increments
Hawaiigmseq <- function(x, y) {
	data.frame(Timestep = c(2011:2100),
		Dry.Forest = c(seq(1, zonalmean[1,x], length.out = 40), 
			seq(zonalmean[1,x], zonalmean[1,y], length.out = 50)),
		Dry.Grassland = c(seq(1, zonalmean[2,x], length.out = 40),
			seq(zonalmean[2,x], zonalmean[2,y], length.out = 50)),
		Dry.Shrubland = c(seq(1, zonalmean[3,x], length.out = 40),
			seq(zonalmean[3,x], zonalmean[3,y], length.out = 50)),
		Dry.Plantation = c(seq(1, zonalmean[4,x], length.out = 40),
			seq(zonalmean[4,x], zonalmean[4,y], length.out = 50)),
		Dry.Agriculture = c(seq(1, zonalmean[5,x], length.out = 40),
			seq(zonalmean[5,x], zonalmean[5,y], length.out = 50)),
		Dry.WoodyCrop = c(seq(1, zonalmean[6,x], length.out = 40),
			seq(zonalmean[6,x], zonalmean[6,y], length.out = 50)),
		Mesic.Forest = c(seq(1, zonalmean[7,x], length.out = 40),
			seq(zonalmean[7,x], zonalmean[7,y], length.out = 50)),
		Mesic.Grassland = c(seq(1, zonalmean[8,x], length.out = 40),
			seq(zonalmean[8,x], zonalmean[8,y], length.out = 50)),
		Mesic.Shrubland = c(seq(1, zonalmean[9,x], length.out = 40),
			seq(zonalmean[9,x], zonalmean[9,y], length.out = 50)),
		Mesic.Plantation = c(seq(1, zonalmean[10,x], length.out = 40),
			seq(zonalmean[10,x], zonalmean[10,y], length.out = 50)),
		Mesic.Agriculture = c(seq(1, zonalmean[11,x], length.out = 40),
			seq(zonalmean[11,x], zonalmean[11,y], length.out = 50)),
		Mesic.WoodyCrop = c(seq(1, zonalmean[12,x], length.out = 40),
			seq(zonalmean[12,x], zonalmean[12,y], length.out = 50)),
		Wet.Forest = c(seq(1, zonalmean[13,x], length.out = 40),
			seq(zonalmean[13,x], zonalmean[13,y], length.out = 50)),
		Wet.Grassland = c(seq(1, zonalmean[14,x], length.out = 40),
			seq(zonalmean[14,x], zonalmean[14,y], length.out = 50)),
		Wet.Shrubland = c(seq(1, zonalmean[15,x], length.out = 40),
			seq(zonalmean[15,x], zonalmean[15,y], length.out = 50)),
		Wet.Plantation = c(seq(1, zonalmean[16,x], length.out = 40),
			seq(zonalmean[16,x], zonalmean[16,y], length.out = 50)),
		Wet.Agriculture = c(seq(1, zonalmean[17,x], length.out = 40),
			seq(zonalmean[17,x], zonalmean[17,y], length.out = 50)),
		Wet.WoodyCrop = c(seq(1, zonalmean[18,x], length.out = 40),
			seq(zonalmean[18,x], zonalmean[18,y], length.out = 50))) %>%
	pivot_longer(-Timestep, names_to = "MZ_SC", values_to = "Value") %>%
	arrange(MZ_SC) %>%
	mutate(SecondaryStratumID = "Hawai'i") %>%
	data.frame()
}
# Kahoolawe annual NPP multiplier increments
Kahoolawegmseq <- function(x, y) {
	data.frame(Timestep = c(2011:2100),
		Dry.Forest = c(seq(1, zonalmean[1,x], length.out = 40), 
			seq(zonalmean[1,x], zonalmean[1,y], length.out = 50)),
		Dry.Grassland = c(seq(1, zonalmean[2,x], length.out = 40),
			seq(zonalmean[2,x], zonalmean[2,y], length.out = 50)),
		Dry.Shrubland = c(seq(1, zonalmean[3,x], length.out = 40),
			seq(zonalmean[3,x], zonalmean[3,y], length.out = 50))) %>%
	pivot_longer(-Timestep, names_to = "MZ_SC", values_to = "Value") %>%
	arrange(MZ_SC) %>%
	mutate(SecondaryStratumID = "Kaho'olawe") %>%
	data.frame()
}
# Oahu annual NPP multiplier increments
Oahugmseq<- function(x,y) {
	data.frame(Timestep = c(2011:2100),
		Dry.Forest = c(seq(1, zonalmean[1,x], length.out = 40), 
			seq(zonalmean[1,x], zonalmean[1,y], length.out = 50)),
		Dry.Grassland = c(seq(1, zonalmean[2,x], length.out = 40),
			seq(zonalmean[2,x], zonalmean[2,y], length.out = 50)),
		Dry.Shrubland = c(seq(1, zonalmean[3,x], length.out = 40),
			seq(zonalmean[3,x], zonalmean[3,y], length.out = 50)),
		Dry.Plantation = c(seq(1, zonalmean[4,x], length.out = 40),
			seq(zonalmean[4,x], zonalmean[4,y], length.out = 50)),
		Dry.Agriculture = c(seq(1, zonalmean[5,x], length.out = 40),
			seq(zonalmean[5,x], zonalmean[5,y], length.out = 50)),
		Dry.WoodyCrop = c(seq(1, zonalmean[6,x], length.out = 40),
			seq(zonalmean[6,x], zonalmean[6,y], length.out = 50)),
		Mesic.Forest = c(seq(1, zonalmean[7,x], length.out = 40),
			seq(zonalmean[7,x], zonalmean[7,y], length.out = 50)),
		Mesic.Grassland = c(seq(1, zonalmean[8,x], length.out = 40),
			seq(zonalmean[8,x], zonalmean[8,y], length.out = 50)),
		Mesic.Shrubland = c(seq(1, zonalmean[9,x], length.out = 40),
			seq(zonalmean[9,x], zonalmean[9,y], length.out = 50)),
		Mesic.Plantation = c(seq(1, zonalmean[10,x], length.out = 40),
			seq(zonalmean[10,x], zonalmean[10,y], length.out = 50)),
		Mesic.Agriculture = c(seq(1, zonalmean[11,x], length.out = 40),
			seq(zonalmean[11,x], zonalmean[11,y], length.out = 50)),
		Mesic.WoodyCrop = c(seq(1, zonalmean[12,x], length.out = 40),
			seq(zonalmean[12,x], zonalmean[12,y], length.out = 50)),
		Wet.Forest = c(seq(1, zonalmean[13,x], length.out = 40),
			seq(zonalmean[13,x], zonalmean[13,y], length.out = 50)),
		Wet.Grassland = c(seq(1, zonalmean[14,x], length.out = 40),
			seq(zonalmean[14,x], zonalmean[14,y], length.out = 50)),
		Wet.Shrubland = c(seq(1, zonalmean[15,x], length.out = 40),
			seq(zonalmean[15,x], zonalmean[15,y], length.out = 50)),
		Wet.Plantation = c(seq(1, zonalmean[16,x], length.out = 40),
			seq(zonalmean[16,x], zonalmean[16,y], length.out = 50))) %>%
	pivot_longer(-Timestep, names_to = "MZ_SC", values_to = "Value") %>%
	arrange(MZ_SC) %>%
	mutate(SecondaryStratumID = "O'ahu") %>%
	data.frame()
}

# Kauai annual NPP multiplier increments
Kauaigmseq<- function(x,y) {
	data.frame(Timestep = c(2011:2100),
		Dry.Forest = c(seq(1, zonalmean[1,x], length.out = 40), 
			seq(zonalmean[1,x], zonalmean[1,y], length.out = 50)),
		Dry.Grassland = c(seq(1, zonalmean[2,x], length.out = 40),
			seq(zonalmean[2,x], zonalmean[2,y], length.out = 50)),
		Dry.Shrubland = c(seq(1, zonalmean[3,x], length.out = 40),
			seq(zonalmean[3,x], zonalmean[3,y], length.out = 50)),
		Dry.Plantation = c(seq(1, zonalmean[4,x], length.out = 40),
			seq(zonalmean[4,x], zonalmean[4,y], length.out = 50)),
		Dry.Agriculture = c(seq(1, zonalmean[5,x], length.out = 40),
			seq(zonalmean[5,x], zonalmean[5,y], length.out = 50)),
		Dry.WoodyCrop = c(seq(1, zonalmean[6,x], length.out = 40),
			seq(zonalmean[6,x], zonalmean[6,y], length.out = 50)),
		Mesic.Forest = c(seq(1, zonalmean[7,x], length.out = 40),
			seq(zonalmean[7,x], zonalmean[7,y], length.out = 50)),
		Mesic.Grassland = c(seq(1, zonalmean[8,x], length.out = 40),
			seq(zonalmean[8,x], zonalmean[8,y], length.out = 50)),
		Mesic.Shrubland = c(seq(1, zonalmean[9,x], length.out = 40),
			seq(zonalmean[9,x], zonalmean[9,y], length.out = 50)),
		Mesic.Plantation = c(seq(1, zonalmean[10,x], length.out = 40),
			seq(zonalmean[10,x], zonalmean[10,y], length.out = 50)),
		Mesic.Agriculture = c(seq(1, zonalmean[11,x], length.out = 40),
			seq(zonalmean[11,x], zonalmean[11,y], length.out = 50)),
		Mesic.WoodyCrop = c(seq(1, zonalmean[12,x], length.out = 40),
			seq(zonalmean[12,x], zonalmean[12,y], length.out = 50)),
		Wet.Forest = c(seq(1, zonalmean[13,x], length.out = 40),
			seq(zonalmean[13,x], zonalmean[13,y], length.out = 50)),
		Wet.Grassland = c(seq(1, zonalmean[14,x], length.out = 40),
			seq(zonalmean[14,x], zonalmean[14,y], length.out = 50)),
		Wet.Shrubland = c(seq(1, zonalmean[15,x], length.out = 40),
			seq(zonalmean[15,x], zonalmean[15,y], length.out = 50)),
		Wet.Plantation = c(seq(1, zonalmean[16,x], length.out = 40),
			seq(zonalmean[16,x], zonalmean[16,y], length.out = 50)),
		Wet.Agriculture = c(seq(1, zonalmean[17,x], length.out = 40),
			seq(zonalmean[17,x], zonalmean[17,y], length.out = 50))) %>%
	pivot_longer(-Timestep, names_to = "MZ_SC", values_to = "Value") %>%
	arrange(MZ_SC) %>%
	mutate(SecondaryStratumID = "Kaua'i") %>%
	data.frame()
}

# Lanai annual NPP multiplier increments
Lanaigmseq<- function(x,y) {
	data.frame(Timestep = c(2011:2100),
		Dry.Forest = c(seq(1, zonalmean[1,x], length.out = 40), 
			seq(zonalmean[1,x], zonalmean[1,y], length.out = 50)),
		Dry.Grassland = c(seq(1, zonalmean[2,x], length.out = 40),
			seq(zonalmean[2,x], zonalmean[2,y], length.out = 50)),
		Dry.Shrubland = c(seq(1, zonalmean[3,x], length.out = 40),
			seq(zonalmean[3,x], zonalmean[3,y], length.out = 50)),	
		Dry.Agriculture = c(seq(1, zonalmean[5,x], length.out = 40),
			seq(zonalmean[5,x], zonalmean[5,y], length.out = 50)),	
		Mesic.Forest = c(seq(1, zonalmean[7,x], length.out = 40),
			seq(zonalmean[7,x], zonalmean[7,y], length.out = 50)),
		Mesic.Grassland = c(seq(1, zonalmean[8,x], length.out = 40),
			seq(zonalmean[8,x], zonalmean[8,y], length.out = 50)),
		Mesic.Shrubland = c(seq(1, zonalmean[9,x], length.out = 40),
			seq(zonalmean[9,x], zonalmean[9,y], length.out = 50)),	
		Wet.Forest = c(seq(1, zonalmean[13,x], length.out = 40),
			seq(zonalmean[13,x], zonalmean[13,y], length.out = 50)),
		Wet.Grassland = c(seq(1, zonalmean[14,x], length.out = 40),
			seq(zonalmean[14,x], zonalmean[14,y], length.out = 50)),
		Wet.Shrubland = c(seq(1, zonalmean[15,x], length.out = 40),
			seq(zonalmean[15,x], zonalmean[15,y], length.out = 50))) %>%
	pivot_longer(-Timestep, names_to = "MZ_SC", values_to = "Value") %>%
	arrange(MZ_SC) %>%
	mutate(SecondaryStratumID = "Lana'i") %>%
	data.frame()
}

# Maui annual NPP multiplier increments
Mauigmseq<- function(x,y) {
	data.frame(Timestep = c(2011:2100),
		Dry.Forest = c(seq(1, zonalmean[1,x], length.out = 40), 
			seq(zonalmean[1,x], zonalmean[1,y], length.out = 50)),
		Dry.Grassland = c(seq(1, zonalmean[2,x], length.out = 40),
			seq(zonalmean[2,x], zonalmean[2,y], length.out = 50)),
		Dry.Shrubland = c(seq(1, zonalmean[3,x], length.out = 40),
			seq(zonalmean[3,x], zonalmean[3,y], length.out = 50)),
		Dry.Agriculture = c(seq(1, zonalmean[5,x], length.out = 40),
			seq(zonalmean[5,x], zonalmean[5,y], length.out = 50)),
		Dry.WoodyCrop = c(seq(1, zonalmean[6,x], length.out = 40),
			seq(zonalmean[6,x], zonalmean[6,y], length.out = 50)),
		Mesic.Forest = c(seq(1, zonalmean[7,x], length.out = 40),
			seq(zonalmean[7,x], zonalmean[7,y], length.out = 50)),
		Mesic.Grassland = c(seq(1, zonalmean[8,x], length.out = 40),
			seq(zonalmean[8,x], zonalmean[8,y], length.out = 50)),
		Mesic.Shrubland = c(seq(1, zonalmean[9,x], length.out = 40),
			seq(zonalmean[9,x], zonalmean[9,y], length.out = 50)),	
		Mesic.Agriculture = c(seq(1, zonalmean[11,x], length.out = 40),
			seq(zonalmean[11,x], zonalmean[11,y], length.out = 50)),
		Mesic.WoodyCrop = c(seq(1, zonalmean[12,x], length.out = 40),
			seq(zonalmean[12,x], zonalmean[12,y], length.out = 50)),
		Wet.Forest = c(seq(1, zonalmean[13,x], length.out = 40),
			seq(zonalmean[13,x], zonalmean[13,y], length.out = 50)),
		Wet.Grassland = c(seq(1, zonalmean[14,x], length.out = 40),
			seq(zonalmean[14,x], zonalmean[14,y], length.out = 50)),
		Wet.Shrubland = c(seq(1, zonalmean[15,x], length.out = 40),
			seq(zonalmean[15,x], zonalmean[15,y], length.out = 50)),
		Wet.Agriculture = c(seq(1, zonalmean[17,x], length.out = 40),
			seq(zonalmean[17,x], zonalmean[17,y], length.out = 50))) %>%	
	pivot_longer(-Timestep, names_to = "MZ_SC", values_to = "Value") %>%
	arrange(MZ_SC) %>%
	mutate(SecondaryStratumID = "Maui") %>%
	data.frame()
}
			
# Molokai annual NPP multiplier increments
Molokaigmseq<- function(x,y) {
	data.frame(Timestep = c(2011:2100),
		Dry.Forest = c(seq(1, zonalmean[1,x], length.out = 40), 
			seq(zonalmean[1,x], zonalmean[1,y], length.out = 50)),
		Dry.Grassland = c(seq(1, zonalmean[2,x], length.out = 40),
			seq(zonalmean[2,x], zonalmean[2,y], length.out = 50)),
		Dry.Shrubland = c(seq(1, zonalmean[3,x], length.out = 40),
			seq(zonalmean[3,x], zonalmean[3,y], length.out = 50)),
		Dry.Agriculture = c(seq(1, zonalmean[5,x], length.out = 40),
			seq(zonalmean[5,x], zonalmean[5,y], length.out = 50)),
		Dry.WoodyCrop = c(seq(1, zonalmean[6,x], length.out = 40),
			seq(zonalmean[6,x], zonalmean[6,y], length.out = 50)),
		Mesic.Forest = c(seq(1, zonalmean[7,x], length.out = 40),
			seq(zonalmean[7,x], zonalmean[7,y], length.out = 50)),
		Mesic.Grassland = c(seq(1, zonalmean[8,x], length.out = 40),
			seq(zonalmean[8,x], zonalmean[8,y], length.out = 50)),
		Mesic.Shrubland = c(seq(1, zonalmean[9,x], length.out = 40),
			seq(zonalmean[9,x], zonalmean[9,y], length.out = 50)),
		Mesic.Plantation = c(seq(1, zonalmean[10,x], length.out = 40),
			seq(zonalmean[10,x], zonalmean[10,y], length.out = 50)),
		Wet.Forest = c(seq(1, zonalmean[13,x], length.out = 40),
			seq(zonalmean[13,x], zonalmean[13,y], length.out = 50)),
		Wet.Grassland = c(seq(1, zonalmean[14,x], length.out = 40),
			seq(zonalmean[14,x], zonalmean[14,y], length.out = 50)),
		Wet.Shrubland = c(seq(1, zonalmean[15,x], length.out = 40),
			seq(zonalmean[15,x], zonalmean[15,y], length.out = 50)),
		Wet.Plantation = c(seq(1, zonalmean[16,x], length.out = 40),
			seq(zonalmean[16,x], zonalmean[16,y], length.out = 50))) %>%
	pivot_longer(-Timestep, names_to = "MZ_SC", values_to = "Value") %>%
	arrange(MZ_SC) %>%
	mutate(SecondaryStratumID = "Moloka'i") %>%
	data.frame()
}

# Make annual growth multiplier sequences for RCPs 4.5 & 8.5
Hawaiigm45 <- Hawaiigmseq(3,2)
Hawaiigm85 <- Hawaiigmseq(5,4)
Kahoolawegm45 <- Kahoolawegmseq(7,6)
Kahoolawegm85 <- Kahoolawegmseq(9,8)
Kauaigm45 <- Kauaigmseq(11,10)
Kauaigm85 <- Kauaigmseq(13,12)
Lanaigm45 <- Lanaigmseq(15,14)
Lanaigm85 <- Lanaigmseq(17,16)
Mauigm45 <- Mauigmseq(19,18)
Mauigm85 <- Mauigmseq(21,20)
Molokaigm45 <- Molokaigmseq(23,22)
Molokaigm85 <- Molokaigmseq(25,24)
Oahugm45 <- Oahugmseq(27,26)
Oahugm85 <- Oahugmseq(29,28)

# create list of Island growth multiplier sequences for RCP 4.5
gm45list<- list(Hawaiigm45, Kahoolawegm45, Kauaigm45, Lanaigm45, 
	Mauigm45, Molokaigm45, Oahugm45)
# create list of Island growth multiplier sequences for RCP 8.5
gm85list<- list(Hawaiigm85, Kahoolawegm85, Kauaigm85, Lanaigm85, 
	Mauigm85, Molokaigm85, Oahugm85)

# create function to bind growth multiplier sequences together 
# and format as Flow Multiplier table for ST-Sim 
gmall <- function(x) {
	bind_rows(x) %>%
	separate(MZ_SC, c("StratumID", "StateClassID")) %>%
	mutate(StateClassID = paste(.$StateClassID, ":All", sep = ""),
		FlowGroupID = "Growth [Type]", 
		DistributionType = "Normal", 
		DistributionFrequencyID = "Iteration and Timestep",
		DistributionSD = 0.025) %>%
	select(Timestep, StratumID, SecondaryStratumID, StateClassID, FlowGroupID,
		Value, DistributionType, DistributionFrequencyID, DistributionSD)
}

# create final NPP multiplier data frames for RCPs 4.5 and 8.5
NPPmulti45 <- gmall(gm45list)
NPPmulti85 <- gmall(gm85list)

# write NPP multiplier data frames to .csv files
write.csv(NPPmulti45, "./Model_InputData/RCP45_NPPmultipliers.csv", row.names = FALSE)
write.csv(NPPmulti85, "./Model_InputData/RCP85_NPPmultipliers.csv", row.names = FALSE)

