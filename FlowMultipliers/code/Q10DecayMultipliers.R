## Calculate future flow multipliers for decay and 
## CO2 efflux for litter, down dead wood, and soil
## based on statistically downscaled
## CMIP5 projections of future temperature
## for RCP 4.5 and RCP 8.5. 
## Paul C. Selmants
## 2020-03-23 (ISO 8601)

# load required packages into R
library(raster)
library(rgdal)
library(tidyr, warn.conflicts = FALSE)
library(dplyr, warn.conflicts = FALSE)

# read in raster of combined Moisture Zones and State Classes, where:
# 1 = Dry Forest	 	 7 = Mesic Forest			13 = Wet Forest
# 2 = Dry Grass		 	 8 = Mesic Grass			14 = Wet Grass
# 3 = Dry Shrub		 	 9 = Mesic Shrub			15 = Wet Shrub 
# 4 = Dry Plantation	10 = Mesic Plantation		16 = Wet Plantation
# 5 = Dry Ag			11 = Mesic Ag				17 = Wet Ag
# 6 = Dry WoodyCrop		12 = Mesic WoodyCrop		18 = Wet WoodyCrop
MZSC <- raster("MZSC.tif")

# create list of individual island .tif files
islandlist <- list.files('islands', full.names = TRUE)
#create raster stack of individual island .tif files
islandstack <- stack(islandlist)

# mask MZ-specific SC raster with raster stack of islands
mzsc_island <- mask(MZSC, islandstack) 
names(mzsc_island) <- names(islandstack)

# create list of temperature delta .tif files
tdeltalist <- list.files(pattern = 'sd_Tdeltas', full.names = TRUE)
# stack temperature delta rasters and match projection to MZSC
tdeltastack <- stack(tdeltalist) %>%
	projectRaster(., MZSC, method = 'bilinear')
# create a vector of rcp names
rcpnames <- c('rcp45late', 'rcp45mid', 'rcp85late', 'rcp85mid')
# rename layers in Tdeltastack
names(tdeltastack) <- rcpnames

# Create raster stack of future litter decay and litter emission multipliers 
# for RCPs 4.5 & 8.5 using Q10 = 2.17 (from Bothwell et al. 2014) 
Q10litter <- 2.17^(1/(10/tdeltastack))
# Create raster stack of general decay input multipliers
# for RCPs 4.5 & 8.5 using Q10 = 2.0 (Kurz et al. 2009) 
Q10general <- 2.0^(1/(10/tdeltastack))

# create function to mask Q10 multiplier stacks with individual rasters
# from islandstack
islandmask <- function(x,y) {
	mask(x, islandstack[[y]])
}

# mask general Q10 multiplier stack with island rasters, rename layers
Hawaii_Q10gen <- islandmask(Q10general, 1)
names(Hawaii_Q10gen) <- paste(rcpnames, "Hawaii", sep = "_")
Kahoolawe_Q10gen <- islandmask(Q10general,2)
names(Kahoolawe_Q10gen) <- paste(rcpnames, "Kahoolawe", sep = "_")
Lanai_Q10gen <- islandmask(Q10general,4)
names(Lanai_Q10gen) <- paste(rcpnames, "Lanai", sep = "_")
Maui_Q10gen <- islandmask(Q10general,5)
names(Maui_Q10gen) <- paste(rcpnames, "Maui", sep = "_")
Molokai_Q10gen <- islandmask(Q10general,6)
names(Molokai_Q10gen) <- paste(rcpnames, "Molokai", sep = "_")
Oahu_Q10gen <- islandmask(Q10general,7)
names(Oahu_Q10gen) <- paste(rcpnames, "Oahu", sep = "_")
Kauai_Q10gen <- islandmask(Q10general,3)
names(Kauai_Q10gen) <- paste(rcpnames, "Kauai", sep = "_")

# mask litter Q10 multiplier stack with island rasters, rename layers
Hawaii_Q10lit <- islandmask(Q10litter, 1)
names(Hawaii_Q10lit) <- paste(rcpnames, "Hawaii", sep = "_")
Kahoolawe_Q10lit <- islandmask(Q10litter,2)
names(Kahoolawe_Q10lit) <- paste(rcpnames, "Kahoolawe", sep = "_")
Lanai_Q10lit <- islandmask(Q10litter,4)
names(Lanai_Q10lit) <- paste(rcpnames, "Lanai", sep = "_")
Maui_Q10lit <- islandmask(Q10litter,5)
names(Maui_Q10lit) <- paste(rcpnames, "Maui", sep = "_")
Molokai_Q10lit <- islandmask(Q10litter,6)
names(Molokai_Q10lit) <- paste(rcpnames, "Molokai", sep = "_")
Oahu_Q10lit <- islandmask(Q10litter,7)
names(Oahu_Q10lit) <- paste(rcpnames, "Oahu", sep = "_")
Kauai_Q10lit <- islandmask(Q10litter,3)
names(Kauai_Q10lit) <- paste(rcpnames, "Kauai", sep = "_")

# create list of general Q10 island raster stacks
genQ10stacks <- mget(ls(pattern = "_Q10gen"))
# create brick of general Q10 island raster stacks
genQ10islandbrick <- brick(genQ10stacks) 
# create list of litter Q10 island raster stacks
litQ10stacks <- mget(ls(pattern = "_Q10lit"))
# create brick of litter Q10 island raster stacks
litQ10islandbrick <- brick(litQ10stacks)

# calculate zonal means for Q10 multipliers
zonalmean_gen <- zonal(genQ10islandbrick, MZSC, fun = "mean", na.rm = TRUE) %>%
	data.frame()
zonalmean_lit <- zonal(litQ10islandbrick, MZSC, fun = "mean", na.rm = TRUE) %>%
	data.frame()

# create function to calculate zonal sd for Q10 multipliers and re-format
zonalsd <- function(x) {
	zonal(x, MZSC, fun = "sd", na.rm = TRUE) %>%
	data.frame() %>%
	mutate(MZ = rep(c("Dry", "Mesic", "Wet"), each = 6)) %>%
	pivot_longer(-c(zone,MZ), names_to = "rcp_island", values_to = "Q10_sd") %>%
	group_by(MZ, rcp_island) %>%
	summarize(sd_mean = mean(Q10_sd, na.rm = TRUE)) %>%
	separate(rcp_island, c("RCPtime", "Island")) %>%
	arrange(Island) %>%
	filter(sd_mean > 0)
}

# calculate zonal sd for Q10 multipliers and re-format
zonalsd_gen <- zonalsd(genQ10islandbrick)
zonalsd_lit <- zonalsd(litQ10islandbrick)

# filter Q10 zonal sd dataframes by RCP
genQ10sd45 <- filter(zonalsd_gen, grepl('rcp45', RCPtime)) %>%
	data.frame()
genQ10sd85 <- filter(zonalsd_gen, grepl('rcp85', RCPtime)) %>%
	data.frame()
litQ10sd45 <- filter(zonalsd_lit, grepl('rcp45', RCPtime)) %>%
	data.frame()
litQ10sd85 <- filter(zonalsd_lit, grepl('rcp85', RCPtime)) %>%
	data.frame()

# create sdyear function
sdyear <- function(y) {
	data.frame(
		"Timestep" = rep(c(2011:2100), 91),
		"SecondaryStratumID" = c(rep("Hawai'i", 1620), rep("Kaho'olawe", 270),
			rep("Kaua'i", 1530), rep("Lanai'i", 900), rep("Maui", 1260), 
			rep("Moloka'i", 1170), rep("O'ahu", 1440)), 
		"StratumID" = c(rep(c("Dry", "Mesic", "Wet"), each = 540), rep("Dry", 270), 
			rep(c("Dry", "Mesic", "Wet"), each = 510), rep(c("Dry", "Mesic", "Wet"), each = 300),
			rep(c("Dry", "Mesic", "Wet"), each = 420), rep(c("Dry", "Mesic", "Wet"), each = 390),
			rep(c("Dry", "Mesic", "Wet"), each = 480)),
		"DistributionSD" = c(
			rep(c(rep(y[2,4],40), rep(y[1,4],50)),6),
			rep(c(rep(y[4,4],40), rep(y[3,4],50)),6),
			rep(c(rep(y[6,4],40), rep(y[5,4],50)),6),
			rep(c(rep(y[8,4],40), rep(y[7,4],50)),3),
			rep(c(rep(y[10,4],40), rep(y[9,4],50)),6),
			rep(c(rep(y[12,4],40), rep(y[11,4],50)),6),
			rep(c(rep(y[14,4],40), rep(y[13,4],50)),4),
			rep(c(rep(y[16,4],40), rep(y[15,4],50)),6),
			rep(c(rep(y[18,4],40), rep(y[17,4],50)),6),
			rep(c(rep(y[20,4],40), rep(y[19,4],50)),5),
			rep(c(rep(y[22,4],40), rep(y[21,4],50)),4),
			rep(c(rep(y[24,4],40), rep(y[23,4],50)),3),
			rep(c(rep(y[26,4],40), rep(y[25,4],50)),3),
			rep(c(rep(y[28,4],40), rep(y[27,4],50)),5),
			rep(c(rep(y[30,4],40), rep(y[29,4],50)),5),
			rep(c(rep(y[32,4],40), rep(y[31,4],50)),4),
			rep(c(rep(y[34,4],40), rep(y[33,4],50)),5),
			rep(c(rep(y[36,4],40), rep(y[35,4],50)),4),
			rep(c(rep(y[38,4],40), rep(y[37,4],50)),4)))
		
}

# create sd dataframes by year, MZ, and Island for each RCP
yearQ10gensd45 <- sdyear(genQ10sd45)
yearQ10gensd85 <- sdyear(genQ10sd85)
yearQ10litsd45 <- sdyear(litQ10sd45)
yearQ10litsd85 <- sdyear(litQ10sd85)

### create functions to construct Q10 multiplier annual increments ###

# Island of Hawaii annual Q10 multiplier increments
HawaiiQ10seq <- function(x,y,z) {
	data.frame(Timestep = c(2011:2100),
		Dry.Forest = c(seq(1, x[1,y], length.out = 40), 
			seq(x[1,y], x[1,z], length.out = 50)),
		Dry.Grassland = c(seq(1, x[2,y], length.out = 40),
			seq(x[2,y], x[2,z], length.out = 50)),
		Dry.Shrubland = c(seq(1, x[3,y], length.out = 40),
			seq(x[3,y], x[3,z], length.out = 50)),
		Dry.Plantation = c(seq(1, x[4,y], length.out = 40),
			seq(x[4,y], x[4,z], length.out = 50)),
		Dry.Agriculture = c(seq(1, x[5,y], length.out = 40),
			seq(x[5,y], x[5,z], length.out = 50)),
		Dry.WoodyCrop = c(seq(1, x[6,y], length.out = 40),
			seq(x[6,y], x[6,z], length.out = 50)),
		Mesic.Forest = c(seq(1, x[7,y], length.out = 40),
			seq(x[7,y], x[7,z], length.out = 50)),
		Mesic.Grassland = c(seq(1, x[8,y], length.out = 40),
			seq(x[8,y], x[8,z], length.out = 50)),
		Mesic.Shrubland = c(seq(1, x[9,y], length.out = 40),
			seq(x[9,y], x[9,z], length.out = 50)),
		Mesic.Plantation = c(seq(1, x[10,y], length.out = 40),
			seq(x[10,y], x[10,z], length.out = 50)),
		Mesic.Agriculture = c(seq(1, x[11,y], length.out = 40),
			seq(x[11,y], x[11,z], length.out = 50)),
		Mesic.WoodyCrop = c(seq(1, x[12,y], length.out = 40),
			seq(x[12,y], x[12,z], length.out = 50)),
		Wet.Forest = c(seq(1, x[13,y], length.out = 40),
			seq(x[13,y], x[13,z], length.out = 50)),
		Wet.Grassland = c(seq(1, x[14,y], length.out = 40),
			seq(x[14,y], x[14,z], length.out = 50)),
		Wet.Shrubland = c(seq(1, x[15,y], length.out = 40),
			seq(x[15,y], x[15,z], length.out = 50)),
		Wet.Plantation = c(seq(1, x[16,y], length.out = 40),
			seq(x[16,y], x[16,z], length.out = 50)),
		Wet.Agriculture = c(seq(1, x[17,y], length.out = 40),
			seq(x[17,y], x[17,z], length.out = 50)),
		Wet.WoodyCrop = c(seq(1, x[18,y], length.out = 40),
			seq(x[18,y], x[18,z], length.out = 50))) %>%
	pivot_longer(-Timestep, names_to = "MZ_SC", values_to = "Value") %>%
	arrange(MZ_SC) %>%
	mutate(SecondaryStratumID = "Hawai'i") %>%
	data.frame()
}
# Kahoolawe annual Q10 multiplier increments
KahoolaweQ10seq <- function(x,y,z) {
	data.frame(Timestep = c(2011:2100),
		Dry.Forest = c(seq(1, x[1,y], length.out = 40), 
			seq(x[1,y], x[1,z], length.out = 50)),
		Dry.Grassland = c(seq(1, x[2,y], length.out = 40),
			seq(x[2,y], x[2,z], length.out = 50)),
		Dry.Shrubland = c(seq(1, x[3,y], length.out = 40),
			seq(x[3,y], x[3,z], length.out = 50))) %>%
	pivot_longer(-Timestep, names_to = "MZ_SC", values_to = "Value") %>%
	arrange(MZ_SC) %>%
	mutate(SecondaryStratumID = "Kaho'olawe") %>%
	data.frame()
}
# Oahu annual Q10 multiplier increments
OahuQ10seq<- function(x,y,z) {
	data.frame(Timestep = c(2011:2100),
		Dry.Forest = c(seq(1, x[1,y], length.out = 40), 
			seq(x[1,y], x[1,z], length.out = 50)),
		Dry.Grassland = c(seq(1, x[2,y], length.out = 40),
			seq(x[2,y], x[2,z], length.out = 50)),
		Dry.Shrubland = c(seq(1, x[3,y], length.out = 40),
			seq(x[3,y], x[3,z], length.out = 50)),
		Dry.Plantation = c(seq(1, x[4,y], length.out = 40),
			seq(x[4,y], x[4,z], length.out = 50)),
		Dry.Agriculture = c(seq(1, x[5,y], length.out = 40),
			seq(x[5,y], x[5,z], length.out = 50)),
		Dry.WoodyCrop = c(seq(1, x[6,y], length.out = 40),
			seq(x[6,y], x[6,z], length.out = 50)),
		Mesic.Forest = c(seq(1, x[7,y], length.out = 40),
			seq(x[7,y], x[7,z], length.out = 50)),
		Mesic.Grassland = c(seq(1, x[8,y], length.out = 40),
			seq(x[8,y], x[8,z], length.out = 50)),
		Mesic.Shrubland = c(seq(1, x[9,y], length.out = 40),
			seq(x[9,y], x[9,z], length.out = 50)),
		Mesic.Plantation = c(seq(1, x[10,y], length.out = 40),
			seq(x[10,y], x[10,z], length.out = 50)),
		Mesic.Agriculture = c(seq(1, x[11,y], length.out = 40),
			seq(x[11,y], x[11,z], length.out = 50)),
		Mesic.WoodyCrop = c(seq(1, x[12,y], length.out = 40),
			seq(x[12,y], x[12,z], length.out = 50)),
		Wet.Forest = c(seq(1, x[13,y], length.out = 40),
			seq(x[13,y], x[13,z], length.out = 50)),
		Wet.Grassland = c(seq(1, x[14,y], length.out = 40),
			seq(x[14,y], x[14,z], length.out = 50)),
		Wet.Shrubland = c(seq(1, x[15,y], length.out = 40),
			seq(x[15,y], x[15,z], length.out = 50)),
		Wet.Plantation = c(seq(1, x[16,y], length.out = 40),
			seq(x[16,y], x[16,z], length.out = 50))) %>%
	pivot_longer(-Timestep, names_to = "MZ_SC", values_to = "Value") %>%
	arrange(MZ_SC) %>%
	mutate(SecondaryStratumID = "O'ahu") %>%
	data.frame()
}
# Kauai annual Q10 multiplier increments
KauaiQ10seq<- function(x,y,z) {
	data.frame(Timestep = c(2011:2100),
		Dry.Forest = c(seq(1, x[1,y], length.out = 40), 
			seq(x[1,y], x[1,z], length.out = 50)),
		Dry.Grassland = c(seq(1, x[2,y], length.out = 40),
			seq(x[2,y], x[2,z], length.out = 50)),
		Dry.Shrubland = c(seq(1, x[3,y], length.out = 40),
			seq(x[3,y], x[3,z], length.out = 50)),
		Dry.Plantation = c(seq(1, x[4,y], length.out = 40),
			seq(x[4,y], x[4,z], length.out = 50)),
		Dry.Agriculture = c(seq(1, x[5,y], length.out = 40),
			seq(x[5,y], x[5,z], length.out = 50)),
		Dry.WoodyCrop = c(seq(1, x[6,y], length.out = 40),
			seq(x[6,y], x[6,z], length.out = 50)),
		Mesic.Forest = c(seq(1, x[7,y], length.out = 40),
			seq(x[7,y], x[7,z], length.out = 50)),
		Mesic.Grassland = c(seq(1, x[8,y], length.out = 40),
			seq(x[8,y], x[8,z], length.out = 50)),
		Mesic.Shrubland = c(seq(1, x[9,y], length.out = 40),
			seq(x[9,y], x[9,z], length.out = 50)),
		Mesic.Plantation = c(seq(1, x[10,y], length.out = 40),
			seq(x[10,y], x[10,z], length.out = 50)),
		Mesic.Agriculture = c(seq(1, x[11,y], length.out = 40),
			seq(x[11,y], x[11,z], length.out = 50)),
		Mesic.WoodyCrop = c(seq(1, x[12,y], length.out = 40),
			seq(x[12,y], x[12,z], length.out = 50)),
		Wet.Forest = c(seq(1, x[13,y], length.out = 40),
			seq(x[13,y], x[13,z], length.out = 50)),
		Wet.Grassland = c(seq(1, x[14,y], length.out = 40),
			seq(x[14,y], x[14,z], length.out = 50)),
		Wet.Shrubland = c(seq(1, x[15,y], length.out = 40),
			seq(x[15,y], x[15,z], length.out = 50)),
		Wet.Plantation = c(seq(1, x[16,y], length.out = 40),
			seq(x[16,y], x[16,z], length.out = 50)),
		Wet.Agriculture = c(seq(1, x[17,y], length.out = 40),
			seq(x[17,y], x[17,z], length.out = 50))) %>%
	pivot_longer(-Timestep, names_to = "MZ_SC", values_to = "Value") %>%
	arrange(MZ_SC) %>%
	mutate(SecondaryStratumID = "Kaua'i") %>%
	data.frame()
}
# Lanai annual Q10 multiplier increments
LanaiQ10seq<- function(x,y,z) {
	data.frame(Timestep = c(2011:2100),
		Dry.Forest = c(seq(1, x[1,y], length.out = 40), 
			seq(x[1,y], x[1,z], length.out = 50)),
		Dry.Grassland = c(seq(1, x[2,y], length.out = 40),
			seq(x[2,y], x[2,z], length.out = 50)),
		Dry.Shrubland = c(seq(1, x[3,y], length.out = 40),
			seq(x[3,y], x[3,z], length.out = 50)),	
		Dry.Agriculture = c(seq(1, x[5,y], length.out = 40),
			seq(x[5,y], x[5,z], length.out = 50)),	
		Mesic.Forest = c(seq(1, x[7,y], length.out = 40),
			seq(x[7,y], x[7,z], length.out = 50)),
		Mesic.Grassland = c(seq(1, x[8,y], length.out = 40),
			seq(x[8,y], x[8,z], length.out = 50)),
		Mesic.Shrubland = c(seq(1, x[9,y], length.out = 40),
			seq(x[9,y], x[9,z], length.out = 50)),	
		Wet.Forest = c(seq(1, x[13,y], length.out = 40),
			seq(x[13,y], x[13,z], length.out = 50)),
		Wet.Grassland = c(seq(1, x[14,y], length.out = 40),
			seq(x[14,y], x[14,z], length.out = 50)),
		Wet.Shrubland = c(seq(1, x[15,y], length.out = 40),
			seq(x[15,y], x[15,z], length.out = 50))) %>%
	pivot_longer(-Timestep, names_to = "MZ_SC", values_to = "Value") %>%
	arrange(MZ_SC) %>%
	mutate(SecondaryStratumID = "Lana'i") %>%
	data.frame()
}
# Maui annual Q10 multiplier increments
MauiQ10seq<- function(x,y,z) {
	data.frame(Timestep = c(2011:2100),
		Dry.Forest = c(seq(1, x[1,y], length.out = 40), 
			seq(x[1,y], x[1,z], length.out = 50)),
		Dry.Grassland = c(seq(1, x[2,y], length.out = 40),
			seq(x[2,y], x[2,z], length.out = 50)),
		Dry.Shrubland = c(seq(1, x[3,y], length.out = 40),
			seq(x[3,y], x[3,z], length.out = 50)),
		Dry.Agriculture = c(seq(1, x[5,y], length.out = 40),
			seq(x[5,y], x[5,z], length.out = 50)),
		Dry.WoodyCrop = c(seq(1, x[6,y], length.out = 40),
			seq(x[6,y], x[6,z], length.out = 50)),
		Mesic.Forest = c(seq(1, x[7,y], length.out = 40),
			seq(x[7,y], x[7,z], length.out = 50)),
		Mesic.Grassland = c(seq(1, x[8,y], length.out = 40),
			seq(x[8,y], x[8,z], length.out = 50)),
		Mesic.Shrubland = c(seq(1, x[9,y], length.out = 40),
			seq(x[9,y], x[9,z], length.out = 50)),	
		Mesic.Agriculture = c(seq(1, x[11,y], length.out = 40),
			seq(x[11,y], x[11,z], length.out = 50)),
		Mesic.WoodyCrop = c(seq(1, x[12,y], length.out = 40),
			seq(x[12,y], x[12,z], length.out = 50)),
		Wet.Forest = c(seq(1, x[13,y], length.out = 40),
			seq(x[13,y], x[13,z], length.out = 50)),
		Wet.Grassland = c(seq(1, x[14,y], length.out = 40),
			seq(x[14,y], x[14,z], length.out = 50)),
		Wet.Shrubland = c(seq(1, x[15,y], length.out = 40),
			seq(x[15,y], x[15,z], length.out = 50)),
		Wet.Agriculture = c(seq(1, x[17,y], length.out = 40),
			seq(x[17,y], x[17,z], length.out = 50))) %>%	
	pivot_longer(-Timestep, names_to = "MZ_SC", values_to = "Value") %>%
	arrange(MZ_SC) %>%
	mutate(SecondaryStratumID = "Maui") %>%
	data.frame()
}
# Molokai annual Q10 multiplier increments
MolokaiQ10seq<- function(x,y,z) {
	data.frame(Timestep = c(2011:2100),
		Dry.Forest = c(seq(1, x[1,y], length.out = 40), 
			seq(x[1,y], x[1,z], length.out = 50)),
		Dry.Grassland = c(seq(1, x[2,y], length.out = 40),
			seq(x[2,y], x[2,z], length.out = 50)),
		Dry.Shrubland = c(seq(1, x[3,y], length.out = 40),
			seq(x[3,y], x[3,z], length.out = 50)),
		Dry.Agriculture = c(seq(1, x[5,y], length.out = 40),
			seq(x[5,y], x[5,z], length.out = 50)),
		Dry.WoodyCrop = c(seq(1, x[6,y], length.out = 40),
			seq(x[6,y], x[6,z], length.out = 50)),
		Mesic.Forest = c(seq(1, x[7,y], length.out = 40),
			seq(x[7,y], x[7,z], length.out = 50)),
		Mesic.Grassland = c(seq(1, x[8,y], length.out = 40),
			seq(x[8,y], x[8,z], length.out = 50)),
		Mesic.Shrubland = c(seq(1, x[9,y], length.out = 40),
			seq(x[9,y], x[9,z], length.out = 50)),
		Mesic.Plantation = c(seq(1, x[10,y], length.out = 40),
			seq(x[10,y], x[10,z], length.out = 50)),
		Wet.Forest = c(seq(1, x[13,y], length.out = 40),
			seq(x[13,y], x[13,z], length.out = 50)),
		Wet.Grassland = c(seq(1, x[14,y], length.out = 40),
			seq(x[14,y], x[14,z], length.out = 50)),
		Wet.Shrubland = c(seq(1, x[15,y], length.out = 40),
			seq(x[15,y], x[15,z], length.out = 50)),
		Wet.Plantation = c(seq(1, x[16,y], length.out = 40),
			seq(x[16,y], x[16,z], length.out = 50))) %>%
	pivot_longer(-Timestep, names_to = "MZ_SC", values_to = "Value") %>%
	arrange(MZ_SC) %>%
	mutate(SecondaryStratumID = "Moloka'i") %>%
	data.frame()
}

# Make annual general Q10 multiplier sequences by island for RCPs 4.5 & 8.5
HawaiiQ10gen45 <- HawaiiQ10seq(zonalmean_gen,3,2)
HawaiiQ10gen85 <- HawaiiQ10seq(zonalmean_gen,5,4)
KahoolaweQ10gen45 <- KahoolaweQ10seq(zonalmean_gen,7,6)
KahoolaweQ10gen85 <- KahoolaweQ10seq(zonalmean_gen,9,8)
KauaiQ10gen45 <- KauaiQ10seq(zonalmean_gen,11,10)
KauaiQ10gen85 <- KauaiQ10seq(zonalmean_gen,13,12)
LanaiQ10gen45 <- LanaiQ10seq(zonalmean_gen,15,14)
LanaiQ10gen85 <- LanaiQ10seq(zonalmean_gen,17,16)
MauiQ10gen45 <- MauiQ10seq(zonalmean_gen,19,18)
MauiQ10gen85 <- MauiQ10seq(zonalmean_gen,21,20)
MolokaiQ10gen45 <- MolokaiQ10seq(zonalmean_gen,23,22)
MolokaiQ10gen85 <- MolokaiQ10seq(zonalmean_gen,25,24)
OahuQ10gen45 <- OahuQ10seq(zonalmean_gen,27,26)
OahuQ10gen85 <- OahuQ10seq(zonalmean_gen,29,28)

# Make annual litter Q10 multiplier sequences by island for RCPs 4.5 & 8.5
HawaiiQ10lit45 <- HawaiiQ10seq(zonalmean_lit,3,2)
HawaiiQ10lit85 <- HawaiiQ10seq(zonalmean_lit,5,4)
KahoolaweQ10lit45 <- KahoolaweQ10seq(zonalmean_lit,7,6)
KahoolaweQ10lit85 <- KahoolaweQ10seq(zonalmean_lit,9,8)
KauaiQ10lit45 <- KauaiQ10seq(zonalmean_lit,11,10)
KauaiQ10lit85 <- KauaiQ10seq(zonalmean_lit,13,12)
LanaiQ10lit45 <- LanaiQ10seq(zonalmean_lit,15,14)
LanaiQ10lit85 <- LanaiQ10seq(zonalmean_lit,17,16)
MauiQ10lit45 <- MauiQ10seq(zonalmean_lit,19,18)
MauiQ10lit85 <- MauiQ10seq(zonalmean_lit,21,20)
MolokaiQ10lit45 <- MolokaiQ10seq(zonalmean_lit,23,22)
MolokaiQ10lit85 <- MolokaiQ10seq(zonalmean_lit,25,24)
OahuQ10lit45 <- OahuQ10seq(zonalmean_lit,27,26)
OahuQ10lit85 <- OahuQ10seq(zonalmean_lit,29,28)

# create lists of Island Q10 multiplier sequences for each Q10 value & RCP
genQ1045list <- list(HawaiiQ10gen45, KahoolaweQ10gen45, KauaiQ10gen45, 
	LanaiQ10gen45, MauiQ10gen45, MolokaiQ10gen45, OahuQ10gen45)
genQ1085list <- list(HawaiiQ10gen85, KahoolaweQ10gen85, KauaiQ10gen85, 
	LanaiQ10gen85, MauiQ10gen85, MolokaiQ10gen85, OahuQ10gen85)
litQ1045list <- list(HawaiiQ10lit45, KahoolaweQ10lit45, KauaiQ10lit45, 
	LanaiQ10lit45, MauiQ10lit45, MolokaiQ10lit45, OahuQ10lit45)
litQ1085list <- list(HawaiiQ10lit85, KahoolaweQ10lit85, KauaiQ10lit85, 
	LanaiQ10lit85, MauiQ10lit85, MolokaiQ10lit85, OahuQ10lit85)

# create function to bind growth multiplier sequences together 
# and format as Flow Multiplier table for ST-Sim 
Q10all <- function(x,y) {
	bind_rows(x) %>%
	separate(MZ_SC, c("StratumID", "StateClassID")) %>%
	mutate(StateClassID = paste(.$StateClassID, ":All", sep = ""), 
		DistributionType = "Normal", 
		DistributionFrequencyID = "Iteration and Timestep",
		DistributionSD = y$DistributionSD) 
}

# create general & litter Q10 multiplier data frames for RCPs 4.5 and 8.5
genQ10multi45 <- Q10all(genQ1045list, yearQ10gensd45)
genQ10multi85 <- Q10all(genQ1085list, yearQ10gensd85)
litQ10multi45 <- Q10all(litQ1045list, yearQ10litsd45)
litQ10multi85 <- Q10all(litQ1085list, yearQ10litsd85)

# create function to make Flow Multiplier tables for specific Flow Types
Q10flowtypes <- function(x,y) {
	x %>%
	mutate(FlowGroupID = y) %>%
	select(Timestep, StratumID, SecondaryStratumID, StateClassID, FlowGroupID,
		Value, DistributionType, DistributionFrequencyID, DistributionSD)
}

# make Flow Multiplier Tables for each RCP and flow type 
soile45 <- Q10flowtypes(genQ10multi45, "Emission (soil) [Type]")
soile85 <- Q10flowtypes(genQ10multi85, "Emission (soil) [Type]") 
woodd45 <- Q10flowtypes(genQ10multi45, "Deadwood Decay [Type]")
woodd85 <- Q10flowtypes(genQ10multi85, "Deadwood Decay [Type]")
litd45 <- Q10flowtypes(litQ10multi45, "Litter Decay [Type]")
litd85 <- Q10flowtypes(litQ10multi85, "Litter Decay [Type]")
lite45 <- Q10flowtypes(litQ10multi45, "Emission (litter) [Type]")
lite85 <- Q10flowtypes(litQ10multi85, "Emission (litter) [Type]")

# combine Flow Multipliers into single Q10 table for each RCP
Q10FlowMulti45 <- bind_rows(lite45, soile45, litd45, woodd45)
Q10FlowMulti85 <- bind_rows(lite85, soile85, litd85, woodd85)

# write Q10 Flow Multiplier Tables to .csv files
write.csv(Q10FlowMulti45, "../processed/RCP45_Q10multipliers.csv", row.names = FALSE)
write.csv(Q10FlowMulti85, "../processed/RCP85_Q10multipliers.csv", row.names = FALSE) 

# Filter out Kaho'olawe forest Q10 multipliers
Q1045NoForKaho <- filter(Q10FlowMulti45, 
	SecondaryStratumID != "Kaho'olawe" | StateClassID != "Forest:All")
Q1085NoForKaho <- filter(Q10FlowMulti85, 
	SecondaryStratumID != "Kaho'olawe" | StateClassID != "Forest:All")

#write modified Q10 Flow Multiplier Tables to .csv files
write.csv(Q1045NoForKaho, "../processed/RCP45Q10multi_NoKahoFor.csv", row.names = FALSE)
write.csv(Q1085NoForKaho, "../processed/RCP85Q10multi_NoKahoFor.csv", row.names = FALSE)








