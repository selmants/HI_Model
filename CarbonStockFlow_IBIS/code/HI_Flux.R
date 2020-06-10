# Format Hawaii IBIS summary files for input as Flow Pathways in
# LUCAS ST-SIM including only years 20-110 (stable) for each Flow
# Paul C. Selmants
# 2017-01-05 (ISO 8601)
# Updated to include Agriculture (covmax2) and WoodyCrop (covmax5)

# Set working directory to "CarbonStockFlow_IBIS" folder in HI_Model
# GitHub repository (https://github.com/selmants/HI_Model)

## load required packages into R
library(dplyr)
library(tidyr)

## make list of IBIS fallw (mortality, kgC/m2/y) summary output text files  
fallw_list <- list.files(pattern = 'fallw', recursive = TRUE,
	full.names = TRUE)
## read IBIS fallw text files into R and format for ST-SIM
fallw <- lapply(fallw_list[c(1,3,5)], FUN = read.table, header = TRUE) %>%
	bind_rows() %>%
	slice(c(7:9, 23:25, 39:41)) %>%
	mutate(FromStateClassID = rep(c('Forest:All', 'Shrubland:All', 'WoodyCrop:All'),
		each = 3)) %>%
	select(FromStateClassID, region_id, X20:X110) %>%
	gather(year, Value, X20:X110) %>%
	arrange(FromStateClassID, region_id) %>%
	mutate(Age = rep(20:110, 9),
		Flux = 'fallw',
		FromStratumID = rep(c('Dry', 'Mesic', 'Wet'), 3,  each = 91)) %>%
	select(FromStratumID, FromStateClassID, Age, Flux, Value)

## make list of IBIS rawlitc (litterfall, kgC/m2/y) summary output text files
rawlitc_list <- list.files(pattern = 'rawlitc', recursive = TRUE, 
	full.names = TRUE)	
## read IBIS rawlitc text files into R and format for ST-SIM
rawlitc <- lapply(rawlitc_list[c(1:5)], FUN = read.table, header = TRUE) %>%
	bind_rows() %>%
	slice(c(7:9, 23:25, 39:41, 55:57, 71:73)) %>%
	mutate(FromStateClassID = rep(c('Forest:All', 'Agriculture:All', 'Shrubland:All',
		'Grassland:All', 'WoodyCrop:All'),
		each = 3)) %>%
	select(FromStateClassID, region_id, X20:X110) %>%
	gather(year, Value, X20:X110) %>%
	arrange(FromStateClassID, region_id) %>%
	mutate(Age = rep(20:110, 15),
		Flux = 'rawlitc',
		FromStratumID = rep(c('Dry', 'Mesic', 'Wet'), 5, each = 91)) %>%
	select(FromStratumID, FromStateClassID, Age, Flux, Value) 

## make list of IBIS stdwcloss (deadfall, kgC/m2/y) summary output text files 
stdwcloss_list <- list.files(pattern = 'stdwcloss', recursive = TRUE, 
	full.names = TRUE)
## read IBIS stdwcloss text files into R and format for ST-SIM
stdwcloss <- lapply(stdwcloss_list[c(1,3,5)], FUN = read.table, header = TRUE) %>%
	bind_rows() %>%
	slice(c(7:9, 23:25, 39:41)) %>%
	mutate(FromStateClassID = rep(c('Forest:All', 'Shrubland:All', 'WoodyCrop:All'),
		each = 3)) %>%
	select(FromStateClassID, region_id, X20:X110) %>%
	gather(year, Value, X20:X110) %>%
	arrange(FromStateClassID, region_id) %>%
	mutate(Age = rep(20:110, 9),
		Flux = 'stdwcloss',
		FromStratumID = rep(c('Dry', 'Mesic', 'Wet'), 3,  each = 91)) %>%
	select(FromStratumID, FromStateClassID, Age, Flux, Value)

## make list of IBIS down2lit (deadwood decay, kgC/m2/y) summary output text files
down2lit_list <- list.files(pattern = 'down2lit', recursive = TRUE, 
	full.names = TRUE)
## read IBIS down2lit text files into R and format for ST-SIM
down2lit <- lapply(down2lit_list[c(1,3,5)], FUN = read.table, header = TRUE) %>%
	bind_rows() %>%
	slice(c(7:9, 23:25, 39:41)) %>%
	mutate(FromStateClassID = rep(c('Forest:All', 'Shrubland:All', 'WoodyCrop:All'),
		each = 3)) %>%
	select(FromStateClassID, region_id, X20:X110) %>%
	gather(year, Value, X20:X110) %>%
	arrange(FromStateClassID, region_id) %>%
	mutate(Age = rep(20:110, 9),
		Flux = 'down2lit',
		FromStratumID = rep(c('Dry', 'Mesic', 'Wet'), 3,  each = 91)) %>%
	select(FromStratumID, FromStateClassID, Age, Flux, Value) 

## make list of IBIS lit2co2 (litter CO2 efflux, kgC/m2/y) summary output text files
lit2co2_list <- list.files(pattern = 'lit2co2', recursive = TRUE,
	full.names = TRUE)
## read IBIS lit2co2 text files into R and format for ST-SIM
lit2co2 <- lapply(lit2co2_list[c(1:5)], FUN = read.table, header = TRUE) %>%
	bind_rows() %>%
	slice(c(7:9, 23:25, 39:41, 55:57, 71:73)) %>%
	mutate(FromStateClassID = rep(c('Forest:All', 'Agriculture:All', 'Shrubland:All',
		'Grassland:All', 'WoodyCrop:All'), each = 3)) %>%
	select(FromStateClassID, region_id, X20:X110) %>%
	gather(year, Value, X20:X110) %>%
	arrange(FromStateClassID, region_id) %>%
	mutate(Age = rep(20:110, 15),
		Flux = 'lit2co2',
		FromStratumID = rep(c('Dry', 'Mesic', 'Wet'), 5,  each = 91)) %>%
	select(FromStratumID, FromStateClassID, Age, Flux, Value) 

## make list of IBIS lit2soc (litter decay flux to soil, kgC/m2/y) summary output text files
lit2soc_list <- list.files(pattern = 'lit2soc', recursive = TRUE,
	full.names = TRUE)
## read IBIS lit2co2 text files into R and format for ST-SIM
lit2soc <- lapply(lit2soc_list[c(1:5)], FUN = read.table, header = TRUE) %>%
	bind_rows() %>%
	slice(c(7:9, 23:25, 39:41, 55:57, 71:73)) %>%
	mutate(FromStateClassID = rep(c('Forest:All', 'Agriculture:All', 'Shrubland:All',
		'Grassland:All', 'WoodyCrop:All'), each = 3)) %>%
	select(FromStateClassID, region_id, X20:X110) %>%
	gather(year, Value, X20:X110) %>%
	arrange(FromStateClassID, region_id) %>%
	mutate(Age = rep(20:110, 15),
		Flux = 'lit2soc',
		FromStratumID = rep(c('Dry', 'Mesic', 'Wet'), 5,  each = 91)) %>%
	select(FromStratumID, FromStateClassID, Age, Flux, Value) 

## make list of IBIS soc2co2 (total SOC loss, kgC/m2/y) summary output text files
socloss_list <- list.files(pattern = 'soc2co2', recursive = TRUE, full.names = TRUE)
## read IBIS soc2co2 text files into R and format for ST-SIM
socloss <- lapply(socloss_list[c(1:5)], FUN = read.table, header = TRUE) %>%
	bind_rows() %>%
	slice(c(7:9, 23:25, 39:41, 55:57, 71:73)) %>%
	mutate(FromStateClassID = rep(c('Forest:All', 'Agriculture:All', 'Shrubland:All',
	 'Grassland:All', 'WoodyCrop:All'), each = 3)) %>%
	select(FromStateClassID, region_id, X20:X110) %>%
	gather(year, Value, X20:X110) %>%
	arrange(FromStateClassID, region_id) %>%
	mutate(Age = rep(20:110, 15),
		Flux = 'socloss',
		FromStratumID = rep(c('Dry', 'Mesic', 'Wet'), 5,  each = 91)) %>%
	select(FromStratumID, FromStateClassID, Age, Flux, Value)

## make list of IBIS yrleach (soil leaching, kgC/m2/y) summary output text files
yrleach_list <- list.files(pattern = 'yrleach', recursive = TRUE, 
	full.names = TRUE)
## read IBIS yrleach text files into R and format for ST-SIM
yrleach <- lapply(yrleach_list[c(1:5)], FUN = read.table, header = TRUE) %>%
	bind_rows() %>%
	slice(c(7:9, 23:25, 39:41, 55:57, 71:73)) %>%
	mutate(FromStateClassID = rep(c('Forest:All', 'Agriculture:All', 'Shrubland:All',
	 'Grassland:All', 'WoodyCrop:All'), each = 3)) %>%
	select(FromStateClassID, region_id, X20:X110) %>%
	gather(year, Value, X20:X110) %>%
	arrange(FromStateClassID, region_id) %>%
	mutate(Age = rep(20:110, 15),
		Flux = 'yrleach',
		FromStratumID = rep(c('Dry', 'Mesic', 'Wet'), 5, each = 91)) %>%
	select(FromStratumID, FromStateClassID, Age, Flux, Value)

soc2co2 <- socloss %>%
	mutate(soc2co2Value = Value - yrleach$Value,
		soc2co2Flux = 'soc2co2') %>%
	select(FromStratumID, FromStateClassID, Age, Flux = soc2co2Flux, Value = soc2co2Value)
	
## read IBIS cgrain text files for Ag into R and format for ST-SIM
cgrain <- read.table('./data/base/sum_covmax2/sum_cgrain_m.nc.txt', header = TRUE) %>%
	bind_rows() %>%
	slice(c(7:9, 23:25, 39:41)) %>%
	mutate(FromStateClassID = 'Agriculture:All') %>%
	select(FromStateClassID, region_id, X20:X110) %>%
	gather(year, Value, X20:X110) %>%
	arrange(FromStateClassID, region_id) %>%
	mutate(Age = rep(20:110, 3),
		Flux = 'cgrain',
		FromStratumID = rep(c('Dry', 'Mesic', 'Wet'), each = 91)) %>%
	select(FromStratumID, FromStateClassID, Age, Flux, Value)
# read IBIS strawc text files for Ag into R and format for ST-SIM
strawc <- read.table('./data/base/sum_covmax2/sum_strawc_m.nc.txt', header = TRUE) %>%
	bind_rows() %>%
	slice(c(7:9, 23:25, 39:41)) %>%
	mutate(FromStateClassID = 'Agriculture:All') %>%
	select(FromStateClassID, region_id, X20:X110) %>%
	gather(year, Value, X20:X110) %>%
	arrange(FromStateClassID, region_id) %>%
	mutate(Age = rep(20:110, 3),
		Flux = 'strawc',
		FromStratumID = rep(c('Dry', 'Mesic', 'Wet'), each = 91)) %>%
	select(FromStratumID, FromStateClassID, Age, Flux, Value) 
# read IBIS cfruit text files for WoodyCrop into R and format for ST-SIM
cfruit <- read.table('./data/base/sum_covmax5/sum_cfruit_m.nc.txt', header = TRUE) %>%
	bind_rows() %>%
	slice(c(7:9, 23:25, 39:41)) %>%
	mutate(FromStateClassID = 'WoodyCrop:All') %>%
	select(FromStateClassID, region_id, X20:X110) %>%
	gather(year, Value, X20:X110) %>%
	arrange(FromStateClassID, region_id) %>%
	mutate(Age = rep(20:110, 3),
		Flux = 'cfruit',
		FromStratumID = rep(c('Dry', 'Mesic', 'Wet'), each = 91)) %>%
	select(FromStratumID, FromStateClassID, Age, Flux, Value)	

## bind fluxes by rows into a single dataframe 
HI_flux <- bind_rows(fallw, rawlitc, stdwcloss, down2lit,
	lit2co2, lit2soc, soc2co2, yrleach, cgrain, strawc, cfruit) %>%
	arrange(FromStratumID, FromStateClassID) %>%
	filter(FromStateClassID != 'WoodyCrop:All' | FromStratumID != 'Dry') 

## write flux data to .csv file
write.csv(HI_flux, './data/processed/HI_Flux.csv', row.names = FALSE)
