## Calculate annual statewide NPP 
## for the years 2010-2019
## using MODIS MOD17 NPP estimates 
## Paul C. Selmants
## 2021-01-25 (ISO 8601)

## R version 4.0.2

# load required packages into R
library(raster) #version 3.3-13
library(rgdal) #version 1.4-16
library(tidyr, warn.conflicts = FALSE) #version 1.1.1
library(dplyr, warn.conflicts = FALSE) #version 1.0.2
library(ggplot2) #version 3.3.2

## Set working directory at highest level in HI_Model
## GitHub repository, https://github.com/selmants/HI_Model 

# create list of annual MOD17 NPP .tif files
npplist <- list.files('./MOD17_NPP/data', full.names = TRUE)
#create raster stack of annual NPP .tif files
stack <- stack(npplist)
# multiply all values by MODIS scale factor (0.0001)	
# to get NPP values in kg C m-2 y-1
npp <- stack * 1e-04

# export pixel NPP data to dataframe, 
# pivot to long format, and sum values to get 
# statewide NPP estimates in Tg C y-1
npp_data <- as.data.frame(npp, xy=TRUE, na.rm = TRUE) %>%
	pivot_longer(!x:y, names_to = "year", values_to = "npp") %>%
	group_by(year) %>%
	summarise(NPP_Tgy = (sum(npp)*2.5e5)*1e-9) %>%
	mutate(Year = as.numeric(gsub("hawaii_npp_", "", year))) %>%
	select(Year, NPP_Tgy) %>%
	as.data.frame() 



