## Calculate annual statewide NPP 
## for the years 2010-2019
## using MODIS MOD17 NPP estimates 
## Paul C. Selmants
## 2021-01-25 (ISO 8601)

## R version 3.6.1

# load required packages into R
library(raster) #version 3.1-5
library(rgdal) #version 1.4-8
library(tidyr, warn.conflicts = FALSE) #version 1.0.2
library(dplyr, warn.conflicts = FALSE) #version 0.8.5
library(ggplot2) #version

## Set working directory at highest level in HI_Model
## GitHub repository, https://github.com/selmants/HI_Model 

# create list of annual MOD17 NPP .tif files
npplist <- list.files('./MOD17_NPP/data', full.names = TRUE)
#create raster stack of annual NPP .tif files
stack <- stack(npplist)
# multiply all values by MODIS scale factor (0.0001)	
# to get NPP values in kg C m-2 y-1
npp <- stack * 1e-04

# export pixel NPP data to dataframe
# and sum values within each year to get 
# statewide NPP estimates in Tg C y-1
npp_data <- as.data.frame(npp, xy=TRUE, na.rm = TRUE) %>%



