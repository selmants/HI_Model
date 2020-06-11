## Create dataframes of annual NPP multipliers
## for different rates of CO2 fertilization (CFE)
## based on projections of annual atmospheric CO2 
## concentrations under RCP 4.5 and RCP 8.5
## Paul C. Selmants
## R version 3.6.1
## 2020-04-10


## set working directory to "Model_InputData" sub-directory of 
## HI_Model GitHub repository, https://github.com/selmants/HI_Model

# load dplyr v. 0.8.3 into R
library(dplyr)

# read in climate-based NPP multipliers for RCP 4.5 and RCP 8.5
nppmulti45 <- read.csv("RCP45_NPPmultipliers.csv", stringsAsFactors = FALSE)
nppmulti85 <- read.csv("RCP85_NPPmultipliers.csv", stringsAsFactors = FALSE)

# read in RCP 4.5 mid-year atm CO2 concentrations
rcp45 <- read.table(
	"http://www.pik-potsdam.de/~mmalte/rcps/data/RCP45_MIDYEAR_CONCENTRATIONS.DAT",
	header = FALSE,
	stringsAsFactors = FALSE, 
	skip = 39) %>% 
select(year = V1, rcp45CO2ppm = V4) %>%
filter(year >2009, year <2101) %>%
mutate(incr45 = rcp45CO2ppm - .[1,2], 
		cfe025 = 1+(incr45*0.025)/100, 
		cfe05 = 1+(incr45*0.05)/100,
		cfe075 = 1+(incr45*0.075)/100,
		cfe10 = 1+(incr45*0.10)/100,
		cfe125 = 1+(incr45*0.125)/100,
		cfe15 = 1+(incr45*0.15)/100) %>%
filter(year > 2010) %>%
select(year, rcp45CO2ppm, cfe025:cfe15)
# read in RCP 8.5 mid-year atm CO2 concentrations
rcp85 <- read.table(
	"http://www.pik-potsdam.de/~mmalte/rcps/data/RCP85_MIDYEAR_CONCENTRATIONS.DAT",
	header = FALSE,
	stringsAsFactors = FALSE, 
	skip = 39) %>% 
select(year = V1, rcp85CO2ppm = V4) %>%
filter(year >2009, year <2101) %>%
mutate(incr85 = rcp85CO2ppm - .[1,2],
		cfe025 = 1+(incr85*0.025)/100, 
		cfe05 = 1+(incr85*0.05)/100,
		cfe075 = 1+(incr85*0.075)/100,
		cfe10 = 1+(incr85*0.10)/100,
		cfe125 = 1+(incr85*0.125)/100,
		cfe15 = 1+(incr85*0.15)/100) %>%
filter(year > 2010) %>%
select(year, rcp85CO2ppm, cfe025:cfe15)

# create dataframe of NPP CFE multipliers for RCP 8.5 
# from 2060-2100 limited to CFE at 600ppm atm CO2 
rcp85_600ppm <- data.frame(
	"year" = c(2060:2100),
	"rcp85CO2ppm" = c(rcp85[51:91,2]),
	"cfe025" = rcp85[51,3],
	"cfe05" = rcp85[51,4],
	"cfe075" = rcp85[51,5],
	"cfe10" = rcp85[51,6],
	"cfe125" = rcp85[51,7],
	"cfe15" = rcp85[51,8])
# filter CFEmulti_rcp85 to years 2012-2059
early85 <- filter(rcp85, year <2060) 
# combine earlyCFE85 with CFE85_600
rcp85LIMIT <- bind_rows(early85, rcp85_600ppm) 

# write function to create CFE NPP multiplier tables
cfe <- function(x,y,z) {
	data.frame(Timestep = x[, 1],
		StratumID = x[, 2],
		SecondaryStratumID = x[, 3],
		StateClassID = x[, 4],
		FlowGroupID = "Growth [Type]",
		Value = rep(c(y[, z]), 91)* x[, 6],
		DistributionType = "Normal",
		DistributionFrequencyID = "Iteration and Timestep",
		DistributionSD = 0.025)
}

# create CFE NPP multiplier tables for RCP 4.5
rcp45cfe025 <- cfe(nppmulti45, rcp45, 3)
rcp45cfe05 <- cfe(nppmulti45, rcp45, 4)
rcp45cfe075 <- cfe(nppmulti45, rcp45, 5)
rcp45cfe10 <- cfe(nppmulti45, rcp45, 6)
rcp45cfe125 <- cfe(nppmulti45, rcp45, 7)
rcp45cfe15 <- cfe(nppmulti45, rcp45, 8)

# create CFE NPP multiplier tables for RCP 8.5
rcp85cfe025 <- cfe(nppmulti85, rcp85LIMIT, 3)
rcp85cfe05 <- cfe(nppmulti85, rcp85LIMIT, 4)
rcp85cfe075 <- cfe(nppmulti85, rcp85LIMIT, 5)
rcp85cfe10 <- cfe(nppmulti85, rcp85LIMIT, 6)
rcp85cfe125<- cfe(nppmulti85, rcp85LIMIT, 7)
rcp85cfe15 <- cfe(nppmulti85, rcp85LIMIT, 8)

# make a list of cfe multiplier dataframes
cfe_list <- list(rcp45cfe025, rcp45cfe05, rcp45cfe075, rcp45cfe10, rcp45cfe125,
	rcp45cfe15, rcp85cfe025, rcp85cfe05, rcp85cfe075, rcp85cfe10, rcp85cfe125,
	rcp85cfe15)

# apply names to items in cfe_list 
names(cfe_list) <- c("rcp45cfe025", "rcp45cfe05", "rcp45cfe075", 
	"rcp45cfe10", "rcp45cfe125","rcp45cfe15", "rcp85cfe025", "rcp85cfe05", 
	"rcp85cfe075", "rcp85cfe10", "rcp85cfe125","rcp85cfe15") 

# write each dataframes to .csv file 
# using list names to name each file
lapply(1:length(cfe_list), function(i) write.csv(cfe_list[[i]],
	file = paste0(names(cfe_list[i]), ".csv"),
	row.names = FALSE))