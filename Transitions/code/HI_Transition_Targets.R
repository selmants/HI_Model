## Transition Targets for two Urbanization/Ag
## Change scenarios in Hawaii LUCAS Model 
## Paul C. Selmants
## R version 3.6.1
## 2019-09-09 ISO-8601
## updated 2019-11-05 ISO-8601
## updated 2020-04-23 ISO-8601

# load dplyr package (version 0.8.5)
library(dplyr) 

# read .csv baseline data into R 
lulc <- read.csv("lulc_historic.csv", stringsAsFactors = FALSE)
ddev <- read.csv("HI_ddev.csv", stringsAsFactors = FALSE)

# Historical urbanization transition targets by island for 2010-2019, 
# based on statewide population projections from HI Office of Planning
UrbanHist <- ddev %>%
	filter(end_year <= 2020) %>%
	mutate( 
		Iteration = "",
		StratumID = "",
		TransitionGroupID = "URBANIZATION",
		DistributionType = "",
		DistributionFrequencyID = "",
		DistributionSD = as.numeric(NA),
		DistributionMin = as.numeric(NA), 
		DistributionMax = as.numeric(NA)) %>%
	select(Iteration, Timestep = start_year, StratumID, 
		SecondaryStratumID = island, TransitionGroupID, 
		Amount = ddev_km2y, DistributionType, DistributionFrequencyID, 
		DistributionSD, DistributionMin, DistributionMax)
# Low urbanization transition targets by island for 2020-2045 
# based on statewide population projections from HI Dept. of Business, Economic
# Development, and Tourism (DBEDT). 
UrbanLow_early <- ddev %>%
	filter(start_year >= 2020) %>%
	mutate(min = ddev_km2y*0.25, 
		Iteration = "",
		StratumID = "",
		TransitionGroupID = "URBANIZATION",
		Amount = as.numeric(NA),
		DistributionType = "Uniform",
		DistributionFrequencyID = "Iteration and Timestep",
		DistributionSD = as.numeric(NA),
		DistributionMin = as.numeric(NA), 
		DistributionMax = as.numeric(NA)) %>%
	select(Iteration, Timestep = start_year, StratumID, 
		SecondaryStratumID = island, TransitionGroupID, 
		Amount, DistributionType, DistributionFrequencyID, 
		DistributionSD, DistributionMin = min, DistributionMax = ddev_km2y) 
# Low urbanization transition targets by island for 2045-2070
# using zero to 25% of minimum annual population projection per island
UrbanLow_mid <- ddev %>%
	group_by(island) %>%
	summarise(DistributionMax = min(ddev_km2y)*0.25) %>%
	mutate(Iteration = "",
		Timestep = 2045,
		StratumID = "",
		TransitionGroupID = "URBANIZATION",
		Amount = as.numeric(NA), 
		DistributionType = "Uniform",
		DistributionFrequencyID = "Iteration and Timestep", 
		DistributionSD = as.numeric(NA),
		DistributionMin = 0) %>%
	select(Iteration, Timestep, StratumID, 
		SecondaryStratumID = island, TransitionGroupID, 
		Amount, DistributionType, DistributionFrequencyID, 
		DistributionSD, DistributionMin, DistributionMax) %>%
	data.frame(.)
# Low urbanization transition targes by island for 2070-2100 using
# zero to 0.10 x minimum annual population projection per island
UrbanLow_late <- ddev %>%
	group_by(island) %>%
	summarise(DistributionMax = min(ddev_km2y)*0.10) %>%
	mutate(Iteration = "",
		Timestep = 2070,
		StratumID = "",
		TransitionGroupID = "URBANIZATION",
		Amount = as.numeric(NA), 
		DistributionType = "Uniform",
		DistributionFrequencyID = "Iteration and Timestep", 
		DistributionSD = as.numeric(NA),
		DistributionMin = 0) %>%
	select(Iteration, Timestep, StratumID, 
		SecondaryStratumID = island, TransitionGroupID, 
		Amount, DistributionType, DistributionFrequencyID, 
		DistributionSD, DistributionMin, DistributionMax) %>%
	data.frame(.)
# combine low urbanization dataframes, remove Kaho'olawe zero values 
Low_Urban <- bind_rows(UrbanHist, UrbanLow_early, UrbanLow_mid, UrbanLow_late) %>%
	filter(SecondaryStratumID != "Kaho'olawe")

# High urbanization transition targets by island for 2010-2045
# based on mid- to max range of historical annual rates (1992-2010 NOAA CCAP)  
UrbanHigh_early <- lulc %>%
	filter(LC_change == "Urbanization") %>%
	mutate(Iteration = "",
		Timestep = 2020,
		StratumID = "",
		TransitionGroupID = "URBANIZATION",
		Amount = as.numeric(NA),
		DistributionType = "Uniform",
		DistributionFrequencyID = "Iteration and Timestep",
		DistributionSD = as.numeric(NA)) %>%
	select(Iteration, Timestep, StratumID, SecondaryStratumID = Island, TransitionGroupID,
		Amount, DistributionType, DistributionFrequencyID, DistributionSD, 
		DistributionMin = mid, DistributionMax = maximum)
# High urbanization transition targets for 2045-2100
# based on minimum to maximum range of historical annual rates (1992-2010) 
UrbanHigh_mid <- lulc %>%
	filter(LC_change == "Urbanization") %>%
	mutate(Iteration = "",
		Timestep = 2045,
		StratumID = "",
		TransitionGroupID = "URBANIZATION",
		Amount = as.numeric(NA),
		DistributionType = "Uniform",
		DistributionFrequencyID = "Iteration and Timestep",
		DistributionSD = as.numeric(NA)) %>%
	select(Iteration, Timestep, StratumID, SecondaryStratumID = Island, TransitionGroupID,
		Amount, DistributionType, DistributionFrequencyID, DistributionSD, 
		DistributionMin = minimum, DistributionMax = maximum)
# High urbanization transition targets for 2045-2100
# based on zero-max range of historical annual rates (1992-2010) 
UrbanHigh_late <- lulc %>%
filter(LC_change == "Urbanization") %>%
	mutate(Iteration = "",
		Timestep = 2070,
		StratumID = "",
		TransitionGroupID = "URBANIZATION",
		Amount = as.numeric(NA),
		DistributionType = "Uniform",
		DistributionFrequencyID = "Iteration and Timestep",
		DistributionSD = as.numeric(NA),
		DistributionMin = 0) %>%
	select(Iteration, Timestep, StratumID, SecondaryStratumID = Island, TransitionGroupID,
		Amount, DistributionType, DistributionFrequencyID, DistributionSD, 
		DistributionMin, DistributionMax = maximum)

# combine high urbanization dataframes
High_Urban <- bind_rows(UrbanHist, UrbanHigh_early, UrbanHigh_mid, UrbanHigh_late)

# Historical Agricultural Contraction transition targets by island 
# for 2010-2020 (High) or 2010-2100 (Low) using 0-minimum range of 
# historical annual rates (1992-2010 NOAA CCAP)
AgCon_Hist <- lulc %>%
	filter(LC_change == "Ag contraction") %>%
	mutate(Iteration = "",
		Timestep = 2010,
		StratumID = "",
		TransitionGroupID = "AGRICULTURAL CONTRACTION",
		Amount = as.numeric(NA),
		DistributionType = "Uniform",
		DistributionFrequencyID = "Iteration and Timestep",
		DistributionSD = as.numeric(NA),
		DistributionMin = 0) %>%
	select(Timestep, StratumID, SecondaryStratumID = Island, TransitionGroupID,
		Amount, DistributionType, DistributionFrequencyID, DistributionSD, 
		DistributionMin, DistributionMax = minimum)
# High Agricultural Contraction transition targets by island for 2020-2045
# based on zero-mid range of historical annual rates (1992-2010 NOAA CCAP)
AgConHigh_early <- lulc %>%
	filter(LC_change == "Ag contraction") %>%
	mutate(Iteration = "",
		Timestep = 2020,
		StratumID = "",
		TransitionGroupID = "AGRICULTURAL CONTRACTION",
		Amount = as.numeric(NA),
		DistributionType = "Uniform",
		DistributionFrequencyID = "Iteration and Timestep",
		DistributionSD = as.numeric(NA),
		DistributionMin = 0) %>%
	select(Timestep, StratumID, SecondaryStratumID = Island, TransitionGroupID,
		Amount, DistributionType, DistributionFrequencyID, DistributionSD, 
		DistributionMin, DistributionMax = mid) 
# High Agricultural Contraction transition targets by island for 2045-2100
# based on zero-minimum range of historical annual rates (1992-2010 NOAA CCAP)
AgConHigh_late <- lulc %>%
	filter(LC_change == "Ag contraction") %>%
	mutate(Iteration = "",
		Timestep = 2045,
		StratumID = "",
		TransitionGroupID = "AGRICULTURAL CONTRACTION",
		Amount = as.numeric(NA),
		DistributionType = "Uniform",
		DistributionFrequencyID = "Iteration and Timestep",
		DistributionSD = as.numeric(NA),
		DistributionMin = 0) %>%
	select(Timestep, StratumID, SecondaryStratumID = Island, TransitionGroupID,
		Amount, DistributionType, DistributionFrequencyID, DistributionSD, 
		DistributionMin, DistributionMax = minimum)
# combine high Ag contraction dataframes
High_AgCon <- bind_rows(AgCon_Hist, AgConHigh_early, AgConHigh_late) 

# Historical Agricultural Expansion transition targets by island 
# for 2010-2020 (High) or 2010-2045 (Low) using zero to minimum range
# of historical annual rates (1992-2010 NOAA CCAP)
AgExHist <- lulc %>%
	filter(LC_change == "Ag expansion") %>%
	mutate(Iteration = "", 
		Timestep = 2010,
		StratumID = "",
		SecondaryStratumID = "",
		TransitionGroupID = "AGRICULTURAL EXPANSION",
		Amount = as.numeric(NA),
		DistributionType = "Uniform",
		DistributionFrequencyID = "Iteration and Timestep",
		DistributionSD = as.numeric(NA),
		DistributionMin = 0) %>%
	select(Timestep, StratumID, SecondaryStratumID = Island, TransitionGroupID,
		Amount, DistributionType, DistributionFrequencyID, DistributionSD, 
		DistributionMin, DistributionMax = minimum)
# High Agricultural Expansion transition targets by island from 2020-2045
# using mid- to maximum range of historical annual rates (1992-2010 NOAA CCAP) 
AgExHigh_early <- lulc %>%
	filter(LC_change == "Ag expansion") %>%
	mutate(Iteration = "", 
		Timestep = 2020,
		StratumID = "",
		SecondaryStratumID = "",
		TransitionGroupID = "AGRICULTURAL EXPANSION",
		Amount = as.numeric(NA),
		DistributionType = "Uniform",
		DistributionFrequencyID = "Iteration and Timestep",
		DistributionSD = as.numeric(NA)) %>%
	select(Timestep, StratumID, SecondaryStratumID = Island, TransitionGroupID,
		Amount, DistributionType, DistributionFrequencyID, DistributionSD, 
		DistributionMin = mid, DistributionMax = maximum)
# High Agricultural Expansion transition targets by island from 2045-2070
# using minimum to mid range of historical annual rates (1992-2010 NOAA CCAP) 
AgExHigh_mid <- lulc %>%
	filter(LC_change == "Ag expansion") %>%
	mutate(Iteration = "", 
		Timestep = 2045,
		StratumID = "",
		SecondaryStratumID = "",
		TransitionGroupID = "AGRICULTURAL EXPANSION",
		Amount = as.numeric(NA),
		DistributionType = "Uniform",
		DistributionFrequencyID = "Iteration and Timestep",
		DistributionSD = as.numeric(NA)) %>%
	select(Timestep, StratumID, SecondaryStratumID = Island, TransitionGroupID,
		Amount, DistributionType, DistributionFrequencyID, DistributionSD, 
		DistributionMin = minimum, DistributionMax = maximum)
# High Agricultural Expansion transition targets by island from 2045-2070
# using zero to mid range of historical annual rates (1992-2010 NOAA CCAP)
AgExHigh_late <- lulc %>%
	filter(LC_change == "Ag expansion") %>%
	mutate(Iteration = "", 
		Timestep = 2070,
		StratumID = "",
		SecondaryStratumID = "",
		TransitionGroupID = "AGRICULTURAL EXPANSION",
		Amount = as.numeric(NA),
		DistributionType = "Uniform",
		DistributionFrequencyID = "Iteration and Timestep",
		DistributionSD = as.numeric(NA),
		DistributionMin = 0) %>%
	select(Timestep, StratumID, SecondaryStratumID = Island, TransitionGroupID,
		Amount, DistributionType, DistributionFrequencyID, DistributionSD, 
		DistributionMin, DistributionMax = maximum)

# combine high Ag Expansion dataframes
High_AgEx <- bind_rows(AgExHist, AgExHigh_early, AgExHigh_mid, AgExHigh_late)

# Low Agricultural Expansion transition targets by island from 2045-2100 using
# zero to 0.5 x minimum range of historical annual rates (1992-2010 NOAA CCAP)
AgExLow_late <- lulc %>%
	filter(LC_change == "Ag expansion") %>%
	mutate(Iteration = "", 
		Timestep = 2020,
		StratumID = "",
		SecondaryStratumID = "",
		TransitionGroupID = "AGRICULTURAL EXPANSION",
		Amount = as.numeric(NA),
		DistributionType = "Uniform",
		DistributionFrequencyID = "Iteration and Timestep",
		DistributionSD = as.numeric(NA),
		DistributionMin = 0,
		DistributionMax = minimum*0.1) %>%
	select(Timestep, StratumID, SecondaryStratumID = Island, TransitionGroupID,
		Amount, DistributionType, DistributionFrequencyID, DistributionSD, 
		DistributionMin, DistributionMax)

# combine low Ag Expansion dataframes
Low_AgEx <- bind_rows(AgExHist, AgExLow_late)

# zero area Plantation Harvest transition targets (to be combined 
# with both High and Low LULC targets)
Zero_PlantHarvest <- data.frame("Iteration" = "",
	"Timestep" = rep(2010,16),
	"SecondaryStratumID" = rep(c("Hawai'i", "Moloka'i", "O'ahu", "Kaua'i"), 4),
	"TransitionGroupID" = rep(c("MANAGEMENT: Plantation Harvest [Type]", 
		"MANAGEMENT: Plantation->Ag [Type]", "MANAGEMENT: Plantation->Grassland [Type]",
		"MANAGEMENT: Plantation->WoodyCrop [Type]"), each = 4),
	"Amount" = 0, 
	"DistributionType" = "", 
	"DistributionFrequencyID" = "",
	"DistributionSD" = as.numeric(NA), 
	"DistributionMin" = as.numeric(NA),
	"DistributionMax" = as.numeric(NA),
	stringsAsFactors = FALSE) 

# High LULC Plantation Harvest transition targets for Hawai'i Island and Kaua'i 
High_PlantHarvest <- data.frame("Iteration" = "",
	"Timestep" = c(rep(2012, 2), rep(2017,2), rep(2019,3)), 
	"SecondaryStratumID" = c(rep(c("Hawai'i", "Kaua'i"),2), "Kaua'i", 
		rep("Hawai'i",2)), 
	"TransitionGroupID" = c(rep(c("MANAGEMENT: Plantation->Grassland [Type]",
		"MANAGEMENT: Plantation Harvest [Type]"),2), 
		"MANAGEMENT: Plantation Harvest [Type]",
		"MANAGEMENT: Plantation->Ag [Type]", 
		"MANAGEMENT: Plantation Harvest [Type]"),
	"Amount" = c(2.024, rep(0,2), 0.5, rep(0.75,3)), 
	"DistributionType" = c("Normal", rep("",2), rep("Normal",4)),
	"DistributionFrequencyID" = c("Iteration and Timestep", rep("",2), 
		rep("Iteration and Timestep",4)),
	"DistributionSD" = c(0.25, rep(NA, 2), 0.05, rep(0.1,3)), 
	"DistributionMin" = as.numeric(NA),
	"DistributionMax" = as.numeric(NA),
	stringsAsFactors = FALSE) 

# Low LULC Plantation Harvest transition targets for Hawai'i Island and Kaua'i 
Low_PlantHarvest <- data.frame("Iteration" = "",
	"Timestep" = c(rep(2012, 2), rep(2017,2), rep(2019,3)), 
	"SecondaryStratumID" = c(rep(c("Hawai'i", "Kaua'i"),2), rep("Hawai'i",3)), 
	"TransitionGroupID" = c(rep(c("MANAGEMENT: Plantation->Grassland [Type]",
		"MANAGEMENT: Plantation Harvest [Type]"),2), 
		"MANAGEMENT: Plantation->Ag [Type]",
	"MANAGEMENT: Plantation->WoodyCrop [Type]", 
	"MANAGEMENT: Plantation Harvest [Type]"),
	"Amount" = c(2.024, rep(0,2), 0.5, rep(0.25, 3)), 
	"DistributionType" = c("Normal", rep("",2), rep("Normal",4)),
	"DistributionFrequencyID" = c("Iteration and Timestep", rep("",2), 
		rep("Iteration and Timestep", 4)),
	"DistributionSD" = c(0.25, rep(NA, 2), rep(0.05, 4)), 
	"DistributionMin" = as.numeric(NA),
	"DistributionMax" = as.numeric(NA),
	stringsAsFactors = FALSE) 

# Merge Low LULC transition targets together  
LowLULC_Targets <- bind_rows(list(Low_Urban, High_AgCon, 
	Low_AgEx, Zero_PlantHarvest, Low_PlantHarvest))

# Merge High LULC transition targets together 
HighLULC_Targets <- bind_rows(list(High_Urban, AgCon_Hist, 
	High_AgEx, Zero_PlantHarvest, High_PlantHarvest))

# write Low LULC Scenario transition targets to .csv file
write.csv(LowLULC_Targets, "./InputData/TransitionTargets_LowLULC.csv", 
	row.names = FALSE, na = "") 
 
# write High Urbanization Scenario transition targets to .csv file,
# save in HI_Model/Build_STsim/data directory
write.csv(HighLULC_Targets, "./InputData/TransitionTargets_HighLULC.csv", 
	row.names = FALSE, na = "") 

