## Transition Pathways for Urbanization,
## Ag Contraction, Ag Expansion, and Fire
## in Hawaii Model
## Paul C. Selmants
## 2018-08-30 
## UPDATED with new fire data 2019-10-31
## R version 3.6.1

## Set R working directory to highest level of HI_Model
## GitHub repository, https://github.com/selmants/HI_Model

# load dplyr package v. 0.8.3
library(dplyr)

# Plantation Harvest transition pathways by island   
Plant_Harvest <- data.frame("Iteration" = "",
		"Timestep" = "",
		"StratumIDSource" = rep(c("Dry", "Mesic", "Wet"), each = 4, 4),
		"StateClassIDSource" = "Plantation:All",
		"StratumIDDest" = "", 
		"StateClassIDDest" = rep(c("Plantation:All", "Agriculture:All",
			"Grassland:All", "WoodyCrop:All"), each = 12),
		"SecondaryStratumID" = rep(c("Hawai'i", "Moloka'i", "O'ahu", "Kaua'i"),
			12),
		"TertiaryStratumID" = "",
		"TransitionTypeID" = rep(c("MANAGEMENT: Plantation Harvest", 
		"MANAGEMENT: Plantation->Ag", "MANAGEMENT: Plantation->Grassland",
		"MANAGEMENT: Plantation->WoodyCrop"), each = 12), 
		"Probability" = 1, 
		"Proportion" = NA,
		"AgeMin" = 5, 
		stringsAsFactors = FALSE) 

# Urbanization transition pathways by island 
Urban <- data.frame("Iteration" = "",
		"Timestep" = "",
		"StratumIDSource" = rep(c("Dry", "Mesic", "Wet"), each = 6, 5),  
		"StateClassIDSource" = rep(c("Agriculture:All", "Barren:All",
			"Forest:All", "Grassland:All", "Shrubland:All"), each = 18), 
		"StratumIDDest" = "",
		"StateClassIDDest" = "Developed:All",
		"SecondaryStratumID" = rep(c("Hawai'i", "Kaua'i",
			"Lana'i", "Maui", "Moloka'i", "O'ahu"), 15),
		"TertiaryStratumID" = "",
		"TransitionTypeID" = rep(c("URBANIZATION: Agriculture->Developed",
			"URBANIZATION: Barren->Developed", 
			"URBANIZATION: Forest->Developed",
			"URBANIZATION: Grassland->Developed",
			"URBANIZATION: Shrubland->Developed"), each = 18), 
		"Probability" = 1, 
		"Proportion" = NA,
		"AgeMin" = NA,
		stringsAsFactors = FALSE) 

# Agricultural Contraction transition pathways by island
AgCon <- data.frame("Iteration" = "",
		"Timestep" = "",
		"StratumIDSource" = rep(c("Dry", "Mesic", "Wet"), each = 6, 3), 
		"StateClassIDSource" = "Agriculture:All",
		"StratumIDDest" = "",
		"StateClassIDDest" = rep(c("Forest:All", "Grassland:All",
			"Shrubland:All"), each = 18),
		"SecondaryStratumID" = rep(c("Hawai'i", "Kaua'i",
			"Lana'i", "Maui", "Moloka'i", "O'ahu"), 9),
		"TertiaryStratumID" = "",
		"TransitionTypeID" = rep(c("AGRICULTURAL CONTRACTION: Ag->Forest",
			"AGRICULTURAL CONTRACTION: Ag->Grassland",
			"AGRICULTURAL CONTRACTION: Ag->Shrubland"), each = 18),
		"Probability" = 1, 
		"Proportion" = NA,
		"AgeMin" = NA,
		stringsAsFactors = FALSE)

# Agricultural Expansion transition pathways by island
AgEx <- data.frame("Iteration" = "",
		"Timestep" = "",
		"StratumIDSource" = rep(c("Dry", "Mesic", "Wet"), each = 6, 3), 
		"StateClassIDSource" = rep(c("Forest:All", "Grassland:All",
			"Shrubland:All"), each = 18),
		"StratumIDDest" = "",
		"StateClassIDDest" = "Agriculture:All",
		"SecondaryStratumID" = rep(c("Hawai'i", "Kaua'i",
			"Lana'i", "Maui", "Moloka'i", "O'ahu"), 9),
		"TertiaryStratumID" = "",
		"TransitionTypeID" = rep(c("AGRICULTURAL EXPANSION: Forest->Ag",
			"AGRICULTURAL EXPANSION: Grassland->Ag",
			"AGRICULTURAL EXPANSION: Shrubland->Ag"), each = 18),
		"Probability" = 1, 
		"Proportion" = NA,
		"AgeMin" = NA,
		stringsAsFactors = FALSE) 

# Make numeric vectors of fire severity proportions 
# for each combination of Moisture Zone and State Class
ForestHigh <- c(0.0087, 0.1207, 0.0052)
ForestLow <- c(0.9086, 0.5469, 0.6496)
ForestMed <- c(0.0826, 0.3324, 0.3452)
GrassHigh <- c(0.0150, 0.0109, 0.0056)
GrassLow <- c(0.8801, 0.8271, 0.7178)
GrassMed <- c(0.1074, 0.1620, 0.2767)
ShrubHigh <- c(0.0047, 0.0637, 0.0348)
ShrubLow <- c(0.8629, 0.5454, 0.2750)
ShrubMed <- c(0.1325, 0.3910, 0.6902)
# Fire transition pathways, statewide updated with new pathways
# for fire in Ag, Plantation and WoodyCrop State Classes.  
Fire <- data.frame("Iteration" = "",
		"Timestep" = "",
		"StratumIDSource" = rep(c("Dry", "Mesic", "Wet"), 18), 
		"StateClassIDSource" = rep(c("Forest:All", "Grassland:All",
			"Shrubland:All", "Agriculture:All", "Plantation:All", 
			"WoodyCrop:All"), each = 9),
		"StratumIDDest" = "",
		"StateClassIDDest" = rep(c("Grassland:All", "Forest:All",
			"Grassland:All", "Grassland:All", "Grassland:All", "Grassland:All",
			"Grassland:All", "Shrubland:All", "Grassland:All", "Agriculture:All", 
			"Agriculture:All", "Agriculture:All", "Grassland:All", "Plantation:All",
			"Plantation:All", "Grassland:All", "WoodyCrop:All", "WoodyCrop:All"),
			 each = 3),
		"SecondaryStratumID" = "",
		"TertiaryStratumID" = "",
		"TransitionTypeID" = rep(c("FIRE: Forest High Severity",
			"FIRE: Forest Low Severity", "FIRE: Forest Medium Severity",
			"FIRE: Grassland High Severity", "FIRE: Grassland Low Severity",
			"FIRE: Grassland Medium Severity", "FIRE: Shrubland High Severity",
			"FIRE: Shrubland Low Severity", "FIRE: Shrubland Medium Severity",
			"FIRE: Ag High Severity", "FIRE: Ag Low Severity", 
			"FIRE: Ag Medium Severity", "FIRE: Plantation High Severity",
			"FIRE: Plantation Low Severity", "FIRE: Plantation Medium Severity",
			"FIRE: WoodyCrop High Severity", "FIRE: WoodyCrop Low Severity", 
			"FIRE: WoodyCrop Medium Severity"), each = 3), 
		"Probability" = 1,
		"Proportion" = c(ForestHigh, ForestLow, ForestMed, GrassHigh, GrassLow,
			GrassMed, ShrubHigh, ShrubLow, ShrubMed, GrassHigh, GrassLow, GrassMed,
			rep(c(ForestHigh, ForestLow, ForestMed), 2)), 
		"AgeMin" = as.numeric(NA), 
		"AgeMax" = as.numeric(NA),
		"AgeRelative" = "",
		"AgeReset" = rep(c("", "No", "No", "", "", "", "", "No", "", "", "", "",
			"", "No", "No", "", "No", "No"), each = 3),
		stringsAsFactors = FALSE)

# bind rows of dataframes to make Transition Pathways
LULC <- bind_rows(list(Urban, AgCon, 
	AgEx, Fire, Plant_Harvest)) %>%
	mutate(TSTMin = NA,
		TSTMax = NA, 
		TSTRelative = NA)
# write Transition Pathways to csv file 
write.csv(LULC, "./InputData/TransitionPathways_Fire.csv", row.names = FALSE, na = "")

