## Disturbance Event Flow Pathways
## for HI LUCAS Model 
## Soil flux from Don et al. (2010)
## Other fluxes from Sleeter et al. (2017)

## Paul C. Selmants 
## 2019-09-10 (ISO 8601) 

## UPDATED 2020-02-06

#load required packages into R
library(dplyr)

#Flow pathways and multipliers for Agricultural Expansion
AgEx <- data.frame("FromStratumID" = "",
	"FromStockTypeID" = c(rep(c("Deadwood", "Litter", "Living Biomass", 
		"Soil"), 3), "Living Biomass"),
	"ToStockTypeID" = c(rep("Atmosphere",12), "Harvest Products"),
	"TransitionGroupID" = c(rep(c("AGRICULTURAL EXPANSION: Forest->Ag [Type]",
		"AGRICULTURAL EXPANSION: Grassland->Ag [Type]", 
		"AGRICULTURAL EXPANSION: Shrubland->Ag [Type]"), each = 4), 
		"AGRICULTURAL EXPANSION: Forest->Ag [Type]"), 
	"FlowTypeID" = c(rep(c("Emission (deadwood)", "Emission (litter)", 
		"Emission (biomass)", "Emission (soil)"), 3), "Harvest (Timber)"), 
	"Multiplier" = c(rep(1,2), 0.45, 0.13, rep(1, 3), 0.10, rep(1, 3),
		0.10, 0.55),
	stringsAsFactors = FALSE)

#Flow pathways and multipliers for Urbanization
Urban <- data.frame("FromStratumID" = "",
	"FromStockTypeID" = c(rep(c("Deadwood", "Litter", "Living Biomass",
	 "Soil"), 4), "Living Biomass"),
	"ToStockTypeID" = c(rep("Atmosphere",16), "Harvest Products"),
	"TransitionGroupID" = c(rep(c("URBANIZATION: Agriculture->Developed [Type]", 
		"URBANIZATION: Forest->Developed [Type]",
		"URBANIZATION: Grassland->Developed [Type]", 
		"URBANIZATION: Shrubland->Developed [Type]"), each = 4), 
		"URBANIZATION: Forest->Developed [Type]"), 
	"FlowTypeID" = c(rep(c("Emission (deadwood)", "Emission (litter)", 
		"Emission (biomass)", "Emission (soil)"), 4), "Harvest (Timber)"), 
	"Multiplier" = c(rep(1,3), 0.07, rep(1,2), 0.45, 0.13, rep(1, 3), 0.07,
		rep(1, 3), 0.10, 0.55),
	stringsAsFactors = FALSE)

#Flow pathways and multipliers for rotation harvesting of Plantations
RotHarvest <- data.frame("FromStratumID" = "",
	"FromStockTypeID" = rep("Living Biomass", 3),
	"ToStockTypeID" = c("Atmosphere", "Harvest Products", "Standing Deadwood"),
	"TransitionGroupID" = "MANAGEMENT: Plantation Harvest [Type]", 
	"FlowTypeID" = c("Emission (biomass)", "Harvest (Timber)", "Mortality"),
	"Multiplier" = c(0.35, 0.55, 0.035),
	stringsAsFactors = FALSE)

#Flow pathways and multipliers for conversion harvesting of Plantations 
ConHarvest <- data.frame("FromStratumID" = "",
	"FromStockTypeID" = rep(c("Deadwood", "Litter", "Living Biomass",
	 "Soil", "Living Biomass"), 3), 
	"ToStockTypeID" = rep(c(rep("Atmosphere", 4), "Harvest Products"), 3), 
	"TransitionGroupID" = c(rep(c("MANAGEMENT: Plantation->Ag [Type]", 
		"MANAGEMENT: Plantation->Grassland [Type]",
		"MANAGEMENT: Plantation->WoodyCrop [Type]"), each = 5)), 
	"FlowTypeID" = rep(c("Emission (deadwood)", "Emission (litter)", 
		"Emission (biomass)", "Emission (soil)", "Harvest (Timber)"), 3), 
	"Multiplier" = rep(c(1, 1 ,0.45, 0.10, 0.55), 3),
	stringsAsFactors = FALSE) 

# Flow pathways and multipliers for Ag fire (high, medium, and low severity) 
Fire_Ag <- data.frame("FromStratumID" = "",
	"FromStockTypeID" = rep(c("Litter", "Living Biomass"), 3),
	"ToStockTypeID" = "Atmosphere", 
	"TransitionGroupID" = rep(c("FIRE: Ag High Severity [Type]", 
		"FIRE: Ag Low Severity [Type]", 
		"FIRE: Ag Medium Severity [Type]"), each = 2),
	"FlowTypeID" = rep(c("Emission (litter)", "Emission (biomass)"), 3), 
	"Multiplier" = c(rep(c(0.9865, 0.4897), 3)),
	stringsAsFactors = FALSE)

#Flow pathways and multipliers for high, medium, and low severity forest fire
Fire_Forest <- data.frame("FromStratumID" = rep(c(rep("Dry", 2), 
		rep("Mesic", 2), rep("Wet", 2), rep("", 2)), 3),
	"FromStockTypeID" = rep(c(rep(c("Deadwood", "Litter"), 3), 
		rep("Living Biomass", 2)), 3),
	"ToStockTypeID" = rep(c(rep("Atmosphere", 7), "Standing Deadwood"), 3), 
	"TransitionGroupID" = c(rep(c("FIRE: Forest High Severity [Type]", 
		"FIRE: Forest Low Severity [Type]", "FIRE: Forest Medium Severity [Type]"), 
		each = 8)),
	"FlowTypeID" = rep(c(rep(c("Emission (deadwood)", "Emission (litter)"), 3), 
		"Emission (biomass)", "Mortality"), 3), 
	"Multiplier" = c(0.2133, 0.7804, 0.5417, 0.9376, 0.4135, 1, 0.28, 0.72, 
		0.1748, 0.5974, 0.4655, 0.8829, 0.3518, 1, 0.056, 0.194, 0.1934, 
		0.6983, 0.5136, 0.9102, 0.3884, 1, 0.168, 0.332),
	stringsAsFactors = FALSE)

# Flow pathways and multipliers for high, medium, and low severity grassland fire
Fire_Grassland <- data.frame("FromStratumID" = "",
	"FromStockTypeID" = rep(c("Litter", "Living Biomass"), 3),
	"ToStockTypeID" = "Atmosphere", 
	"TransitionGroupID" = rep(c("FIRE: Grassland High Severity [Type]", 
		"FIRE: Grassland Low Severity [Type]", 
		"FIRE: Grassland Medium Severity [Type]"), each = 2),
	"FlowTypeID" = rep(c("Emission (litter)", "Emission (biomass)"), 3), 
	"Multiplier" = c(rep(c(0.9865, 0.4897), 3)),
	stringsAsFactors = FALSE) 

#Flow pathways and multipliers for WoodyCrop fire (high, medium, and low severity)
Fire_Plantation <- data.frame("FromStratumID" = rep(c(rep("Dry", 2), 
		rep("Mesic", 2), rep("Wet", 2), rep("", 2)), 3),
	"FromStockTypeID" = rep(c(rep(c("Deadwood", "Litter"), 3), 
		rep("Living Biomass", 2)), 3),
	"ToStockTypeID" = rep(c(rep("Atmosphere", 7), "Standing Deadwood"), 3), 
	"TransitionGroupID" = c(rep(c("FIRE: Plantation High Severity [Type]", 
		"FIRE: Plantation Low Severity [Type]", "FIRE: Plantation Medium Severity [Type]"),
		each = 8)),
	"FlowTypeID" = rep(c(rep(c("Emission (deadwood)", "Emission (litter)"), 3), 
		"Emission (biomass)", "Mortality"), 3), 
	"Multiplier" = c(0.2133, 0.7804, 0.5417, 0.9376, 0.4135, 1, 0.28, 0.72,
		 0.1748, 0.5974, 0.4655, 0.8829, 0.3518, 1, 0.056, 0.194, 0.1934, 
		 0.6983, 0.5136, 0.9102, 0.3884, 1, 0.168, 0.332),
	stringsAsFactors = FALSE)

#Flow pathways and multipliers for high, medium, and low severity shrubland fire
Fire_Shrubland <- data.frame("FromStratumID" = rep(c(rep("Dry", 2), 
		rep("Mesic", 2), rep("Wet", 2), rep("", 2)), 3),
	"FromStockTypeID" = rep(c(rep(c("Deadwood", "Litter"), 3), 
		rep("Living Biomass", 2)), 3),
	"ToStockTypeID" = rep(c(rep("Atmosphere", 7), "Standing Deadwood"), 3), 
	"TransitionGroupID" = c(rep(c("FIRE: Shrubland High Severity [Type]", 
		"FIRE: Shrubland Low Severity [Type]", 
		"FIRE: Shrubland Medium Severity [Type]"), each = 8)),
	"FlowTypeID" = rep(c(rep(c("Emission (deadwood)", "Emission (litter)"), 3), 
		"Emission (biomass)", "Mortality"), 3), 
	"Multiplier" = c(0.3301, 0.7975, rep(c(0.19, 0.9955), 2), 0.2965, 0.7035, 
		0.2734, 0.6268, rep(c(0.1411, 0.9911), 2), 0.0425, 0.2075, 0.312, 
		0.7206, rep(c(0.1716, 0.9934), 2), 0.1711, 0.3289),
	stringsAsFactors = FALSE)

#Flow pathways and multipliers for WoodyCrop fire (high, medium, and low severity)
Fire_WoodyCrop <- data.frame("FromStratumID" = rep(c(rep("Dry", 2), 
		rep("Mesic", 2), rep("Wet", 2), rep("", 2)), 3),
	"FromStockTypeID" = rep(c(rep(c("Deadwood", "Litter"), 3), 
		rep("Living Biomass", 2)), 3),
	"ToStockTypeID" = rep(c(rep("Atmosphere", 7), "Standing Deadwood"), 3), 
	"TransitionGroupID" = c(rep(c("FIRE: WoodyCrop High Severity [Type]", 
		"FIRE: WoodyCrop Low Severity [Type]", "FIRE: WoodyCrop Medium Severity [Type]"), 
		each = 8)),
	"FlowTypeID" = rep(c(rep(c("Emission (deadwood)", "Emission (litter)"), 3), 
		"Emission (biomass)", "Mortality"), 3), 
	"Multiplier" = c(0.2133, 0.7804, 0.5417, 0.9376, 0.4135, 1, 0.28, 0.72,
		0.1748, 0.5974, 0.4655, 0.8829, 0.3518, 1, 0.056, 0.194, 0.1934, 
		0.6983, 0.5136, 0.9102, 0.3884, 1, 0.168, 0.332),
	stringsAsFactors = FALSE)

# Bind rows of event flow pathways together
Events <- bind_rows(list(AgEx, Fire_Forest, Fire_Grassland, Fire_Plantation,
	Fire_Shrubland, Fire_WoodyCrop, RotHarvest, ConHarvest, Urban)) %>%
	mutate(Iteration = "",
		Timestep = "",
		FromStateClassID = "",
		FromAgeMin = "",
		ToStratumID = "",
		ToStateClassID = "",
		ToAgeMin = "",
		StateAttributeTypeID = "") %>% 
	select(Iteration, Timestep, FromStratumID, FromStateClassID, FromAgeMin, 
		FromStockTypeID, ToStratumID, ToStateClassID, ToAgeMin, ToStockTypeID, 
		TransitionGroupID, StateAttributeTypeID, FlowTypeID, Multiplier)

# write Events dataframe to .csv file, save in 
# HI_Model/Build_STsim/data
write.csv(Events, "./data/FlowPathways_Events.csv", row.names = FALSE)

