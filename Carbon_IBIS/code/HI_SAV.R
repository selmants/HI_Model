## Format Hawaii IBIS summary files for input as 
## State Attribute Values in LUCAS ST-SIM
## Paul C. Selmants
## 2020-04-02 (ISO 8601)
## Summarized NPP for all State Classes and 
## SAV for non-tree State Classes

# Set working directory to "CarbonStockFlow_IBIS" folder in HI_Model
# GitHub repository (https://github.com/selmants/HI_Model)

## load required packages into R
library(dplyr)
library(tidyr)

## make a list of IBIS cbiotot (live biomass in kgC/m2) summary output
## ASCII text files for all covmax classes 
biotot_list <- list.files(pattern = 'cbiotot', recursive = TRUE,
	full.names = TRUE)
## read IBIS cbiotot (live biomass in kgC/m2) summary output ASCII 
## text files for all covmax classes into R and format for ST-SIM 
biomass <- lapply(biotot_list[c(1:5)], FUN = read.table, header = TRUE) %>%
	bind_rows() %>%
	slice(c(7:9, 23:25, 39:41, 55:57, 71:73)) %>%
	mutate(StateClassID = rep(c('Forest:All', 'Agriculture:All','Shrubland:All',
	 'Grassland:All', 'WoodyCrop:All'), each = 3)) %>%
	select(StateClassID, region_id, X1:X110) %>%
	gather(year, Value, X1:X110) %>%
	arrange(StateClassID, region_id) %>%
	mutate(AgeMin = rep(1:110, 15),
		AgeMax = as.integer(rep(c(1:109,""), 15)),
		StratumID = rep(c('Dry', 'Mesic', 'Wet'), 5,  each = 110),
		StateAttributeTypeID = 'Initial Conditions: Living Biomass') %>%
	select(StratumID, StateClassID, StateAttributeTypeID, AgeMin, AgeMax, Value)

## make a list of IBIS stdwdc (standing deadwood in kgC/m2) summary output 
## ASCII text files for covmax 1(forest), 3(shrubland), and 5(woodycrop)
stdw_list <- list.files(pattern = 'stdwdc', recursive = TRUE,
	full.names = TRUE)
## read IBIS stdwdc (standing deadwood C in kgC/m2) summary output ASCII 
## text files for forest, shrub, woodycrop into R and format for ST-SIM 
stdw <- lapply(stdw_list[c(1,3,5)], FUN = read.table, header = TRUE) %>%
	bind_rows() %>%
	slice(c(7:9, 23:25, 39:41)) %>%
	mutate(StateClassID = rep(c('Forest:All', 'Shrubland:All', 'WoodyCrop:All'),
		each = 3)) %>%
	select(StateClassID, region_id, X1:X110) %>%
	gather(year, Value, X1:X110) %>%
	arrange(StateClassID, region_id) %>%
	mutate(AgeMin = rep(1:110, 9),
		AgeMax = as.integer(rep(c(1:109,""), 9)),
		StratumID = rep(c('Dry', 'Mesic', 'Wet'), 3,  each = 110),
		StateAttributeTypeID = 'Initial Conditions: Standing Deadwood') %>%
	select(StratumID, StateClassID, StateAttributeTypeID, AgeMin, AgeMax, Value)

## make a list of IBIS deadwdc (down deadwood in kgC/m2) summary output 
## ASCII text files for covmax1 (forest), 3(shrubland), and 5(woodycrop)
ddw_list <- list.files(pattern = 'deadwdc', recursive = TRUE,
	full.names = TRUE)
## read IBIS stdwdc (down deadwood C in kgC/m2) summary output ASCII 
## text files into R and format for ST-SIM 
ddw <- lapply(ddw_list[c(1,3,5)], FUN = read.table, header = TRUE) %>%
	bind_rows() %>%
	slice(c(7:9, 23:25, 39:41)) %>%
	mutate(StateClassID = rep(c('Forest:All', 'Shrubland:All', 'WoodyCrop:All'),
		each = 3)) %>%
	select(StateClassID, region_id, X1:X110) %>%
	gather(year, Value, X1:X110) %>%
	arrange(StateClassID, region_id) %>%
	mutate(AgeMin = rep(1:110, 9),
		AgeMax = as.integer(rep(c(1:109,""), 9)),
		StratumID = rep(c('Dry', 'Mesic', 'Wet'), 3,  each = 110),
		StateAttributeTypeID = 'Initial Conditions: Deadwood') %>%
	select(StratumID, StateClassID, StateAttributeTypeID, AgeMin, AgeMax, Value)

## make a list of IBIS totcsoi (total soil carbon in kgC/m2) summary output text files   
soc_list <- list.files(pattern = 'totcsoi', recursive = TRUE,
	full.names = TRUE)
## read IBIS totcsoi (soil carbon in kgC/m2) summary output text files into R
## and format for ST-SIM 
soc <- lapply(soc_list[c(1:5)], FUN = read.table, header = TRUE) %>%
	bind_rows() %>%
	slice(c(7:9, 23:25, 39:41, 55:57, 71:73)) %>%
	mutate(StateClassID = rep(c('Forest:All', 'Agriculture:All', 'Shrubland:All',
	 'Grassland:All', 'WoodyCrop:All'), each = 3)) %>%
	select(StateClassID, region_id, X1:X110) %>%
	gather(year, Value, X1:X110) %>%
	arrange(StateClassID, region_id) %>%
	mutate(AgeMin = rep(1:110, 15),
		AgeMax = as.integer(rep(c(1:109,""), 15)),
		StratumID = rep(c('Dry', 'Mesic', 'Wet'), 5,  each = 110),
		StateAttributeTypeID = 'Initial Conditions: Soil') %>%
	select(StratumID, StateClassID, StateAttributeTypeID, AgeMin, AgeMax, Value)

## make a list of IBIS aynpp (NPP in kgC/m2/y) summary output text files
npp_list <- list.files(pattern = 'aynpp', recursive = TRUE, full.names = TRUE)
## read IBIS aynpp (NPP in kgC/m2/y) summary output text files into R and 
## format for ST-SIM
npp <- lapply(npp_list[c(1:5)], FUN = read.table, header = TRUE) %>%
	bind_rows() %>%
	slice(c(7:9, 23:25, 39:41, 55:57, 71:73)) %>%
	mutate(StateClassID = rep(c('Forest:All', 'Agriculture:All', 'Shrubland:All',
	 'Grassland:All', 'WoodyCrop:All'), each = 3)) %>%
	select(StateClassID, region_id, X1:X110) %>%
	gather(year, Value, X1:X110) %>%
	arrange(StateClassID, region_id) %>%
	mutate(AgeMin = rep(1:110, 15),
		AgeMax = as.integer(rep(c(1:109,""), 15)), 
		StratumID = rep(c('Dry', 'Mesic', 'Wet'), 5, each = 110),
		StateAttributeTypeID = 'NPP') %>%
	select(StratumID, StateClassID, StateAttributeTypeID, AgeMin, AgeMax, Value)

## make a list of IBIS totlit (litter pool in kgC/m2) summary output text files
totlit_list <- list.files(pattern = 'totlit', recursive = TRUE, full.names = TRUE)
## read IBIS totlit (litter pool in kgC/m2) summary output text files into R
## and format for ST-SIM
litter <- lapply(totlit_list[c(1:5)], FUN = read.table, header = TRUE) %>%
	bind_rows() %>%
	slice(c(7:9, 23:25, 39:41, 55:57, 71:73)) %>%
	mutate(StateClassID = rep(c('Forest:All', 'Agriculture:All', 'Shrubland:All',
	 'Grassland:All', 'WoodyCrop:All'), each = 3)) %>%
	select(StateClassID, region_id, X1:X110) %>%
	gather(year, Value, X1:X110) %>%
	arrange(StateClassID, region_id) %>%
	mutate(AgeMin = rep(1:110, 15),
		AgeMax = as.integer(rep(c(1:109,""), 15)), 
		StratumID = rep(c('Dry', 'Mesic', 'Wet'), 5, each = 110),
		StateAttributeTypeID = 'Initial Conditions: Litter') %>%
	select(StratumID, StateClassID, StateAttributeTypeID, AgeMin, AgeMax, Value)

## row bind live biomass, litter, standing & down deadwood, soil organic carbon, 
## litter, and NPP into a single State Attribute Values (SAV) Table 
SAVtable <- bind_rows(biomass, stdw, ddw, litter, soc, npp) %>%
	arrange(StratumID, StateClassID) %>%
	filter(StateClassID != 'WoodyCrop:All' | StratumID != 'Dry')

## read in SAV table, filter on Wet WoodyCrop, and calculate Dry WoodyCrop 
## SAV based on IBIS Dry/Wet Shrub (covmax3) ratios 
DWC_SAV <- SAVtable %>%
	filter(StateClassID == 'WoodyCrop:All', StratumID == 'Wet') %>%
	mutate(ratio = rep(c(0.3679, 0.3662, 0.3647, 0.2216, 
		0.2519, 0.3666), each = 110),
		DryValue = Value*ratio, 
		MZ = 'Dry') %>%
	select(StratumID = MZ, StateClassID, StateAttributeTypeID, AgeMin, AgeMax,
		Value = DryValue)

## read in SAV table, filter on Forest, and change StateClassID label
## to 'Plantation:All'
Plantat_SAV <- SAVtable %>%
	filter(StateClassID == 'Forest:All') %>%
	mutate(stcl = 'Plantation:All') %>%
	select(StratumID, StateClassID = stcl, StateAttributeTypeID, AgeMin, AgeMax, Value)

## make general State Class data frame
stateclass_all <- data.frame(StratumID = "", 
	StateClassID = c("Agriculture:All","Barren:All", "Developed:All", "Forest:All",
		"Grassland:All", "Plantation:All", "Shrubland:All", "WoodyCrop:All"), 
	StateAttributeTypeID = c("Agriculture", "Barren", "Developed", "Forest", 
		"Grassland", "Plantation", "Shrubland", "WoodyCrop"), 
	Value = 1, stringsAsFactors = FALSE)
## make general Moisture Zone (StratumID) data frame  
mz_all <- data.frame(StratumID = c("Dry", "Mesic", "Wet"), StateClassID = "", 
	StateAttributeTypeID = c("Dry", "Mesic", "Wet"), Value = 1, 
	stringsAsFactors = FALSE) 
## row bind stateclass_all and mz_all
stateclassmz <- bind_rows(mz_all, stateclass_all) 

## row bind SAVtable, DWC_SAV, stateclass_all, and mz_all into final SAV table
SAV <- bind_rows(SAVtable, DWC_SAV, Plantat_SAV, stateclassmz) %>%
	arrange(StratumID, StateClassID)

## Build brief (summarized) version of SAV table where only tree-dominated 
## State Classes have annual attribute values

# filter SAV to NPP in Forest, Plantation & Woody Crop, calculate means 
treegrowth <- SAV %>%
	filter(StateClassID %in% c("Forest:All", "Plantation:All", "WoodyCrop:All") &
		StateAttributeTypeID == "NPP") %>%
	group_by(StratumID, StateClassID, StateAttributeTypeID) %>%
	summarise(Value = mean(Value)) %>%
	data.frame(.) 
# filter SAV to non-NPP State Attributes in Forest, Plantation, & WoodyCrop 
treeother <- SAV %>%
	filter(StateClassID %in% c("Forest:All", "Plantation:All", "WoodyCrop:All") &
		StateAttributeTypeID != "NPP") 
# combine all tree State Attribute Values together in a single dataframe
trees <- bind_rows(treegrowth, treeother) %>%
	arrange(StratumID, StateClassID, StateAttributeTypeID) 

# filter SAV to just Ag, Grassland, and Shrubland State Classes
nontree_brief <- SAV %>%
	filter(StateClassID %in% c("Agriculture:All", "Grassland:All", "Shrubland:All")) %>%
	group_by(StratumID, StateClassID, StateAttributeTypeID) %>%
	summarise(Value = mean(Value)) %>%
	data.frame(.)

# filter SAV to just Barren and Developed State Classes
noveg <- SAV %>% 
	filter(StateClassID %in% c("Barren:All", "Developed:All")) 

# combine summarized SAVs from all State Classes together in a single dataframe
SAV_brief <- bind_rows(noveg, nontree_brief, trees) %>%
	arrange(StratumID, StateClassID, StateAttributeTypeID)

# write SAV to .csv file
write.csv(SAV, "./data/processed/SAV_Full.csv", row.names = FALSE, na = "")
# write SAV_brief to .csv file
write.csv(SAV_brief, "../InputData/SAVbrief.csv", row.names = FALSE, na = "")
