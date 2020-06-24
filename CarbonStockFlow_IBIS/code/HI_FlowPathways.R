## Flow pathway multipliers for HI LUCAS model
## forest (covmax1), ag (covmax2), shrub (covmax3), 
## grass (covmax4), and woodycrop (covmax5)
## state classes within each moisture zone (MZ)
## Paul C. Selmants
## 2019-02-04 (ISO 8601)

# Set working directory to "CarbonStockFlow_IBIS" folder of HI_Model
# GitHub repository (https://github.com/selmants/HI_Model)

## This R script is dependent on output from HI_SAV.R and HI_Flux.R scripts ##

#load required packages into R
library(dplyr)
library(tidyr)

#read State Attribute Value (SAV) file into R,
#filter out values < 19 years, calculate lag 
#(previous year) values for each carbon stock, filter out year 19, and 
#filter out Dry WoodyCrop values (not accurately represented by IBIS). 
SAV <- read.csv('./data/processed/SAV_Full.csv', stringsAsFactors = FALSE) %>%
	filter(AgeMin >= 19) %>%
	group_by(StratumID, StateClassID, StateAttributeTypeID) %>%
	mutate(lagValue = lag(Value)) %>%
	filter(AgeMin != 19) %>%
	filter(StateClassID != 'WoodyCrop:All' | StratumID != 'Dry') %>%
	filter(StateClassID != 'Plantation:All') %>%
	as.data.frame(.)
# Read IBIS flux values .csv file into R
flux <- read.csv('./data/processed/HI_Flux.csv', stringsAsFactors = FALSE)

				#### All COVER CLASSES ####
#make separate R objects for State Attribute Values used as input 
#to Flow Multiplier equations. 
biomass <- filter(SAV, StateAttributeTypeID == 'Initial Conditions: Living Biomass')		
litter <- filter(SAV, StateAttributeTypeID == 'Initial Conditions: Litter')	
soc <- filter(SAV, StateAttributeTypeID == 'Initial Conditions: Soil')
#make separate R objects for fluxes used as variables in 
#Flow Multiplier calculations for ALL cover classes
NPP <- filter(SAV, StateAttributeTypeID == 'NPP')
rawlitc <- filter(flux, Flux == 'rawlitc')
lit2soc <- filter(flux, Flux == 'lit2soc')

#construct data frame of multipliers for growth as proportion of NPP
growth <- data.frame(FromStratumID = rep(c('Dry', 'Mesic', 'Wet'), each = 5),
	FromStateClassID = rep(c('Agriculture:All', 'Forest:All', 'Grassland:All', 
		'Shrubland:All', 'WoodyCrop:All'), 3),
	FromStockTypeID = 'Atmosphere', 
	ToStockTypeID = 'Living Biomass', 
	StateAttributeTypeID = 'NPP', 
	FlowTypeID = 'Growth', 
	Multiplier = 1.0,
	stringsAsFactors = FALSE) 
#calculate multipliers for litterfall as proportion of previous year
#living biomass + current year NPP (No Ag)
litterfall <- rawlitc %>%
	mutate(turnover = Value/(NPP$Value + biomass$lagValue)) %>%
	group_by(FromStratumID, FromStateClassID) %>%
	summarize(Multiplier = mean(turnover)) %>%
	mutate(FromStockTypeID = 'Living Biomass', 
		ToStockTypeID = 'Litter',
		StateAttributeTypeID = "", 
		FlowTypeID = 'Litterfall') %>%
	filter(FromStateClassID != 'Agriculture:All') %>%
	as.data.frame(.)
#calculate multipliers for soil CO2 emission as proportion of 
#current year litter decay (SOC Input)
soilemiss <- flux %>%
	filter(Flux == 'soc2co2') %>% 
	mutate(turnover = Value/lit2soc$Value) %>%
	group_by(FromStratumID, FromStateClassID) %>%
	summarize(Multiplier = mean(turnover)) %>%
	mutate(FromStockTypeID = 'Soil',
		ToStockTypeID = 'Atmosphere',
		StateAttributeTypeID = 'SOC Input',
		FlowTypeID = 'Emission (soil)') %>%
	as.data.frame(.)

#calculate soil CO2 emission multipliers as 
# a proportion of current year NPP
soilemissNPP <- flux %>%
	filter(Flux == 'soc2co2') %>% 
	mutate(turnover = Value/NPP$Value) %>%
	group_by(FromStratumID, FromStateClassID) %>%
	summarize(Multiplier = mean(turnover)) %>%
	mutate(FromStockTypeID = 'Soil',
		ToStockTypeID = 'Atmosphere',
		StateAttributeTypeID = 'NPP',
		FlowTypeID = 'Emission (soil)') %>%
	as.data.frame(.)

#calculate multipliers for leaching as proportion 
#of current year litter decay (SOC Input)
leaching <- flux %>%
	filter(Flux == 'yrleach') %>%
	mutate(turnover = Value/(lit2soc$Value)) %>%
	group_by(FromStratumID, FromStateClassID) %>%
	summarize(Multiplier = mean(turnover)) %>%
	mutate(FromStockTypeID = 'Soil',
		ToStockTypeID = 'Aquatic',
		StateAttributeTypeID = 'SOC Input',
		FlowTypeID = 'Leaching') %>%
	as.data.frame(.)

# calculate leaching multipliers as
# a proportion of current year NPP
leachingNPP <- flux %>%
	filter(Flux == 'yrleach') %>%
	mutate(turnover = Value/NPP$Value) %>%
	group_by(FromStratumID, FromStateClassID) %>%
	summarize(Multiplier = mean(turnover)) %>%
	mutate(FromStockTypeID = 'Soil',
		ToStockTypeID = 'Aquatic',
		StateAttributeTypeID = 'NPP',
		FlowTypeID = 'Leaching') %>%
	as.data.frame(.)

#row bind multipliers for all cover classes
multi_all <- bind_rows(growth, litterfall, soilemissNPP, leachingNPP)

				#### WOODY COVER CLASSES ####
#make target subset of woody cover classes for filtering
woody <- c("Forest:All", "Shrubland:All", "WoodyCrop:All")

#make separate R objects for State Attribute Values used as input to
#Flow multiplier equations for woody cover classes
woodybiomass <- filter(SAV, StateClassID %in% woody, 
	StateAttributeTypeID == 'Initial Conditions: Living Biomass')
woodylitter <- filter(SAV, StateClassID %in% woody, 
	StateAttributeTypeID == 'Initial Conditions: Litter')
stdw <- filter(SAV, StateAttributeTypeID == 'Initial Conditions: Standing Deadwood')	
ddw <- filter(SAV, StateAttributeTypeID == 'Initial Conditions: Deadwood') 

#make separate R objects for fluxes used as variables in 
#Flow Multiplier calculations for woody cover classes (covmax 1,3,5)
woodyNPP <- filter(SAV, StateClassID %in% woody, 
	StateAttributeTypeID == 'NPP')
fallw <- filter(flux, Flux == 'fallw')
down2lit <- filter(flux, Flux == 'down2lit')
woodyrawlitc <- filter(flux, FromStateClassID %in% woody, 
	Flux == 'rawlitc')
woodylit2soc <- filter(flux, FromStateClassID %in% woody, 
	Flux == 'lit2soc')

#calculate multipliers for woody mortality as proportion of previous year 
#living biomass + current year NPP
mortality <- fallw %>%
	mutate(turnover = Value/(woodyNPP$Value + woodybiomass$lagValue)) %>%
	group_by(FromStratumID, FromStateClassID) %>%
	summarize(Multiplier = mean(turnover)) %>%
	mutate(FromStockTypeID = 'Living Biomass', 
		ToStockTypeID = 'Standing Deadwood',
		StateAttributeTypeID = "", 
		FlowTypeID = 'Mortality') %>%
	as.data.frame(.)
#calculate multipliers for standing dead tree fall as proportion of previous year
#standing deadwood + current year live tree mortality
deadfall <- flux %>%
	filter(Flux == 'stdwcloss') %>%
	mutate(turnover = Value/(fallw$Value + stdw$lagValue)) %>%
	group_by(FromStratumID, FromStateClassID) %>%
	summarize(Multiplier = mean(turnover)) %>%
	mutate(FromStockTypeID = 'Standing Deadwood', 
		ToStockTypeID = 'Deadwood',
		StateAttributeTypeID = "",
		FlowTypeID = 'Deadfall') %>%
	as.data.frame(.)
#calculate multipliers for deadwood decay as proportion of previous year 
#down deadwood + curent year deadfall
dwdecay <- flux %>%
	filter(Flux == 'stdwcloss') %>%
	mutate(turnover = down2lit$Value/(Value + ddw$lagValue)) %>%
	group_by(FromStratumID, FromStateClassID) %>%
	summarize(Multiplier = mean(turnover)) %>%
	mutate(FromStockTypeID = 'Deadwood', 
		ToStockTypeID = 'Litter',
		StateAttributeTypeID = "",
		FlowTypeID= 'Deadwood Decay') %>%
	as.data.frame(.)
#calculate multipliers for litter decay as proportion of previous year
#litter + current year litterfall + current year deadwood decay
woodylitdecay <- woodyrawlitc %>%
	mutate(turnover = woodylit2soc$Value/(Value + down2lit$Value +
		woodylitter$lagValue)) %>%
	group_by(FromStratumID, FromStateClassID) %>%
	summarize(Multiplier = mean(turnover)) %>%
	mutate(FromStockTypeID = 'Litter',
		ToStockTypeID = 'Soil',
		StateAttributeTypeID = "",
		FlowTypeID= 'Litter Decay') %>%
	as.data.frame(.)
#calculate multipliers for litter CO2 emission as proportion of previous year
#litter + current year litterfall + current year deadwood decay
woodylitemiss <- flux %>%
	filter(FromStateClassID %in% woody, Flux == 'lit2co2') %>%
	mutate(turnover = Value/(woodyrawlitc$Value + down2lit$Value 
		+ woodylitter$lagValue)) %>%
	group_by(FromStratumID, FromStateClassID) %>%
	summarize(Multiplier = mean(turnover)) %>%
	mutate(FromStockTypeID = 'Litter',
		ToStockTypeID = 'Atmosphere',
		StateAttributeTypeID = "",
		FlowTypeID = 'Emission (litter)') %>%
	as.data.frame(.)
#row bind multipliers for woody cover classes
multi_woody <- bind_rows(mortality, deadfall, dwdecay, woodylitdecay, 
	woodylitemiss) 

				#### AG & GRASS COVER CLASSES ####
#make target subset of Ag and Grass cover classes
ag_grass <- c('Agriculture:All', 'Grassland:All')
#calculate multipliers for litter decay as proportion of current year 
#littefall + previous year litter
ag_grasslitdecay <- rawlitc%>% 
		mutate(turnover = lit2soc$Value/(Value + litter$lagValue)) %>%
		group_by(FromStratumID, FromStateClassID) %>%
		summarize(Multiplier = mean(turnover)) %>%
		mutate(FromStockTypeID = 'Litter',
			ToStockTypeID = 'Soil',
			StateAttributeTypeID = "",
			FlowTypeID = 'Litter Decay') %>%
		filter(FromStateClassID %in% ag_grass) %>%
		as.data.frame(.)
#calculate multipliers for litter CO2 emission as proportion of current year 
#litterfall + previous year litter 
ag_grasslitemiss <- flux %>% 
	filter(Flux == 'lit2co2') %>%
	mutate(turnover = Value/(rawlitc$Value + litter$lagValue)) %>%
	group_by(FromStratumID, FromStateClassID) %>%
	summarize(Multiplier = mean(turnover)) %>%
	mutate(FromStockTypeID = 'Litter',
		ToStockTypeID = 'Atmosphere',
		StateAttributeTypeID = "",
		FlowTypeID = 'Emission (litter)') %>%
	filter(FromStateClassID %in% ag_grass) %>%
	as.data.frame(.)

#row bind multipliers for Ag and Grass cover classses
multi_aggrass <- bind_rows(ag_grasslitdecay, ag_grasslitemiss)

			#### AG & WOODY CROP COVER CLASSES ####
#make separate R object for Ag litterfall
agnpp <- filter(NPP, StateClassID == 'Agriculture:All')
##calculate multiplier for Ag littefall as proportion of Ag NPP
aglitterfall <- rawlitc %>%
	filter(FromStateClassID == 'Agriculture:All') %>%
	mutate(turnover = Value/agnpp$Value) %>%
	group_by(FromStratumID, FromStateClassID) %>% 
	summarize(Multiplier = mean(turnover)) %>%
	mutate(FromStockTypeID = 'Living Biomass',
		ToStockTypeID = 'Litter',
		StateAttributeTypeID = "",
		FlowTypeID = 'Litterfall') %>%
	as.data.frame(.)
#calculate multiplier for grain production as proportion of Ag NPP
grain <- flux %>%
	filter(Flux == 'cgrain') %>%
	mutate(turnover = Value/agnpp$Value) %>%
	group_by(FromStratumID, FromStateClassID) %>% 
	summarize(Multiplier = mean(turnover)) %>%
	mutate(FromStockTypeID = 'Living Biomass',
		ToStockTypeID = 'Ag Products',
		StateAttributeTypeID = "",
		FlowTypeID = 'Harvest (Grain)') %>%
	as.data.frame(.)
#calculate multiplier for straw production as proportion of Ag NPP
straw <- flux %>%
	filter(Flux == 'strawc') %>%
	mutate(turnover = Value/agnpp$Value) %>%
	group_by(FromStratumID, FromStateClassID) %>% 
	summarize(Multiplier = mean(turnover)) %>%
	mutate(FromStockTypeID = 'Living Biomass',
		ToStockTypeID = 'Ag Products',
		StateAttributeTypeID = "",
		FlowTypeID = 'Harvest (Straw)') %>%
	as.data.frame(.)
#make separate R object for woody crop litterfall
wcnpp <- filter(NPP, StateClassID == 'WoodyCrop:All') 
#calculate multiplier for fruit production as proportion of woody crop NPP
fruit <- flux %>%
	filter(Flux == 'cfruit') %>%
	mutate(turnover = Value/wcnpp$Value) %>%
	group_by(FromStratumID, FromStateClassID) %>%
	summarize(Multiplier = mean(turnover)) %>%
	mutate(FromStockTypeID = 'Living Biomass',
		ToStockTypeID = 'Ag Products',
		StateAttributeTypeID = "",
		FlowTypeID = 'Harvest (Fruit)') %>%
	as.data.frame(.)
#row bind multipliers for ag & woody crop cover classes
multi_ag <- bind_rows(aglitterfall, grain, straw, fruit)

#row bind all multipliers into a single dataframe
tempFM <- bind_rows(multi_all, multi_woody, multi_aggrass, multi_ag) %>%
	arrange(FromStratumID, FromStateClassID) %>%
	select(FromStratumID, FromStateClassID, FromStockTypeID, ToStockTypeID, 
		StateAttributeTypeID, FlowTypeID, Multiplier)

#Estimate Dry WoodyCrop Flow Multipliers by multiplying each Mesic WoodyCrop 
#Multiplier by ratio of IBIS Dry to Mesic Shrub (covmax3) Flow values
DWC_FM <- tempFM %>%
	filter(FromStateClassID == 'WoodyCrop:All', FromStratumID == 'Mesic') %>%
	mutate(ratio = c(1.0, 1.0046, 1.01135, 0.1022, 0.9990, 0.9999,
		0.9993, 1.0782, 1.0767, 0.9926), 
		DryMultiplier = Multiplier*ratio, 
		MZ = 'Dry') %>%
	select(FromStratumID = MZ, FromStateClassID, FromStockTypeID, ToStockTypeID, 
		StateAttributeTypeID, FlowTypeID, Multiplier = DryMultiplier)
#Make Plantation Flow Multipliers by relabeling dry, mesic, and wet
#Forest multipliers
Plantat_FM <- tempFM %>%
	filter(FromStateClassID == 'Forest:All') %>%
	mutate(stateclass = 'Plantation:All') %>%
	select(FromStratumID, FromStateClassID = stateclass, FromStockTypeID, 
		ToStockTypeID, StateAttributeTypeID, FlowTypeID, Multiplier)

# bind rows of Flow Multipliers into single dataframe, add colums for ST-Sim, 
# and arrange by Moisture Zone and State Class 
FlowPathways <- bind_rows(tempFM, DWC_FM, Plantat_FM) %>%
	arrange(FromStratumID, FromStateClassID) 

#write FlowPathways dataframe to .csv file, save in HI_Model/InputData 
write.csv(FlowPathways, "FlowPathways.csv", row.names = FALSE, na = "")

