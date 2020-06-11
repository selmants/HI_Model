#Hawaii population and urban area projections
#Paul C. Selmants
#2019-08-21 (ISO-8601)

library(dplyr)
library(tidyr)

# set R working directory to highest level of HI_Model
# GitHub repository, https://github.com/selmants/HI_Model

# read in projected de facto population by county (includes residents & tourists)
# data from State of Hawaii Dept. of Business, Economic Development & Tourism
proj_pop <- read.csv('./Transitions/data/base/HI_proj_pop.csv', stringsAsFactors = FALSE)

#filter by Maui County and calculate population by island based on 2010
#population distribution and convert from wide to long format
MC <- proj_pop %>%
	filter(county == 'Maui') %>%
	mutate('Maui' = 0.93*population,
		"Lana'i" = 0.02*population,
		"Moloka'i" = 0.05*population,
		"Kaho'olawe" = 0) %>%
	select(year, "Kaho'olawe", "Lana'i", "Maui", "Moloka'i") %>%
	gather(island, population, "Kaho'olawe":"Moloka'i")
#round Maui County population values to nearest whole number
MC$population <- round(MC$population,0) 
	
#combine county projections with island-specific Maui county projections to
#get statewide population projections by island
HI_proj <- proj_pop %>%
	filter(county != "Maui") %>%
	mutate(island = rep(c("Hawai'i", "O'ahu", "Kaua'i"), each = 8)) %>%
	select(year, island, population) %>%
	bind_rows(MC) %>%
	arrange(island) 

#calculate 2010 population density using urban area (in km2) from
#2010 NOAA CCAP map (Impervious Surface)
Pd <- HI_proj %>%
	filter(year == 2010) %>%
	mutate(urban_area = c(123.13, 0, 39.49, 4.38, 62.37, 7.05, 223.39),
		Pd = population/urban_area) %>%
	select(island, Pd)
#convert 'NaN' to zero for Kaho'olawe Pd	
Pd[is.na(Pd)] <- 0 

#combine statewide population projections by island with population density
#values by island, then calculate annual rate of change in developed area by island
HI_ddev <- left_join(HI_proj, Pd, by = 'island') %>%
	group_by(island) %>%
	mutate(lagpop = lag(population),
		dpop = (population - lagpop)/5, 
		ddev_km2y = dpop/Pd,
		ddev_km2y = replace_na(ddev_km2y, 0)) %>%
	filter(year > 2010) %>%
	mutate(start_year = as.integer(c(2010, 2015, 2020, 2025, 2030, 2035, 2040))) %>%
	select(island, start_year, end_year = year, ddev_km2y) %>%
	data.frame(.) 

#Replace 2010-2015 urbanization rates with historical rates from NOAA CCAP data
HI_ddev$ddev_km2y[01] <- 1.9977 #Hawai'i Island
HI_ddev$ddev_km2y[15] <- 0.7879 #Kaua'i
HI_ddev$ddev_km2y[22] <- 0.0814 #Lana'i
HI_ddev$ddev_km2y[29] <- 1.1399 #Maui
HI_ddev$ddev_km2y[36] <- 0.1357 #Moloka'i
HI_ddev$ddev_km2y[43] <- 1.8844 #O'ahu

#write ddev_range to .csv file
write.csv(HI_ddev, "./Transitions/data/processed/HI_ddev.csv", row.names = FALSE)

