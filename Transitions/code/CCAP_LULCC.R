# HI CCAP land cover change, 2001-2005
# Urbanization rate and Ag change rate
# Paul C. Selmants 
# 2019-08-19

#load packages into R
library(dplyr)
library(tidyr)

## Set R working directory to highest level of HI_Model
## GitHub repository, https://github.com/selmants/HI_Model

#make a list of NOAA CCAP 1992-2001 land cover change files
ccap01_list <- list.files(path = "./Transitions/data/base/", 
	pattern = "LCchange_1992")
#make a list of NOAA CCAP 2001-2005 land cover change files
ccap05_list <- list.files(path = "./Transitions/data/base/", 
	pattern = "LCchange_2001")
#make a list of NOAA CCAP 2001-2005 land cover change files
ccap10_list <- list.files(path = "./Transitions/data/base/", 
	pattern = "LCchange_2005") 

#read in ccap 1992-2001 files, bind together, filter out zero values
LC92_01 <- lapply(ccap01_list[c(1:4)], FUN = read.csv, header = TRUE,
 	stringsAsFactors = FALSE) %>%
	bind_rows() %>%
	mutate(County = c(rep(c("Hawai'i", "Kaua'i", "Maui"), each = 626),
		rep("O'ahu", 548))) %>%
	filter(Count > 0) %>%
	select(County, Count, Code_1992 = X1992_Code, Class_1992 = X1992_Class, 
		Code_2001 = X2001_Code, Class_2001 = X2001_Class, Class_Name)
#read in ccap 2001-2005 files, bind together, filter out zero values
LC01_05 <- lapply(ccap05_list[c(1:4)], FUN = read.csv, header = TRUE,
 	stringsAsFactors = FALSE) %>%
 	bind_rows() %>%
 	mutate(County = c(rep(c("Hawai'i", "Kaua'i"), each = 626), 
 		rep(c("Maui", "O'ahu"), each = 548))) %>%
 	filter(Count > 0) %>%
 	select(County, Count, Code_2001 = X2001_Code, Class_2001 = X2001_Class, 
 		Code_2005 = X2005_Code, Class_2005 = X2005_Class, Class_Name)
#read in ccap 2001-2005 files, bind together, rename column headers
LC05_10 <- lapply(ccap10_list, FUN=read.csv, header = TRUE,
	stringsAsFactors = FALSE) %>%
	bind_rows() %>%
	select(Island, CCAP_class = Full.C.CAP.Class.Scheme, Area_2005 = X2005,
		Loss, Gain, Area_2010 = X2010, NetChange = Net.Change, Area_2011 = X2011)

#urbanization rate in km2/y by island, 1992-2001
 urban2001 <- LC92_01 %>%
 	filter(Code_2001 %in% c(2:4) & !Code_1992 %in% c(2:4)) %>%
 	group_by(County) %>%
 	summarize(rate_km2y = (sum(Count)*0.0009)/9) %>%
 	mutate(LC_change = "Urbanization",
 		end_year = as.integer(2001),
 		Island = c("Hawai'i", "Kaua'i", "", "O'ahu")) %>%
 	data.frame(.) %>%
 	add_row(., County = "Maui", rate_km2y = .[3,2]*0.06, 
 		LC_change = "Urbanization", end_year = 2001, Island = "Lana'i") %>%
 	add_row(., County = "Maui", rate_km2y = .[3,2]*0.84, 
 		LC_change = "Urbanization", end_year = 2001, Island = "Maui") %>%
 	add_row(., County = "Maui", rate_km2y = .[3,2]*0.10, 
 		LC_change = "Urbanization", end_year = 2001, Island = "Moloka'i") %>%
 	filter(Island != "") %>%
 	arrange(Island) %>%
 	select(Island, rate_km2y, LC_change, end_year)
#Ag expansion rate in km2/y by island, 1992-2001
AgEx2001 <- LC92_01 %>%
	filter(Code_2001 == 6 & !Code_1992 %in% c(6)) %>%
	group_by(County) %>%
	summarize(rate_km2y = (sum(Count)*0.0009)/9) %>%
	mutate(LC_change = "Ag expansion",
 		end_year = as.integer(2001),
 		Island = c("Hawai'i", "Kaua'i", "", "O'ahu")) %>%
	data.frame(.) %>%
	add_row(., County = "Maui", rate_km2y = 0, 
 		LC_change = "Ag expansion", end_year = 2001, Island = "Lana'i") %>%
 	add_row(., County = "Maui", rate_km2y = .[3,2]*0.95, 
 		LC_change = "Ag expansion", end_year = 2001, Island = "Maui") %>%
 	add_row(., County = "Maui", rate_km2y = .[3,2]*0.05, 
 		LC_change = "Ag expansion", end_year = 2001, Island = "Moloka'i") %>%
 	filter(Island != "") %>%
 	arrange(Island) %>%
 	select(Island, rate_km2y, LC_change, end_year)
#Ag contraction rate in km2/y by island, 1992-2001
AgCon2001 <- LC92_01 %>%
	filter(Code_1992 == 6 & !Code_2001 %in% c(6)) %>%
	group_by(County) %>%
	summarize(rate_km2y = (sum(Count)*0.0009)/9) %>%
	mutate(LC_change = "Ag contraction",
 		end_year = as.integer(2001),
 		Island = c("Hawai'i", "Kaua'i", "", "O'ahu")) %>%
	data.frame(.) %>%
	add_row(., County = "Maui", rate_km2y = 0, 
 		LC_change = "Ag contraction", end_year = 2001, Island = "Lana'i") %>%
 	add_row(., County = "Maui", rate_km2y = .[3,2]*0.95, 
 		LC_change = "Ag contraction", end_year = 2001, Island = "Maui") %>%
 	add_row(., County = "Maui", rate_km2y = .[3,2]*0.05, 
 		LC_change = "Ag contraction", end_year = 2001, Island = "Moloka'i") %>%
 	filter(Island != "") %>%
 	arrange(Island) %>%
 	select(Island, rate_km2y, LC_change, end_year)
#combine urbanization, Ag expansion, and Ag contraction rates for 1992-2001
lulc1992_2001 <- bind_rows(urban2001, AgEx2001, AgCon2001)

#urbanization rate in km2/y by island, 2001-2005
 urban2005 <- LC01_05 %>%
 	filter(Code_2005 %in% c(2:4) & !Code_2001 %in% c(2:4)) %>%
 	group_by(County) %>%
 	summarize(rate_km2y = (sum(Count)*0.0009)/4) %>%
 	mutate(LC_change = "Urbanization",
 		end_year = as.integer(2005),
 		 Island = c("Hawai'i", "Kaua'i", "", "O'ahu")) %>%
 	data.frame(.) %>%
 	add_row(., County = "Maui", rate_km2y = .[3,2]*0.06, 
 		LC_change = "Urbanization", end_year = 2005, Island = "Lana'i") %>%
 	add_row(., County = "Maui", rate_km2y = .[3,2]*0.84, 
 		LC_change = "Urbanization", end_year = 2005, Island = "Maui") %>%
 	add_row(., County = "Maui", rate_km2y = .[3,2]*0.10, 
 		LC_change = "Urbanization", end_year = 2005, Island = "Moloka'i") %>%
 	filter(Island != "") %>%
 	arrange(Island) %>%
 	select(Island, rate_km2y, LC_change, end_year)
#Ag expansion rate in km2/y by island, 2001-2005
AgEx2005 <- LC01_05 %>%
	filter(Code_2005 == 6 & !Code_2001 %in% c(6)) %>%
	group_by(County) %>%
	summarize(rate_km2y = (sum(Count)*0.0009)/4) %>%
	mutate(LC_change = "Ag expansion",
		Island = c("Hawai'i", "Kaua'i", "O'ahu")) %>%
	data.frame(.) %>% 
	add_row(., County = "Maui", rate_km2y = 0.0, 
		LC_change = "Ag expansion", Island = "Lana'i") %>%
	add_row(., County = "Maui", rate_km2y = 0.0, 
		LC_change = "Ag expansion", Island = "Maui") %>%
	add_row(., County = "Maui", rate_km2y = 0.0, 
		LC_change = "Ag expansion", Island = "Moloka'i") %>%
	arrange(Island) %>%
 	mutate(end_year = as.integer(2005)) %>%
	select(Island, rate_km2y, LC_change, end_year)
#Ag contraction rate in km2/y by island, 2001-2005
AgCon2005 <- LC01_05 %>%
	filter(Code_2001 == 6 & !Code_2005 %in% c(6)) %>%
	group_by(County) %>%
	summarize(rate_km2y = (sum(Count)*0.0009)/4) %>%
	mutate(LC_change = "Ag contraction",
 		end_year = as.integer(2005), 
 		Island = c("Hawai'i", "Kaua'i", "", "O'ahu")) %>%
	data.frame(.) %>%
	add_row(., County = "Maui", rate_km2y = .[3,2]*0.06, 
 		LC_change = "Ag contraction", end_year = 2005, Island = "Lana'i") %>%
 	add_row(., County = "Maui", rate_km2y = .[3,2]*0.84, 
 		LC_change = "Ag contraction", end_year = 2005, Island = "Maui") %>%
 	add_row(., County = "Maui", rate_km2y = .[3,2]*0.10, 
 		LC_change = "Ag contraction", end_year = 2005, Island = "Moloka'i") %>%
 	filter(Island != "") %>%
 	arrange(Island) %>%
 	select(Island, rate_km2y, LC_change, end_year)
#combine urbanization, Ag expansion, and Ag contraction rates for 2001-2005
lulc2001_2005 <- bind_rows(urban2005, AgEx2005, AgCon2005)

#urbanization rate in km2/y by island, 2005-2010
urban2010 <- LC05_10 %>%
	filter(CCAP_class == "Impervious Surface") %>%
	mutate(timespan_year = as.integer(c(4,5,5,4,4,5)), 
		rate = (NetChange*2.59)/timespan_year) %>% 
	group_by(Island) %>%
	summarize(rate_km2y = sum(rate)) %>%
	mutate(LC_change = "Urbanization",
		end_year = as.integer(rep(2010, 6))) %>%
	select(Island, rate_km2y, LC_change, end_year) %>%
	data.frame(.)
#agricultural expansion rate in km2/y by island, 2005-2010
AgEx2010 <- LC05_10 %>%
	filter(CCAP_class == "Cultivated") %>%
	mutate(timespan_year = as.integer(c(4,5,5,4,4,5)), 
		rate = (Gain*2.59)/timespan_year) %>%
	group_by(Island) %>%
	summarize(rate_km2y = sum(rate)) %>%
	mutate(LC_change = "Ag expansion",
		end_year = as.integer(rep(2010, 6))) %>%
	select(Island, rate_km2y, LC_change, end_year) %>%
	data.frame(.) 
#agricultural contraction rate in Km2/y by island 2005-2010
AgCon2010 <- LC05_10 %>%
	filter(CCAP_class == "Cultivated") %>%
	mutate(timespan_year = as.integer(c(4,5,5,4,4,5)), 
		rate = -(Loss*2.59)/timespan_year) %>%
	group_by(Island) %>%
	summarize(rate_km2y = sum(rate)) %>%
	mutate(LC_change = "Ag contraction",
		end_year = as.integer(rep(2010, 6))) %>%
	select(Island, rate_km2y, LC_change, end_year) %>%
	data.frame(.) 
#combine urbanization, Ag expansion, and Ag contraction rates for 2005-2010
lulc2005_2010 <- bind_rows(urban2010, AgEx2010, AgCon2010) %>%
	mutate(rate = round(.$rate_km2y, 4)) %>%
	select(Island, rate_km2y = rate, LC_change, end_year)

#calculate min, mid, max values for annual rates of urbanization, 
#Ag expansion, and Ag contraction from 1992-2010
lulc_minmax <- bind_rows(lulc1992_2001, lulc2001_2005, lulc2005_2010) %>%
	group_by(Island, LC_change) %>%
	summarize(minimum = min(rate_km2y), 
		mid=median(rate_km2y),
		maximum = max(rate_km2y)) %>%
	data.frame(.) 

#write lulcminmax to .csv file
write.csv(lulc_minmax, "./Transitions/data/processed/lulc_historic.csv", 
	row.names = FALSE)
	


