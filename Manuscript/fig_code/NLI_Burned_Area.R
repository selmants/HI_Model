## Graph of Annual Area Burned, 
## Island of Hawaii, 1999-2022
## Paul C. Selmants
## R version 4.3.1
## 2019-10-26

library(sf)
library(tidyverse)

# read in statewide annual fire perimeters shapefile
fireshape <- st_read(
	"./data/base/fire/2022_1999_Hawaii_Fire_Perimeters_UH_NREM/fires_1999_2022.shp")

fire_df <- as.data.frame(fireshape) %>%
	select(YYYYMMDD, Island, Year, area_ha)

# sum area burned by Island in km2 by year
fire_annual <- fire_df %>%
	group_by(Island, Year) %>%
	summarize(area_km2 = sum(area_ha)*0.01) %>%
	ungroup(.)

# convert Year column from character to integer
fire_annual$Year <- as.integer(fire_annual$Year)

# zero dataframe for 2021 
zero21 <- data.frame(Island = "Hawaii",
	Year = 2001,
	area_km2 = 0.0)

# add 2021 to Hawaii Island area burned dataframe
areaburn <- bind_rows(fire_annual, zero21)

# graph of annual area burned on Hawaii Island (1999-2022)
firefig <- filter(areaburn, Island == "Hawaii") %>%
	ggplot(., aes(Year, area_km2)) +
	geom_line(color = "#67000D", linewidth = 0.6) +
	geom_point(color = "#67000D", size = 1.5, shape = 25, fill = "#67000D") +
	labs(x = "Year",
		y = expression(Area~burned~(km^{2}))) +
	scale_x_continuous(limits = c(1998, 2023), breaks = seq(1998, 2023, 4)) +
		scale_y_continuous(limits = c(0, 200), breaks = seq(0, 200, 25)) +
		geom_hline(aes(yintercept=18.14476), linetype="dashed", size = 0.35) +
		theme_bw() +
		theme(panel.grid.minor=element_blank(),
		axis.title = element_text(size = 10),
		axis.text = element_text(size = 8))

