## Graph of Annual Area Burned, 
## State of Hawaii, 1999-2019
## Paul C. Selmants
## R version 3.6.1
## 2019-10-26

# GitHub repository (https://github.com/selmants/HI_Model)

## This R script is dependent on output from Fire_probabilities.R script ## 

# load required R packages
library(ggplot2)
library(dplyr)
library(cowplot)


# read statewide annual burned area data into R
data <- read.csv('./Fire/data/processed/AnnualFire.csv', stringsAsFactors = FALSE) 

# line graph of annual area burned statewide
stateplot <- data %>%
	group_by(year) %>%
	summarize(burnarea_km2 = sum(burnarea_km2)) %>%
	as.data.frame()%>%
		{ggplot(., aes(year, burnarea_km2)) +
		geom_line(color = "#67000D", size = 0.6) +
		geom_point(color = "#67000D", size = 1.5, shape = 25, fill = "#67000D") +
		labs(x = "Year",
		y = expression(Area~burned~(km^{2}))) +
		scale_x_continuous(limits = c(1998, 2019), breaks = seq(1998, 2018, 4)) +
		scale_y_continuous(limits = c(0, 175), breaks = seq(0, 175, 25)) +
		geom_hline(aes(yintercept=28.75), linetype="dashed", size = 0.35) +
		theme_bw() +
		theme(panel.grid.minor=element_blank(),
		axis.title = element_text(size = 10),
		axis.text = element_text(size = 8))}

# facet plot of annual area burned by island (Hawai, Maui, Oahu, Kauai)
islandplot <- data %>%
	filter(island %in% c("Hawaii", "Maui", "Oahu", "Kauai")) %>%
	{ggplot(., aes(year, burnarea_km2)) +
		facet_wrap(~ island) +
		geom_line(color = "#67000D", size = 0.35) +
		geom_point(color = "#67000D", size = 0.45, shape = 25, fill = "#67000D") +
		labs(x = "Year",
			y = expression(Area~burned~(km^{2}))) +
		scale_x_continuous(limits = c(1998, 2019), breaks = seq(1998, 2018, 5)) +
		scale_y_continuous(limits = c(0, 138.875), breaks = seq(0, 125, 25)) +
		theme_bw() +
		theme(panel.grid.minor = element_blank(),
			axis.text = element_text(size = 7),
			axis.title = element_text(size = 9.5))}
				
# Create two-panel fire figure
firepanel <- plot_grid(stateplot, islandplot, 
	labels = c("A", "B"), rel_widths = c(1.15, 1))

# save as .png file 
ggsave("./Output/manuscript/fig_images/FigS4_AnnualAreaBurned.png",
 width = 7.5, height = 3)
