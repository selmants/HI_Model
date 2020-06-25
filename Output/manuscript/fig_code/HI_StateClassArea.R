## Processing HI LUCAS model output:
## summarizing area of different  
## State Classes over time
## by land use scenario
## Paul C. Selmants
## 2020-04-22

# load required R packages
library(dplyr)
library(ggplot2)
library(cowplot)
library(stringr)
library(tidyr) 

# read State Class model output data into R and summarize 
# statewide by land use scenario
sc <- read.csv("StateClassArea.csv", stringsAsFactors = FALSE) %>%
	mutate(StateClassID = str_remove(.$StateClassID, ":All")) %>%
	group_by(Scenario, Timestep, Iteration, StateClassID) %>%
	summarize(area = sum(Area)) %>%
	separate(Scenario, c("Climate", "LandUse"), sep = 5) %>%
	group_by(LandUse, Timestep, StateClassID) %>%
	summarize(area_mean = mean(area), 
		area_min = min(area), 
		area_max = max(area)) %>%
	data.frame() 

#Make Land Use color palette
lu_pal <- c("#A6761D","#1B9E77")

# ggplot faceted land cover change graph
scfig <- sc %>%
filter(StateClassID %in% c("Agriculture",
	"Developed", "Forest", "Grassland", "Shrubland")) %>% 
	{ggplot(., aes(x = Timestep, y = area_mean, color = LandUse)) +
	facet_wrap(~ StateClassID, scales = "free_y") +
	geom_ribbon(aes(ymin = area_min, ymax = area_max, fill = LandUse),
		alpha = 0.4, color = NA) +
	geom_line(size = 0.5) +
	scale_fill_manual(values = lu_pal, labels=c("High", "Low"),
			name = "Land use\nscenario") +
	scale_colour_manual(values = lu_pal, labels=c("High", "Low"), 
			name = "Land use\nscenario") +
	scale_x_continuous(limits = c(2000,2100), breaks = seq(2000,2100,20)) +
	scale_y_continuous(breaks = scales::pretty_breaks(5), limits = c(NA, NA)) +
	labs(x = "Year", 
		 y = expression(Land~area~(km^{2})), 
		color = "Land Use\nScenario",
		fill = "Land Use\nScenario") +
	theme_bw() +
	theme(panel.grid.minor = element_blank(), 
		legend.position = c(0.85,0.25),
		legend.text = element_text(size=10))}

# save figure to .png file 
ggsave("figS5_StateClassArea.png", height = 4, width = 7, dpi = 300)
	
