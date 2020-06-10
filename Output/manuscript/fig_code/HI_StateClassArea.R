## Processing HI LUCAS model output:
## summarizing State Class areas over time
## by land use scenario
## Paul C. Selmants
## 2020-04-22

# load required R packages
library(dplyr)
library(ggplot2)
library(cowplot)
library(stringr)
library(tidyr) 

# read State Class model output data into R 
sc <- read.csv("StateClassArea.csv", stringsAsFactors = FALSE) %>%
	mutate(StateClassID = str_remove(.$StateClassID, ":All"))

# summarize State Class Area statewide by Land Use Scenario
StateClass <- sc %>%
	group_by(Scenario, Timestep, Iteration, StateClassID) %>%
	summarize(area = sum(Area)) %>%
	separate(Scenario, c("Climate", "LandUse"), sep = 5) %>%
	group_by(LandUse, Timestep, StateClassID) %>%
	summarize(area_mean = mean(area), 
		area_min = min(area), 
		area_max = max(area)) %>%
	data.frame() 

# ggplot faceted land cover change graph
scfig <- StateClass %>%
filter(StateClassID %in% c("Agriculture",
	"Developed", "Forest", "Grassland", "Shrubland")) %>% 
	{ggplot(., aes(x = Timestep, y = area_mean, color = LandUse)) +
	facet_wrap(~ StateClassID, scales = "free_y") +
	geom_ribbon(aes(ymin = area_min, ymax = area_max, fill = LandUse),
		alpha = 0.4, color = NA) +
	geom_line(size = 0.75) +
	scale_x_continuous(limits = c(2000,2100), breaks = seq(2000,2100,20)) +
	scale_y_continuous(breaks = scales::pretty_breaks(5), limits = c(NA, NA)) +
	labs(x = "Year", 
		 y = expression(Land~area~(km^{2})), 
		color = "Land Use\nScenario",
		fill = "Land Use\nScenario") +
	theme_bw() +
	theme(panel.grid.minor = element_blank(), 
		legend.position = c(0.825,0.25),
		legend.text = element_text(size=10))}
	

# write function to create ggplot graphs of land cover change
fig <- function(a,b,c) {
	ggplot(filter(StateClass, StateClassID == a), 
	aes(x = Timestep, y = area_mean, color = LandUse)) +
	geom_ribbon(aes(ymin = area_min, ymax = area_max, fill=LandUse),
		alpha = 0.4, color = NA) +
	geom_line(size = 0.75) +
	scale_x_continuous(limits = c(2010,2100), breaks = seq(2000,2100, 20)) +
	scale_y_continuous(limits = c(b,c), breaks = seq(b,c,200)) +
	theme_bw() +
	labs(x = "Year", 
		 y = expression(Land~area~(km^{2})), 
		color = "Land Use\nScenario",
		fill = "Land Use\nScenario") +
	theme(panel.grid.minor = element_blank(),
		legend.position = "Null") +
	ggtitle(a)	
}

# make land cover change graphs 
urbfig <- fig("Developed", 500, 1500)
forfig <- fig("Forest", 5000, 6000)
agfig <- fig("Agriculture", 0, 1000)
grassfig <- fig("Grassland", 2500, 3500) 
shrubfig <- fig("Shrubland", 2000, 3000)

# make multi-panel land cover change figure
lc_fourpanel <- plot_grid(urbfig,forfig, agfig, grassfig, 
		ncol = 2, scale = 1)
	
