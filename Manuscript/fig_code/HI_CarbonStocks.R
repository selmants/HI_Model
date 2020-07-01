## HI LUCAS model output:
## summarize & graph Statewide 
## Carbon stocks, fluxes, and balance
## Paul C. Selmants
## 2020-05-20

# load required R packages
library(dplyr)
library(ggplot2)
library(cowplot)

## Download LUCAS model output data from USGS ScienceBase: 
## https://doi.org/10.5066/P9AWLFKZ

# read Hawaii LUCAS model C stock output data into R
Cstocks <- read.csv("Cstocks_Island.csv", stringsAsFactors = FALSE) %>%
	group_by(Scenario, Timestep, SecondaryStratumID, StockGroupID) %>%
	summarize(CstockTg = mean(Amount)/1000,
		Cstock_min = min(Amount)/1000,
		Cstock_max = max(Amount)/1000) %>%
	data.frame()
# remove " [Type]" at trailing end of all stock types  
Cstocks$StockGroupID <- gsub('.{7}$', '', Cstocks$StockGroupID) 

# create function to summarize carbon stocks statewide
stocksum <- function(x) {
	 x %>%
	group_by(Scenario, Timestep) %>%
	summarize(C_Tg = sum(CstockTg), 
		Cmin = sum(Cstock_min),
		Cmax = sum(Cstock_max)) %>%
	select(Scenario, Timestep, C_Tg, Cmin, Cmax) %>%
	arrange(Scenario, Timestep) %>%
	as.data.frame() 
}

# summarize TEC and individual C stocks statewide
TEC <- stocksum(Cstocks)
# sum of standing and down dead wood
Deadwood <- Cstocks %>%
	filter(grepl("Deadwood", StockGroupID)) %>%
	stocksum(.)
# Living biomass
LB <- Cstocks %>%
	filter(StockGroupID == "Living Biomass") %>%
	stocksum(.)
# Litter
Litter <- Cstocks %>%
	filter(StockGroupID == "Litter") %>%
	stocksum(.)
# Soil organic carbon
SOC <- Cstocks %>%
	filter(StockGroupID == "Soil") %>%
	stocksum(.)

# merge individual C stocks into single dataframe
indCstocks <- bind_rows(Deadwood, LB, Litter, SOC) %>%
	mutate(StockGroupID = rep(c("Dead wood", "Live biomass", "Litter",
		"Soil organic matter"), each = 364)) %>%
	arrange(Scenario, Timestep) %>%
	select(Scenario, Timestep, StockGroupID, C_Tg, Cmin, Cmax)

# create color palette for scenarios
cpal <- c("#66A61E", "#D95F02", "#666666", "#E7298A")

# graph Total Ecosystem C (in Tg) for all four scenarios
TECfig <- ggplot(TEC, aes(Timestep, C_Tg, color = Scenario)) +
	geom_ribbon(aes(ymin = Cmin, ymax = Cmax, fill=Scenario),
		alpha = 0.4, color = NA) +
	geom_line(size = 0.6) +
	labs(x = "Year", 
		y = "Total ecosystem carbon (Tg)") +
	scale_x_continuous(limits = c(2010, 2100), breaks = seq(2010, 2100, 10)) +
	scale_y_continuous(limits = c(310, 375), breaks = seq(310, 370, 10)) +
	scale_color_manual(values = cpal, 
		name = "          Scenario",
		breaks = c("RCP45Low", "RCP45High", "RCP85Low", "RCP85High"),
		labels = c("RCP 4.5, Low Land Use", "RCP 4.5, High Land Use",   
			"RCP 8.5, Low Land Use", "RCP 8.5, High Land Use")) +
	scale_fill_manual(values = cpal,
		name = "          Scenario",
		breaks = c("RCP45Low", "RCP45High", "RCP85Low", "RCP85High"),
		labels = c("RCP 4.5, Low Land Use", "RCP 4.5, High Land Use",   
			"RCP 8.5, Low Land Use", "RCP 8.5, High Land Use")) +
	theme_bw() +
	theme(panel.grid.minor = element_blank(),
		axis.title.y = element_text( 
        	margin = margin(t=0, r=10, b=0, l=0)),
		legend.position = c(0.25, 0.75))
        
# graph individual C stocks for all four scenarios
stocksfig <- ggplot(indCstocks, aes(Timestep, C_Tg, color = Scenario)) +
	facet_wrap(~ StockGroupID, scales = "free_y") +
	geom_line(size = 0.25) +
	geom_ribbon(aes(ymin = Cmin, ymax = Cmax, fill=Scenario),
		alpha = 0.4, color = NA) +
	labs(x = "Year",
		 y = "Carbon (Tg)") +
	scale_x_continuous(limits = c(2010, 2100), breaks = seq(2000, 2100, 20)) +
	scale_y_continuous(breaks = scales::pretty_breaks(4)) +
	scale_color_manual(values = cpal, 
		breaks = c("RCP45Low", "RCP45High", "RCP85Low","RCP85High")) +
	scale_fill_manual(values = cpal,
		breaks = c("RCP45Low", "RCP45High", "RCP85Low", "RCP85High")) +
	theme_bw() +
	theme(panel.grid.minor = element_blank(),
		axis.title.y = element_text( 
        	margin = margin(t=0, r=8, b=0, l=0)),
		legend.position = "none")

# Create two-panel CFE figure
cstockpanel <- plot_grid(TECfig, stocksfig, 
	labels = c("(a)", "(b)"), rel_widths = c(1.2, 1))

# save C stocks fig as .png file
ggsave("fig2_Cstocks.png", cstockpanel, width = 9, height = 3.5)
