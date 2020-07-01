## Processing HI LUCAS model output:
## summarizing Flows to estimate annual
## and cumulative net biome productivity (NBP)
## Paul C. Selmants
## 2020-05-20

# load packages
library(dplyr)
library(tidyr)
library(ggplot2)
library(cowplot)

## Download LUCAS model output data from USGS ScienceBase: 
## https://doi.org/10.5066/P9AWLFKZ

# read HI LUCAS baseline C flow data into R
baseflow <- read.csv("BaseFlows.csv", stringsAsFactors = FALSE)
# read HI LUCAS Transition/Harvest C Loss data into R
transflow <- read.csv("TransitHarvFlows.csv", stringsAsFactors = FALSE)

# calculate statewide NPP
NPP <- baseflow %>%
	filter(FlowGroupID == "Growth [Type]") %>%
	group_by(Scenario, Timestep, Iteration) %>%
	summarize(NPP = sum(Amount)) %>%
	as.data.frame()	
# calculate statewide baseline Rh 
Rh <- baseflow %>%
	filter(FlowGroupID %in% c("Emission (litter) [Type]",
		"Emission (soil) [Type]")) %>% 
	group_by(Scenario, Timestep, Iteration) %>%
	summarize(Rh = sum(Amount)) %>%
	as.data.frame()
# calculate statewide C loss by leaching/erosion
Leach <- baseflow %>%
	filter(FlowGroupID == "Leaching [Type]") %>%
	group_by(Scenario, Timestep, Iteration) %>%
	summarize(Leach = sum(Amount)) %>%
	as.data.frame()
# calculate disturbance C losses (∆LULC, fire, ag harvest)
LUCloss <- transflow %>%
	group_by(Scenario, Timestep, Iteration) %>%
	summarize(LUCloss = sum(Amount)) %>%
	as.data.frame()

# join NPP and C loss dataframes together
cflowjoin <- left_join(NPP, Rh) %>%
	left_join(., Leach) %>%
	left_join(., LUCloss) 

# convert c flow data to long format, separate Scenario column into 
# RCP and LandUse, summarize flow data by mean, min, and max in Tg 
cflows <- cflowjoin %>% 
	pivot_longer(cols = NPP:LUCloss, 
		names_to = "flowtype",
		values_to = "Amount") %>%
	separate(Scenario, c("RCP", "LandUse"), sep = 5) %>%
	group_by(RCP, LandUse, Timestep, flowtype) %>%
	summarize(flowmean = mean(Amount)/1000,
		flowmin = min(Amount)/1000,
		flowmax = max(Amount)/1000)

# convert flowtype column in cflows to factor
cflows$flowtype <- factor(cflows$flowtype, levels = c("NPP", "Rh", "LUCloss", "Leach"))

#Make LU color palette
lu_pal <- c("#A6761D","#1B9E77")

# New facet label names for RCPs
rcp.labs <- c("RCP 4.5", "RCP 8.5")
names(rcp.labs) <- c("RCP45", "RCP85")
# New facet label names for flow types
flow.labs <- c("NPP", "Rh", "Land use")
names(flow.labs) <- c("NPP", "Rh", "LUCloss")

# Graph of NPP and Rh for all four scenarios
cflowfig <- cflows %>%
	filter(flowtype %in% c("NPP", "Rh", "LUCloss")) %>%
	{ggplot(., aes(Timestep, flowmean, color = LandUse)) +
		facet_grid(rows = vars(flowtype), cols = vars(RCP), scales = "free_y",
			labeller = labeller(RCP = rcp.labs, flowtype = flow.labs)) +
		geom_line(size = 0.25) +
		geom_ribbon(aes(ymin = flowmin, ymax = flowmax, fill=LandUse),
			alpha = 0.4, color = NA) +
		scale_fill_manual(values = lu_pal, labels=c("High", "Low"),
			name = "Land use\nscenario") +
		scale_colour_manual(values = lu_pal, labels=c("High", "Low"), 
			name = "Land use\nscenario") +
		labs(x = "Year",
		 y = expression(Carbon~flux~(Tg~y^{-1}))) +
	scale_x_continuous(limits = c(2010, 2100), breaks = seq(2000, 2100, 20)) +
	scale_y_continuous(breaks = scales::pretty_breaks(4)) +
		theme_bw() +
		theme(panel.grid.minor = element_blank())}

# save cflowfig 
ggsave("../fig_images/fig3_Cflows.png", width = 5, height = 5)

# calculate NEP and NBP from C flows
cbal <- cflowjoin %>%
	group_by(Scenario, Timestep, Iteration) %>%
	summarize(NEP = NPP - Rh, 
		NBP = NEP - Leach - LUCloss) 

# Function to filter cbal dataframe by Land Use scenario 
# and summarize data by mean, min, and max in Tg
nbpfun <- function(x,y) {
	cbal %>%
	filter(Scenario %in% c(x, y)) %>%
	group_by(Scenario, Timestep) %>%
	summarize(NBP_mean = mean(NBP)/1000, 
		NBP_min = min(NBP)/1000,
		NBP_max = max(NBP)/1000) %>%
	as.data.frame()
}

#Make RCP color palette
rcp_pal <- c("#00BFC4", "#F8766D")

# Graph of Statewide NECB for Low Land Use Change scenarios 
lowfig <- nbpfun("RCP45Low", "RCP85Low") %>%
	{ggplot(., aes(Timestep, NBP_mean, color = Scenario)) +
		geom_ribbon(aes(ymin = NBP_min, ymax = NBP_max, fill = Scenario),
	 		alpha = 0.4, color = NA) +
		geom_line(size = 0.75) +
		geom_hline(aes(yintercept=0), linetype="dashed") +
		labs(y = expression(Net~biome~productivity~(Tg~C~y^{-1}))) + 
		scale_x_continuous(limits = c(2010, 2100), breaks = seq(2010, 2100, 10)) +
		scale_y_continuous(limits = c(-0.75, 1), breaks = seq(-0.75, 1, 0.25)) +
		scale_fill_manual(values = rcp_pal, labels=c("RCP 4.5", "RCP 8.5")) +
		scale_colour_manual(values = rcp_pal, labels=c("RCP 4.5", "RCP 8.5")) +
		guides(fill=guide_legend(title=NULL)) +
		guides(color=guide_legend(title=NULL)) +
		theme_bw() +
		theme(
        	axis.title.x = element_blank(),
        	axis.text.x  = element_text(size=10),
        	axis.title.y = element_blank(),
        	axis.text.y = element_text(size=10),  
        	legend.text = element_text(size = 10),
        	legend.position = c(0.15, 0.24), 
			panel.grid.minor = element_blank()) +
		geom_text(x = 2080.5, y = 0.88, size = 4, color = "black",
			label = "(a) Low land use change")}
		
# Graph of Statewide NECB for High Land Use Change scenarios
highfig <- nbpfun("RCP45High", "RCP85High") %>%
	{ggplot(., aes(Timestep, NBP_mean, color = Scenario)) +
		geom_ribbon(aes(ymin = NBP_min, ymax = NBP_max, fill = Scenario),
			alpha = 0.4, color = NA) +
		geom_line(size = 0.75) +
		geom_hline(aes(yintercept=0), linetype="dashed") +
		labs(x = "Year", 
			y = expression(Net~biome~productivity~(Tg~C~y^{-1}))) +
		scale_x_continuous(limits = c(2010, 2100), breaks = seq(2010, 2100, 10)) +
		scale_y_continuous(limits = c(-0.75, 1), breaks = seq(-0.75, 1, 0.25)) +
		scale_fill_manual(values = rcp_pal, labels=c("RCP 4.5", "RCP 8.5")) +
		scale_colour_manual(values = rcp_pal, labels=c("RCP 4.5", "RCP 8.5")) +
		theme_bw() +
		theme(
        	axis.title.x = element_text(size = 12),
        	axis.text.x  = element_text(size=10),
        	axis.title.y = element_blank(),
        	axis.text.y = element_text(size=10),  
        	legend.position = "none",
			panel.grid.minor = element_blank()) +
		geom_text(x = 2081, y = 0.88, size = 4, color = "black", 
			label = "(b) High land use change")}

# make two panel NBP figure
NBP_panels <- plot_grid(lowfig, highfig, ncol = 1, scale = 0.93) +
draw_label(expression(Net~biome~productivity~(Tg~C~y^{-1})), 
	x=0, y=0.55, vjust= 1.1, angle=90, size = 12) 

# save two panel NBP fig as .png file
ggsave("fig4_NBPpanels.png", width = 7, height = 6.5)

# calculate cumulative ∆NBP when swticking land use & climate scenarios
dcumNBP <- cbal %>%
	group_by(Scenario, Iteration) %>%
	summarize(cumNBP = sum(NBP)/1000) %>%
	pivot_wider(names_from = Scenario, values_from = cumNBP) %>%
	mutate(LU45 = RCP45Low - RCP45High, 
		LU85 = RCP85Low - RCP85High, 
		emissLow = RCP45Low - RCP85Low, 
		emissHigh = RCP45High - RCP85High) %>%
	select(Iteration, LU45, LU85, emissLow, emissHigh) %>%
	pivot_longer(-Iteration, names_to = "Scenario", values_to = "dNBP") %>%
	select(Scenario, Iteration, dNBP) %>%
	arrange(Scenario)

# boxplots of cumulative C gained by switching from high to low 
# land use change scenarios
ludC <- dcumNBP %>%
	filter(Scenario %in% c("LU45", "LU85")) %>%
	{ggplot(., aes(Scenario, dNBP, fill = Scenario)) +
		geom_boxplot() +
		scale_fill_manual(values = rcp_pal) +
		scale_x_discrete(labels = c("RCP 4.5", "RCP 8.5")) +
		scale_y_continuous(limits = c(9,18), breaks = seq(9,18,1)) +
		labs(y = "Cumulative change in NBP (Tg C)",
			title = "Avoided conversion (high to low land use)") +
		theme_bw() +
		theme(axis.title.x = element_blank(),
			axis.title.y = element_blank(),
			panel.grid.minor = element_blank(), 
			legend.position = "none") +
		coord_flip()
		}

# boxplots of cumulative C gained by switching from high to low 
# global emissions scenarios
climdC <- dcumNBP %>%
	filter(Scenario %in% c("emissLow", "emissHigh")) %>%
	{ggplot(., aes(Scenario, dNBP, fill = Scenario)) +
		geom_boxplot() +
		scale_fill_manual(values = lu_pal) +
		scale_x_discrete(limits = c("emissLow", "emissHigh"), 
			labels = c("Low Land Use", "High Land Use")) +
		scale_y_continuous(limits = c(19,28), breaks = seq(19,28,1)) +
		labs(y = "Cumulative change in NBP (Tg C)",
			title = "Emissions reduction (RCP 8.5 to RCP 4.5)") +
		theme_bw() +
		theme(axis.title.y = element_blank(),
			panel.grid.minor = element_blank(),
			legend.position = "none") +
		coord_flip()
		}

# make two-panel cumulative dNBP figure
cumdNBP_panels <- plot_grid(ludC, climdC, ncol = 1, align = "v")

# save two panel cumulative dNBP figure
ggsave("fig5_cumdNBP.png", width = 6, height = 4)

