# CFE carbon balance 
# Hawaii LUCAS Model 
# Paul C. Selmants
# 2020-04-14

# load required R packages
library(dplyr)
library(ggplot2)
library(cowplot)

## Download LUCAS model output data from USGS ScienceBase: 
## https://doi.org/10.5066/P9AWLFKZ

# read LUCAS CFE C stocks from RCP85High output data into R
CFE_Cstocks <- read.csv("./HI_Cstocks/Cstocks_CFE.csv", stringsAsFactors = FALSE) %>%
	select(-Scenario)
# read in LUCAS C stocks by scenario and filter on RCP85High scenario 
Cstocks <- read.csv("./HI_Cstocks/Cstocks_Island.csv", stringsAsFactors = FALSE) %>%
	filter(Scenario == "RCP85High") %>%
	mutate(CFE_rate = 0) %>%
	select(CFE_rate, Timestep:Amount)

# read in CFE base flow data from RCP85High Scenario
cfe_baseflow <- read.csv("./HI_Cflows/BaseFlows_CFE.csv", stringsAsFactors = FALSE) %>%
	select(-Scenario)
# read in CFE C loss data (transition, fire, harvest)
cfe_closs <- read.csv("./HI_Cflows/TransitHarvFlows_CFE.csv", 
	stringsAsFactors = FALSE) %>%
	select(-Scenario)
# read in zero CFE base flow data 
baseflowzero <- read.csv("./HI_Cflows/BaseFlows.csv", stringsAsFactors = FALSE) %>%
	filter(Scenario == "RCP85High") %>%
	mutate(CFE_Rate = 0) %>%
	select(CFE_Rate, Timestep:Amount)
# read zero CFE non-Rh C Loss data 
closszero <- read.csv("./HI_Cflows/TransitHarvFlows.csv", stringsAsFactors = FALSE) %>%
	filter(Scenario == "RCP85High") %>%
	mutate(CFE_Rate = 0) %>%
	select(CFE_Rate, Timestep:Amount)

## Summarize C stock data and create TEC by CFE figure ## 

# combine CFE Cstock data together and summarize as Statewide TEC 
cfetec <- bind_rows(Cstocks, CFE_Cstocks) %>%
	filter(CFE_rate %in% c(0, 5, 10, 15)) %>%
	group_by(CFE_rate, Timestep, SecondaryStratumID, StockGroupID) %>%
	summarize(CstockTg = mean(Amount)/1000, 
		Cstock_min = min(Amount)/1000, 
		Cstock_max = max(Amount)/1000) %>%
	group_by(CFE_rate, Timestep) %>%
	summarize(C_Tg = sum(CstockTg), 
		Cmin = sum(Cstock_min),
		Cmax = sum(Cstock_max)) %>%
	data.frame() %>%
	mutate_at(vars(CFE_rate), as.factor)

# create color palette for scenarios
cpal <- c("#A9A9A9", "#74C476", "#238B45", "#00441B")

# graph Total Ecosystem C (in Tg) by CFE rate
cfeTECfig <- ggplot(cfetec, aes(Timestep, C_Tg, color = CFE_rate)) +
	geom_ribbon(aes(ymin = Cmin, ymax = Cmax, fill=CFE_rate),
		alpha = 0.4, color = NA) +
	geom_line(size = 0.8) +
	labs(x = "Year",
		 y = "Total Ecosystem Carbon (Tg)") +
	scale_x_continuous(limits = c(2010, 2100), breaks = seq(2010, 2100, 10)) +
	scale_y_continuous(limits = c(316, 430), breaks = seq(325, 425, 25)) +
	scale_color_manual(values = cpal, name = "Rate of\nCFE (%)") +
	scale_fill_manual(values = cpal, name = "Rate of\nCFE (%)") +	
	theme_bw() +
	theme(
        axis.title.x = element_text(size=12,
        	margin = margin(t=10, r=0, b=0, l=0)),
        axis.text.x  = element_text(size=10),
        axis.title.y = element_text(size=12, 
        	margin = margin(t=0, r=10, b=0, l=0)),
        axis.text.y = element_text(size=10),
        legend.title = element_text(size = 12), 
        legend.text = element_text(size = 10),
        legend.position = c(0.15, 0.73),
		panel.grid.minor = element_blank())		 

## Summarize CFE NBP and create Mean NBP by CFE figure ## 

# combine base flow data
cfeflow <- bind_rows(baseflowzero, cfe_baseflow) 
# combine C loss data 
cfeCloss <- bind_rows(closszero, cfe_closs)

# calculate statewide NPP
NPP <- cfeflow %>%
	filter(FlowGroupID == "Growth [Type]") %>%
	group_by(CFE_Rate, Timestep, Iteration) %>%
	summarize(NPP = sum(Amount)/1000) %>%
	as.data.frame()	
# calculate statewide baseline Rh 
Rh <- cfeflow %>%
	filter(FlowGroupID %in% c("Emission (litter) [Type]",
		"Emission (soil) [Type]")) %>% 
	group_by(CFE_Rate, Timestep, Iteration) %>%
	summarize(Rh = sum(Amount)/1000) %>%
	as.data.frame()
# calculate statewide C loss by leaching/erosion
Leach <- cfeflow %>%
	filter(FlowGroupID == "Leaching [Type]") %>%
	group_by(CFE_Rate, Timestep, Iteration) %>%
	summarize(Leach = sum(Amount)/1000) %>%
	as.data.frame()
# calculate disturbance C losses (∆LULC, fire, ag harvest)
LUCloss <- cfeCloss %>%
	group_by(CFE_Rate, Timestep, Iteration) %>%
	summarize(LUCloss = sum(Amount)/1000) %>%
	as.data.frame()
	
#Join NPP and C loss dataframes together
cbal <- left_join(NPP, Rh) %>%
	left_join(., Leach) %>%
	left_join(., LUCloss) %>%
	group_by(CFE_Rate, Timestep, Iteration) %>%
	summarize(NEP = NPP - Rh, 
		NBP = NEP - Leach - LUCloss) %>%
	data.frame() 

#calculate mean NBP and sd across simulation period (2010-2100)
nbp <- cbal %>%
	group_by(CFE_Rate) %>%
	summarize(NBPmean = mean(NBP), 
		NBPsd = sd(NBP)) %>%
	data.frame()

#create CFE mean NBP fig
cfeNBPfig <- ggplot(nbp, aes(CFE_Rate, NBPmean)) +
	geom_point(size = 3, color = "#006D2C") +
	geom_errorbar(aes(ymin = NBPmean-NBPsd, ymax = NBPmean+NBPsd), 
		size = 0.75, width = 0, color = "#006D2C") +
	geom_hline(aes(yintercept=0), linetype = "dashed") +
	scale_x_continuous(limits = c(-0.02, 15.2), breaks = seq(0, 15, 2.5)) +
	scale_y_continuous(limits = c(-0.25, 1.5), breaks = seq(-0.25, 1.5, 0.25)) +
	ylab(expression(Net~biome~productivity~(Tg~C~y^{-1}))) + 
	xlab(expression(Rate~of~CO[2]~fertilization~effect~('%'))) +
	theme_bw() +
	theme(axis.title.x = element_text(size = 12),
        axis.text.x  = element_text(size=10),
        axis.title.y = element_text(size = 12),
        axis.text.y = element_text(size=10),
        panel.grid.minor = element_blank())

# Create two-panel CFE figure
cfepanel <- plot_grid(cfeTECfig, cfeNBPfig, 
	labels = c("(a)", "(b)"), rel_widths = c(1.6, 1))

ggsave("fig6_cfe.png", cfepanel, width = 9, height = 3.75, dpi = 400)

