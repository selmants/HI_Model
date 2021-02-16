## Annual fire emissions estimates, 
## State of Hawaii, 2011-2100
## Paul C. Selmants
## R version 3.6.1
## 2021-02-01

# load packages
library(dplyr)
library(tidyr)
library(ggplot2)
library(cowplot)

## Download LUCAS model output data from USGS ScienceBase: 
## https://doi.org/10.5066/P9AWLFKZ

# read HI LUCAS Transition/Harvest C Loss data into R and 
# filter on fire emissions, summing by Island, Iteration, and Scenario
fire <- read.csv("TransitHarvFlows.csv", stringsAsFactors = FALSE) %>%
	filter(TransitionGroupID == "FIRE", 
		grepl("Emission", FlowGroupID)) %>%
		group_by(Scenario, Timestep, Iteration, FromSecondaryStratumID) %>%
		summarize(fire_emiss = sum(Amount)) %>%
		as.data.frame() %>%
		select(Scenario, Timestep, Iteration, Island = FromSecondaryStratumID,
			fire_emiss)

