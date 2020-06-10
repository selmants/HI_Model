## Graph of Annual Area Burned, 
## State of Hawaii, 1999-2019
## Paul C. Selmants
## R version 3.6.1
## 2019-10-26

# Set working directory to "Fire" folder in HI_Model
# GitHub repository (https://github.com/selmants/HI_Model)

## This R script is dependent on output from Fire_probabilities.R script ## 

# load ggplot2 version 3.2.0
library(ggplot2)
# load dplyr version 0.8.3
library(dplyr)

# read statewide annual burned area data into R
data <- read.csv('AnnualFire.csv', stringsAsFactors = FALSE) 

# sum burned area statewide by year
statewide <- data %>%
	group_by(year) %>%
	summarize(burnarea_km2 = sum(burnarea_km2)) %>%
	as.data.frame()

# make line graph of annual area burned
p <- ggplot(statewide, aes(year, burnarea_km2)) +
	geom_line(color = "#D95F02", size = 0.5) +
	geom_point(color = "#D95F02", size = 2, shape = 25, fill = "#D95F02") +
	xlab("Year") +
	ylab(expression(Area~burned~(km^{2}))) +
	scale_x_continuous(limits = c(1999, 2019), breaks = seq(2000, 2018, 2)) +
	scale_y_continuous(limits = c(0, 175), breaks = seq(0, 175, 25)) +
	geom_hline(aes(yintercept=28.75), linetype="dashed", size = 0.3) +
	theme(panel.grid.minor=element_blank()) +
	theme(axis.title.x = element_text(size = 16),
          axis.text.x  = element_text(size = 12)) +
	theme(axis.title.y = element_text(size = 16),
          axis.text.y  = element_text(size = 12))

ggsave("./data/processed/HI_AnnualAreaBurned.png", width = 7, height = 3.5)