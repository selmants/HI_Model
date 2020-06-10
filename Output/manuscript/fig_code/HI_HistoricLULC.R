# Historic rates of land cover change
# for Hawaii LUCAS model
# Paul C. Selmants
# 2020-03-10

# load packages
library(dplyr)
library(ggplot2)

# read in historic land cover change data
ccap <- read.csv("lulc_historic.csv", stringsAsFactors = FALSE) %>%
	group_by(LC_change) %>%
	summarize(median = sum(mid), 
		min = sum(minimum), 
		max = sum(maximum)) %>%
	as.data.frame()

urbagex <- ccap %>%
	filter(LC_change != "Ag contraction")
# make historic dLULC graph
urbagexfig <- ggplot(urbagex, aes(LC_change, median)) +
	geom_point(size = 4, shape = 15) +
	geom_errorbar(aes(ymin = min, ymax = max), width = 0) +
	scale_y_continuous(limits = c(0, 10), breaks = seq(0,10,1)) +
	ylab(expression(Annual~rate~(km^2~y^{-1}))) +
	theme(panel.grid.minor = element_blank(),
		axis.title.y = element_blank(), 
		axis.text.y = element_text(size = 12), 
		axis.title.x = element_text(size = 14),
		axis.text.x = element_text(size=12))+
	coord_flip() 

ggsave("histLULC.png", width = 9, height = 3)	