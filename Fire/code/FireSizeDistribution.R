## Fire size distribution
## Paul C. Selmants
## 2019-10-18

## Set working directory to "Fire" folder in HI_Model
## GitHub repository (https://github.com/selmants/HI_Model)

# load required packages into R
library(raster)
library(dplyr)

# read in Fire Perimeters shapefile
fires <- shapefile("./Fire/data/base/2019_1999_Hawaii_Fire_Perimeters.shp")

# calculate area of each polygon in km2
fires$area_km2 <- area(fires)/1000000

# make dataframe of year and fire size in km2
size <- data.frame("year" = fires$Year,
			"area_km2" = fires$area_km2) %>%
		arrange(year) 

# create function to calculate proportion of fires within a size range
propsize <- function(x,y){
	(sum(size$area_km2 > x & size$area_km2 <= y))/269
}

# calculate proportion of fires within 8 size ranges, from 0-105km2
p1k <- propsize(0,1)
p2k <- propsize(1,2)
p5k <- propsize(2,5)
p10k <- propsize(5,10)
p25k <- propsize(10,25)
p50k <- propsize(25,50)
p100k <- propsize(50,100)
p200k <- propsize(100,200)

# create Transition Size Distribution dataframe
sizedist <- data.frame(
	"TransitionGroupID" = "FIRE",
	"MaximumArea" = c(1,2,5,10,25,50,100,200),
	"RelativeAmount" = c(p1k,p2k,p5k,p10k,p25k,p50k,p100k,p200k))

# write sizedist dataframe to .csv file
write.csv(sizedist, "./data/processed/FireSizeDistribution.csv", row.names = FALSE)