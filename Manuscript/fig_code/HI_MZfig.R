# Use ggplot to visualize
# Moisture Zones in the 
# Hawaiian Islands
# Paul C. Selmants
# 2020-06-23

# load required packages
library(raster)
library(rgdal)
library(dplyr)
library(ggplot2)

## Data in GitHub repository: https://github.com/selmants/HI_Model

# read Moisture Zone TIFF into R, where: 
# 1 = Dry 		
# 2 = Mesic 		
# 3 = Wet	
MZ <- raster('./InputData/spatial_data/initial_conditions/MZ_3_250m.tif')

# read annual MAT Tiff into R
MAT <- raster("./Climate/MAT_annual.tif")

# re-sample MZ projection to Lat. Long. using MAT
MZdeg <- projectRaster(MZ, MAT, method = 'ngb')
# convert lc raster to points dataframe for usage in ggplot 
MZ_df <- rasterToPoints(MZdeg) %>%
	data.frame(.) %>%
	select(x, y, layer = MZ_3_250m) 
# convert layer column to factor 
MZ_df$layer <- as.factor(MZ_df$layer)
# Create custom palette for MZ classes
mzpal <- c("#FDBF6F", "#B2DF8A", "#1F78B4")

# create statewide Moisture Zone map for Hawaii
mzmap <- ggplot(data=MZ_df) +
	geom_tile(aes(x=x, y=y, fill=layer)) +
	xlab(expression("Longitude " ( degree*W))) +
	ylab(expression("Latitude " ( degree*N))) +
	scale_fill_manual("Moisture Zone", values = mzpal,
		 labels=c("Dry", "Mesic", "Wet")) +
	theme_bw() + 
	theme(axis.title.x = element_text(size=10),
        axis.text.x  = element_text(size=8),
        axis.title.y = element_text(size=10),
        axis.text.y = element_text(size=8),
    legend.position = c(0.188, 0.465),
    legend.justification = c("left", "top"),
    legend.title = element_text(size = 12),
    legend.text = element_text(size = 10),
    legend.background = element_rect(fill="white", 
    	linetype = "solid", size = 0.5, color = "white"))

# save Moisture Zone map as .png file 
ggsave("./Manuscript/fig_images/figS1_MZ.png", 
	height = 5, width = 7, dpi = 400)
