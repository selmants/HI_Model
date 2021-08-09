# Using ggplot2 to visualize
# Hawaiʻi land cover classes
# Paul C. Selmants
# 2020-02-24

# load required packages
library(raster)
library(rgdal)
library(dplyr)
library(ggplot2)

## Data in GitHub repository: https://github.com/selmants/HI_Model

# read State Class TIFF into R, where: 
# 1 = Water 		6 = Forest
# 2 = Urban 		7 = Grassland
# 3 = Plantation	8 = Agriculture
# 4 = WoodyCrop		9 = Wetland
# 5 = Barren	   10 = Shrubland
StateClass <- raster(
    './InputData/spatial_data/initial_conditions/StateClassNew_250m.tif')

# read annual MAT Tiff into R
MAT <- raster("./Climate/MAT_annual.tif")

# make reclassification matrix for simplified land cover raster
lc_rcl <- c(1, NA, 2, 2, 3, 3, 4, 4, 5, 5, 6, 6,
	7, 7, 8, 4, 9, NA, 10, 10) %>%
	matrix(., ncol = 2, byrow = TRUE)
# reclassify StateClass to create simplified land cover raster, where: 
# 2 = Urban 		 6 = Forest
# 3 = Plantation	 7 = Grassland
# 4 = Agriculture	10 = Shrubland
# 5 = Barren	
lc <- reclassify(StateClass, lc_rcl)
# re-sample lc projection to Lat. Long. using MAT
lcdeg <- projectRaster(lc, MAT, method = 'ngb')
# convert lc raster to points dataframe for usage in ggplot 
lc_df <- rasterToPoints(lcdeg) %>%
	data.frame(.) %>%
	select(x, y, layer = StateClassNew_250m) 
# convert layer column to factor 
lc_df$layer <- as.factor(lc_df$layer)
# Create custom colorblind palette for land cover classes
lcpal <- c("#000000", "#CC79A7", "#A6761D", "#969696", "#006D2C", 
	"#FFD92F", "#B2DF8A") 
# create statewide land cover map for Hawaii
lcmap <- ggplot(data=lc_df) +
	geom_tile(aes(x=x, y=y, fill=layer)) +
	xlab(expression("Longitude " ( degree*W))) +
	ylab(expression("Latitude " ( degree*N))) +
	scale_fill_manual("Land Cover", values = lcpal,
		 labels=c("Developed", "Tree Plantation", "Agriculture",
		 	"Bare Ground", "Forest", "Grassland", "Shrubland")) +
	theme_bw() + 
	theme(axis.title.x = element_text(size=10),
        axis.text.x  = element_text(size=8),
        axis.title.y = element_text(size=10),
        axis.text.y = element_text(size=8),
    legend.position = c(0.15, 0.6),
    legend.justification = c("left", "top"),
    legend.background = element_rect(fill="white", 
    	linetype = "solid", size = 0.5, color = "white")) +
    coord_fixed() +
    annotate(geom = "text", x= -155, y = 20.15, label = "Hawaiʻi Island", 
        size = 3.5) +
    annotate(geom = "text", x= -156.025, y = 20.95, label = "Maui",
        size = 3.5) +
    annotate(geom = "text", x= -157.025, y = 20.55, label = "Kahoʻolawe",
        size = 3.5) +
    annotate(geom = "text", x= -157.2, y = 20.8, label = "Lānaʻi",
        size = 3.5) +
    annotate(geom = "text", x= -156.78, y = 21.275, label = "Molokaʻi",
        size = 3.5) +
    annotate(geom = "text", x= -157.7, y = 21.65, label = "Oʻahu",
        size = 3.5) +
    annotate(geom = "text", x= -159.1, y = 22.2, label = "Kauaʻi",
        size = 3.5)

# save Land Cover map as .png file in HI_Model/output_processing/output
ggsave("./Manuscript/fig_images/fig1_HI_LC.png", height = 5, width = 7, dpi = 400)
