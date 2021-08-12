# create NPP figures based on 
# IBIS mean values per state type 
# (MZ & land cover class), NPP
# stationary spatial multiplier
# based on contemporary climate,
# and NPP temporal multipliers based on
# CMIP5 climate projections
# Paul C. Selmants 
# 2021-08-04
# R version 4.1.0

# load required packages into R
library(raster) #version 3.1-5
library(rgdal) #version 1.4-8
library(tidyr, warn.conflicts = FALSE) #version 1.0.2
library(dplyr, warn.conflicts = FALSE) #version 0.8.5
library(ggplot2)
library(viridis)
library(RColorBrewer)
library(cowplot) 


## Set working directory at highest level in HI_Model
## GitHub repository, https://github.com/selmants/HI_Model 

# read in raster of combined Moisture Zones and land cover classes, where:
# 1 = Dry Forest	 	 7 = Mesic Forest			13 = Wet Forest
# 2 = Dry Grass		 	 8 = Mesic Grass			14 = Wet Grass
# 3 = Dry Shrub		 	 9 = Mesic Shrub			15 = Wet Shrub 
# 4 = Dry Plantation	10 = Mesic Plantation		16 = Wet Plantation
# 5 = Dry Ag			11 = Mesic Ag				17 = Wet Ag
# 6 = Dry WoodyCrop		12 = Mesic WoodyCrop		18 = Wet WoodyCrop
MZSC <- raster("./Fire/data/processed/MZSC.tif")

# read in NCEAS NPP stationary spatial multiplier raster, 
# match crs projection to MZSC, and mask with MZSC 
# to remove barren and urban areas
npp_sm <- raster(
	"./InputData/spatial_data/spatial_multipliers/NCEAS_NPP_sm.tif") %>%
	projectRaster(.,MZSC) %>%
	mask(., MZSC)

# read in RCP 4.5 NPP temporal spatial multiplier raster, 
# match crs projection to MZSC, and mask with MZSC 
npp_rcp45 <- raster(
	"./InputData/spatial_data/spatial_multipliers/RCP45_NPPmulti_end.tif") %>%
	projectRaster(., MZSC) %>%
	mask(., MZSC)

# read in RCP 8.5 NPP temporal spatial multiplier raster, 
# match crs projection to MZSC, and mask with MZSC 
npp_rcp85 <- raster(
	"./InputData/spatial_data/spatial_multipliers/RCP85_NPPmulti_end.tif") %>%
	projectRaster(., MZSC) %>%
	mask(., MZSC)

# read annual MAT raster as template for Lat. Long. conversion
MAT <- raster("./Climate/MAT_annual.tif")

# create reclassification matrix corresponding to IBIS mean NPP values
# for each state type (MZ and land cover class)
npp_rcl <- data.frame(sc = seq(1,18),
	npp = c(0.29,0.21,0.31,0.29,0.34,0.21,0.84,0.47,0.55,0.84,0.32,0.67,
		1.14,0.81,0.85,1.14,0.3,0.56)) %>%
		as.matrix(.) %>%
		unname(.) 

# reclassify MZSC with IBIS mean NPP values
IBIS_NPP <- reclassify(MZSC, npp_rcl)

# multiply IBIS_NPP raster by npp spatial multiplier raster
npp_initial <- IBIS_NPP * npp_sm 

# create function to re-sample raster projections to Lat./Long. 
# then convert to points dataframe for usage in ggplot
r2pfun <- function(x) {
	projectRaster(x, MAT, method = 'bilinear') %>%
	rasterToPoints(.) %>%
	data.frame(.)
}
# convert NPP rasters to points dataframes using r2p function
nppsm_df <- r2pfun(npp_sm) %>%
	select(x,y,layer=NCEAS_NPP_sm)
nppinit_df <- r2pfun(npp_initial)
npp45_df <- r2pfun(npp_rcp45) %>%
	select(x,y,layer=RCP45_NPPmulti_end)
npp85_df <- r2pfun(npp_rcp85) %>%
	select(x,y,layer=RCP85_NPPmulti_end)

# create statewide NPP spatial multiplier map for Hawaii
nppsm_fig <- ggplot(data=nppsm_df) +
	geom_tile(aes(x=x, y=y, fill=layer)) +
	labs(title = "NPP spatial multipliers",
		x = element_blank(),
		y = expression("Latitude " ( degree*N))) +
	scale_fill_viridis("multiplier", option = "cividis", 
		direction = -1, 
		limits = c(0.25,5), 
		breaks = seq(0.25,5,1)) +
	theme_bw() +
	theme(legend.position = c(0.1, 0.68),
		legend.justification = c("left", "top"),
		plot.margin = unit(c(0, 0, 0, 0), "cm")) +
	coord_fixed()

# create legend title for initial NPP graph
nameColor <- bquote(kg~C~m^-2~y^-1)

# create statewide initial NPP map for Hawaii
nppinit_fig <- ggplot(data=nppinit_df) +
	geom_tile(aes(x=x, y=y, fill=layer)) +
	labs(title = "Initial NPP",
		x = expression("Longitude " ( degree*W)),
		y = expression("Latitude " ( degree*N))) +
	scale_fill_distiller(nameColor, 
		palette = "YlGn", limits = c(0, 1.5), 
		breaks = seq(0,1.5,0.5), direction = 1) +
	theme_bw() +
	theme(legend.position = c(0.1, 0.7),
		legend.justification = c("left", "top"),
		plot.margin = unit(c(0, 0, 0, 0), "cm")) +
	coord_fixed()

# create two panel NPP figure
nppfig <- plot_grid(nppsm_fig, nppinit_fig, ncol =1, align = "hv")

# save npp fig as .png file 
ggsave("./Manuscript/fig_images/figS5_initialNPP.png", nppfig, 
	width = 5, height = 7, dpi = 400)

# create temporal NPP multiplier map for RCP 4.5 
npp45_fig <- ggplot(data=npp45_df) +
	geom_tile(aes(x=x,y=y,fill=layer)) +
	labs(title = "NPP temporal multipliers by 2100, RCP 4.5",
		x = element_blank(),
		y = expression("Latitude " ( degree*N))) +
	scale_fill_viridis("multiplier", option = "cividis", 
		direction = -1, 
		limits = c(0.2, 1.4), 
		breaks = seq(0.2,1.4,0.2)) +
	theme_bw() +
	theme(legend.position = c(0.1, 0.7),
		legend.justification = c("left", "top"),
		plot.margin = unit(c(0, 0, 0, 0), "cm")) +
	coord_fixed()

# create temporal NPP multiplier map for RCP 8.5 
npp85_fig <- ggplot(data=npp85_df) +
	geom_tile(aes(x=x,y=y,fill=layer)) +
	labs(title = "NPP temporal multipliers by 2100, RCP 8.5",
		x = expression("Longitude " ( degree*W)),
		y = expression("Latitude " ( degree*N))) +
	scale_fill_viridis("multiplier", option = "cividis", 
		direction = -1, 
		limits = c(0.2, 1.4), 
		breaks = seq(0.2,1.4,0.2)) +
	theme_bw() +
	theme(legend.position = c(0.1, 0.7),
		legend.justification = c("left", "top"),
		plot.margin = unit(c(0, 0, 0, 0), "cm")) +
	coord_fixed()

# create two panel NPP temporal multiplier figure
npptmfig <- plot_grid(npp45_fig, npp85_fig, ncol =1, align = "hv")

# save npp temporal multiplier fig as .png file 
ggsave("./Manuscript/fig_images/figS6_tempNPPmulti.png", npptmfig, 
	width = 5, height = 7, dpi = 400)


