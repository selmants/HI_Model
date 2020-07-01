# Use ggplot to visualize
# current and end-of-century
# rainfall and air temperature 
# in the Hawaiian Islands 
# Paul C. Selmants
# 2020-02-24

## Data in GitHub repository: https://github.com/selmants/HI_Model

# load required R packages
library(raster)
library(rgdal)
library(ggplot2)
library(dplyr)
library(viridis)
library(RColorBrewer)
library(cowplot) 

# create list of annual rainfall .tif files
rainlist <- list.files('rainfall_tiff_annual', full.names = TRUE)
#create raster stack of annual rainfall (mm) .tif files
rainstack <- stack(rainlist)
# make raster layer of 30-year mean annual rainfall and re-sample
# to change projection to UTM (this takes ~30 seconds)
rain30ymean <- calc(rainstack, mean)
# read in rasters of late century rainfall anomolies for RCPs 4.5 & 8.5
rcp85anom <- raster("sd_rfanom_rcp85_late.tif")
rcp45anom <- raster("sd_rfanom_rcp45_late.tif")
# make rasters of late century rainfall differences from present
raindiff85 <- rain30ymean*(rcp85anom/100)
raindiff45 <- rain30ymean*(rcp45anom/100)

# read in current mean annual temperature raster
MAT <- raster("MAT_annual.tif") 
# read in rasters of late century temperature differences for RCPs 4.5 & 8.5
Tdelta_rcp45 <- raster("sd_Tdeltas_rcp45_late.tif")
Tdelta_rcp85 <- raster("sd_Tdeltas_rcp85_late.tif")

#prepare rain30ymean and raindiff data to plot with geom_tile in ggplot
rain_df <- rasterToPoints(rain30ymean) %>%
	data.frame(.)

r45_df <- rasterToPoints(raindiff45) %>%
	data.frame(.)

r85_df <- rasterToPoints(raindiff85) %>%
	data.frame(.)
#prepare MAT and dT data to plot with geom_tile in ggplot2 
MAT_df <- rasterToPoints(MAT) %>%
	data.frame(.) %>%
	select(x, y, layer = MAT_annual)

t45_df <- rasterToPoints(Tdelta_rcp45) %>%
	data.frame(.) %>%
	select(x, y, layer = sd_Tdeltas_rcp45_late)

t85_df <- rasterToPoints(Tdelta_rcp85) %>%
	data.frame(.) %>%
	select(x, y, layer = sd_Tdeltas_rcp85_late)

#plot of Mean Annual Rainfall using viridis palette
rain <- ggplot(data=rain_df) +
	geom_tile(aes(x=x, y=y, fill=layer)) +
	labs(title = "Mean Annual Rainfall",
		x = expression("Longitude " ( degree*W)),
		y = expression("Latitude " ( degree*N))) +
	scale_fill_viridis("mm", 
		direction = -1, limits = c(100, 9600), 
		breaks = seq(250,9500, 1750)) +
	theme_bw() +
	theme(legend.position = c(0.2, 0.62),
		legend.justification = c("left", "top"),
		plot.margin = unit(c(0, 0, 0, 0), "cm")) +
	coord_fixed()

# plot of Mean Annual Temperature using viridis palette
airtem <- ggplot(data=MAT_df) +
	geom_tile(aes(x=x, y=y, fill=layer)) +
	labs(title = "Mean Annual Temperature",
		x = element_blank(),
		y = expression("Latitude " ( degree*N))) +
	scale_fill_viridis("°C", option = "plasma", 
		direction = 1, limits = c(3,25), breaks = seq(4,24,4)) +
	theme_bw() +
	theme(legend.position = c(0.2, 0.62),
		legend.justification = c("left", "top"),
		plot.margin = unit(c(0, 0, 0, 0), "cm")) +
	coord_fixed()

# create two panel current climate figure
climatefig <- plot_grid(airtem, rain, ncol =1, align = "hv")
# save current climate fig as .png file 
ggsave("figS2_CurrentClimate.png", climatefig, width = 5, height = 7)

# plot of deltaT at 2100 for RCP 4.5 using RColorBrewer palette
dt45 <- ggplot(t45_df) +
	geom_tile(aes(x=x, y=y, fill=layer)) +
	labs(x = element_blank(),
		y = element_blank(),
		title = "Temperature change, RCP 4.5") +
	scale_fill_distiller("°C", palette = "YlOrRd",
		limits = c(1.5,5), breaks = seq(1.5,5, 0.5), direction = 1) +
	theme_bw() +
	theme(legend.position="Null",
		plot.margin = unit(c(0, 0, 0, 0), "cm")) +
	coord_fixed()

# plot of deltaT at 2100 for RCP 8.5 using RColorBrewer palette
dt85 <- ggplot(data=t85_df) +
	geom_tile(aes(x=x, y=y, fill=layer)) +
	labs(x = element_blank(),
		y = element_blank(),
		title = "Temperature change, RCP 8.5") +
	scale_fill_distiller("°C", palette = "YlOrRd", 
		limits = c(1.5,5), breaks = seq(1.5,5, 0.5), direction = 1) +
	theme_bw() +
	theme(legend.justification=c("left", "top"), 
		legend.position=c(0.05,0.8),
		legend.title=element_text(size=9),
  	legend.text=element_text(size = 7),
		plot.margin = unit(c(0, 0, 0, 0), "cm")) +
	coord_fixed() 

# Create New color palette for change in rainfall
pal = c("#A50026","#D73027","#F46D43","#FF7F00","#FFFF99","#4575B4","#313695")

# plot of dRainfall by end of century under RCP 4.5 using RColorBrewer	
r45<- ggplot(data=r45_df) + 
  geom_tile(aes(x=x,y=y,fill=layer)) + 
  labs(title = "Rainfall change, RCP 4.5",
  		x = element_blank(),
		y = element_blank()) +
  scale_fill_gradientn("mm", colors= pal, 
  	limits = c(-1383,580), breaks = seq(-1250, 500, 250)) +
  theme_bw() +
  theme(legend.position="Null",
  	plot.margin = unit(c(0, 0, 0, 0), "cm")) +
  coord_fixed()

# plot of dRainfall by end of century under RCP 8.5 using RColorBrewer	
r85<- ggplot(data=r85_df) + 
  geom_tile(aes(x=x,y=y,fill=layer)) + 
  labs(title = "Rainfall change, RCP 8.5",
  		x = element_blank(),
		y = element_blank()) +
  scale_fill_gradientn("mm", colors= pal, 
  	limits = c(-1383, 580), breaks = seq(-1250,500,250)) +
  theme_bw() +
  theme(legend.justification=c("left", "top"), 
  	legend.position=c(0.05,0.8),
  	legend.title=element_text(size=9),
  	legend.text=element_text(size = 7),
  	plot.margin = unit(c(0, 0, 0, 0), "cm")) +
  coord_fixed()

# four panel plot of climate change by RCP
dclimatefig <- plot_grid(dt45, dt85, r45, r85, ncol = 2, align = "hv",
	axis = "l")
# save four panel plot of change in temperature as .png file
ggsave("figS3_FutureClimate.png", dclimatefig, width = 6.5, height = 5.5)

