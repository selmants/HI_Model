# Use ggplot to visualize
# current and end-of-century
# rainfall and air temperature  
# Paul C. Selmants
# 2020-02-24

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
	labs(title = "(b)",
		x = expression("Longitude " ( degree*W)),
		y = expression("Latitude " ( degree*N))) +
	scale_fill_viridis("Mean Annual\nRainfall (mm)", 
		direction = -1, limits = c(100, 9600), 
		breaks = seq(250,9500, 1750)) +
	theme_void() +
	theme(legend.position = c(0.2,0.55),
		legend.justification = c("left", "top")) +
	coord_fixed()

# plot of Mean Annual Temperature using viridis palette
airtem <- ggplot(data=MAT_df) +
	geom_tile(aes(x=x, y=y, fill=layer)) +
	labs(title = "(a)",
		x = expression("Longitude " ( degree*W)),
		y = expression("Latitude " ( degree*N))) +
	scale_fill_viridis("Mean Annual\nTemperature (°C)", option = "plasma", 
		direction = 1, limits = c(3,25), breaks = seq(4,24,4)) +
	theme_void() +
	theme(legend.position = c(0.2, 0.55),
		legend.justification = c("left", "top")) +
	coord_fixed()

# create two panel current climate figure
climatefig <- plot_grid(airtem, rain, ncol = 1, align = "v", axis = 'l')
# save current climate fig as .png file 
ggsave("CurrentClimate.png", climatefig, width = 6, height = 8)

# plot of deltaT at 2100 for RCP 4.5 using RColorBrewer palette
dt45 <- ggplot(t45_df) +
	geom_tile(aes(x=x, y=y, fill=layer)) +
	labs(title = "(a) RCP 4.5") +
	scale_fill_distiller("Change in\ntemperature (°C)", palette = "YlOrRd",
		limits = c(1.5,5), breaks = seq(1.5,5, 0.5), direction = 1) +
	theme_void() +
	theme(legend.position="Null") +
	coord_fixed()

# plot of deltaT at 2100 for RCP 8.5 using RColorBrewer palette
dt85 <- ggplot(data=t85_df) +
	geom_tile(aes(x=x, y=y, fill=layer)) +
	labs(title = "(b) RCP 8.5") + 
	scale_fill_distiller("Change in\ntemperature (°C)", palette = "YlOrRd", 
		limits = c(1.5,5), breaks = seq(1.5,5, 0.5), direction = 1) +
	theme_void() +
	theme(legend.justification=c("left", "top"), 
		legend.position=c(0.15,0.55)) +
	coord_fixed() 

# two panel plot of change in temperature by 2100
dTfig <- plot_grid(dt45, dt85, ncol = 1, align = "v", axis = "l")
# save two panel plot of change in temperature as .png file
ggsave("FutureTemperature.png", dTfig, width = 6, height = 8)

# Create New color palette for change in rainfall
pal = c("#A50026","#D73027","#F46D43","#FF7F00","#FFFF99","#4575B4","#313695")

# plot of dRainfall by end of century under RCP 4.5 using RColorBrewer	
r45<- ggplot(data=r45_df) + 
  geom_tile(aes(x=x,y=y,fill=layer)) + 
  labs(title = "(a) RCP 4.5") +
  scale_fill_gradientn("Change in\nrainfall (mm)", colors= pal, 
  	limits = c(-1383,580), breaks = seq(-1250, 500, 250)) +
  theme_void() +
  theme(legend.position="Null") +
  coord_fixed()

## plot of dRainfall by end of century under RCP 8.5 using RColorBrewer	
r85<- ggplot(data=r85_df) + 
  geom_tile(aes(x=x,y=y,fill=layer)) + 
  labs(title = "(b) RCP 8.5") +
  scale_fill_gradientn("Change in\nRainfall (mm)", colors= pal, 
  	limits = c(-1383, 580), breaks = seq(-1250,500,250)) +
  theme_void() +
  theme(legend.justification=c("left", "top"), 
  	legend.position=c(0.15,0.55)) +
  coord_fixed()

# two panel plot of change in rainfall
dRainfig <- plot_grid(r45, r85, ncol = 1, align = "v", axis = "l")
# save two panel change in rainfall plot as .png file
ggsave("FutureRainfall.png", dRainfig, width = 6, height = 8)



