################################################################################ 
# Code to open SWOT PIXC product and create plots over Yukon River
################################################################################ 
# load in libraries
library(ncdf4)
library(raster)
library(sf)
library(terra)
library(ggplot2)
library(tidyverse)
library(viridis)
library(RColorBrewer)
library(elevatr)
library(rgeoboundaries)
library(ggnewscale)
library(stars)
library(usmap)
library(maptools)
library(ggspatial)

################################################################################ 
# Open PIXC
################################################################################
pixc <- nc_open('Alaska/SWOT_L2_HR_PIXC_018_043_266L_20240711T110509_20240711T110520_PIC0_01.nc')
# get variables of interest
lon <- ncvar_get(pixc, "pixel_cloud/longitude")
lat <- ncvar_get(pixc, "pixel_cloud/latitude")
wse <- ncvar_get(pixc, "pixel_cloud/height")
class <- ncvar_get(pixc, "pixel_cloud/classification")
qual <- ncvar_get(pixc, "pixel_cloud/classification_qual")
# close file
nc_close(test)
# merge into df
df <- data.frame(cbind(lon, lat, wse, class, qual))
# get open water only
df <- df[df$class == 4,]
# quality control
df <- df[df$qual <= 4,]


################################################################################ 
# Fig. 2: Alaska hillshade
################################################################################ 
# Get Alaska location
states <- rgeoboundaries::geoboundaries("USA", "adm1")
alaska <- states[states$shapeName == "Alaska",]
# convert to sf
alaska_sf <- st_as_sf(alaska$geometry)
# get elevation at appropriate zoom
elev_alaska <- get_elev_raster(alaska, z = 5)
# clip elevation to state outline
elev_alaska <- crop(elev_alaska, alaska_sf)
elev_alaska <- mask(elev_alaska, alaska_sf)
# convert to df
elev_df_alaska <- as.data.frame(elev_alaska, xy = TRUE)
colnames(elev_df_alaska)[3] <- 'elevation'
# create hillshade from elev
elev_sr_alaska <- as(elev_alaska, "SpatRaster")
slope_alaska <- terrain(elev_sr_alaska, "slope", unit = 'radians')
aspect_alaska <- terrain(elev_sr_alaska, "aspect", unit = 'radians')
hill_alaska <- shade(slope_alaska, aspect_alaska, 30, 270)
names(hill_alaska) <- "shades"
hill_alaska <- as.data.frame(hill_alaska, xy = TRUE)
# get outline of alaska 
ak <- map_data('world') %>% filter(region =='USA') %>% filter(subregion =='Alaska')
ak <- ak[ak$long < 0,] # remove islands near russia

# now add yukon river
# these are sword v16 North America nodes
sword_files <- list.files('NA_16/', pattern = glob2rx('*nodes*v16.shp*'), 
                          full.names = T)
# read in each file and append to one big dataframe
sword <- data.frame()
for(file in sword_files){
  print(file)
  data <- read_sf(file)
  sword <- rbind(sword, data)
}
# get Yukon River
yukon <- sword[sword$river_name == 'Yukon River',] 

# plot!
ggplot() +
  geom_raster(data = hill_alaska, aes(x = x, y = y, fill = shades), show.legend = F, alpha = 0.4) +
  scale_fill_gradientn(colors = c('black', '#202020', 'white'), na.value = 'white') +
  new_scale_fill() +
  geom_raster(data = elev_df_alaska, aes(x = x, y = y, fill = elevation), 
              alpha = 0.4, show.legend = F) +
  scale_fill_gradientn(colors = c('black', 'darkgray', 'lightgray'), na.value = 'white') +
  geom_polygon(data = ak, aes(x = long, y = lat, group = group), 
               fill = NA, color = 'black', linewidth = 1) +
  xlim(-172, -130) + ylim(52.2, 71.5) +
  labs(x = element_blank(), y = element_blank(), color = 'WSE (m)') +
  new_scale_fill() +
  geom_point(data = yukon[yukon$wse < 250,], 
            aes(x = x, y = y, color = wse), size = 2, lineend = 'round') +
  scale_color_viridis() +
  coord_fixed(1.8) + 
  annotate("point", x = -162.8829356, y = 61.9336866, color = "red") +
  theme_void() +
  theme(legend.position = c(0.82, 0.68))



################################################################################ 
# Plot 2: PIXC 
################################################################################ 
# get county for elevation info
usa <- rgeoboundaries::geoboundaries("USA", "adm2")
kusilvak <- usa[usa$shapeName == "Kusilvak",]
# get Alaska outline
states <- rgeoboundaries::geoboundaries("USA", "adm1")
alaska <- states[states$shapeName == "Alaska",]
# convert to sf
alaska_sf <- st_as_sf(alaska$geometry)
# get elevation
elev_yukon <- get_elev_raster(kusilvak, z = 10)
# clip
elev_yukon <- crop(elev_yukon, alaska_sf)
elev_yukon <- mask(elev_yukon, alaska_sf)
elev_df_yukon <- as.data.frame(elev_yukon, xy = TRUE)
colnames(elev_df_yukon)[3] <- 'elevation'
# clip to area of interest
elev_df_yukon <- elev_df_yukon[elev_df_yukon$x > -166.26 & elev_df_yukon$x < 158.53,]
elev_df_yukon <- elev_df_yukon[elev_df_yukon$y > 58.33 & elev_df_yukon$y < 63.78,]
# create hillshade
elev_sr_yukon <- as(elev_yukon, "SpatRaster")
slope_yukon <- terrain(elev_sr_yukon, "slope", unit = 'radians')
aspect_yukon <- terrain(elev_sr_yukon, "aspect", unit = 'radians')
hill_yukon <- shade(slope_yukon, aspect_yukon, 30, 270)
names(hill_yukon) <- "shades"
hill_yukon <- as.data.frame(hill_yukon, xy = TRUE)

# log transform for visualization (pretty flat around here)
hill_yukon$shades_log <- log(hill_yukon$shades)
elev_df_yukon$elevation_log <- log(elev_df_yukon$elevation)

# plot PIXC!
ggplot() +
  geom_raster(data = hill_yukon, aes(x = x, y = y, fill = shades), 
              show.legend = F, alpha = 1) +
  scale_fill_gradientn(colors = c('black', '#202020', 'darkgray',  'white'),
                       trans = 'log') +
  new_scale_fill() +
  geom_raster(data = elev_df_yukon, aes(x = x, y = y, fill = elevation_log), 
              alpha = 0.5, show.legend = F) +
  scale_fill_gradientn(colors = c('black', '#202020', 'darkgray', 'white')) +
  geom_point(data = df, aes(x = lon, y = lat, color = wse), size = 0.01) +
  scale_color_viridis() +
  labs(x = element_blank(), y = element_blank(), 
       color = 'WSE (m)') +
  annotation_scale(plot_unit = 'km',
                   location = 'bl', style = 'ticks',
                   width_hint = 0.1) +
  coord_sf(crs = 4326) +
  theme_void() +
  guides(color = guide_colorbar(ticks.colour = 'black', 
                                frame.colour = 'black')) +
  xlim (-165, -163) + ylim(62.2, 63.1)


################################################################################ 
# Plot 3: Yukon close-up
################################################################################ 
ggplot() +
  geom_raster(data = hill_yukon, aes(x = x, y = y, fill = shades), show.legend = F, alpha = 0.9) +
  scale_fill_gradientn(colors = c('black', '#202020', 'darkgray',  'white'), 
                       na.value = 'white', trans = 'log') +
  new_scale_fill() +
  geom_raster(data = elev_df_yukon, aes(x = x, y = y, fill = elevation), 
              alpha = 0.5, show.legend = F) +
  scale_fill_gradientn(colors = c('black', '#202020', 'darkgray', 'lightgray'),
                       na.value = 'white', trans = 'log') +
  geom_point(data = df, aes(x = lon, y = lat, color = wse), size = 0.1) +
  scale_color_viridis(limits = c(5, 20)) +
  xlim(-164.3, -163.6) + ylim(62.4, 62.7) +
  labs(x = element_blank(), y = element_blank(), 
       color = 'WSE (m)') +
  annotation_scale(plot_unit = 'km', location = 'bl', 
                   style = 'ticks', width_hint = 0.1) +
  coord_sf(crs = 4326) +
  theme_void() +
  guides(color = guide_colorbar(ticks.colour = 'black', 
                                frame.colour = 'black')) 





