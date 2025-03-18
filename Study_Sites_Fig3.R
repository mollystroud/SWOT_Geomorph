################################################################################ 
# Create national map of WSE with Klamath and Blue Earth insets
################################################################################ 
library(ggplot2)
library(tidyverse)
library(raster)
library(viridis)
library(Thermimage)
library(roll)
library(elevatr)
library(terra)
library(sf)
library(rgeoboundaries)
library(ggnewscale)

################################################################################
# national map
################################################################################
sword_files <- list.files('NA/', pattern = glob2rx('*nodes*v17.shp*'), 
                          full.names = T)
# read in each file and append to one big dataframe
sword <- data.frame()
for(file in sword_files){
  print(file)
  data <- read_sf(file)
  data <- data[data$wse > 1,]
  sword <- rbind(sword, data)
}
# area of interest
sword <- sword[sword$y < 49.2 & sword$y > 24.94,]
sword <- sword[sword$lon < -122.4379 & sword$lon > -124.0788,]
sword <- sword[sword$strm_order > 0,]

# write out as shp
st_write(sword, 'NA/sword_all.shp')
usa <- map_data('usa')
# read in shp
sword <- read_sf('NA/sword_usa_clipped.shp')

# plot national map
ggplot() + geom_polygon(data = usa, aes(x = long, y = lat, group = group), 
                        color = 'black', fill = 'black') +
  geom_point(data = sword, aes(x = x, y = y, color = wse, 
                               size = log(strm_order))) +
  scale_color_viridis(guide = guide_colorbar(frame.color = 'black'), 
                      trans = 'log', breaks = c(5, 50, 500)) +
  theme_classic() +
  labs(x = element_blank(), y = element_blank(), color = 'WSE (m)') +
  scale_size_continuous(range = c(0, 0.6)) +
  guides(size = 'none') +
  coord_fixed(1.3) +
  theme(legend.position = c(0.9, 0.3), 
        axis.line = element_blank(), axis.ticks = element_blank(), 
        axis.text = element_blank()) 


################################################################################
# Klamath inset
################################################################################
# read in SWOT data (averaged klamath wse from rivsp product)
klamath_avg_wse <- read_csv('rivsp_klamathdata.csv')
# get CA/usa 
usa <- rgeoboundaries::geoboundaries("USA", "adm1")
CA <- usa[usa$shapeName == "California",]
elev <- get_elev_raster(locations = CA, z = 9, clip = "locations")
elev_df <- as.data.frame(elev, xy = TRUE)
colnames(elev_df)[3] <- 'elevation'
# create hillshade
elev_sr <- as(elev, "SpatRaster")
slope <- terrain(elev_sr, "slope", unit = 'radians')
aspect <- terrain(elev_sr, "aspect", unit = 'radians')
hill <- shade(slope, aspect, 30, 270)
names(hill) <- "shades"
hill <- as.data.frame(hill, xy = TRUE)
# plot map of river with colored WSE
ggplot() +
  geom_raster(data = hill, aes(x = x, y = y, fill = shades), 
              show.legend = F, alpha = .9) +
  scale_fill_gradientn(colors = c('black', '#4f4f4f', 'white')) +
  new_scale_fill() +
  geom_raster(data = elev_df, aes(x = x, y = y, fill = elevation), 
              alpha = 0.6, show.legend = F) +
  scale_fill_gradientn(colors = c('black', 'darkgray', 'white')) +
  geom_point(data = klamath_avg_wse, aes(x = lon, y = lat, color = wse)) +
  scale_color_viridis() +
  ylim(41.11, 41.97) + xlim(-124.15, -122.3) +
  labs(x = element_blank(), y = element_blank(), 
       color = 'WSE (m)') +
  theme_void()  +
  guides(color = guide_colorbar(ticks.colour = 'black', 
                                frame.colour = 'black')) +
  annotation_scale(plot_unit = 'km',
                   location = 'br', 
                   style = 'ticks', width_hint = 0.1) +
  coord_fixed(1.3)


################################################################################
# Blue Earth inset
################################################################################
# read in SWOT data (averaged blue earth wse from rivsp product)
blueearth_avg_wse <- read_csv('rivsp_blueearthdata.csv')
# get MN/usa
usa <- rgeoboundaries::geoboundaries("USA", "adm2")
MN <- usa[usa$shapeName == "Blue Earth",]
# get elev and hillshade
elev_MN <- get_elev_raster(MN, z = 13)
elev_df_MN <- as.data.frame(elev_MN, xy = TRUE)
colnames(elev_df_MN)[3] <- 'elevation'
elev_sr_MN <- as(elev_MN, "SpatRaster")
slope_MN <- terrain(elev_sr_MN, "slope", unit = 'radians')
aspect_MN <- terrain(elev_sr_MN, "aspect", unit = 'radians')
hill_MN <- shade(slope_MN, aspect_MN, 30, 270)
names(hill_MN) <- "shades"
hill_MN <- as.data.frame(hill_MN, xy = TRUE)

# make plot
ggplot() +
  geom_raster(data = hill_MN, aes(x = x, y = y, fill = shades), 
              show.legend = F, alpha = 0.9) +
  scale_fill_gradientn(colors = c('black', '#202020', 'white')) +
  new_scale_fill() +
  geom_raster(data = elev_df_MN, aes(x = x, y = y, fill = elevation), 
              alpha = 0.6, show.legend = F) +
  scale_fill_gradientn(colors = c('black', 'darkgray', 'lightgray')) +
  geom_path(data = blueearth_avg_wse, aes(x = lon, y = lat, color = mean_wse),
            size = 2, lineend = 'round') +
  scale_color_viridis(breaks = c(240, 250, 260)) +
  xlim(-94.138, -94.095) + ylim(44.061, 44.14) +
  labs(x = element_blank(), y = element_blank(), 
       color = 'WSE (m)') +
  annotation_scale(plot_unit = 'km',
                   location = 'br', 
                   style = 'ticks', width_hint = 0.5) +
  coord_fixed(1.3) +
  theme_void() +
  guides(color = guide_colorbar(ticks.colour = 'black', 
                                frame.colour = 'black'))





