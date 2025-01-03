################################################################################ 
# Visualize Rapidan Dam failure and slope changes
################################################################################ 
library(tidyverse)
library(sf)
library(ggplot2)
library(ggrepel)
library(viridis)
library(raster)
library(dataRetrieval)
library(patchwork)

################################################################################
# plot RiverSP product over the Blue Earth for dates of interest
################################################################################
# get all SWOT data for this location
floodfiles <- list.files('DamFailure_Downloads/', pattern = '.shp$', 
                         full.names = T)
# combine together
flooddata <- data.frame()
for(file in floodfiles){
  print(file)
  data <- read_sf(file)
  data <- data[data$wse > 1,]
  flooddata_all <- rbind(flooddata_all, data)
}

# Get Blue Earth
flooddata <- flooddata[flooddata$river_name == 'Blue Earth River',]
# AOI
flooddata <- flooddata[flooddata$lat < 44.24 & flooddata$lat > 43.91,]
flooddata <- flooddata[flooddata$lon < -94.0 & flooddata$lon > -94.3,]
# Quality
flooddata <- flooddata[flooddata$node_q < 3,] # quality check
# Format
flooddata$date <- as.Date(flooddata$time_str) # extract date
# Get dist downstream
flooddata$dist_downstream <- max(flooddata$p_dist_out) - flooddata$p_dist_out

# plot water surface elevation along distance to outlet
elev <- ggplot(flooddata, aes(x = dist_downstream/1000, y = wse, color = date)) +
  geom_point(size = 2) +
  theme_classic() +
  scale_color_viridis_c(direction = -1, option = 'inferno', trans = 'date') +
  guides(color = guide_colorbar(reverse = T)) +
  labs(x = 'Distance downstream (km)', y = 'Water surface elevation (m)', 
       color = element_blank()) + #+ xlim(175, 575) 
  geom_vline(xintercept = 7.7, linewidth = 1.5, color = 'red', linetype = 'dashed') +
  theme(legend.position = c(.8, .65))
elev

################################################################################
# calculate upstream and downstream slopes + plot
################################################################################
# Get upstream and downstream regions
upstream <- flooddata[flooddata$dist_downstream < 7700,] #7700 is where dam is
downstream <- flooddata[flooddata$dist_downstream > 7700,]
# get correct pre/post failure dates
avg_upstream_prefailure <- upstream[upstream$date < '2024-06-27',]
avg_upstream_postfailure <- upstream[upstream$date > '2024-06-27',]
avg_downstream_prefailure <- downstream[downstream$date < '2024-06-27',]
avg_downstream_postfailure <- downstream[downstream$date > '2024-06-27',]
# get averages over pre and post
avg_upstream_prefailure <- avg_upstream_prefailure %>%
  group_by(dist_downstream) %>%
  summarise(avg_wse = mean(wse))
avg_upstream_postfailure <- avg_upstream_postfailure %>%
  group_by(dist_downstream) %>%
  summarise(avg_wse = mean(wse))
avg_downstream_prefailure <- avg_downstream_prefailure %>%
  group_by(dist_downstream) %>%
  summarise(avg_wse = mean(wse))
avg_downstream_postfailure <- avg_downstream_postfailure %>%
  group_by(dist_downstream) %>%
  summarise(avg_wse = mean(wse))
# finally, get slopes
upstream_pre_slope <- lm(avg_wse ~ dist_downstream, 
                         data = avg_upstream_prefailure)$coefficients[[2]]
upstream_post_slope <- lm(avg_wse ~ dist_downstream, 
                          data = avg_upstream_postfailure)$coefficients[[2]]
downstream_pre_slope <- lm(avg_wse ~ dist_downstream, 
                           data = avg_downstream_prefailure)$coefficients[[2]]
downstream_post_slope <- lm(avg_wse ~ dist_downstream, 
                            data = avg_downstream_postfailure)$coefficients[[2]]

# get all for upstream
upstream_slopes <- upstream %>%
  group_by(date) %>%
  summarize(slope = lm(wse ~ dist_downstream)$coefficients[[2]])
downstream_slopes <- downstream %>%
  group_by(date) %>%
  summarize(slope = lm(wse ~ dist_downstream)$coefficients[[2]])

# plot
slope_change <- ggplot(upstream_slopes) +
  geom_line(aes(x = date, y = abs(slope*1000), color = 'upstream'), 
            linewidth = 2) +
  geom_line(data = downstream_slopes, aes(x = date, y = abs(slope*1000), 
                                          color = 'downstream'), linewidth = 2) +
  theme_classic() +
  labs(x = element_blank(), y = 'Slope (m/km)') +
  scale_x_date(date_breaks = '2 month') +
  scale_color_manual(name = "", values = c('upstream' = '#662C91', 
                                           'downstream' = '#17A398')) +
  geom_vline(xintercept = as.numeric(as.Date('2024-06-24')), linewidth = 1.5, 
             color = '#0354A0', linetype = 'dashed') +
  theme(legend.position = c(.85, .2))
slope_change





