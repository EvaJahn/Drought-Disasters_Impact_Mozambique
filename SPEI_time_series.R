#################################################################################
### calculate SPEI with accumulation periods of 1,3,6 and 12 months for the whole
### country of Mozambique
#################################################################################
### More detailed analysis of drought event 8 
#################################################################################

setwd("D:/R Data and Script")

# import libraries 

library(ncdf4)
library(raster)
library(rgdal)
library(rasterVis)
library(RColorBrewer)
library(hexbin)
library(latticeExtra)
library(lubridate)
library(ggplot2)
library(scales)
library(dplyr)
library(sp)
library(readr)
library(reshape2)
library(tidyr)


### Load data ==================================================================

spei01 <- subset(brick(choose.files()), 1189:1416)

spei03 <- subset(brick(choose.files()), 1189:1416)

spei06 <- subset(brick(choose.files()), 1189:1416)

spei12 <- subset(brick(choose.files()), 1189:1416)


# Mozambique shapefile

shapeMOZ <- readOGR("D:/R Data and Script", "gadm36_MOZ_0")

# load all other shapefiles 
shps <- dir(getwd(), "*.shp")

for (shp in shps) {
  assign(shp, readOGR(shp))
}




### crop SPEI dataset to Mozambique ============================================


crop_to_area <- function(SPEI, shape)  {
  cropped <- crop(SPEI, shape)
  masked <- mask(cropped, shape)
  plot(masked)
  return(masked)
}


spei01_cropped <- crop_to_area(spei01, shapeMOZ)

spei03_cropped <- crop_to_area(spei03, shapeMOZ)

spei06_cropped <- crop_to_area(spei06, shapeMOZ)

spei12_cropped <- crop_to_area(spei12, shapeMOZ)


# crop to drought region 8 

spei01_cropped_8 <- crop_to_area(spei01, drought_8.shp)

spei12_cropped_8 <- crop_to_area(spei12, drought_8.shp)





# get dates of raster layers
idx <- names(spei01_cropped)
time <- as.Date(substring(idx, 2, 11), format = "%Y.%m.%d")



### define weighting raster
plot(area(spei01_cropped, weights = TRUE, na.rm = TRUE))  
w <- area(spei01_cropped, weights = TRUE, na.rm=TRUE) 
plot(w)
cellStats(w, sum)  # zur Kontrolle: muss 1 sein 


### overlay SPEI raster layer with weighting raster layer
area_weighted_spei01 <- spei01_cropped * w 
plot(area_weighted_spei01)

area_weighted_spei03 <- spei03_cropped * w 
plot(area_weighted_spei03)

area_weighted_spei06 <- spei06_cropped * w 
plot(area_weighted_spei06)

area_weighted_spei12 <- spei12_cropped * w 
plot(area_weighted_spei12)


### calculate weighted SPEI mean values (for 4 accumulation periods)
wmean_spei01 <- cellStats(area_weighted_spei01, sum)
wmean_spei01
plot(wmean_spei01, type="l")

wmean_spei03 <- cellStats(area_weighted_spei03, sum)
wmean_spei03
plot(wmean_spei03, type="l")

wmean_spei06 <- cellStats(area_weighted_spei06, sum)
wmean_spei06
plot(wmean_spei06, type="l")

wmean_spei12 <- cellStats(area_weighted_spei12, sum)
wmean_spei12
plot(wmean_spei12, type="l")


### save in data frame 

SPEIdf <- data.frame(wmean_spei01, wmean_spei03, wmean_spei06, wmean_spei12)
SPEIdf

SPEIdf$Date <- time


### restructure data frame for multi-panel plot 

SPEIdfneu<- gather(SPEIdf, key = "SPEI", value = "Werte", 1:4)
SPEIdfneu






##### Plot: 4 different accumulation periods of SPEI

ggplot(SPEIdfneu, aes(Date, Werte)) + 
  geom_line() + 
  labs(title = "Different SPEI accumulation periods",
       subtitle = "1, 3, 6 and 12 months",
       y = "SPEI", x ="Year") + 
  facet_wrap(~SPEI, strip.position = "right", ncol = 1, nrow = 4) + 
  theme_bw() + 
  geom_area() +
  theme(legend.position = "none") 








### More detailed analysis of drought event 8 ==================================




### Drought 8: timeseries

timeseries <- function(SPEI, shape, start, end)  {
  
  # Region zuschneiden
  cropped <- crop(SPEI, shape)
  masked <- mask(cropped, shape)
  
  # Subset auf Zeitraum 
  drought_event <- subset(masked, start:end) 
  
  # Wichtungsraster definieren
  w <- area(drought_event, weights = TRUE, na.rm=TRUE) 
  
  # Wichtungsraster mit SPEI Raster multiplizieren
  area_weighted_spei <- drought_event * w 
  
  # Mittelwerte berechnen
  spei12_wmean_8_gesMOZ <- cellStats(area_weighted_spei, sum)
  
  # Plot
  #p <- plot(mean_weighted, type="l")
  
  #return(mean_weighted)
}


# SPEI01 weighted mean, whole country MOZ

spei01_wmean_8_gesMOZ <- timeseries(spei01, gadm36_MOZ_0.shp, start = which(time == as.Date("2016-01-16")), end = which(time == as.Date("2017-01-16")))
plot(spei01_wmean_8_gesMOZ, type = "l")

# SPEI01 weighted mean,  drought region 8

spei01_wmean_8_region <- timeseries(spei01, drought_8.shp, start = which(time == as.Date("2016-01-16")), end = which(time == as.Date("2017-01-16")))
plot(spei01_wmean_8_region, type = "l")

# SPEI12 weighted mean, whole country MOZ

spei12_wmean_8_gesMOZ <-  timeseries(spei12, gadm36_MOZ_0.shp, start = which(time == as.Date("2016-01-16")), end = which(time == as.Date("2017-01-16")))
plot(spei12_wmean_8_gesMOZ, type = "l")

# SPEI12 weighted mean, drought region 8

spei12_wmean_8_region <-  timeseries(spei12, drought_8.shp, start = which(time == as.Date("2016-01-16")), end = which(time == as.Date("2017-01-16")))
plot(spei12_wmean_8_region, type = "l")






### save all drought 8 time series in one data frame
drought8_df <- data.frame(spei01_wmean_8_gesMOZ, spei01_wmean_8_region, spei12_wmean_8_gesMOZ, spei12_wmean_8_region)

drought8_df$Date <- time[193:205]
drought8_df

# rename columns 
names(drought8_df)
names(drought8_df) <- c("SPEI01 complete country", "SPEI01 drought region 8 only",
                        "SPEI12 complete country", "SPEI12 drought region 8 only", "Date")

names(drought8_df)

### restructure dataframe for multi-panel plot 

drought8_df_gathered<- gather(drought8_df, key = "SPEI", value = "Werte", 1:4)
drought8_df_gathered




##### Plot: Drought 8, SPEI01 and SPEI12 compared 

ggplot(drought8_df_gathered, aes(Date, Werte)) + 
  geom_line(color="black") + 
  labs(title = "SPEI01 and SPEI12 compared for different regional extent",
       subtitle = "Complete country vs. drought region 8 only",
       y = "SPEI", x ="") + 
  facet_wrap(~SPEI, strip.position = "top", ncol = 2, nrow = 2) + 
  theme_bw() +
  theme(axis.text.x = element_text(angle=50, hjust=1)) +
  geom_hline(aes(yintercept = 0), color = "#FF6633", linetype = "longdash")

  






### Drought 8 with prolonged time frame ========================================


#SPEI12 time series for drought 8 with +- 6 months

spei12_wmean_8_region_long <-  timeseries(spei12, drought_8.shp, start = which(time == as.Date("2015-07-16")), end = which(time == as.Date("2017-07-16")))
plot(spei12_wmean_8_region_long, type = "l")

drought8_long_df <- data.frame(spei12_wmean_8_region_long)

drought8_long_df$Date <- time[187:211]
drought8_long_df

names(drought8_long_df) <- c("SPEI12", "Date")

names(drought8_long_df)



### Plot: Drought 8 with prolonged time frame

ggplot() + 
  geom_rect(aes(xmin=as.Date("2016-01-16"), xmax=as.Date("2017-01-16")), 
            ymin=-Inf, ymax=Inf, fill="#FF6633", alpha=0.3, col="#FF6633") +
  geom_line(data = drought8_long_df, aes(x=Date, y=spei12_wmean_8_region_long), color = "black") +
  labs(title = "SPEI12 for drought event 8",
       subtitle = "+/- 6 months",
       y = "SPEI12", x ="") +
  theme_bw() +
  geom_hline(aes(yintercept = 0), color = "#FF6633", linetype = "longdash") +
  theme(axis.text.x = element_text(angle=50, hjust=1))





