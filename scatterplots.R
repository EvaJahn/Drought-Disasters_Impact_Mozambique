################################################################################
### Scatter plots 
# EM-DAT number of people affected  vs.  SPEI12 (different parameters) 
# - mean value of each individual drought event
# - duration of each drought event 
# - accumulated moisture deficit of each drought event
################################################################################

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

### load data ==================================================================

spei12 <- subset(brick(choose.files()), 1189:1416)


shps <- dir(getwd(), "*.shp")

for (shp in shps) {
  assign(shp, readOGR(shp))
}

shapeMOZ <- readOGR("D:/R Data and Script", "gadm36_MOZ_0")


### crop to country of Mozambique

crop_to_area <- function(SPEI, shape)  {
  cropped <- crop(SPEI, shape)
  masked <- mask(cropped, shape)
  plot(masked)
  return(masked)
}

spei12_cropped <- crop_to_area(spei12, shapeMOZ)








### Data preparation============================================================

### EM-DAT number of people affected 
affected <- c(100000, 600000, 119500, 1400000, 520000, 500000, 460000, 2300000)

### EM-DAT drought duration in months
duration <- c(12, 10, 1, 20, 1, 13, 10, 13)

### SPEI12, drought region, time series for each drought event

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
  spei12_wmean <- cellStats(area_weighted_spei, sum)
}

# get date from raster layers
idx <- names(spei12)
time <- as.Date(substring(idx, 2, 11), format = "%Y.%m.%d")


ts_dr1 <- timeseries(spei12, drought_1.shp, start = which(time == as.Date("2001-01-16")), end = which(time == as.Date("2001-12-16")))

ts_dr2 <- timeseries(spei12, drought_2.shp, start = which(time == as.Date("2002-03-16")), end = which(time == as.Date("2002-12-16")))

ts_dr3 <- timeseries(spei12, drought_3.shp,  start = which(time == as.Date("2003-01-16")), end = which(time == as.Date("2003-01-16")))

ts_dr4 <- timeseries(spei12, drought_4.shp, start = which(time == as.Date("2005-05-16")), end = which(time == as.Date("2006-12-16")))

ts_dr5 <- timeseries(spei12, drought_5.shp, start = which(time == as.Date("2007-08-16")), end = which(time == as.Date("2007-08-16")))

ts_dr6 <- timeseries(spei12, drought_6.shp, start = which(time == as.Date("2008-12-16")), end = which(time == as.Date("2009-12-16")))

ts_dr7 <- timeseries(spei12, drought_7.shp, start = which(time == as.Date("2010-03-16")), end = which(time == as.Date("2010-12-16")))

ts_dr8 <- timeseries(spei12, drought_8.shp, start = which(time == as.Date("2016-01-16")), end = which(time == as.Date("2017-01-16")))



### SPEI12, complete MOZ, time series for each drought event  

#ts_dr1_MOZ <- timeseries(spei12, shapeMOZ, start = which(time == as.Date("2001-01-16")), end = which(time == as.Date("2001-12-16")))

#ts_dr2_MOZ <- timeseries(spei12, shapeMOZ, start = which(time == as.Date("2002-03-16")), end = which(time == as.Date("2002-12-16")))

#ts_dr3_MOZ <- timeseries(spei12, shapeMOZ,  start = which(time == as.Date("2003-01-16")), end = which(time == as.Date("2003-01-16")))

#ts_dr4_MOZ <- timeseries(spei12, shapeMOZ, start = which(time == as.Date("2005-05-16")), end = which(time == as.Date("2006-12-16")))

#ts_dr5_MOZ <- timeseries(spei12, shapeMOZ, start = which(time == as.Date("2007-08-16")), end = which(time == as.Date("2007-08-16")))

#ts_dr6_MOZ <- timeseries(spei12, shapeMOZ, start = which(time == as.Date("2008-12-16")), end = which(time == as.Date("2009-12-16")))

#ts_dr7_MOZ <- timeseries(spei12, shapeMOZ, start = which(time == as.Date("2010-03-16")), end = which(time == as.Date("2010-12-16")))

#ts_dr8_MOZ <- timeseries(spei12, shapeMOZ, start = which(time == as.Date("2016-01-16")), end = which(time == as.Date("2017-01-16")))



### mean values of each time series (drought region)

mean_dr1 <- mean(ts_dr1)
mean_dr2 <- mean(ts_dr2)
mean_dr3 <- mean(ts_dr3)
mean_dr4 <- mean(ts_dr4)
mean_dr5 <- mean(ts_dr5)
mean_dr6 <- mean(ts_dr6)
mean_dr7 <- mean(ts_dr7)
mean_dr8 <- mean(ts_dr8)


### mean values of each time series (complete MOZ)

#mean_dr1_MOZ <- mean(ts_dr1_MOZ)
#mean_dr2_MOZ <- mean(ts_dr2_MOZ)
#mean_dr3_MOZ <- mean(ts_dr3_MOZ)
#mean_dr4_MOZ <- mean(ts_dr4_MOZ)
#mean_dr5_MOZ <- mean(ts_dr5_MOZ)
#mean_dr6_MOZ <- mean(ts_dr6_MOZ)
#mean_dr7_MOZ <- mean(ts_dr7_MOZ)
#mean_dr8_MOZ <- mean(ts_dr8_MOZ)





### save in dataframe

mean_regional <- c(mean_dr1, mean_dr2, mean_dr3, mean_dr4, mean_dr5, mean_dr6, mean_dr7, mean_dr8)

#mean_MOZ <- c(mean_dr1_MOZ, mean_dr2_MOZ, mean_dr3_MOZ, mean_dr4_MOZ, mean_dr5_MOZ, mean_dr6_MOZ, mean_dr7_MOZ, mean_dr8_MOZ)

dat <- data.frame(affected, mean_regional)
dat





##### Plot: meanSPEI and number of people affected

ggplot() +
  geom_point(data = dat, aes(x=affected, y=mean_regional)) +
  theme_bw() + 
  labs(x= "Number of people affected (EM-DAT)",
       y= "Mean SPEI12",
       title = "Mean SPEI12 and number of people affected",
       subtitle = "For each drought event; mean SPEI for the respective drought region") +
  geom_hline(aes(yintercept = 0), color = "#FF6633", linetype = "longdash")





##### Plot: duration and number of people affected

dat2 <- data.frame(affected, duration)
dat2

ggplot() +
  geom_point(data = dat2, aes(x=affected, y=duration)) +
  theme_bw() + 
  labs(x= "Number of people affected (EM-DAT)",
       y= "Duration of drought event (months)",
       title = "Duration of drought event and number of people affected",
       subtitle = "For each drought event; duration in months")
  



##### Plot: accumulated moisture deficit and number of people affected

# SPEI values for each drought event summed up 

dr1_acc <- sum(ts_dr1)
dr2_acc <- sum(ts_dr2)
dr3_acc <- sum(ts_dr3)
dr4_acc <- sum(ts_dr4)
dr5_acc <- sum(ts_dr5)
dr6_acc <- sum(ts_dr6)
dr7_acc <- sum(ts_dr7)
dr8_acc <- sum(ts_dr8)


accumulated_def <- c(dr1_acc, dr2_acc, dr3_acc, dr4_acc, dr5_acc, dr6_acc, dr7_acc, dr8_acc)

dat3 <- data.frame(affected, accumulated_def)
dat3

ggplot() +
  geom_point(data = dat3, aes(x=affected, y=accumulated_def)) +
  theme_bw() + 
  labs(x= "Number of people affected (EM-DAT)",
       y= "Accumulated SPEI deficit",
       title = "Accumulated moisture deficit (SPEI) and number of people affected",
       subtitle = "For each drought event") +
  geom_hline(aes(yintercept = 0), color = "#FF6633", linetype = "longdash")


