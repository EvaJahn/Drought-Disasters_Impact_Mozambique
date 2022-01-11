################################################################################
### Levelplots of SPEI for Mozambique
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


### Load data ==================================================================

# load .nc file as raster & subset to time frame 2000-2018 

spei <- subset(brick(choose.files()), 1189:1416)


# load all shapefiles
shps <- dir(getwd(), "*.shp")

for (shp in shps) {
  assign(shp, readOGR(shp))
}



### Functions ==================================================================

# crop to study area
crop_to_area <- function(SPEI, shape)  {
  cropped <- crop(SPEI, shape)
  masked <- mask(cropped, shape)
  plot(masked)
  return(masked)
}






# create levelplots for drought region and period
# Input: SPEI, region, start and end date, title for plot
my_levelplot <- function(SPEI, shape, start, end, title)  {
  
  # crop to study area
  cropped <- crop(SPEI, shape)
  masked <- mask(cropped, shape)
  
  # subset to time span
  drought_event <- subset(masked, start:end) # Zeitpunkt Start und Ende)
  
  # Levelplot 
  p <- rasterVis::levelplot(drought_event,
                       par.settings=RdBuTheme,
                       main=title,
                       scales =list(draw=FALSE),                
                       layout=c(4,4),
                       names.attr=rasterNames[start:end],
                       at=my.at,
                       xlim=c(30,41),
                       ylim=c(-27,-10.5),
                       margin=FALSE)
  
  # add shape of complete country Mozambique, for comparison
  p + latticeExtra::layer(sp.polygons(gadm36_MOZ_0.shp, lwd=0.8, col='grey'))

}








### Levelplots =================================================================

# crop to study area
speiMOZ <- crop_to_area(spei, gadm36_MOZ_0.shp)


# get dates from raster layers
idx <- names(spei)
time <- as.Date(substring(idx, 2, 11), format = "%Y.%m.%d")


# change layer names for plot
rasterNames <- gsub("X", "", names(spei))
rasterNames <- ymd(rasterNames)
rasterNames <- strtrim(rasterNames, 7)
rasterNames




### adjust scaling for plot
max = speiMOZ[speiMOZ > 3]=3
min = speiMOZ[speiMOZ < (-3)]=(-3)
my.at <- seq(min, max, by = 0.25)





##### Levelplots for each drought event
###
# (to change SPEI accumulation period --> adjust script above!)
###

plot_drought1 <- my_levelplot(speiMOZ, gadm36_MOZ_0.shp, start = which(time == as.Date("2001-01-16")), end = which(time == as.Date("2001-12-16")),
                              title = "SPEI01 - Drought event 1")
plot_drought1 + latticeExtra::layer(sp.polygons(drought_1.shp, lwd=0.7, col='black'))


plot_drought2 <- my_levelplot(speiMOZ, gadm36_MOZ_0.shp, start = which(time == as.Date("2002-03-16")), end = which(time == as.Date("2002-12-16")),
                              title = "SPEI01 - Drought event 2")
plot_drought2 + latticeExtra::layer(sp.polygons(drought_2.shp, lwd=0.7, col='black'))


plot_drought3 <- my_levelplot(speiMOZ, gadm36_MOZ_0.shp, start = which(time == as.Date("2003-01-16")), end = which(time == as.Date("2003-01-16")),
                              title = "SPEI01 - Drought event 3: 2003-01")
plot_drought3 + latticeExtra::layer(sp.polygons(drought_3.shp, lwd=0.9, col='black'))


plot_drought4 <- my_levelplot(speiMOZ, gadm36_MOZ_0.shp, start = which(time == as.Date("2005-05-16")), end = which(time == as.Date("2006-12-16")),
                              title = "SPEI01 - Drought event 4")
plot_drought4 + latticeExtra::layer(sp.polygons(drought_4.shp, lwd=0.7, col='black'))      


plot_drought5 <- my_levelplot(speiMOZ, gadm36_MOZ_0.shp, start = which(time == as.Date("2007-08-16")), end = which(time == as.Date("2007-08-16")),
                              title = "SPEI01 - Drought event 5: 2007-08")
plot_drought5 + latticeExtra::layer(sp.polygons(drought_5.shp, lwd=0.9, col='black'))


plot_drought6 <- my_levelplot(speiMOZ, gadm36_MOZ_0.shp, start = which(time == as.Date("2008-12-16")), end = which(time == as.Date("2009-12-16")),
                              title = "SPEI01 - Drought event 6")
plot_drought6 + latticeExtra::layer(sp.polygons(drought_6.shp, lwd=0.7, col='black'))


plot_drought7 <- my_levelplot(speiMOZ, gadm36_MOZ_0.shp, start = which(time == as.Date("2010-03-16")), end = which(time == as.Date("2010-12-16")),
                              title = "SPEI01 - Drought event 7")
plot_drought7 + latticeExtra::layer(sp.polygons(drought_7.shp, lwd=0.7, col='black'))


plot_drought8 <- my_levelplot(speiMOZ, gadm36_MOZ_0.shp, start = which(time == as.Date("2016-01-16")), end = which(time == as.Date("2017-01-16")),
                              title = "SPEI01 - Drought event 8")
plot_drought8 + latticeExtra::layer(sp.polygons(drought_8.shp, lwd=0.7, col='black'))





##### Levelplot for complete time span since 2000 
pdf(file="spei01_complete_timespan.pdf", width = 9, height = 12)
print(plot_complete_timespan <- my_levelplot(speiMOZ, gadm36_MOZ_0.shp, start = which(time == as.Date("2000-01-16")), end = which(time == as.Date("2018-12-16")),
                              title = "SPEI01"))
dev.off()




