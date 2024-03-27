library(eixport)
library(raster)
library(stars)
library(cptcity)
library(sf)
library(rnaturalearth)

wrfo <- "E:/Eeshan/variables_ts/Avg.gas.VULCAN.2014-01.nc"

# reading the concerned variable from the file
carbon_dioxide <- wrf_get(wrfo,
                          "CO2_TST", 
                          as_raster = T)

# getting just the values in the first level
carbon_dioxide <- carbon_dioxide[[1]]

# changing the projection
carbon_dioxide <- projectRaster((carbon_dioxide), crs = '+proj=longlat +datum=WGS84 +no_defs')

# longitude and latitude information
ll_table <- read.csv("E:/Eeshan/evaluation/results/spatial_overlay_VULCAN_Jan_2014.csv")
ll_table <- ll_table[,c(1:2,4)]
colnames(ll_table) <- c('Longitude', 'Latitude','Mixing_ratio')

# data.frame containing the lat, long and mixing ratio values
xyz <- data.frame(latitude = ll_table$Latitude,
                  longitude = ll_table$Longitude,
                  mixing_ratio = ll_table$Mixing_ratio)

xyzSP <- SpatialPointsDataFrame(coords = xyz[,1:2], 
                                data = xyz, 
                                proj4string = CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"))

xyzSP$Z <- carbon_dioxide[xyzSP,]

ncolors <- 200

myColorDiff <- colorRampPalette(c("blue","green","yellow","orange","red","purple"))
xyzSP$color <- myColorDiff(ncolors)[(xyzSP$mixing_ratio - minValue(carbon_dioxide))/
                                         (maxValue(carbon_dioxide)- minValue(carbon_dioxide)) * ncolors]

plot(carbon_dioxide, 
     col = myColorDiff(ncolors), 
     interpolate = T,
     legend.width=1, 
     legend.shrink=1,
     axes = F)
maps::map("world", add=TRUE, lwd=1, interior = FALSE, col = "black")
maps::map("usa", add=TRUE, lwd=1, interior = FALSE, col = "grey")
maps::map("state", add=TRUE, lwd=1)
plot(xyzSP, 
     add = TRUE, 
     bg = xyzSP$color, 
     pch = 21, 
     cex = 1.5)
title(expression(ppm),adj = 1,cex = 2)
title(expression(CO2),adj = 0,cex = 2)
title('VULCAN Jan 2012')

## Set Axis
interval <- 10
vet_lat <- seq(-180, 180, by = interval)
lab_lat <- c(
  paste0(seq(180, interval, by = -interval), "\U00B0", "W"),
  "0", paste0(seq(interval, 180, by = interval), "\U00B0", "W")
)
interval <- 5
vet_lon <- seq(-90, 90, by = interval)
lab_lon <- c(
  paste0(seq(90, interval, by = -interval), "\U00B0", "S"),
  "0", paste0(seq(interval, 90, by = interval), "\U00B0", "N")
)
axis(2, at = vet_lon, labels = lab_lon)
axis(1, at = vet_lat, labels = lab_lat)

## Grid
grid(col = "darkgrey", lty = 5, lw = 1.5)

# is important to use the variable in the model original projection or calculate from a array instead of a raster
mi <- paste('Min:', formatC(cellStats(carbon_dioxide,'min',na.rm=T),  digits = 2, format = "f"))
me <- paste('Mean:',formatC(cellStats(carbon_dioxide,'mean',na.rm=T), digits = 2, format = "f"))
ma <- paste('Max:', formatC(cellStats(carbon_dioxide,'max',na.rm=T),  digits = 2, format = "f"))
legend('bottomright',legend = c(mi,me,ma),
       xjust = 0.4,horiz = T,y.intersp=0.1,x.intersp=0.1,
       text.width=15)# this is important to set the size of the box



