library(eixport)
library(raster)
library(stars)
library(cptcity)
library(sf)
library(rnaturalearth)
library(rgdal)
library(hackWRF)
library(openair)

results <- read.xlsx2('E:/Eeshan/evaluation/results/results_without_background_DJF.xlsx',
                      sheetName = "Paper")

## extract just the site names
results$sitenames <- gsub(".*\\((.*?)\\).*", "\\1", results$site)

# Drop the other columns and just keep the sitenames, the dataset and the MB columns
results <- results[, -c(1,3,4,5,7,8)]

colnames(results) <- c('dataset','MB','Sites')

# Replace blank values with NA
results$Sites <- ifelse(results$Sites == "", NA, results$Sites)

# Replace missing values with previous non-missing value
results$Sites <- na.locf(results$Sites)

# read the site lat lon
site_list <- read.csv('E:/Eeshan/evaluation/site_information.csv')
site_list <- site_list[,-c(4,5)]
colnames(site_list) <- c('Latitude','Longitude','Sites')

# Capitalize all letters in col1
site_list$Sites <- chartr("abcdefghijklmnopqrstuvwxyz", "ABCDEFGHIJKLMNOPQRSTUVWXYZ", site_list$Sites)

# Join dataframes based on the common column "ID"
final_df <- merge(results, 
                  site_list, 
                  by = "Sites", 
                  all = TRUE)

# Split the dataframe into separate dataframes based on the values in the 'Category' column
df_EDGAR <- subset(final_df, final_df$dataset == "EDGAR")
df_ODIAC <- subset(final_df, final_df$dataset == "ODIAC")
df_FFDAS <- subset(final_df, final_df$dataset == "FFDAS")
df_VULCAN <- subset(final_df, final_df$dataset == "VULCAN")

# Replace blank values with NA
df_FFDAS$MB <- ifelse(df_FFDAS$MB == "", NA, df_FFDAS$MB)
df_FFDAS <- na.omit(df_FFDAS)
df_VULCAN$MB <- ifelse(df_VULCAN$MB == "", NA, df_VULCAN$MB)
df_VULCAN <- na.omit(df_VULCAN)

df_EDGAR$MB <- as.numeric(df_EDGAR$MB)

# data.frame containing the lat, long and MB values
dataset_MB <- data.frame(latitude = df_EDGAR$Latitude,
                         longitude = df_EDGAR$Longitude,
                         MB = df_EDGAR$MB)

# range for Q2 is (-1,1)
# setting the range
conLim <- c(-30,30)
inp <- dataset_MB
## Rename the columns
colnames(inp) <- c("y", "x", "z")

## Rearrenge Colors to match
conLimRange <- conLim[2] - conLim[1]
new_color <- colorRampPalette(c("#0000FF",'#FFFFFF',"#FF0000"))
fColor <- new_color(11)
binRange <- conLimRange / length(fColor)

fColorTemp <- c("")

# binRange
for (ii in 1:nrow(inp)) { #nrow(inp)
  # print(ii)
  for (jj in 1:length(fColor)) {
    # print(jj)
    if (inp$z[ii] >= binRange * (jj - 1) + conLim[1]  && inp$z[ii] < binRange * jj  + conLim[1]) {
      print(paste(binRange * (jj - 1)+ conLim[1], "-", (binRange * jj)+ conLim[1] ))
      print(inp$z[ii])
      print(jj)
      # stop()
      fColorTemp[ii] <- fColor[jj]
      break
    } else if (inp$z[ii] >= conLim[2]) {
      fColorTemp[ii] <- fColor[length(fColor)]
    } else if (inp$z[ii] <= conLim[1]) {
      fColorTemp[ii] <- fColor[1]
    }
  }
}

## Make spatial points
coordinates(inp) <- ~ x + y
proj4string(inp) <- CRS("+init=epsg:4326")
inp_lcc <- sp::spTransform(inp, CRSobj = "+proj=lcc +lat_1=33 +lat_2=45 +lat_0=39.3034477233887 +lon_0=-97.6499862670898 +x_0=0 +y_0=0 +a=6370000 +b=6370000 +units=m +no_defs")

r <- readRDS("E:/Eeshan/METAR/empty_raster.Rds")
r <- projectRaster(r, crs = "+proj=lcc +lat_1=33 +lat_2=45 +lat_0=39.3034477233887 +lon_0=-97.6499862670898 +x_0=0 +y_0=0 +a=6370000 +b=6370000 +units=m +no_defs")

##################### create usa and world map in lcc cord #####################
### usa ###
usa <- maps::map(database = "state", regions = ".", fill = TRUE)
dev.off()
IDs <- sapply(strsplit(usa$names, ":"), function(x) x[1])
usa <- maptools::map2SpatialPolygons(usa, IDs = IDs, proj4string = sp::CRS("+proj=longlat +datum=WGS84"))
usa_lcc <- sp::spTransform(x = usa, CRSobj = "+proj=lcc +lat_1=33 +lat_2=45 +lat_0=39.3034477233887 +lon_0=-97.6499862670898 +x_0=0 +y_0=0 +a=6370000 +b=6370000 +units=m +no_defs")

### world ###

world <- maps::map(database = "world", regions = ".", fill = TRUE)
dev.off()
IDs_w <- sapply(strsplit(world$names, ":"), function(x) x[1])
world <- maptools::map2SpatialPolygons(world, IDs = IDs_w, proj4string = sp::CRS("+proj=longlat +datum=WGS84"))
world_lcc <- sp::spTransform(x = world, CRSobj = "+proj=lcc +lat_1=33 +lat_2=45 +lat_0=39.3034477233887 +lon_0=-97.6499862670898 +x_0=0 +y_0=0 +a=6370000 +b=6370000 +units=m +no_defs")

############################ calculate output ratio ############################

x_min <- -2551512
x_max <- 2542888
y_min <- -1083528
y_max <- 1092472
ratio <- (x_max - x_min) / (y_max - y_min)
height_value <- 1000

############################### make axis labels ###############################
### x ###

x_axis <- data.frame(c(x_min), seq(y_min, y_max, by = 100))
colnames(x_axis) <- c("x", "y")
sp::coordinates(x_axis) <- ~ x + y
sp::proj4string(x_axis) <- sp::CRS("+proj=lcc +lat_1=33 +lat_2=45 +lat_0=39.3034477233887 +lon_0=-97.6499862670898 +x_0=0 +y_0=0 +a=6370000 +b=6370000 +units=m +no_defs")
x_axis_latlon <- sp::spTransform(x = x_axis, CRSobj = "+init=epsg:4326")
x_axis_latlon_df <- as.data.frame(x_axis_latlon)
index_n30 <- which.min(abs(x_axis_latlon_df[, 2] - 30))
index_n40 <- which.min(abs(x_axis_latlon_df[, 2] - 40))
index_n50 <- which.min(abs(x_axis_latlon_df[, 2] - 50))
x_axis_lcc_intervals <- x_axis_latlon_df[c(index_n30, index_n40, index_n50), ]
colnames(x_axis_lcc_intervals) <- c("lon", "lat")
sp::coordinates(x_axis_lcc_intervals) <- ~ lon + lat
sp::proj4string(x_axis_lcc_intervals) <- sp::CRS("+init=epsg:4326")
x_axis_lcc_intervals <- sp::spTransform(x = x_axis_lcc_intervals, CRSobj = "+proj=lcc +lat_1=33 +lat_2=45 +lat_0=39.3034477233887 +lon_0=-97.6499862670898 +x_0=0 +y_0=0 +a=6370000 +b=6370000 +units=m +no_defs")
x_axis_lcc_intervals_df <- as.data.frame(x_axis_lcc_intervals)

### y ###
y_axis <- data.frame(seq(x_min, x_max, by = 100), c(y_min))
colnames(y_axis) <- c("x", "y")
sp::coordinates(y_axis) <- ~ x + y
sp::proj4string(y_axis) <- sp::CRS("+proj=lcc +lat_1=33 +lat_2=45 +lat_0=39.3034477233887 +lon_0=-97.6499862670898 +x_0=0 +y_0=0 +a=6370000 +b=6370000 +units=m +no_defs")
y_axis_latlon <- sp::spTransform(x = y_axis, CRSobj = "+init=epsg:4326")
y_axis_latlon_df <- as.data.frame(y_axis_latlon)
index_w70 <- which.min(abs(y_axis_latlon_df[, 1] + 70))
index_w80 <- which.min(abs(y_axis_latlon_df[, 1] + 80))
index_w90 <- which.min(abs(y_axis_latlon_df[, 1] + 90))
index_w100 <- which.min(abs(y_axis_latlon_df[, 1] + 100))
index_w110 <- which.min(abs(y_axis_latlon_df[, 1] + 110))
index_w120 <- which.min(abs(y_axis_latlon_df[, 1] + 120))
y_axis_lcc_intervals <- y_axis_latlon_df[c(index_w70, index_w80, index_w90, index_w100, index_w110, index_w120), ]
colnames(y_axis_lcc_intervals) <- c("lon", "lat")
sp::coordinates(y_axis_lcc_intervals) <- ~ lon + lat
sp::proj4string(y_axis_lcc_intervals) <- sp::CRS("+init=epsg:4326")
y_axis_lcc_intervals <- sp::spTransform(x = y_axis_lcc_intervals, CRSobj = "+proj=lcc +lat_1=33 +lat_2=45 +lat_0=39.3034477233887 +lon_0=-97.6499862670898 +x_0=0 +y_0=0 +a=6370000 +b=6370000 +units=m +no_defs")
y_axis_lcc_intervals_df <- as.data.frame(y_axis_lcc_intervals)

############################ create the grid in lcc ############################

grid_lcc <- raster::raster(ext = raster::extent(-180, 0, 0, 90), res = c(10, 10))
grid_lcc <- raster::rasterToPolygons(grid_lcc)
grid_lcc <- sp::spTransform(grid_lcc, CRSobj = "+proj=lcc +lat_1=33 +lat_2=45 +lat_0=39.3034477233887 +lon_0=-97.6499862670898 +x_0=0 +y_0=0 +a=6370000 +b=6370000 +units=m +no_defs")

############################ plotting ############################
plot(r, 
     zlim=c(-30, 30),
     xlim = c(-2551512, 2542888),
     ylim = c(-1083528, 1092472),
     col = new_color(11),
     legend.width=1, 
     legend.shrink=1,
     axes = F)

## Add plot as a layer
raster::plot(inp_lcc, 
             bg = fColorTemp, 
             pch = 21, 
             cex = 1.5, 
             add = T)

raster::plot(usa_lcc, add = TRUE, lwd = 2)
raster::plot(world_lcc, add = TRUE, lwd = 0.5)
raster::plot(grid_lcc, add = TRUE, border = "#6e6e6e", lwd = 0.5, lty = c(3))
axis(2, at = c(-655128,512172,1692472), labels = paste0(c("30", "40", "50"), "\U00B0", "N"))
axis(1, at = c(1892488,807488,-229112,-1311412,-2393712), labels = paste0(c("80", "90", "100", "110", "120"), "\U00B0", "W"))

title(expression("ppm"),
      adj = 1,
      cex = 2.5)
title(expression('CO'[2]),
      adj = 0,
      cex = 2.5)


