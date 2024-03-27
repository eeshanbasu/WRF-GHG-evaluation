library(eixport)
library(raster)
library(stars)
library(cptcity)
library(sf)
library(rnaturalearth)
library(rgdal)
library(hackWRF)
library(openair)
library(xlsx)
library(zoo)

results <- read.xlsx2('E:/Eeshan/evaluation/results/results_without_background_JJA.xlsx',
                      sheetName = "Paper")

## extract just the site names
results$sitenames <- gsub(".*\\((.*?)\\).*", "\\1", results$site)

# Drop the other columns and just keep the sitenames, the dataset and the MB columns
results <- results[, -c(1,3,4,7,8)]

colnames(results) <- c('dataset','r','MB','Sites')

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

final_df <- na.omit(final_df)

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

df_VULCAN$MB <- as.numeric(df_VULCAN$MB)
df_VULCAN$r <- as.numeric(df_VULCAN$r)

# data.frame containing the lat, long and MB values
dataset_MB <- data.frame(latitude = df_VULCAN$Latitude,
                         longitude = df_VULCAN$Longitude,
                         MB = df_VULCAN$MB,
                         r = df_VULCAN$r)

inp <- dataset_MB
## Rename the columns
colnames(inp) <- c("y", "x", "z", "R")

# Set the range for colors and shapes
conLim <- c(-30, 30)
shapeRange <- c(0.10, 0.30, 0.50, 0.70, 0.90)
shapeSymbols <- c(21, 22, 23, 24, 25)

# Rearrange Colors to match
conLimRange <- conLim[2] - conLim[1]
new_color <- colorRampPalette(c("#0000FF", "#FFFFFF", "#FF0000"))
fColor <- new_color(11)
binRange <- conLimRange / length(fColor)

# Assign shapes and colors
fColorTemp <- c("")
pchTemp <- c("")

# Set specific ranges and shapes
range1 <- c(0.1, 0.3)
range2 <- c(0.3, 0.5)
range3 <- c(0.5, 0.7)
range4 <- c(0.7, 0.9)
range5 <- c(0.9, 1.0)

# Corresponding shapes for each range
shape1 <- 21
shape2 <- 22
shape3 <- 23
shape4 <- 24
shape5 <- 25

# Assign shapes based on R values
for (ii in 1:nrow(inp)) {
  # Assign shape based on R value
  if (inp$R[ii] >= range1[1] && inp$R[ii] <= range1[2]) {
    pchTemp[ii] <- shape1
  } else if (inp$R[ii] >= range2[1] && inp$R[ii] <= range2[2]) {
    pchTemp[ii] <- shape2
  } else if (inp$R[ii] >= range3[1] && inp$R[ii] <= range3[2]) {
    pchTemp[ii] <- shape3
  } else if (inp$R[ii] >= range4[1] && inp$R[ii] <= range4[2]) {
    pchTemp[ii] <- shape4
  } else if (inp$R[ii] >= range5[1] && inp$R[ii] <= range5[2]) {
    pchTemp[ii] <- shape5
  }
  
  # Assign color based on MB value
  for (jj in 1:length(fColor)) {
    if (inp$z[ii] >= binRange * (jj - 1) + conLim[1] && inp$z[ii] < binRange * jj + conLim[1]) {
      fColorTemp[ii] <- fColor[jj]
      break
    } else if (inp$z[ii] >= conLim[2]) {
      fColorTemp[ii] <- fColor[length(fColor)]
    } else if (inp$z[ii] <= conLim[1]) {
      fColorTemp[ii] <- fColor[1]
    }
  }
}

pchTemp <- as.numeric(pchTemp)

## Make spatial points
coordinates(inp) <- ~ x + y
proj4string(inp) <- CRS("+init=epsg:4326")
inp_lcc <- sp::spTransform(inp, CRSobj = "+proj=lcc +lat_1=33 +lat_2=45 +lat_0=39.3034477233887 +lon_0=-97.6499862670898 +x_0=0 +y_0=0 +a=6370000 +b=6370000 +units=m +no_defs")

r <- readRDS("E:/Eeshan/METAR/empty_raster.Rds")
r <- projectRaster(r, crs = "+proj=lcc +lat_1=33 +lat_2=45 +lat_0=39.3034477233887 +lon_0=-97.6499862670898 +x_0=0 +y_0=0 +a=6370000 +b=6370000 +units=m +no_defs")

usa_lcc <- readRDS("C:/Users/basu.e/Downloads/important_R/data/usa_lcc.rds")
world_lcc <- readRDS("C:/Users/basu.e/Downloads/important_R/data/world_lcc.rds")
grid_lcc <- readRDS("C:/Users/basu.e/Downloads/important_R/data/grid_lcc.rds")

############################ calculate output ratio ############################
x_min <- -2551512
x_max <- 2542888
y_min <- -1083528
y_max <- 1092472
ratio <- (x_max - x_min) / (y_max - y_min)
height_value <- 1000

############################ plotting ############################
plot(r, 
     zlim=c(-30, 30),
     xlim = c(-2551512, 2542888),
     ylim = c(-1083528, 1092472),
     col = new_color(11),
     legend.width=0.8, 
     legend.shrink=1.0,
     axes = F)

## Add plot as a layer
raster::plot(inp_lcc, 
             bg = fColorTemp, 
             pch = pchTemp, 
             cex = 1.5, 
             add = T)

raster::plot(usa_lcc, add = TRUE, lwd = 2)
raster::plot(world_lcc, add = TRUE, lwd = 0.5)
raster::plot(grid_lcc, add = TRUE, border = "#6e6e6e", lwd = 0.5, lty = c(3))
axis(2, at = c(-655128,512172,1692472), labels = paste0(c("30", "40", "50"), "\U00B0", "N"))
axis(1, at = c(1840000,807488,-229000,-1291412,-2353712), labels = paste0(c("80", "90", "100", "110", "120"), "\U00B0", "W"))

title(expression(atop("ppm")),
      adj = 1,
      cex = 2.5)
title(expression(atop('MB for CO'[2], paste('& R'))),
      adj = 0,
      cex = 2.5)

# Set specific ranges for each shape in the legend
legendRange1 <- c(0.1, 0.3)
legendRange2 <- c(0.3, 0.5)
legendRange3 <- c(0.5, 0.7)
legendRange4 <- c(0.7, 0.9)
legendRange5 <- c(0.9, 1.0)

# Create a function to get the range of R values for each shape in the legend
getLegendRange <- function(legendRange) {
  paste0(legendRange[1], " - ", legendRange[2])
}

# Create legend labels with range for each shape
legendLabels <- c(
  getLegendRange(legendRange1),
  getLegendRange(legendRange2),
  getLegendRange(legendRange3),
  getLegendRange(legendRange4),
  getLegendRange(legendRange5)
)

# Add legend for shapes
legend("bottomleft",
       legend = legendLabels,
       pch = c(shape1, shape2, shape3, shape4, shape5),
       cex = 0.9)

# Calculate min, max, and mean MB values
minMB <- min(dataset_MB$MB, na.rm = TRUE)
maxMB <- max(dataset_MB$MB, na.rm = TRUE)
meanMB <- mean(dataset_MB$MB, na.rm = TRUE)

# Create legend labels for min, max, and mean MB values
legendLabelsMB <- c(
  paste0("Min: ", minMB),
  paste0("Mean: ", round(meanMB, 2)),
  paste0("Max: ", maxMB)
  
)

# Add legend for MB values
legend("bottomright",
       legend = legendLabelsMB,
       pch = NA,
       horiz = T,
       cex = 0.8)  # No shapes for this legend




