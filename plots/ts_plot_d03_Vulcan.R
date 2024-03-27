#install.packages("readxl")
library(sirad)
library(xlsx)
library(readxl)
library(openxlsx)
library(lubridate)
library(dplyr)

target_file <- "E:/Eeshan/evaluation/NACP_UrbanC_three_domains.xlsx"
destination_plot_folder <- "E:/Eeshan/ts_text_files/plots/time_series_plots/d03/July/2013/"

site_names <- getSheetNames(target_file)

for (site_name in site_names){
  
  df <- read_excel(target_file,
                   sheet = site_name,
                   col_types = c('date','numeric','numeric','numeric','numeric','numeric',
                                 'numeric','numeric','numeric','numeric',
                                 'numeric','numeric','numeric','numeric'))
  
  # reading in data for specific months
  df_July <- df %>% 
    filter(df$Time > as.POSIXct("2013-06-30 23:00:00", tz="UTC") & df$Time < as.POSIXct("2013-08-01 00:00:00", tz="UTC"))
  
  df <- as.data.frame(df_July)
  
  df <- as.data.frame(df_July)
  df_temp <- na.omit(df_July)
  
  if (dim(df_temp)[1] == 0) {
    # Skip the sheet if df_temp has dimensions of 0 rows and 6 columns
    next
  }
  
  png(filename = paste0(destination_plot_folder,"/",site_name,".Vulcan.png"),
      width = 870,
      height = 620)
  
  plot(df$Time,
       df$Obs,
       pch = 19, 
       col = 'black',
       xaxt = 'n',
       yaxt = 'n',
       ylim = range(range(df_temp[-1])[1] - 20, range(df_temp[-1])[2] + 20),
       ylab = '',
       xlab = '',
       main = paste0(site_name),
       cex.main = 1.5)
  
  # axis(1,cex.axis=1.2)
  mtext('Local Time, 2013', 
        side=1, 
        line=3.2, 
        cex=1.5)  
  
  axis(2, cex.axis=1.2, las = 1)
  mtext(expression('CO'[2]*' (ppm)'), 
        side=2, 
        line=2.6, 
        cex=1.5)
  
  axis.POSIXct(1, at = seq(from = as.POSIXct(df$Time[1]), 
                           to = as.POSIXct(df$Time[744]), length.out=7),
               #labels = seq(df$Time[1], df$Time[744], length.out=10),
               format = "%m/%d", las = 1,
               cex.axis = 1.2)
  
  lines(df$Time, 
        df$VULCAN,
        col = 'blue',
        lwd = 2)
  
  lines(df$Time, 
        df$VULCAN_d02,
        col = 'red',
        lwd = 2)
  
  lines(df$Time, 
        df$VULCAN_d03,
        col = 'green',
        lwd = 2)
  
  legend('topright',
         legend = c('Obs','36-km (d01)','6-km   (d02)','1-km   (d03)'),
         pch = c(19,NA_integer_,NA_integer_,NA_integer_),
         lty = c(0,1,1,1), 
         col = c('black','blue','red','green'),
         cex = 1.2,
         xjust = 1,
         yjust = 0.5)
  
  dev.off()
  
}