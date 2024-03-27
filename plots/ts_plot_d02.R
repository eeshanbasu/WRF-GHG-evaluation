#install.packages("readxl")
library(sirad)
library(xlsx)
library(readxl)
library(openxlsx)
library(lubridate)
library(dplyr)

target_file <- "E:/Eeshan/evaluation/NACP_UrbanC_two_domains.xlsx"
destination_plot_folder <- "E:/Eeshan/ts_text_files/plots/time_series_plots/d02/July/2014/"

site_names <- getSheetNames(target_file)

for (site_name in site_names){
  
  df <- read_excel(target_file,
                   sheet = site_name,
                   col_types = c('date','numeric','numeric','numeric','numeric','numeric',
                                 'numeric','numeric','numeric','numeric'))
  
  # reading in data for specific months
  df_July <- df %>% 
    filter(df$Time > as.POSIXct("2014-06-30 23:00:00", tz="UTC") & df$Time < as.POSIXct("2014-08-01 00:00:00", tz="UTC"))
  
  df <- as.data.frame(df_July)
  
  png(filename = paste0(destination_plot_folder,"/",site_name,".png"),
      width = 870,
      height = 620)
  
  plot(df$Time,
       df$Obs,
       pch = 19, 
       col = 'black',
       xaxt = 'n',
       yaxt = 'n',
       ylim = range(350,550,na.rm = T),
       ylab = '',
       xlab = '',
       main = paste0(site_name),
       cex.main = 1.5)
  
  # axis(1,cex.axis=1.2)
  mtext('Local Time, 2014', 
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
        df$EDGAR,
        col = 'red',
        lwd = 2)
  
  lines(df$Time, 
        df$FFDAS,
        col = 'blue',
        lwd = 2)
  
  lines(df$Time, 
        df$ODIAC,
        col = 'grey',
        lwd = 2)
  
  lines(df$Time, 
        df$VULCAN,
        col = 'green',
        lwd = 2)
  
  lines(df$Time, 
        df$EDGAR_d02,
        col = 'red',
        lwd = 2,
        lty = 2)
  
  lines(df$Time, 
        df$FFDAS_d02,
        col = 'blue',
        lwd = 2,
        lty = 2)
  
  lines(df$Time, 
        df$ODIAC_d02,
        col = 'grey',
        lwd = 2,
        lty = 2)
  
  lines(df$Time, 
        df$VULCAN_d02,
        col = 'green',
        lwd = 2,
        lty = 2)
  
  
  legend('topright',
         legend = c('Obs','EDGAR',"FFDAS",'ODIAC','VULCAN','EDGAR_d02',"FFDAS_d02",'ODIAC_d02','VULCAN_d02'),
         pch = c(19,NA_integer_,NA_integer_,NA_integer_,NA_integer_,NA_integer_,NA_integer_,NA_integer_,NA_integer_),
         lty = c(0,1,1,1,1,2,2,2,2), 
         col = c('black','red','blue','grey','green','red','blue','grey','green'),
         cex = 1.2)
  
  dev.off()
  
}