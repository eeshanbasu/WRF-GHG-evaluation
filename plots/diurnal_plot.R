#install.packages("readxl")
#library(sirad)
library(xlsx)
library(readxl)
library(openxlsx)
library(lubridate)
library(dplyr)

# the final dataframe where everything will be saved
finalDf <- data.frame()
  
# reading the data
df <- read_excel("E:/Eeshan/evaluation/NACP_UrbanC_all_domains_combined.xlsx",
                 sheet = "TI",
                 col_types = c('date','numeric','numeric','numeric','numeric','numeric',
                               'numeric','numeric','numeric','numeric',
                               'numeric','numeric','numeric','numeric'))

# reading in data for specific months
df_Jan <- df %>% 
  filter(df$Time > as.POSIXct("2013-12-31 23:00:00", tz="UTC") & df$Time < as.POSIXct("2014-02-01 00:00:00", tz="UTC"))

# removing the na values
df_Jan <- na.omit(df_Jan)
df_Jan$Hour <- hour(df_Jan$Time)

## TEST 
TEST   <- as.data.frame(df_Jan)
result <- data.frame(hour = 0:23,
                     Obs = rep(NA,24),
                     EDGAR_d01 = rep(NA,24),
                     EDGAR_d02 = rep(NA,24),
                     EDGAR_d03 = rep(NA,24),
                     FFDAS_d01 = rep(NA,24),
                     FFDAS_d02 = rep(NA,24),  
                     FFDAS_d03 = rep(NA,24),
                     ODIAC_d01 = rep(NA,24),
                     ODIAC_d02 = rep(NA,24),
                     ODIAC_d03 = rep(NA,24),
                     VULCAN_d01 = rep(NA,24),
                     VULCAN_d02 = rep(NA,24),  
                     VULCAN_d03 = rep(NA,24),
                     stringsAsFactors = F)

for(j in 2:(ncol(TEST)-1)){
  for(i in 0:23){
    MEAN_local <- mean(TEST[TEST$Hour == i,j])
    print(i)
    print(j)
    print(MEAN_local)
    result[i+1,j] = MEAN_local
  }
}

write.xlsx(result, file = "E:/Eeshan/evaluation/results/diurnal_plot_three_domains.xlsx", sheetName="TI_Jan", append=TRUE)

# reading in data for specific months
df_July <- df %>% 
  filter(df$Time > as.POSIXct("2014-06-30 23:00:00", tz="UTC") & df$Time < as.POSIXct("2014-08-01 00:00:00", tz="UTC"))

# removing the na values
df_July <- na.omit(df_July)
df_July$Hour <- hour(df_July$Time)

## TEST 
TEST   <- as.data.frame(df_July)
result <- data.frame(hour = 0:23,
                     Obs = rep(NA,24),
                     EDGAR_d01 = rep(NA,24),
                     EDGAR_d02 = rep(NA,24),
                     EDGAR_d03 = rep(NA,24),
                     FFDAS_d01 = rep(NA,24),
                     FFDAS_d02 = rep(NA,24),  
                     FFDAS_d03 = rep(NA,24),
                     ODIAC_d01 = rep(NA,24),
                     ODIAC_d02 = rep(NA,24),
                     ODIAC_d03 = rep(NA,24),
                     VULCAN_d01 = rep(NA,24),
                     VULCAN_d02 = rep(NA,24),  
                     VULCAN_d03 = rep(NA,24),
                     stringsAsFactors = F)

for(j in 2:(ncol(TEST)-1)){
  for(i in 0:23){
    MEAN_local <- mean(TEST[TEST$Hour == i,j])
    print(i)
    print(j)
    print(MEAN_local)
    result[i+1,j] = MEAN_local
  }
}

write.xlsx(result, file = "E:/Eeshan/evaluation/results/diurnal_plot_three_domains.xlsx", sheetName="TI_July", append=TRUE)

# Draw first time series
plot(result$hour,                              
     result$Obs,
     type = "p",
     col = "black",
     pch = 19,
     xlim = c(0,23),
     ylim = c(400, 450),
     main = "Diurnal Plots Over Boston University for January",
     xlab = "Hour of the day",
     ylab = "Carbon Dioxide Mixing Ratio (ppm)")
# Draw second time series
# plots for EDGAR
lines(result$hour,                             
      result$EDGAR,
      type = "b",
      col = 'red',
      pch = 0)
lines(result$hour,                          
      result$EDGAR_d02,
      type = "b",
      col = 'red',
      pch = 1)
lines(result$hour,                          
      result$EDGAR_d03,
      type = "b",
      col = 'red',
      pch = 2)
# plots for FFDAS
lines(result$hour,                             
      result$FFDAS,
      type = "b",
      col = 'blue',
      pch = 3)
lines(result$hour,                          
      result$FFDAS_d02,
      type = "b",
      col = 'blue',
      pch = 4)
lines(result$hour,                          
      result$FFDAS_d03,
      type = "b",
      col = 'blue',
      pch = 5)
# plots for ODIAC
lines(result$hour,                             
      result$ODIAC,
      type = "b",
      col = 'green',
      pch = 6)
lines(result$hour,                          
      result$ODIAC_d02,
      type = "b",
      col = 'green',
      pch = 7)
lines(result$hour,                          
      result$ODIAC_d03,
      type = "b",
      col = 'green',
      pch = 8)
# plots for VULCAN
lines(result$hour,                             
      result$VULCAN,
      type = "b",
      col = 'grey',
      pch = 9)
lines(result$hour,                          
      result$VULCAN_d02,
      type = "b",
      col = 'grey',
      pch = 10)
lines(result$hour,                          
      result$VULCAN_d03,
      type = "b",
      col = 'grey',
      pch = 11)
# add legends to the plot with the line-type
legend("topleft",
       c("Obs","EDGAR_d01","EDGAR_d02","EDGAR_d03","FFDAS_d01","FFDAS_d02","FFDAS_d03","ODIAC_d01","ODIAC_d02","ODIAC_d03","VULCAN_d01","VULCAN_d02","VULCAN_d03"),
       lty=c(NA,1,1,1,1,1,1,1,1,1,1,1,1),
       pch=c(19,0,1,2,3,4,5,6,7,8,9,10,11),
       cex=.5,
       pt.cex = 0.8,
       col=c("black","red","red","red","blue","blue","blue","green","green","green","grey","grey","grey"),
       horiz = T,
       y.intersp = 0.1,
       merge = FALSE)
       


