#install.packages("readxl")
#library(sirad)
library(xlsx)
library(readxl)
library(openxlsx)
library(lubridate)
library(dplyr)

# getting the names of the sheet
sheets <- getSheetNames("E:/Eeshan/evaluation/NACP_UrbanC_three_domains.xlsx")

# the final dataframe where everything will be saved
finalDf <- data.frame()
  
for(s in 1:length(sheets)){

  # reading the data
  df <- read_excel("E:/Eeshan/evaluation/NACP_UrbanC_three_domains.xlsx",
                   sheet = s,
                   col_types = c('date','numeric','numeric','numeric','numeric','numeric',
                                 'numeric','numeric','numeric','numeric',
                                 'numeric','numeric','numeric','numeric'))
  
  # reading in data for specific months
  df_DJF <- df %>% 
    filter(df$Time > as.POSIXct("2013-11-30 23:00:00", tz="UTC") & df$Time < as.POSIXct("2014-03-01 00:00:00", tz="UTC"))
  
  # removing the na values
  df_DJF <- na.omit(df_DJF)
  df_DJF$Hour <- hour(df_DJF$Time)
  
  ## TEST 
  TEST   <- as.data.frame(df_DJF)
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
  
  print(sheets[j])

  
write.xlsx2(result, 
           file = "E:/Eeshan/evaluation/results/diurnal_plot_three_domains_seasonal.xlsx", 
           sheetName = paste0(sheets[s],' DJF'), 
           append=TRUE)

# reading in data for specific months
df_JJA <- df %>% 
  filter(df$Time > as.POSIXct("2014-05-31 23:00:00", tz="UTC") & df$Time < as.POSIXct("2014-09-01 00:00:00", tz="UTC"))

# removing the na values
df_JJA <- na.omit(df_JJA)
df_JJA$Hour <- hour(df_JJA$Time)

## TEST 
TEST   <- as.data.frame(df_JJA)
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

write.xlsx2(result, 
           file = "E:/Eeshan/evaluation/results/diurnal_plot_three_domains_seasonal.xlsx", 
           sheetName = paste0(sheets[s],' JJA'), 
           append=TRUE)

}


result <- read_excel("E:/Eeshan/evaluation/results/diurnal_plot_three_domains_seasonal.xlsx",
                      sheet = "Boston University (BU), MA DJF")

# Draw first time series
plot(result$hour,                              
     result$Obs,
     pch = 19,
     col = "black",
     xaxt = 'n',
     yaxt = 'n',
     xlim = c(0,23),
     ylim = c(400,460),
     main = "Diurnal Plot Over Boston University for DJF",
     xlab = "",
     ylab = "")

axis(1,cex.axis=1.2)
mtext('Hour', 
      side=1, 
      line=3.2, 
      cex=1.5)  

axis(2, 
     cex.axis=1.2, 
     las = 1)
mtext(expression('CO'[2]*' (ppm)'), 
      side=2, 
      line=2.6, 
      cex=1.5)

# Draw second time series
# plots for EDGAR
lines(result$hour,                             
      result$EDGAR_d01,
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
      result$FFDAS_d01,
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
      result$ODIAC_d01,
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
      result$VULCAN_d01,
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
       cex = 0.8,
       pt.cex = 0.8,
       col=c("black","red","red","red","blue","blue","blue","green","green","green","grey","grey","grey"),
       #horiz = T,
       y.intersp = 1.5,
       merge = FALSE,
       bg = 'transparent',
       ncol = 4)

