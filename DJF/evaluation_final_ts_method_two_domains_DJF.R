#install.packages("readxl")
library(sirad)
library(xlsx)
library(readxl)
library(openxlsx)
library(lubridate)
library(dplyr)

# getting the names of the sheet
sheets <- getSheetNames("E:/Eeshan/evaluation/NACP_UrbanC_two_domains.xlsx")

# the final data frame where everything will be saved
finalDf <- data.frame()

for(j in 1:length(sheets)){
  
  # reading the data
  df <- read_excel("E:/Eeshan/evaluation/NACP_UrbanC_two_domains.xlsx",
                   sheet = j,
                   col_types = c('date','numeric','numeric','numeric','numeric','numeric',
                                 'numeric','numeric','numeric','numeric'))
  
  # reading the data
  df_ts <- read_excel("E:/Eeshan/evaluation/background_CO2.xlsx",
                      sheet = 'background',
                      col_types = c('date','numeric'))
  
  # reading in data for specific months
  df_DJF <- df %>% 
    filter(df$Time > as.POSIXct("2014-11-30 23:00:00", tz="UTC") & df$Time < as.POSIXct("2015-03-01 00:00:00", tz="UTC"))
  
  # reading in background concentration data for specific months
  df_DJF_ts <- df_ts %>% 
    filter(df_ts$Time > as.POSIXct("2014-11-30 23:00:00", tz="UTC") & df_ts$Time < as.POSIXct("2015-03-01 00:00:00", tz="UTC"))  
  
  time <- df_DJF[, 1]
  df_without_time <- df_DJF[, -1]
  
  df_ts_without_time <- df_DJF_ts[, -1]
  Obs<- df_without_time$Obs - df_ts_without_time
  EDGAR <- df_without_time$EDGAR - df_ts_without_time
  FFDAS <- df_without_time$FFDAS - df_ts_without_time
  ODIAC <- df_without_time$ODIAC - df_ts_without_time
  VULCAN <- df_without_time$VULCAN - df_ts_without_time
  EDGAR_d02 <- df_without_time$EDGAR_d02 - df_ts_without_time
  FFDAS_d02 <- df_without_time$FFDAS_d02 - df_ts_without_time
  ODIAC_d02 <- df_without_time$ODIAC_d02 - df_ts_without_time
  VULCAN_d02 <- df_without_time$VULCAN_d02 - df_ts_without_time
  
  df_final <- cbind(Obs,EDGAR,FFDAS,ODIAC,VULCAN,EDGAR_d02,FFDAS_d02,ODIAC_d02,VULCAN_d02)
  
  colnames(df_final) <- c('Obs','EDGAR', 'FFDAS', 'ODIAC', 'VULCAN', 'EDGAR_d02', 'FFDAS_d02', 'ODIAC_d02', 'VULCAN_d02')
  
  df_final$ODIAC[df_final$ODIAC < 0] <- NA # removing negative values
  df_final$ODIAC_d02[df_final$ODIAC_d02 < 0] <- NA # removing negative values
  df_final$Obs[df_final$Obs < 0] <- NA # removing negative values
  df_final$EDGAR[df_final$EDGAR > 40] <- NA # removing negative values
  df_final$EDGAR[df_final$EDGAR < 0] <- NA # removing negative values
  df_final$EDGAR_d02[df_final$EDGAR_d02 < 0] <- NA # removing negative values
  
  options(digits = 4) #changing the decimal places 
  df_DJF <- data.frame(time, df_final)
  
  # removing the na values
  df_DJF <- na.omit(df_DJF)
  
  # creating an empty dataframe
  statDf <- data.frame()
  
  # d01  ##### 
  
  columns <- colnames(df_DJF)
  obs <- df_DJF[,2]
  obs <- dplyr::pull(df_DJF, 2) #changing it to numeric
  EDGAR <- df_DJF[,3]
  EDGAR <- dplyr::pull(df_DJF,3)
  
  statsReady <- modeval(measured = obs,
                        calculated = EDGAR,
                        stat=c("N","pearson","MBE","RMBE","MAE","RMAE","RMSE","RRMSE","R2","slope",
                               "intercept","EF","SD","CRM","MPE","AC","ACu","ACs"))
  i <- 1
  
  statDf[i,1] <- paste(sheets[j],'_DJF_d01')
  statDf[i,2] <- "EDGAR_d01"
  statDf[i,3] <- statsReady[[1]] #N
  statDf[i,4] <- format(mean(obs), nsmall = 2, digits = 2)
  statDf[i,5] <- format(mean(EDGAR), nsmall = 2, digits = 2)
  statDf[i,6] <- format(round(statsReady[[2]], 2), nsmall = 2, digits = 2) #R/Corr
  statDf[i,7] <- format(round(statsReady[[3]], 3), nsmall = 2, digits = 2) #MBE
  statDf[i,8] <- format(round(statsReady[[3]]*length(obs)/sum(obs)*100, 3), nsmall = 2, digits = 2) #NMBE
  statDf[i,9] <- format(round(statsReady[[5]], 3), nsmall = 2, digits = 2) #MAE
  statDf[i,10] <- format(round(statsReady[[5]]*length(obs)/sum(obs)*100, 3), nsmall = 2, digits = 2) #NMAE
  statDf[i,11] <- format(round(statsReady[[7]], 3), nsmall = 2, digits = 2) #RMSE
  
  colnames(statDf) <- c('site','dataset','#obs','obs','sim','r','MB(ppm)','NMB(%)','MAE','NME(%)','RMSE(ppm)')
  
  i = i+1
  
  obs <- df_DJF[,2]
  FFDAS <- df_DJF[,4]
  df <- data.frame(obs, FFDAS)
  df <- na.omit(df)
  FFDAS <- dplyr::pull(df,2)
  obs <- dplyr::pull(df, 1) #changing it to numeric
  
  statsReady <- modeval(measured = obs,
                        calculated = FFDAS,
                        stat=c("N","pearson","MBE","RMBE","MAE","RMAE","RMSE","RRMSE","R2","slope",
                               "intercept","EF","SD","CRM","MPE","AC","ACu","ACs"))
  
  # statDf[i,1] <- sheets[j]
  statDf[i,2] <- "FFDAS_d01"
  statDf[i,3] <- statsReady[[1]] #N
  statDf[i,4] <- format(mean(obs), nsmall = 2, digits = 2)
  statDf[i,5] <- format(mean(FFDAS), nsmall = 2, digits = 2)
  statDf[i,6] <- format(round(statsReady[[2]], 2), nsmall = 2, digits = 2) #R/Corr
  statDf[i,7] <- format(round(statsReady[[3]], 3), nsmall = 2, digits = 2) #MBE
  statDf[i,8] <- format(round(statsReady[[3]]*length(obs)/sum(obs)*100, 3), nsmall = 2, digits = 2) #NMBE
  statDf[i,9] <- format(round(statsReady[[5]], 3), nsmall = 2, digits = 2) #MAE
  statDf[i,10] <- format(round(statsReady[[5]]*length(obs)/sum(obs)*100, 3), nsmall = 2, digits = 2) #NMAE
  statDf[i,11] <- format(round(statsReady[[7]], 3), nsmall = 2, digits = 2) #RMSE
  
  i = i+1
  
  obs <- df_DJF[,2]
  obs <- dplyr::pull(df_DJF, 2) #changing it to numeric
  ODIAC <- df_DJF[,5]
  ODIAC <- dplyr::pull(df_DJF,5)
  
  statsReady <- modeval(measured = obs,
                        calculated = ODIAC,
                        stat=c("N","pearson","MBE","RMBE","MAE","RMAE","RMSE","RRMSE","R2","slope",
                               "intercept","EF","SD","CRM","MPE","AC","ACu","ACs"))
  
  #statDf[i,1] <- sheets[j]
  statDf[i,2] <- "ODIAC_d01"
  statDf[i,3] <- statsReady[[1]] #N
  statDf[i,4] <- format(mean(obs), nsmall = 2, digits = 2)
  statDf[i,5] <- format(mean(ODIAC), nsmall = 2, digits = 2)
  statDf[i,6] <- format(round(statsReady[[2]], 2), nsmall = 2, digits = 2) #R/Corr
  statDf[i,7] <- format(round(statsReady[[3]], 3), nsmall = 2, digits = 2) #MBE
  statDf[i,8] <- format(round(statsReady[[3]]*length(obs)/sum(obs)*100, 3), nsmall = 2, digits = 2) #NMBE
  statDf[i,9] <- format(round(statsReady[[5]], 3), nsmall = 2, digits = 2) #MAE
  statDf[i,10] <- format(round(statsReady[[5]]*length(obs)/sum(obs)*100, 3), nsmall = 2, digits = 2) #NMAE
  statDf[i,11] <- format(round(statsReady[[7]], 3), nsmall = 2, digits = 2) #RMSE
  
  i = i+1
  
  obs <- df_DJF[,2]
  VULCAN <- df_DJF[,6]
  df <- data.frame(obs, VULCAN)
  df <- na.omit(df)
  VULCAN <- dplyr::pull(df,2)
  obs <- dplyr::pull(df, 1) #changing it to numeric
  
  statsReady <- modeval(measured = obs,
                        calculated = VULCAN,
                        stat=c("N","pearson","MBE","RMBE","MAE","RMAE","RMSE","RRMSE","R2","slope",
                               "intercept","EF","SD","CRM","MPE","AC","ACu","ACs"))
  
  #statDf[i,1] <- sheets[j]
  statDf[i,2] <- "VULCAN_d01"
  statDf[i,3] <- statsReady[[1]] #N
  statDf[i,4] <- format(mean(obs), nsmall = 2, digits = 2)
  statDf[i,5] <- format(mean(VULCAN), nsmall = 2, digits = 2)
  statDf[i,6] <- format(round(statsReady[[2]], 2), nsmall = 2, digits = 2) #R/Corr
  statDf[i,7] <- format(round(statsReady[[3]], 3), nsmall = 2, digits = 2) #MBE
  statDf[i,8] <- format(round(statsReady[[3]]*length(obs)/sum(obs)*100, 3), nsmall = 2, digits = 2) #NMBE
  statDf[i,9] <- format(round(statsReady[[5]], 3), nsmall = 2, digits = 2) #MAE
  statDf[i,10] <- format(round(statsReady[[5]]*length(obs)/sum(obs)*100, 3), nsmall = 2, digits = 2) #NMAE
  statDf[i,11] <- format(round(statsReady[[7]], 3), nsmall = 2, digits = 2) #RMSE
  
  i = i+1
  
  # d02  #####
  obs <- df_DJF[,2]
  obs <- dplyr::pull(df_DJF, 2) #changing it to numeric
  EDGAR_d02 <- df_DJF[,7]
  EDGAR_d02 <- dplyr::pull(df_DJF,7)
  
  statsReady <- modeval(measured = obs,
                        calculated = EDGAR_d02,
                        stat=c("N","pearson","MBE","RMBE","MAE","RMAE","RMSE","RRMSE","R2","slope",
                               "intercept","EF","SD","CRM","MPE","AC","ACu","ACs"))
  
  statDf[i,1] <- paste(sheets[j],'_DJF_d02')
  statDf[i,2] <- "EDGAR_d02"
  statDf[i,3] <- statsReady[[1]] #N
  statDf[i,4] <- format(mean(obs), nsmall = 2, digits = 2)
  statDf[i,5] <- format(mean(EDGAR_d02), nsmall = 2, digits = 2)
  statDf[i,6] <- format(round(statsReady[[2]], 2), nsmall = 2, digits = 2) #R/Corr
  statDf[i,7] <- format(round(statsReady[[3]], 3), nsmall = 2, digits = 2) #MBE
  statDf[i,8] <- format(round(statsReady[[3]]*length(obs)/sum(obs)*100, 3), nsmall = 2, digits = 2) #NMBE
  statDf[i,9] <- format(round(statsReady[[5]], 3), nsmall = 2, digits = 2) #MAE
  statDf[i,10] <- format(round(statsReady[[5]]*length(obs)/sum(obs)*100, 3), nsmall = 2, digits = 2) #NMAE
  statDf[i,11] <- format(round(statsReady[[7]], 3), nsmall = 2, digits = 2) #RMSE
  
  i = i+1
  
  obs <- df_DJF[,2]
  FFDAS_d02 <- df_DJF[,8]
  df <- data.frame(obs, FFDAS_d02)
  df <- na.omit(df)
  FFDAS_d02 <- dplyr::pull(df,2)
  obs <- dplyr::pull(df, 1) #changing it to numeric
  
  statsReady <- modeval(measured = obs,
                        calculated = FFDAS_d02,
                        stat=c("N","pearson","MBE","RMBE","MAE","RMAE","RMSE","RRMSE","R2","slope",
                               "intercept","EF","SD","CRM","MPE","AC","ACu","ACs"))
  
  #statDf[i,1] <- paste(sheets[j],'_DJF_d02')
  statDf[i,2] <- "FFDAS_d02"
  statDf[i,3] <- statsReady[[1]] #N
  statDf[i,4] <- format(mean(obs), nsmall = 2, digits = 2)
  statDf[i,5] <- format(mean(FFDAS_d02), nsmall = 2, digits = 2)
  statDf[i,6] <- format(round(statsReady[[2]], 2), nsmall = 2, digits = 2) #R/Corr
  statDf[i,7] <- format(round(statsReady[[3]], 3), nsmall = 2, digits = 2) #MBE
  statDf[i,8] <- format(round(statsReady[[3]]*length(obs)/sum(obs)*100, 3), nsmall = 2, digits = 2) #NMBE
  statDf[i,9] <- format(round(statsReady[[5]], 3), nsmall = 2, digits = 2) #MAE
  statDf[i,10] <- format(round(statsReady[[5]]*length(obs)/sum(obs)*100, 3), nsmall = 2, digits = 2) #NMAE
  statDf[i,11] <- format(round(statsReady[[7]], 3), nsmall = 2, digits = 2) #RMSE
  
  i = i+1
  
  obs <- df_DJF[,2]
  obs <- dplyr::pull(df_DJF, 2) #changing it to numeric
  ODIAC_d02 <- df_DJF[,9]
  ODIAC_d02 <- dplyr::pull(df_DJF,9)
  
  statsReady <- modeval(measured = obs,
                        calculated = ODIAC_d02,
                        stat=c("N","pearson","MBE","RMBE","MAE","RMAE","RMSE","RRMSE","R2","slope",
                               "intercept","EF","SD","CRM","MPE","AC","ACu","ACs"))
  
  #statDf[i,1] <- paste(sheets[j],'_DJF_d02')
  statDf[i,2] <- "ODIAC_d02"
  statDf[i,3] <- statsReady[[1]] #N
  statDf[i,4] <- format(mean(obs), nsmall = 2, digits = 2)
  statDf[i,5] <- format(mean(ODIAC_d02), nsmall = 2, digits = 2)
  statDf[i,6] <- format(round(statsReady[[2]], 2), nsmall = 2, digits = 2) #R/Corr
  statDf[i,7] <- format(round(statsReady[[3]], 3), nsmall = 2, digits = 2) #MBE
  statDf[i,8] <- format(round(statsReady[[3]]*length(obs)/sum(obs)*100, 3), nsmall = 2, digits = 2) #NMBE
  statDf[i,9] <- format(round(statsReady[[5]], 3), nsmall = 2, digits = 2) #MAE
  statDf[i,10] <- format(round(statsReady[[5]]*length(obs)/sum(obs)*100, 3), nsmall = 2, digits = 2) #NMAE
  statDf[i,11] <- format(round(statsReady[[7]], 3), nsmall = 2, digits = 2) #RMSE
  
  i = i+1
  
  obs <- df_DJF[,2]  
  VULCAN_d02 <- df_DJF[,10]
  df <- data.frame(obs, VULCAN_d02)
  df <- na.omit(df)
  VULCAN_d02 <- dplyr::pull(df,2)
  obs <- dplyr::pull(df, 1)
  
  statsReady <- modeval(measured = obs,
                        calculated = VULCAN_d02,
                        stat=c("N","pearson","MBE","RMBE","MAE","RMAE","RMSE","RRMSE","R2","slope",
                               "intercept","EF","SD","CRM","MPE","AC","ACu","ACs"))
  
  #statDf[i,1] <- paste(sheets[j],'_DJF_d02')
  statDf[i,2] <- "VULCAN_d02"
  statDf[i,3] <- statsReady[[1]] #N
  statDf[i,4] <- format(mean(obs), nsmall = 2, digits = 2)
  statDf[i,5] <- format(mean(VULCAN_d02), nsmall = 2, digits = 2)
  statDf[i,6] <- format(round(statsReady[[2]], 2), nsmall = 2, digits = 2) #R/Corr
  statDf[i,7] <- format(round(statsReady[[3]], 3), nsmall = 2, digits = 2) #MBE
  statDf[i,8] <- format(round(statsReady[[3]]*length(obs)/sum(obs)*100, 3), nsmall = 2, digits = 2) #NMBE
  statDf[i,9] <- format(round(statsReady[[5]], 3), nsmall = 2, digits = 2) #MAE
  statDf[i,10] <- format(round(statsReady[[5]]*length(obs)/sum(obs)*100, 3), nsmall = 2, digits = 2) #NMAE
  statDf[i,11] <- format(round(statsReady[[7]], 3), nsmall = 2, digits = 2) #RMSE
  
  i = i+1   
  
  print(sheets[j])
  
  finalDf <- rbind(finalDf,statDf)
}

finalDf$`#obs`<- as.numeric(finalDf$`#obs`)
finalDf$obs<-as.numeric(finalDf$obs)
finalDf$sim<-as.numeric(finalDf$sim)
finalDf$r<-as.numeric(finalDf$r)
finalDf$`MB(ppm)`<- as.numeric(finalDf$`MB(ppm)`)
finalDf$`NMB(%)`<- as.numeric(finalDf$`NMB(%)`)
finalDf$`NME(%)`<- as.numeric(finalDf$`NME(%)`)
finalDf$MAE<- as.numeric(finalDf$MAE)
finalDf$`RMSE(ppm)`<- as.numeric(finalDf$`RMSE(ppm)`)

write.xlsx2(finalDf, 
            file = "E:/Eeshan/evaluation/results/results_without_background_ts_DJF_d01_d02.xlsx", 
            sheetName= "2015", 
            append = TRUE)
