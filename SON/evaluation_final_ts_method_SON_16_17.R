#install.packages("readxl")
library(sirad)
library(xlsx)
library(readxl)
library(openxlsx)
library(lubridate)
library(dplyr)

# getting the names of the sheet
sheets <- getSheetNames("E:/Eeshan/evaluation/CCGG_NACP_all_sites.xlsx")

# the final dataframe where everything will be saved
finalDf <- data.frame()

for(j in 1:length(sheets)){
  
  # reading the data
  df <- read_excel("E:/Eeshan/evaluation/CCGG_NACP_all_sites.xlsx",
                   sheet = j,
                   col_types = c('date','numeric','numeric','numeric','numeric','numeric'))
  
  # reading the data
  df_ts <- read_excel("E:/Eeshan/evaluation/background_CO2.xlsx",
                      sheet = 'background',
                      col_types = c('date','numeric'))
  
  # reading in data for specific months
  df_SON <- df %>% 
    filter(df$Time > as.POSIXct("2017-08-31 23:00:00", tz="UTC") & df$Time < as.POSIXct("2017-12-01 00:00:00", tz="UTC"))
  
  # reading in background concentration data for specific months
  df_SON_ts <- df_ts %>% 
    filter(df_ts$Time > as.POSIXct("2017-08-31 23:00:00", tz="UTC") & df_ts$Time < as.POSIXct("2017-12-01 00:00:00", tz="UTC"))  
  
  time <- df_SON[, 1]
  df_without_time <- df_SON[, -1]
  
  df_ts_without_time <- df_SON_ts[, -1]
  Obs<- df_without_time$Obs - df_ts_without_time
  EDGAR <- df_without_time$EDGAR - df_ts_without_time
  FFDAS <- df_without_time$FFDAS - df_ts_without_time
  ODIAC <- df_without_time$ODIAC - df_ts_without_time
  VULCAN <- df_without_time$VULCAN - df_ts_without_time
  
  df_final <- cbind(Obs,EDGAR,FFDAS,ODIAC, VULCAN)
  colnames(df_final) <- c('Obs','EDGAR', 'FFDAS', 'ODIAC', 'VULCAN')
  
  df_final$ODIAC[df_final$ODIAC < 0] <- NA # removing negative values
  df_final$EDGAR[df_final$EDGAR < 0] <- NA # removing negative values
  df_final$EDGAR[df_final$EDGAR > 40] <- NA # removing negative values
  df_final$Obs[df_final$Obs < 0] <- NA # removing negative values
  
  
  options(digits = 4) #changing the decimal places 
  df_SON <- data.frame(time, df_final)
  
  # just for 2017 and 2017
  # omitting results from FFDAS and VULCAN
  df_SON <- select(df_SON, c(-FFDAS,-VULCAN))
  
  # removing the na values
  df_SON <- na.omit(df_SON)
  
  # creating an empty dataframe
  statDf <- data.frame()
  
  columns <- colnames(df_SON)
  obs <- df_SON[,2]
  obs <- dplyr::pull(df_SON, 2) #changing it to numeric
  
  EDGAR <- df_SON[,3]
  EDGAR <- dplyr::pull(df_SON,3)
  
  statsReady <- modeval(measured = obs,
                        calculated = EDGAR,
                        stat=c("N","pearson","MBE","RMBE","MAE","RMAE","RMSE","RRMSE","R2","slope",
                               "intercept","EF","SD","CRM","MPE","AC","ACu","ACs"))
  i <- 1
  
  statDf[i,1] <- paste(sheets[j],'_SON')
  statDf[i,2] <- "EDGAR"
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
  
  # FFDAS <- df_July[,4]
  # FFDAS <- dplyr::pull(df_July,4)
  # 
  # statsReady <- modeval(measured = obs,
  #                       calculated = FFDAS,
  #                       stat=c("N","pearson","MBE","RMBE","MAE","RMAE","RMSE","RRMSE","R2","slope",
  #                              "intercept","EF","SD","CRM","MPE","AC","ACu","ACs"))
  
  # statDf[i,1] <- sheets[j]
  statDf[i,2] <- "FFDAS"
  statDf[i,3] <- NaN
  statDf[i,4] <- NaN
  statDf[i,5] <- NaN
  statDf[i,6] <- NaN
  statDf[i,7] <- NaN
  statDf[i,8] <- NaN
  statDf[i,9] <- NaN
  statDf[i,10] <- NaN
  statDf[i,11] <- NaN
  
  i = i+1
  
  ODIAC <- df_SON[,4]
  ODIAC <- dplyr::pull(df_SON,4)
  
  statsReady <- modeval(measured = obs,
                        calculated = ODIAC,
                        stat=c("N","pearson","MBE","RMBE","MAE","RMAE","RMSE","RRMSE","R2","slope",
                               "intercept","EF","SD","CRM","MPE","AC","ACu","ACs"))
  
  #statDf[i,1] <- sheets[j]
  statDf[i,2] <- "ODIAC"
  statDf[i,3] <- statsReady[[1]] #N
  #statDf[i,4] <- format(mean(obs), nsmall = 2, digits = 2)
  statDf[i,5] <- format(mean(ODIAC), nsmall = 2, digits = 2)
  statDf[i,6] <- format(round(statsReady[[2]], 2), nsmall = 2, digits = 2) #R/Corr
  statDf[i,7] <- format(round(statsReady[[3]], 3), nsmall = 2, digits = 2) #MBE
  statDf[i,8] <- format(round(statsReady[[3]]*length(obs)/sum(obs)*100, 3), nsmall = 2, digits = 2) #NMBE
  statDf[i,9] <- format(round(statsReady[[5]], 3), nsmall = 2, digits = 2) #MAE
  statDf[i,10] <- format(round(statsReady[[5]]*length(obs)/sum(obs)*100, 3), nsmall = 2, digits = 2) #NMAE
  statDf[i,11] <- format(round(statsReady[[7]], 3), nsmall = 2, digits = 2) #RMSE
  
  i = i+1
  
  # VULCAN <- df_July[,6]
  # VULCAN <- dplyr::pull(df_July,6)
  # 
  # statsReady <- modeval(measured = obs,
  #                       calculated = VULCAN,
  #                       stat=c("N","pearson","MBE","RMBE","MAE","RMAE","RMSE","RRMSE","R2","slope",
  #                              "intercept","EF","SD","CRM","MPE","AC","ACu","ACs"))
  
  #statDf[i,1] <- sheets[j]
  statDf[i,2] <- "VULCAN"
  statDf[i,3] <- NaN
  statDf[i,4] <- NaN
  statDf[i,5] <- NaN
  statDf[i,6] <- NaN
  statDf[i,7] <- NaN
  statDf[i,8] <- NaN
  statDf[i,9] <- NaN
  statDf[i,10] <- NaN
  statDf[i,11] <- NaN
  
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
            file = "E:/Eeshan/evaluation/results/results_without_background_ts_SON.xlsx", 
            sheetName="2017", 
            append=TRUE)
