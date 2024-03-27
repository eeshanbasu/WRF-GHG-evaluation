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
  
  # df[df <= 0] <- NA
  # df <- na.omit(df)
  
  
  # reading in data for specific months
  df_DJF_initial_1 <- df %>% 
    filter(df$Time > as.POSIXct("2011-12-31 23:00:00", tz="UTC") & df$Time < as.POSIXct("2012-03-01 00:00:00", tz="UTC"))
  # reading in data for specific months
  df_DJF_initial_2 <- df %>% 
    filter(df$Time > as.POSIXct("2012-11-30 23:00:00", tz="UTC") & df$Time < as.POSIXct("2013-03-01 00:00:00", tz="UTC"))
  # reading in data for specific months
  df_DJF_initial_3 <- df %>% 
    filter(df$Time > as.POSIXct("2013-11-30 23:00:00", tz="UTC") & df$Time < as.POSIXct("2014-03-01 00:00:00", tz="UTC"))
  # reading in data for specific months
  df_DJF_initial_4 <- df %>% 
    filter(df$Time > as.POSIXct("2014-11-30 23:00:00", tz="UTC") & df$Time < as.POSIXct("2015-03-01 00:00:00", tz="UTC"))
  # reading in data for specific months
  df_DJF_initial_5 <- df %>% 
    filter(df$Time > as.POSIXct("2015-11-30 23:00:00", tz="UTC") & df$Time < as.POSIXct("2016-03-01 00:00:00", tz="UTC"))
  # reading in data for specific months
  df_DJF_initial_6 <- df %>% 
    filter(df$Time > as.POSIXct("2016-11-30 23:00:00", tz="UTC") & df$Time < as.POSIXct("2017-03-01 00:00:00", tz="UTC"))
  # reading in data for specific months
  df_DJF_initial_7 <- df %>% 
    filter(df$Time > as.POSIXct("2017-11-30 23:00:00", tz="UTC") & df$Time < as.POSIXct("2018-01-01 00:00:00", tz="UTC"))
  
  df_DJF_initial <- rbind(df_DJF_initial_1,df_DJF_initial_2,df_DJF_initial_3,
                          df_DJF_initial_4,df_DJF_initial_5,df_DJF_initial_6,df_DJF_initial_7)
  
  time <- df_DJF_initial[, 1]
  df_without_time <- df_DJF_initial[, -1]
  test<- df_without_time[] - 375.0 #substracting pre-industrial values 
  options(digits = 5) #changing the decimal places 
  df_DJF <- data.frame(time, test)
  df_DJF <- df_DJF[df_DJF$EDGAR >= 0, ] # drop negative values
  df_DJF <- df_DJF[df_DJF$ODIAC >= 0, ] # drop negative values
  # Drop NA values specifically for column 'A'
  df_DJF <- df_DJF[!is.na(df_DJF$Obs), ]
  
  # creating an empty dataframe
  statDf <- data.frame()
  
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
  
  statDf[i,1] <- paste(sheets[j],'_DJF')
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
  statDf[i,2] <- "FFDAS"
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
  statDf[i,2] <- "ODIAC"
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
  statDf[i,2] <- "VULCAN"
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
  
  print(sheets[j])
  
  finalDf <- rbind(finalDf,statDf)
  
  finalDf$`#obs`<- as.numeric(finalDf$`#obs`)
  finalDf$obs<-as.numeric(finalDf$obs)
  finalDf$sim<-as.numeric(finalDf$sim)
  finalDf$r<-as.numeric(finalDf$r)
  finalDf$`MB(ppm)`<- as.numeric(finalDf$`MB(ppm)`)
  finalDf$`NMB(%)`<- as.numeric(finalDf$`NMB(%)`)
  finalDf$`NME(%)`<- as.numeric(finalDf$`NME(%)`)
  finalDf$MAE<- as.numeric(finalDf$MAE)
  finalDf$`RMSE(ppm)`<- as.numeric(finalDf$`RMSE(ppm)`)
  
}

write.xlsx2(finalDf, 
            file = "E:/Eeshan/evaluation/results/results_without_background_DJF.xlsx", 
            sheetName="Average_6", 
            append=TRUE)
