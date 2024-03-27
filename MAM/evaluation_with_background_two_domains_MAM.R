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
  
  # reading in data for specific months
  df_MAM <- df %>% 
    filter(df$Time > as.POSIXct("2017-02-28 23:00:00", tz="UTC") & df$Time < as.POSIXct("2017-06-01 00:00:00", tz="UTC"))
  
  df_MAM <- df_MAM[df_MAM$EDGAR >= 100, ] # drop values less than 100
  # Drop NA values specifically for column 'A'
  df_MAM <- df_MAM[!is.na(df_MAM$Obs), ]
  df_MAM <- df_MAM[!is.na(df_MAM$ODIAC), ]
  
  # creating an empty dataframe
  statDf <- data.frame()
  
  # d01  ##### 
  
  columns <- colnames(df_MAM)
  obs <- df_MAM[,2]
  obs <- dplyr::pull(df_MAM, 2) #changing it to numeric
  EDGAR <- df_MAM[,3]
  EDGAR <- dplyr::pull(df_MAM,3)
  
  statsReady <- modeval(measured = obs,
                        calculated = EDGAR,
                        stat=c("N","pearson","MBE","RMBE","MAE","RMAE","RMSE","RRMSE","R2","slope",
                               "intercept","EF","SD","CRM","MPE","AC","ACu","ACs"))
  i <- 1
  
  statDf[i,1] <- paste(sheets[j],'_MAM_d01')
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
  
  obs <- df_MAM[,2]
  FFDAS <- df_MAM[,4]
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
  
  obs <- df_MAM[,2]
  obs <- dplyr::pull(df_MAM, 2) #changing it to numeric
  ODIAC <- df_MAM[,5]
  ODIAC <- dplyr::pull(df_MAM,5)
  
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
  
  obs <- df_MAM[,2]
  VULCAN <- df_MAM[,6]
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
  obs <- df_MAM[,2]
  obs <- dplyr::pull(df_MAM, 2) #changing it to numeric
  EDGAR_d02 <- df_MAM[,7]
  EDGAR_d02 <- dplyr::pull(df_MAM,7)
  
  statsReady <- modeval(measured = obs,
                        calculated = EDGAR_d02,
                        stat=c("N","pearson","MBE","RMBE","MAE","RMAE","RMSE","RRMSE","R2","slope",
                               "intercept","EF","SD","CRM","MPE","AC","ACu","ACs"))
  
  statDf[i,1] <- paste(sheets[j],'_MAM_d02')
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
  
  obs <- df_MAM[,2]
  FFDAS_d02 <- df_MAM[,8]
  df <- data.frame(obs, FFDAS_d02)
  df <- na.omit(df)
  FFDAS_d02 <- dplyr::pull(df,2)
  obs <- dplyr::pull(df, 1) #changing it to numeric
  
  statsReady <- modeval(measured = obs,
                        calculated = FFDAS_d02,
                        stat=c("N","pearson","MBE","RMBE","MAE","RMAE","RMSE","RRMSE","R2","slope",
                               "intercept","EF","SD","CRM","MPE","AC","ACu","ACs"))
  
  #statDf[i,1] <- paste(sheets[j],'_MAM_d02')
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
  
  obs <- df_MAM[,2]
  obs <- dplyr::pull(df_MAM, 2) #changing it to numeric
  ODIAC_d02 <- df_MAM[,9]
  ODIAC_d02 <- dplyr::pull(df_MAM,9)
  
  statsReady <- modeval(measured = obs,
                        calculated = ODIAC_d02,
                        stat=c("N","pearson","MBE","RMBE","MAE","RMAE","RMSE","RRMSE","R2","slope",
                               "intercept","EF","SD","CRM","MPE","AC","ACu","ACs"))
  
  #statDf[i,1] <- paste(sheets[j],'_MAM_d02')
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
  
  obs <- df_MAM[,2]  
  VULCAN_d02 <- df_MAM[,10]
  df <- data.frame(obs, VULCAN_d02)
  df <- na.omit(df)
  VULCAN_d02 <- dplyr::pull(df,2)
  obs <- dplyr::pull(df, 1)
  
  statsReady <- modeval(measured = obs,
                        calculated = VULCAN_d02,
                        stat=c("N","pearson","MBE","RMBE","MAE","RMAE","RMSE","RRMSE","R2","slope",
                               "intercept","EF","SD","CRM","MPE","AC","ACu","ACs"))
  
  #statDf[i,1] <- paste(sheets[j],'_MAM_d02')
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
            file = "E:/Eeshan/evaluation/results/results_MAM_d01_d02.xlsx", 
            sheetName= "2017", 
            append = TRUE)
