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
  
  # reading in data for specific months
  df_yearly_initial <- df %>% 
    filter(df$Time > as.POSIXct("2011-12-31 23:00:00", tz="UTC") & df$Time < as.POSIXct("2018-01-01 00:00:00", tz="UTC"))
  
  time <- df_yearly_initial[, 1]
  df_without_time <- df_yearly_initial[, -1]
  test<- df_without_time[] - 370.0 #substracting pre-industrial values 
  options(digits = 5) #changing the decimal places 
  df_yearly <- data.frame(time, test)
  df_yearly <- df_yearly[df_yearly$EDGAR >= 0, ] # drop negative values
  df_yearly <- df_yearly[df_yearly$ODIAC >= 0, ] # drop negative values
  
  # Drop NA values specifically for column obs
  df_yearly <- df_yearly[!is.na(df_yearly$Obs), ]
  
  # creating an empty dataframe
  statDf <- data.frame()
  
  columns <- colnames(df_yearly)
  obs <- df_yearly[,2]
  obs <- dplyr::pull(df_yearly, 2) #changing it to numeric
  
  EDGAR <- df_yearly[,3]
  EDGAR <- dplyr::pull(df_yearly,3)
  
  statsReady <- modeval(measured = obs,
                        calculated = EDGAR,
                        stat=c("N","pearson","MBE","RMBE","MAE","RMAE","RMSE","RRMSE","R2","slope",
                               "intercept","EF","SD","CRM","MPE","AC","ACu","ACs"))
  i <- 1
  
  statDf[i,1] <- paste(sheets[j],'_yearly')
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
  
  obs <- df_yearly[,2]
  FFDAS <- df_yearly[,4]
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
  
  obs <- df_yearly[,2]
  obs <- dplyr::pull(df_yearly, 2) #changing it to numeric
  ODIAC <- df_yearly[,5]
  ODIAC <- dplyr::pull(df_yearly,5)
  
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
  
  obs <- df_yearly[,2]
  VULCAN <- df_yearly[,6]
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
            file = "E:/Eeshan/evaluation/results/results_without_background_yearly.xlsx", 
            sheetName="Average_6", 
            append=TRUE)
