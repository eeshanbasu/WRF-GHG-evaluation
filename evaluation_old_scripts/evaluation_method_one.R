#install.packages("readxl")
library(sirad)
library(xlsx)
library(readxl)
library(openxlsx)
library(lubridate)
library(dplyr)

# getting the names of the sheet
sheets <- getSheetNames("E:/Eeshan/evaluation/evaluation_method_one_paper_one.xlsx")

# the final dataframe where everything will be saved
finalDf <- data.frame()
  
##### BU ##### 
# reading the data
df <- read_excel("E:/Eeshan/evaluation/evaluation_method_one_paper_one.xlsx",
                 sheet = "BU",
                 col_types = c('date','numeric','numeric','numeric','numeric'))

df[df <= 0] <- NA #removing negative values
df <- na.omit(df)

# reading in data for specific months
df_jan <- df %>% 
  filter(df$Time > as.POSIXct("2013-12-31 23:00:00", tz="UTC") & df$Time < as.POSIXct("2014-02-01 00:00:00", tz="UTC"))
df_jan$Obs[df_jan$Obs > 300] <- NA #removing values above 300 ppm
#df_jan$VULCAN_d01[df_jan$VULCAN_d01 > 20] <- NA #removing outliers
df_jan <- na.omit(df_jan)

# creating an empty dataframe
statDf <- data.frame()

# d01
i <- 1
obs <- df_jan[,2]
obs <- dplyr::pull(df_jan, 2) # changing it to numeric
VULCAN <- df_jan[,3]
VULCAN <- dplyr::pull(df_jan,3)

statsReady <- modeval(measured = obs,
                      calculated = VULCAN,
                      stat=c("N","pearson","MBE","RMBE","MAE","RMAE","RMSE","RRMSE","R2","slope",
                             "intercept","EF","SD","CRM","MPE","AC","ACu","ACs"))

statDf[i,1] <- "BU"
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

colnames(statDf) <- c('site','dataset','#obs','obs','sim','r','MB(ppm)','NMB(%)','MAE','NME(%)','RMSE(ppm)')

# d02
i <- 2
obs <- df_jan[,2]
obs <- dplyr::pull(df_jan, 2) # changing it to numeric
VULCAN <- df_jan[,4]
VULCAN <- dplyr::pull(df_jan,4)

statsReady <- modeval(measured = obs,
                      calculated = VULCAN,
                      stat=c("N","pearson","MBE","RMBE","MAE","RMAE","RMSE","RRMSE","R2","slope",
                             "intercept","EF","SD","CRM","MPE","AC","ACu","ACs"))

statDf[i,1] <- ""
statDf[i,2] <- "VULCAN_d02"
statDf[i,3] <- statsReady[[1]] #N
statDf[i,4] <- format(mean(obs), nsmall = 2, digits = 2)
statDf[i,5] <- format(mean(VULCAN), nsmall = 2, digits = 2)
statDf[i,6] <- format(round(statsReady[[2]], 2), nsmall = 2, digits = 2) #R/Corr
statDf[i,7] <- format(round(statsReady[[3]], 3), nsmall = 2, digits = 2) #MBE
statDf[i,8] <- format(round(statsReady[[3]]*length(obs)/sum(obs)*100, 3), nsmall = 2, digits = 2) #NMBE
statDf[i,9] <- format(round(statsReady[[5]], 3), nsmall = 2, digits = 2) #MAE
statDf[i,10] <- format(round(statsReady[[5]]*length(obs)/sum(obs)*100, 3), nsmall = 2, digits = 2) #NMAE
statDf[i,11] <- format(round(statsReady[[7]], 3), nsmall = 2, digits = 2) #RMSE  

# d03
i <- 3
obs <- df_jan[,2]
obs <- dplyr::pull(df_jan, 2) # changing it to numeric
VULCAN <- df_jan[,5]
VULCAN <- dplyr::pull(df_jan,5)

statsReady <- modeval(measured = obs,
                      calculated = VULCAN,
                      stat=c("N","pearson","MBE","RMBE","MAE","RMAE","RMSE","RRMSE","R2","slope",
                             "intercept","EF","SD","CRM","MPE","AC","ACu","ACs"))

statDf[i,1] <- ""
statDf[i,2] <- "VULCAN_d03"
statDf[i,3] <- statsReady[[1]] #N
statDf[i,4] <- format(mean(obs), nsmall = 2, digits = 2)
statDf[i,5] <- format(mean(VULCAN), nsmall = 2, digits = 2)
statDf[i,6] <- format(round(statsReady[[2]], 2), nsmall = 2, digits = 2) #R/Corr
statDf[i,7] <- format(round(statsReady[[3]], 3), nsmall = 2, digits = 2) #MBE
statDf[i,8] <- format(round(statsReady[[3]]*length(obs)/sum(obs)*100, 3), nsmall = 2, digits = 2) #NMBE
statDf[i,9] <- format(round(statsReady[[5]], 3), nsmall = 2, digits = 2) #MAE
statDf[i,10] <- format(round(statsReady[[5]]*length(obs)/sum(obs)*100, 3), nsmall = 2, digits = 2) #NMAE
statDf[i,11] <- format(round(statsReady[[7]], 3), nsmall = 2, digits = 2) #RMSE  



#####UU#####
df <- read_excel("E:/Eeshan/evaluation/evaluation_method_one_paper_one.xlsx",
                 sheet = "UU",
                 col_types = c('date','numeric','numeric'))

df[df <= 0] <- NA #removing negative values
df <- na.omit(df)

# reading in data for specific months
df_jan <- df %>% 
  filter(df$Time > as.POSIXct("2013-12-31 23:00:00", tz="UTC") & df$Time < as.POSIXct("2014-02-01 00:00:00", tz="UTC"))
df_jan$Obs[df_jan$Obs > 300] <- NA #removing values above 300 ppm
df_jan <- na.omit(df_jan)


# d01
i <- 4
obs <- df_jan[,2]
obs <- dplyr::pull(df_jan, 2) # changing it to numeric
VULCAN <- df_jan[,3]
VULCAN <- dplyr::pull(df_jan,3)

statsReady <- modeval(measured = obs,
                      calculated = VULCAN,
                      stat=c("N","pearson","MBE","RMBE","MAE","RMAE","RMSE","RRMSE","R2","slope",
                             "intercept","EF","SD","CRM","MPE","AC","ACu","ACs"))

statDf[i,1] <- "UU"
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

finalDf <- rbind(finalDf,statDf)

write.xlsx(finalDf, 
           file = "E:/Eeshan/evaluation/results/results_method_one_jan.xlsx", 
           sheetName="jan", 
           append=TRUE)



