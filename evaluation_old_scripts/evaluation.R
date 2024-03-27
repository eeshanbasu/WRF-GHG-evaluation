#install.packages("readxl")
library(sirad)
library(readxl)

#####Jan'12, AMT#####
#January
#Argyle, Maine
statDf <- data.frame()
df <- read_excel("E:/Eeshan/evaluation/CCGG_NACP_all_sites.xlsx",
                 sheet = 1,
                 range = "A1:F745",
                 col_types = c('date','numeric','numeric','numeric','numeric','numeric'))

df <- na.omit(df)
head(df)
tail(df)

# reading in the values
obs <- df[,2]
obs <- dplyr::pull(df, 2) #changing it to numeric
EDGAR <- df[,3]
EDGAR <- dplyr::pull(df, 3)
FFDAS <- df[,4]
FFDAS <- dplyr::pull(df, 4)
ODIAC <- df[,5]
ODIAC <- dplyr::pull(df, 5)
VULCAN <- df[,6]
VULCAN <- dplyr::pull(df, 6)

#EDGAR
statsReady <- modeval(measured = obs,
                      calculated = EDGAR,
                      stat=c("N","pearson","MBE","RMBE","MAE","RMAE","RMSE","RRMSE","R2","slope",
                             "intercept","EF","SD","CRM","MPE","AC","ACu","ACs"))

statDf[1,1] <- statsReady[[1]] #N
statDf[1,2] <- format(round(statsReady[[2]], 2), nsmall = 2) #R/Corr
statDf[1,3] <- format(round(statsReady[[3]], 3), nsmall = 3) #MBE
statDf[1,4] <- format(round(statsReady[[3]]*length(obs)/sum(obs)*100, 3), nsmall = 3) #NMBE
statDf[1,5] <- format(round(statsReady[[5]], 3), nsmall = 3) #MAE
statDf[1,6] <- format(round(statsReady[[5]]*length(obs)/sum(obs)*100, 3), nsmall = 3) #NMAE
statDf[1,7] <- format(round(statsReady[[7]], 3), nsmall = 3) #RMSE
mean(obs)
mean(EDGAR)
# statDf[1,8] <- format((mean(obs) - mean(EDGAR)), nsmall = 3) #MB

# Creating the plot
plot(obs, EDGAR, pch = 19, col = "lightblue")
# Regression line
abline(lm(EDGAR ~ obs), col = "red", lwd = 3)
# Pearson correlation
text(paste("Correlation:", round(cor(obs, EDGAR), 2)), x = 25, y = 95)

#FFDAS
statsReady <- modeval(measured = obs,
                      calculated = FFDAS,
                      stat=c("N","pearson","MBE","RMBE","MAE","RMAE","RMSE","RRMSE","R2","slope",
                             "intercept","EF","SD","CRM","MPE","AC","ACu","ACs"))

statDf[1,1] <- statsReady[[1]] #N
statDf[1,2] <- format(round(statsReady[[2]], 2), nsmall = 2) #R/Corr
statDf[1,3] <- format(round(statsReady[[3]], 3), nsmall = 3) #MBE
statDf[1,4] <- format(round(statsReady[[3]]*length(obs)/sum(obs)*100, 3), nsmall = 3) #NMBE
statDf[1,5] <- format(round(statsReady[[5]], 3), nsmall = 3) #MAE
statDf[1,6] <- format(round(statsReady[[5]]*length(obs)/sum(obs)*100, 3), nsmall = 3) #NMAE
statDf[1,7] <- format(round(statsReady[[7]], 3), nsmall = 3) #RMSE
mean(obs)
mean(FFDAS)

#ODIAC
statsReady <- modeval(measured = obs,
                      calculated = ODIAC,
                      stat=c("N","pearson","MBE","RMBE","MAE","RMAE","RMSE","RRMSE","R2","slope",
                             "intercept","EF","SD","CRM","MPE","AC","ACu","ACs"))

statDf[1,1] <- statsReady[[1]] #N
statDf[1,2] <- format(round(statsReady[[2]], 2), nsmall = 2) #R/Corr
statDf[1,3] <- format(round(statsReady[[3]], 3), nsmall = 3) #MBE
statDf[1,4] <- format(round(statsReady[[3]]*length(obs)/sum(obs)*100, 3), nsmall = 3) #NMBE
statDf[1,5] <- format(round(statsReady[[5]], 3), nsmall = 3) #MAE
statDf[1,6] <- format(round(statsReady[[5]]*length(obs)/sum(obs)*100, 3), nsmall = 3) #NMAE
statDf[1,7] <- format(round(statsReady[[7]], 3), nsmall = 3) #RMSE
mean(obs)
mean(ODIAC)

#VULCAN
statsReady <- modeval(measured = obs,
                      calculated = VULCAN,
                      stat=c("N","pearson","MBE","RMBE","MAE","RMAE","RMSE","RRMSE","R2","slope",
                             "intercept","EF","SD","CRM","MPE","AC","ACu","ACs"))

statDf[1,1] <- statsReady[[1]] #N
statDf[1,2] <- format(round(statsReady[[2]], 2), nsmall = 2) #R/Corr
statDf[1,3] <- format(round(statsReady[[3]], 3), nsmall = 3) #MBE
statDf[1,4] <- format(round(statsReady[[3]]*length(obs)/sum(obs)*100, 3), nsmall = 3) #NMBE
statDf[1,5] <- format(round(statsReady[[5]], 3), nsmall = 3) #MAE
statDf[1,6] <- format(round(statsReady[[5]]*length(obs)/sum(obs)*100, 3), nsmall = 3) #NMAE
statDf[1,7] <- format(round(statsReady[[7]], 3), nsmall = 3) #RMSE
mean(obs)
mean(VULCAN)

#####Jul'12 AMT#####
#July
#Argyle, Maine
statDf <- data.frame()
df <- read_excel("E:/Eeshan/evaluation/CCGG_NACP_all_sites.xlsx",
                 sheet = 1,
                 range = "A4370:F5113",
                 col_names = F,
                 col_types = c('date','numeric','numeric','numeric','numeric','numeric'))

df <- na.omit(df)
head(df)
tail(df)

# reading in the values
obs <- df[,2]
obs <- dplyr::pull(df, 2) #changing it to numeric
EDGAR <- df[,3]
EDGAR <- dplyr::pull(df, 3)
FFDAS <- df[,4]
FFDAS <- dplyr::pull(df, 4)
ODIAC <- df[,5]
ODIAC <- dplyr::pull(df, 5)
VULCAN <- df[,6]
VULCAN <- dplyr::pull(df, 6)

#EDGAR
statsReady <- modeval(measured = obs,
                      calculated = EDGAR,
                      stat=c("N","pearson","MBE","RMBE","MAE","RMAE","RMSE","RRMSE","R2","slope",
                             "intercept","EF","SD","CRM","MPE","AC","ACu","ACs"))

statDf[1,1] <- statsReady[[1]] #N
statDf[1,2] <- format(round(statsReady[[2]], 2), nsmall = 2) #R/Corr
statDf[1,3] <- format(round(statsReady[[3]], 3), nsmall = 3) #MBE
statDf[1,4] <- format(round(statsReady[[3]]*length(obs)/sum(obs)*100, 3), nsmall = 3) #NMBE
statDf[1,5] <- format(round(statsReady[[5]], 3), nsmall = 3) #MAE
statDf[1,6] <- format(round(statsReady[[5]]*length(obs)/sum(obs)*100, 3), nsmall = 3) #NMAE
statDf[1,7] <- format(round(statsReady[[7]], 3), nsmall = 3) #RMSE
mean(obs)
mean(EDGAR)
# statDf[1,8] <- format((mean(obs) - mean(EDGAR)), nsmall = 3) #MB

# Creating the plot
plot(obs, EDGAR, pch = 19, col = "lightblue")
# Regression line
abline(lm(EDGAR ~ obs), col = "red", lwd = 3)
# Pearson correlation
text(paste("Correlation:", round(cor(obs, EDGAR), 2)), x = 25, y = 95)

#FFDAS
statsReady <- modeval(measured = obs,
                      calculated = FFDAS,
                      stat=c("N","pearson","MBE","RMBE","MAE","RMAE","RMSE","RRMSE","R2","slope",
                             "intercept","EF","SD","CRM","MPE","AC","ACu","ACs"))

statDf[1,1] <- statsReady[[1]] #N
statDf[1,2] <- format(round(statsReady[[2]], 2), nsmall = 2) #R/Corr
statDf[1,3] <- format(round(statsReady[[3]], 3), nsmall = 3) #MBE
statDf[1,4] <- format(round(statsReady[[3]]*length(obs)/sum(obs)*100, 3), nsmall = 3) #NMBE
statDf[1,5] <- format(round(statsReady[[5]], 3), nsmall = 3) #MAE
statDf[1,6] <- format(round(statsReady[[5]]*length(obs)/sum(obs)*100, 3), nsmall = 3) #NMAE
statDf[1,7] <- format(round(statsReady[[7]], 3), nsmall = 3) #RMSE
mean(obs)
mean(FFDAS)

#ODIAC
statsReady <- modeval(measured = obs,
                      calculated = ODIAC,
                      stat=c("N","pearson","MBE","RMBE","MAE","RMAE","RMSE","RRMSE","R2","slope",
                             "intercept","EF","SD","CRM","MPE","AC","ACu","ACs"))

statDf[1,1] <- statsReady[[1]] #N
statDf[1,2] <- format(round(statsReady[[2]], 2), nsmall = 2) #R/Corr
statDf[1,3] <- format(round(statsReady[[3]], 3), nsmall = 3) #MBE
statDf[1,4] <- format(round(statsReady[[3]]*length(obs)/sum(obs)*100, 3), nsmall = 3) #NMBE
statDf[1,5] <- format(round(statsReady[[5]], 3), nsmall = 3) #MAE
statDf[1,6] <- format(round(statsReady[[5]]*length(obs)/sum(obs)*100, 3), nsmall = 3) #NMAE
statDf[1,7] <- format(round(statsReady[[7]], 3), nsmall = 3) #RMSE
mean(obs)
mean(ODIAC)

#VULCAN
statsReady <- modeval(measured = obs,
                      calculated = VULCAN,
                      stat=c("N","pearson","MBE","RMBE","MAE","RMAE","RMSE","RRMSE","R2","slope",
                             "intercept","EF","SD","CRM","MPE","AC","ACu","ACs"))

statDf[1,1] <- statsReady[[1]] #N
statDf[1,2] <- format(round(statsReady[[2]], 2), nsmall = 2) #R/Corr
statDf[1,3] <- format(round(statsReady[[3]], 3), nsmall = 3) #MBE
statDf[1,4] <- format(round(statsReady[[3]]*length(obs)/sum(obs)*100, 3), nsmall = 3) #NMBE
statDf[1,5] <- format(round(statsReady[[5]], 3), nsmall = 3) #MAE
statDf[1,6] <- format(round(statsReady[[5]]*length(obs)/sum(obs)*100, 3), nsmall = 3) #NMAE
statDf[1,7] <- format(round(statsReady[[7]], 3), nsmall = 3) #RMSE
mean(obs)
mean(VULCAN)


#####Dec '12 HF#####
#December
#Harvard Forest, MA
statDf <- data.frame()
df <- read_excel("E:/Eeshan/evaluation/CCGG_NACP_all_sites.xlsx",
                 sheet = 11,
                 range = "A2595:H3337",
                 col_names = F,
                 col_types = c('date','numeric','numeric','numeric','numeric','numeric','numeric','numeric'))

df <- na.omit(df)
head(df)
tail(df)

# reading in the values
obs <- df[,2]
obs <- dplyr::pull(df, 2) #changing it to numeric
EDGAR <- df[,3]
EDGAR <- dplyr::pull(df, 3)
FFDAS <- df[,4]
FFDAS <- dplyr::pull(df, 4)
ODIAC <- df[,5]
ODIAC <- dplyr::pull(df, 5)
VULCAN <- df[,6]
VULCAN <- dplyr::pull(df, 6)
EDGAR_d02 <- df[,7]
EDGAR_d02 <- dplyr::pull(df, 7)
FFDAS_d02 <- df[,8]
FFDAS_d02 <- dplyr::pull(df, 8)

#EDGAR
statsReady <- modeval(measured = obs,
                      calculated = EDGAR,
                      stat=c("N","pearson","MBE","RMBE","MAE","RMAE","RMSE","RRMSE","R2","slope",
                             "intercept","EF","SD","CRM","MPE","AC","ACu","ACs"))

statDf[1,1] <- statsReady[[1]] #N
statDf[1,2] <- format(round(statsReady[[2]], 2), nsmall = 2) #R/Corr
statDf[1,3] <- format(round(statsReady[[3]], 3), nsmall = 3) #MBE
statDf[1,4] <- format(round(statsReady[[3]]*length(obs)/sum(obs)*100, 3), nsmall = 3) #NMBE
statDf[1,5] <- format(round(statsReady[[5]], 3), nsmall = 3) #MAE
statDf[1,6] <- format(round(statsReady[[5]]*length(obs)/sum(obs)*100, 3), nsmall = 3) #NMAE
statDf[1,7] <- format(round(statsReady[[7]], 3), nsmall = 3) #RMSE
mean(obs)
mean(EDGAR)

#FFDAS
statsReady <- modeval(measured = obs,
                      calculated = FFDAS,
                      stat=c("N","pearson","MBE","RMBE","MAE","RMAE","RMSE","RRMSE","R2","slope",
                             "intercept","EF","SD","CRM","MPE","AC","ACu","ACs"))

statDf[1,1] <- statsReady[[1]] #N
statDf[1,2] <- format(round(statsReady[[2]], 2), nsmall = 2) #R/Corr
statDf[1,3] <- format(round(statsReady[[3]], 3), nsmall = 3) #MBE
statDf[1,4] <- format(round(statsReady[[3]]*length(obs)/sum(obs)*100, 3), nsmall = 3) #NMBE
statDf[1,5] <- format(round(statsReady[[5]], 3), nsmall = 3) #MAE
statDf[1,6] <- format(round(statsReady[[5]]*length(obs)/sum(obs)*100, 3), nsmall = 3) #NMAE
statDf[1,7] <- format(round(statsReady[[7]], 3), nsmall = 3) #RMSE
mean(obs)
mean(FFDAS)

#ODIAC
statsReady <- modeval(measured = obs,
                      calculated = ODIAC,
                      stat=c("N","pearson","MBE","RMBE","MAE","RMAE","RMSE","RRMSE","R2","slope",
                             "intercept","EF","SD","CRM","MPE","AC","ACu","ACs"))

statDf[1,1] <- statsReady[[1]] #N
statDf[1,2] <- format(round(statsReady[[2]], 2), nsmall = 2) #R/Corr
statDf[1,3] <- format(round(statsReady[[3]], 3), nsmall = 3) #MBE
statDf[1,4] <- format(round(statsReady[[3]]*length(obs)/sum(obs)*100, 3), nsmall = 3) #NMBE
statDf[1,5] <- format(round(statsReady[[5]], 3), nsmall = 3) #MAE
statDf[1,6] <- format(round(statsReady[[5]]*length(obs)/sum(obs)*100, 3), nsmall = 3) #NMAE
statDf[1,7] <- format(round(statsReady[[7]], 3), nsmall = 3) #RMSE
mean(obs)
mean(ODIAC)

#VULCAN
statsReady <- modeval(measured = obs,
                      calculated = VULCAN,
                      stat=c("N","pearson","MBE","RMBE","MAE","RMAE","RMSE","RRMSE","R2","slope",
                             "intercept","EF","SD","CRM","MPE","AC","ACu","ACs"))

statDf[1,1] <- statsReady[[1]] #N
statDf[1,2] <- format(round(statsReady[[2]], 2), nsmall = 2) #R/Corr
statDf[1,3] <- format(round(statsReady[[3]], 3), nsmall = 3) #MBE
statDf[1,4] <- format(round(statsReady[[3]]*length(obs)/sum(obs)*100, 3), nsmall = 3) #NMBE
statDf[1,5] <- format(round(statsReady[[5]], 3), nsmall = 3) #MAE
statDf[1,6] <- format(round(statsReady[[5]]*length(obs)/sum(obs)*100, 3), nsmall = 3) #NMAE
statDf[1,7] <- format(round(statsReady[[7]], 3), nsmall = 3) #RMSE
mean(obs)
mean(VULCAN)

#EDGAR_d02
statsReady <- modeval(measured = obs,
                      calculated = EDGAR_d02,
                      stat=c("N","pearson","MBE","RMBE","MAE","RMAE","RMSE","RRMSE","R2","slope",
                             "intercept","EF","SD","CRM","MPE","AC","ACu","ACs"))

statDf[1,1] <- statsReady[[1]] #N
statDf[1,2] <- format(round(statsReady[[2]], 2), nsmall = 2) #R/Corr
statDf[1,3] <- format(round(statsReady[[3]], 3), nsmall = 3) #MBE
statDf[1,4] <- format(round(statsReady[[3]]*length(obs)/sum(obs)*100, 3), nsmall = 3) #NMBE
statDf[1,5] <- format(round(statsReady[[5]], 3), nsmall = 3) #MAE
statDf[1,6] <- format(round(statsReady[[5]]*length(obs)/sum(obs)*100, 3), nsmall = 3) #NMAE
statDf[1,7] <- format(round(statsReady[[7]], 3), nsmall = 3) #RMSE
mean(obs)
mean(EDGAR_d02)

#FFDAS_d02
statsReady <- modeval(measured = obs,
                      calculated = FFDAS_d02,
                      stat=c("N","pearson","MBE","RMBE","MAE","RMAE","RMSE","RRMSE","R2","slope",
                             "intercept","EF","SD","CRM","MPE","AC","ACu","ACs"))

statDf[1,1] <- statsReady[[1]] #N
statDf[1,2] <- format(round(statsReady[[2]], 2), nsmall = 2) #R/Corr
statDf[1,3] <- format(round(statsReady[[3]], 3), nsmall = 3) #MBE
statDf[1,4] <- format(round(statsReady[[3]]*length(obs)/sum(obs)*100, 3), nsmall = 3) #NMBE
statDf[1,5] <- format(round(statsReady[[5]], 3), nsmall = 3) #MAE
statDf[1,6] <- format(round(statsReady[[5]]*length(obs)/sum(obs)*100, 3), nsmall = 3) #NMAE
statDf[1,7] <- format(round(statsReady[[7]], 3), nsmall = 3) #RMSE
mean(obs)
mean(FFDAS_d02)

#####Dec '12 BU#####
#December
#Boston University, MA
statDf <- data.frame()
df <- read_excel("E:/Eeshan/evaluation/CCGG_NACP_all_sites.xlsx",
                 sheet = 10,
                 range = "A2595:H3337",
                 col_names = F,
                 col_types = c('date','numeric','numeric','numeric','numeric','numeric','numeric','numeric'))

df <- na.omit(df)
head(df)
tail(df)

# reading in the values
obs <- df[,2]
obs <- dplyr::pull(df, 2) #changing it to numeric
EDGAR <- df[,3]
EDGAR <- dplyr::pull(df, 3)
FFDAS <- df[,4]
FFDAS <- dplyr::pull(df, 4)
ODIAC <- df[,5]
ODIAC <- dplyr::pull(df, 5)
VULCAN <- df[,6]
VULCAN <- dplyr::pull(df, 6)
EDGAR_d02 <- df[,7]
EDGAR_d02 <- dplyr::pull(df, 7)
FFDAS_d02 <- df[,8]
FFDAS_d02 <- dplyr::pull(df, 8)

#EDGAR
statsReady <- modeval(measured = obs,
                      calculated = EDGAR,
                      stat=c("N","pearson","MBE","RMBE","MAE","RMAE","RMSE","RRMSE","R2","slope",
                             "intercept","EF","SD","CRM","MPE","AC","ACu","ACs"))

statDf[1,1] <- statsReady[[1]] #N
statDf[1,2] <- format(round(statsReady[[2]], 2), nsmall = 2) #R/Corr
statDf[1,3] <- format(round(statsReady[[3]], 3), nsmall = 3) #MBE
statDf[1,4] <- format(round(statsReady[[3]]*length(obs)/sum(obs)*100, 3), nsmall = 3) #NMBE
statDf[1,5] <- format(round(statsReady[[5]], 3), nsmall = 3) #MAE
statDf[1,6] <- format(round(statsReady[[5]]*length(obs)/sum(obs)*100, 3), nsmall = 3) #NMAE
statDf[1,7] <- format(round(statsReady[[7]], 3), nsmall = 3) #RMSE
mean(obs)
mean(EDGAR)

#FFDAS
statsReady <- modeval(measured = obs,
                      calculated = FFDAS,
                      stat=c("N","pearson","MBE","RMBE","MAE","RMAE","RMSE","RRMSE","R2","slope",
                             "intercept","EF","SD","CRM","MPE","AC","ACu","ACs"))

statDf[1,1] <- statsReady[[1]] #N
statDf[1,2] <- format(round(statsReady[[2]], 2), nsmall = 2) #R/Corr
statDf[1,3] <- format(round(statsReady[[3]], 3), nsmall = 3) #MBE
statDf[1,4] <- format(round(statsReady[[3]]*length(obs)/sum(obs)*100, 3), nsmall = 3) #NMBE
statDf[1,5] <- format(round(statsReady[[5]], 3), nsmall = 3) #MAE
statDf[1,6] <- format(round(statsReady[[5]]*length(obs)/sum(obs)*100, 3), nsmall = 3) #NMAE
statDf[1,7] <- format(round(statsReady[[7]], 3), nsmall = 3) #RMSE
mean(obs)
mean(FFDAS)

#ODIAC
statsReady <- modeval(measured = obs,
                      calculated = ODIAC,
                      stat=c("N","pearson","MBE","RMBE","MAE","RMAE","RMSE","RRMSE","R2","slope",
                             "intercept","EF","SD","CRM","MPE","AC","ACu","ACs"))

statDf[1,1] <- statsReady[[1]] #N
statDf[1,2] <- format(round(statsReady[[2]], 2), nsmall = 2) #R/Corr
statDf[1,3] <- format(round(statsReady[[3]], 3), nsmall = 3) #MBE
statDf[1,4] <- format(round(statsReady[[3]]*length(obs)/sum(obs)*100, 3), nsmall = 3) #NMBE
statDf[1,5] <- format(round(statsReady[[5]], 3), nsmall = 3) #MAE
statDf[1,6] <- format(round(statsReady[[5]]*length(obs)/sum(obs)*100, 3), nsmall = 3) #NMAE
statDf[1,7] <- format(round(statsReady[[7]], 3), nsmall = 3) #RMSE
mean(obs)
mean(ODIAC)

#VULCAN
statsReady <- modeval(measured = obs,
                      calculated = VULCAN,
                      stat=c("N","pearson","MBE","RMBE","MAE","RMAE","RMSE","RRMSE","R2","slope",
                             "intercept","EF","SD","CRM","MPE","AC","ACu","ACs"))

statDf[1,1] <- statsReady[[1]] #N
statDf[1,2] <- format(round(statsReady[[2]], 2), nsmall = 2) #R/Corr
statDf[1,3] <- format(round(statsReady[[3]], 3), nsmall = 3) #MBE
statDf[1,4] <- format(round(statsReady[[3]]*length(obs)/sum(obs)*100, 3), nsmall = 3) #NMBE
statDf[1,5] <- format(round(statsReady[[5]], 3), nsmall = 3) #MAE
statDf[1,6] <- format(round(statsReady[[5]]*length(obs)/sum(obs)*100, 3), nsmall = 3) #NMAE
statDf[1,7] <- format(round(statsReady[[7]], 3), nsmall = 3) #RMSE
mean(obs)
mean(VULCAN)

#EDGAR_d02
statsReady <- modeval(measured = obs,
                      calculated = EDGAR_d02,
                      stat=c("N","pearson","MBE","RMBE","MAE","RMAE","RMSE","RRMSE","R2","slope",
                             "intercept","EF","SD","CRM","MPE","AC","ACu","ACs"))

statDf[1,1] <- statsReady[[1]] #N
statDf[1,2] <- format(round(statsReady[[2]], 2), nsmall = 2) #R/Corr
statDf[1,3] <- format(round(statsReady[[3]], 3), nsmall = 3) #MBE
statDf[1,4] <- format(round(statsReady[[3]]*length(obs)/sum(obs)*100, 3), nsmall = 3) #NMBE
statDf[1,5] <- format(round(statsReady[[5]], 3), nsmall = 3) #MAE
statDf[1,6] <- format(round(statsReady[[5]]*length(obs)/sum(obs)*100, 3), nsmall = 3) #NMAE
statDf[1,7] <- format(round(statsReady[[7]], 3), nsmall = 3) #RMSE
mean(obs)
mean(EDGAR_d02)

#FFDAS_d02
statsReady <- modeval(measured = obs,
                      calculated = FFDAS_d02,
                      stat=c("N","pearson","MBE","RMBE","MAE","RMAE","RMSE","RRMSE","R2","slope",
                             "intercept","EF","SD","CRM","MPE","AC","ACu","ACs"))

statDf[1,1] <- statsReady[[1]] #N
statDf[1,2] <- format(round(statsReady[[2]], 2), nsmall = 2) #R/Corr
statDf[1,3] <- format(round(statsReady[[3]], 3), nsmall = 3) #MBE
statDf[1,4] <- format(round(statsReady[[3]]*length(obs)/sum(obs)*100, 3), nsmall = 3) #NMBE
statDf[1,5] <- format(round(statsReady[[5]], 3), nsmall = 3) #MAE
statDf[1,6] <- format(round(statsReady[[5]]*length(obs)/sum(obs)*100, 3), nsmall = 3) #NMAE
statDf[1,7] <- format(round(statsReady[[7]], 3), nsmall = 3) #RMSE
mean(obs)
mean(FFDAS_d02)

#####Jan '12 AMT_BCK#####
#Jan
#Argyle, Maine
statDf <- data.frame()
df <- read_excel("E:/Eeshan/evaluation/CCGG_NACP_all_sites.xlsx",
                 sheet = 13,
                 range = "H1:M745",
                 col_names = T,
                 col_types = c('date','numeric','numeric','numeric','numeric','numeric'))

is.na(df[2]) <-  df[2] < -100.00
df <- na.omit(df)
head(df)
tail(df)

# reading in the values
obs <- df[,2]
obs <- dplyr::pull(df, 2) #changing it to numeric
EDGAR <- df[,3]
EDGAR <- dplyr::pull(df, 3)
FFDAS <- df[,4]
FFDAS <- dplyr::pull(df, 4)
ODIAC <- df[,5]
ODIAC <- dplyr::pull(df, 5)
VULCAN <- df[,6]
VULCAN <- dplyr::pull(df, 6)

#EDGAR
statsReady <- modeval(measured = obs,
                      calculated = EDGAR,
                      stat=c("N","pearson","MBE","RMBE","MAE","RMAE","RMSE","RRMSE","R2","slope",
                             "intercept","EF","SD","CRM","MPE","AC","ACu","ACs"))

statDf[1,1] <- statsReady[[1]] #N
statDf[1,2] <- format(round(statsReady[[2]], 2), nsmall = 2) #R/Corr
statDf[1,3] <- format(round(statsReady[[3]], 3), nsmall = 3) #MBE
statDf[1,4] <- format(round(statsReady[[3]]*length(obs)/sum(obs)*100, 3), nsmall = 3) #NMBE
statDf[1,5] <- format(round(statsReady[[5]], 3), nsmall = 3) #MAE
statDf[1,6] <- format(round(statsReady[[5]]*length(obs)/sum(obs)*100, 3), nsmall = 3) #NMAE
statDf[1,7] <- format(round(statsReady[[7]], 3), nsmall = 3) #RMSE
mean(obs)
mean(EDGAR)

#FFDAS
statsReady <- modeval(measured = obs,
                      calculated = FFDAS,
                      stat=c("N","pearson","MBE","RMBE","MAE","RMAE","RMSE","RRMSE","R2","slope",
                             "intercept","EF","SD","CRM","MPE","AC","ACu","ACs"))

statDf[1,1] <- statsReady[[1]] #N
statDf[1,2] <- format(round(statsReady[[2]], 2), nsmall = 2) #R/Corr
statDf[1,3] <- format(round(statsReady[[3]], 3), nsmall = 3) #MBE
statDf[1,4] <- format(round(statsReady[[3]]*length(obs)/sum(obs)*100, 3), nsmall = 3) #NMBE
statDf[1,5] <- format(round(statsReady[[5]], 3), nsmall = 3) #MAE
statDf[1,6] <- format(round(statsReady[[5]]*length(obs)/sum(obs)*100, 3), nsmall = 3) #NMAE
statDf[1,7] <- format(round(statsReady[[7]], 3), nsmall = 3) #RMSE
mean(obs)
mean(FFDAS)

#ODIAC
statsReady <- modeval(measured = obs,
                      calculated = ODIAC,
                      stat=c("N","pearson","MBE","RMBE","MAE","RMAE","RMSE","RRMSE","R2","slope",
                             "intercept","EF","SD","CRM","MPE","AC","ACu","ACs"))

statDf[1,1] <- statsReady[[1]] #N
statDf[1,2] <- format(round(statsReady[[2]], 2), nsmall = 2) #R/Corr
statDf[1,3] <- format(round(statsReady[[3]], 3), nsmall = 3) #MBE
statDf[1,4] <- format(round(statsReady[[3]]*length(obs)/sum(obs)*100, 3), nsmall = 3) #NMBE
statDf[1,5] <- format(round(statsReady[[5]], 3), nsmall = 3) #MAE
statDf[1,6] <- format(round(statsReady[[5]]*length(obs)/sum(obs)*100, 3), nsmall = 3) #NMAE
statDf[1,7] <- format(round(statsReady[[7]], 3), nsmall = 3) #RMSE
mean(obs)
mean(ODIAC)

#VULCAN
statsReady <- modeval(measured = obs,
                      calculated = VULCAN,
                      stat=c("N","pearson","MBE","RMBE","MAE","RMAE","RMSE","RRMSE","R2","slope",
                             "intercept","EF","SD","CRM","MPE","AC","ACu","ACs"))

statDf[1,1] <- statsReady[[1]] #N
statDf[1,2] <- format(round(statsReady[[2]], 2), nsmall = 2) #R/Corr
statDf[1,3] <- format(round(statsReady[[3]], 3), nsmall = 3) #MBE
statDf[1,4] <- format(round(statsReady[[3]]*length(obs)/sum(obs)*100, 3), nsmall = 3) #NMBE
statDf[1,5] <- format(round(statsReady[[5]], 3), nsmall = 3) #MAE
statDf[1,6] <- format(round(statsReady[[5]]*length(obs)/sum(obs)*100, 3), nsmall = 3) #NMAE
statDf[1,7] <- format(round(statsReady[[7]], 3), nsmall = 3) #RMSE
mean(obs)
mean(VULCAN)



#####Jul '12 AMT_BCK#####
#July
#Argyle, Maine
statDf <- data.frame()
df <- read_excel("E:/Eeshan/evaluation/CCGG_NACP_all_sites.xlsx",
                 sheet = 13,
                 range = "H4370:M5113",
                 col_names = F,
                 col_types = c('date','numeric','numeric','numeric','numeric','numeric'))

is.na(df[2]) <-  df[2] < -100.00
df <- na.omit(df)
head(df)
tail(df)

# reading in the values
obs <- df[,2]
obs <- dplyr::pull(df, 2) #changing it to numeric
EDGAR <- df[,3]
EDGAR <- dplyr::pull(df, 3)
FFDAS <- df[,4]
FFDAS <- dplyr::pull(df, 4)
ODIAC <- df[,5]
ODIAC <- dplyr::pull(df, 5)
VULCAN <- df[,6]
VULCAN <- dplyr::pull(df, 6)

#EDGAR
statsReady <- modeval(measured = obs,
                      calculated = EDGAR,
                      stat=c("N","pearson","MBE","RMBE","MAE","RMAE","RMSE","RRMSE","R2","slope",
                             "intercept","EF","SD","CRM","MPE","AC","ACu","ACs"))

statDf[1,1] <- statsReady[[1]] #N
statDf[1,2] <- format(round(statsReady[[2]], 2), nsmall = 2) #R/Corr
statDf[1,3] <- format(round(statsReady[[3]], 3), nsmall = 3) #MBE
statDf[1,4] <- format(round(statsReady[[3]]*length(obs)/sum(obs)*100, 3), nsmall = 3) #NMBE
statDf[1,5] <- format(round(statsReady[[5]], 3), nsmall = 3) #MAE
statDf[1,6] <- format(round(statsReady[[5]]*length(obs)/sum(obs)*100, 3), nsmall = 3) #NMAE
statDf[1,7] <- format(round(statsReady[[7]], 3), nsmall = 3) #RMSE
mean(obs)
mean(EDGAR)

#FFDAS
statsReady <- modeval(measured = obs,
                      calculated = FFDAS,
                      stat=c("N","pearson","MBE","RMBE","MAE","RMAE","RMSE","RRMSE","R2","slope",
                             "intercept","EF","SD","CRM","MPE","AC","ACu","ACs"))

statDf[1,1] <- statsReady[[1]] #N
statDf[1,2] <- format(round(statsReady[[2]], 2), nsmall = 2) #R/Corr
statDf[1,3] <- format(round(statsReady[[3]], 3), nsmall = 3) #MBE
statDf[1,4] <- format(round(statsReady[[3]]*length(obs)/sum(obs)*100, 3), nsmall = 3) #NMBE
statDf[1,5] <- format(round(statsReady[[5]], 3), nsmall = 3) #MAE
statDf[1,6] <- format(round(statsReady[[5]]*length(obs)/sum(obs)*100, 3), nsmall = 3) #NMAE
statDf[1,7] <- format(round(statsReady[[7]], 3), nsmall = 3) #RMSE
mean(obs)
mean(FFDAS)

#ODIAC
statsReady <- modeval(measured = obs,
                      calculated = ODIAC,
                      stat=c("N","pearson","MBE","RMBE","MAE","RMAE","RMSE","RRMSE","R2","slope",
                             "intercept","EF","SD","CRM","MPE","AC","ACu","ACs"))

statDf[1,1] <- statsReady[[1]] #N
statDf[1,2] <- format(round(statsReady[[2]], 2), nsmall = 2) #R/Corr
statDf[1,3] <- format(round(statsReady[[3]], 3), nsmall = 3) #MBE
statDf[1,4] <- format(round(statsReady[[3]]*length(obs)/sum(obs)*100, 3), nsmall = 3) #NMBE
statDf[1,5] <- format(round(statsReady[[5]], 3), nsmall = 3) #MAE
statDf[1,6] <- format(round(statsReady[[5]]*length(obs)/sum(obs)*100, 3), nsmall = 3) #NMAE
statDf[1,7] <- format(round(statsReady[[7]], 3), nsmall = 3) #RMSE
mean(obs)
mean(ODIAC)

#VULCAN
statsReady <- modeval(measured = obs,
                      calculated = VULCAN,
                      stat=c("N","pearson","MBE","RMBE","MAE","RMAE","RMSE","RRMSE","R2","slope",
                             "intercept","EF","SD","CRM","MPE","AC","ACu","ACs"))

statDf[1,1] <- statsReady[[1]] #N
statDf[1,2] <- format(round(statsReady[[2]], 2), nsmall = 2) #R/Corr
statDf[1,3] <- format(round(statsReady[[3]], 3), nsmall = 3) #MBE
statDf[1,4] <- format(round(statsReady[[3]]*length(obs)/sum(obs)*100, 3), nsmall = 3) #NMBE
statDf[1,5] <- format(round(statsReady[[5]], 3), nsmall = 3) #MAE
statDf[1,6] <- format(round(statsReady[[5]]*length(obs)/sum(obs)*100, 3), nsmall = 3) #NMAE
statDf[1,7] <- format(round(statsReady[[7]], 3), nsmall = 3) #RMSE
mean(obs)
mean(VULCAN)
