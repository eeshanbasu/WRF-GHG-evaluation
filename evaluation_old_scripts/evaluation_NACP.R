#install.packages("readxl")
library(sirad)
library(readxl)
library(openxlsx)

sheets <- getSheetNames("E:/Eeshan/evaluation/NACP_UrbanC_2013_R.xlsx")

finalDf <- data.frame()

for(j in 1:length(sheets)){
  # statDf <- data.frame()

df <- read.xlsx("E:/Eeshan/evaluation/NACP_UrbanC_2013_R.xlsx",
                sheet = j,
                skipEmptyRows = TRUE,
                skipEmptyCols = TRUE)

df <- na.omit(df)


head(df)
tail(df)

columns <- colnames(df)
# for (x in 2:length(columns)){
#   print(x)
# }
obs <- df[,2]
obs <- dplyr::pull(df, 2) #changing it to numeric
i<- 1

vals <- list()
statDf <- data.frame()

for (x in 3:length(columns))
{
  
  cl <- df[,x]
  cl <- dplyr::pull(df,x)
  
# reading in the values
# obs <- df[,2]
# obs <- dplyr::pull(df, 2) #changing it to numeric
# d01 <- df[,3]
# d01 <- dplyr::pull(df, 3)
# d02 <- df[,4]
# d02 <- dplyr::pull(df, 4)
# d03 <- df[,5]
# d03 <- dplyr::pull(df, 5)
# 
# domains <- c(d01, d02, d03)

statsReady <- modeval(measured = obs,
                      calculated = cl,
                      stat=c("N","pearson","MBE","RMBE","MAE","RMAE","RMSE","RRMSE","R2","slope",
                             "intercept","EF","SD","CRM","MPE","AC","ACu","ACs"))

statDf[i,1] <- statsReady[[1]] #N
statDf[i,2] <- format(mean(obs), nsmall = 3)
statDf[i,3] <- format(mean(cl), nsmall = 3)
statDf[i,4] <- format(round(statsReady[[2]], 2), nsmall = 2) #R/Corr
statDf[i,5] <- format(round(statsReady[[3]], 3), nsmall = 3) #MBE
statDf[i,6] <- format(round(statsReady[[3]]*length(obs)/sum(obs)*100, 3), nsmall = 3) #NMBE
statDf[i,7] <- format(round(statsReady[[5]], 3), nsmall = 3) #MAE
statDf[i,8] <- format(round(statsReady[[5]]*length(obs)/sum(obs)*100, 3), nsmall = 3) #NMAE
statDf[i,9] <- format(round(statsReady[[7]], 3), nsmall = 3) #RMSE
statDf[i,10] <- sheets[j]


# vals[i] <- data.frame(matrix(unlist(statDf), ncol = length(statDf), byrow=TRUE))

i = i+1
# print(i)
# finalDf[j] <- vals[i]
}

# finalDf[j] <- append(statDf,statDf)
# print(vals[i])


colnames(statDf) <- c('#obs','obs','sim','r','MB(ppm)','NMB(%)','MAE','NME(%)','RMSE(ppm)','site')
# statDf <- data.frame()
print(sheets[j])
print(statDf)

finalDf <- rbind(finalDf,statDf)
# colnames(statDf) <- c('#obs','obs','sim','r','MB(ppm)','NMB(%)','MAE','NME(%)','RMSE(ppm)')

}

write.csv(finalDf,
          file = 'E:/Eeshan/evaluation/eval_NACP_NOAA_annual_report.csv')

