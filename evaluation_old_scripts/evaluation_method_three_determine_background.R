#install.packages("readxl")
library(sirad)
library(xlsx)
library(readxl)
library(openxlsx)
library(lubridate)
library(dplyr)

# the final dataframe where everything will be saved
finalDf <- data.frame()

##### BU ##### 
# reading the data
df <- read_excel("E:/Eeshan/evaluation/evaluation_method_one_paper_one.xlsx",
                 sheet = "Background",
                 col_types = c('date','numeric','numeric'))

df[df <= 0] <- NA #removing negative values
df <- na.omit(df)

min_BU_background <- min(df$Background_Obs_BU)
min_UU_background <- min(df$Background_Obs_UU)

df_temp <- data.frame(df$Time, df$Background_Obs_UU)
df_temp[df_temp <= 0] <- NA #removing negative values
df_temp <- na.omit(df_temp)
colnames(df_temp) <- c('Time','Background_Obs_UU')

min_UU_background_all_months <- min(df_temp$Background_Obs_UU)
