library(readxl)
library(dplyr)

# # reading the data
# df <- read_excel("E:/Eeshan/evaluation/CCGG_NACP_all_sites_minus_background.xlsx",
#                  sheet = "UU",
#                  col_types = c('date','numeric','numeric','numeric','numeric',
#                                'numeric','numeric','numeric','numeric',
#                                'numeric','numeric','numeric','numeric'))

df <- read_excel("E:/Eeshan/evaluation/CCGG_NACP_all_sites_minus_background.xlsx",
                 sheet = "BU",
                 col_types = c('date','numeric','numeric','numeric','numeric'))

df[df <= 0] <- NA
df <- na.omit(df)

#####Jan#####
# reading in data for specific months
df_Jan_initial <- df %>% 
  filter(df$Time > as.POSIXct("2013-12-31 23:00:00", tz="UTC") & df$Time < as.POSIXct("2014-02-01 00:00:00", tz="UTC"))

time <- df_Jan_initial[, 1]
#df_without_time <- df_Jan_initial[, 9:13]
df_without_time <- df_Jan_initial[, 2:5]
options(digits = 3) #changing the decimal places 
df_Jan <- data.frame(time, df_without_time)

par(mar = c(5, 4, 5, 4) + 0.3) 

plot(df_Jan_initial$Time,
     df_Jan_initial$Obs,
     xaxt = 'n',
     yaxt = 'n',
     xlab = "",
     ylab = "",
     col = 'black',
     pch = 16,
     main= "Boston University (BU), MA",
     cex.main = 1)

lines(df_Jan_initial$Time,
      df_Jan_initial$VULCAN,
      type = 'l',
      col = 'blue')

mtext('Local Time, 2014', 
      side=1, 
      line=3.2, 
      cex=1) 

axis(2, 
     cex.axis=1, 
     las = 1,
     at = pretty(range(max(df_Jan_initial$Obs),min(df_Jan_initial$Obs))))

mtext(expression('CO'[2]*' (ppm)'), 
      side=2, 
      line=2.6, 
      cex=1)

par(new = TRUE)

plot(df_Jan_initial$Time,
     df_Jan_initial$Background_Obs,
     pch = 16,
     col = 'green',
     axes = F,
     xlab = "",
     ylab = "",
     ylim = range(df_Jan_initial$Background_Obs,
                  df_Jan_initial$Background_Sim))

lines(df_Jan$Time,
      df_Jan_initial$Background_Sim,
      type = 'l',
      col = 'red')

axis.POSIXct(1, at = seq(from = as.POSIXct(df_Jan$Time[1]), 
                         to = as.POSIXct(df_Jan$Time[343]), length.out=7),
             #labels = seq(df$Time[1], df$Time[744], length.out=10),
             format = "%m/%d", las = 1,
             cex.axis = 1)

axis(side = 4,
     las = 1,
     at = pretty(range(df_Jan_initial$Background_Obs,
                       df_Jan_initial$Background_Sim)))      

mtext(expression('Background CO'[2]*' (ppm)'), 
                 side = 4, 
                 line = 3)
# add legends to the plot with the line-type
legend("topleft",
       legend = c("Obs","Vulcan","Back_Obs","Back_Sim"),
       lty=c(NA,1,NA,1),
       pch=c(16,NA,16,NA),
       cex=0.8,
       pt.cex = 0.8,
       #horiz = T,
       #y.intersp = 0.1,
       #merge = FALSE,
       col=c("black","blue","green","red"))

mean(df_Jan_initial$Background_Obs)
mean(df_Jan_initial$Background_Sim)


#####July#####
# reading in data for specific months
df_July_initial <- df %>% 
  filter(df$Time > as.POSIXct("2014-06-30 23:00:00", tz="UTC") & df$Time < as.POSIXct("2014-08-01 00:00:00", tz="UTC"))

time <- df_July_initial[, 1]
df_without_time <- df_July_initial[, 2:5]
options(digits = 3) #changing the decimal places 
df_July <- data.frame(time, df_without_time)

par(mar = c(5, 4, 5, 4) + 0.3) 

plot(df_July_initial$Time,
     df_July_initial$Obs,
     xaxt = 'n',
     yaxt = 'n',
     xlab = "",
     ylab = "",
     col = 'black',
     pch = 16,
     main= "Boston University (BU), MA",
     cex.main = 1)

lines(df_July_initial$Time,
      df_July_initial$VULCAN,
      type = 'l',
      col = 'blue')

mtext('Local Time, 2014', 
      side=1, 
      line=3.2, 
      cex=1) 

axis(2, 
     cex.axis=1, 
     las = 1,
     at = pretty(range(max(df_July_initial$Obs),min(df_July_initial$Obs))))

mtext(expression('CO'[2]*' (ppm)'), 
      side=2, 
      line=2.6, 
      cex=1)

par(new = TRUE)

plot(df_July_initial$Time,
     df_July_initial$Background_Obs,
     pch = 16,
     col = 'green',
     axes = F,
     xlab = "",
     ylab = "",
     ylim = range(df_July_initial$Background_Obs,
                  df_July_initial$Background_Sim))

lines(df_July$Time,
      df_July_initial$Background_Sim,
      type = 'l',
      col = 'red')

axis.POSIXct(1, at = seq(from = as.POSIXct(df_July$Time[1]), 
                         to = as.POSIXct(df_July$Time[653]), length.out=7),
             #labels = seq(df$Time[1], df$Time[744], length.out=10),
             format = "%m/%d", las = 1,
             cex.axis = 1)

axis(side = 4,
     las = 1,
     at = pretty(range(df_July_initial$Background_Obs,
                       df_July_initial$Background_Sim)))      

mtext(expression('Background CO'[2]*' (ppm)'), 
      side = 4, 
      line = 3)

# add legends to the plot with the line-type
legend("topright",
       legend = c("Obs","Vulcan","Back_Obs","Back_Sim"),
       lty=c(NA,1,NA,1),
       pch=c(16,NA,16,NA),
       cex=0.8,
       pt.cex = 0.8,
       #horiz = T,
       #y.intersp = 0.1,
       #merge = FALSE,
       col=c("black","blue","green","red"))

mean(df_July_initial$Background_Obs)
mean(df_July_initial$Background_Sim)

