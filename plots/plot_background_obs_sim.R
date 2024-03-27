library(readxl)
library(dplyr)
library(scales)

# reading the data
df <- read_excel("E:/Eeshan/evaluation/CCGG_NACP_all_sites_minus_background.xlsx",
                 sheet = "AMT_Sim_vs_UU_BCK_Obs",
                 col_types = c('date','numeric','numeric','numeric','numeric','numeric','numeric','numeric'))

df[df <= 0] <- NA
df <- na.omit(df)

par(mar = c(5, 4, 5, 4) + 0.3) 

plot(df$Time,
     df$Obs,
     pch = 19,
     col = alpha("black", alpha = 0.3),
     xaxt = 'n',
     yaxt = 'n',
     xlab = "",
     ylab = "",
     ylim = range(max(df[,2:8]),
                  min(df[,2:8])),
     main= expression('CO'[2]*' across three sites'),
     cex.main = 1)

# lines(df$Time,
#       df$EDGAR,
#       col = 'red',
#       lwd = 2)
# 
# lines(df$Time, 
#       df$FFDAS,
#       col = alpha('blue', alpha = 0.3),
#       lwd = 2)
# 
# lines(df$Time, 
#       df$ODIAC,
#       col = alpha('grey', alpha = 0.3),
#       lwd = 2)

lines(df$Time, 
      df$VULCAN,
      col = alpha('green', 
                  alpha = 0.7),
      lwd = 1)

lines(df$Time,
     df$Background_UU,
     col = alpha("blue", 
                 alpha = 1),
     lwd = 1,
     lty = 2)

lines(df$Time,
      df$Background_BU,
      col = alpha("red", 
                  alpha = 1),
      lwd = 1,
      lty = 2)

axis(2, 
     cex.axis=1, 
     las = 1,
     at = pretty(range(max(df[,2:8]),
                       min(df[,2:8]))))

mtext(expression('CO'[2]*' (ppm)'), 
      side=2, 
      line=2.6, 
      cex=1)

axis.POSIXct(1, at = seq(from = as.POSIXct(df$Time[1]), 
                         to = as.POSIXct(df$Time[1198]), 
                         length.out=7),
             #labels = seq(df$Time[1], df$Time[744], length.out=10),
             format = "%m/%d/%Y", 
             las = 1,
             cex.axis = 1)

# add legends to the plot with the line-type
legend('topright',
       legend = c('Obs','VULCAN','Back_UU','Back_UU'),
       pch = c(19,NA_integer_,NA_integer_,NA_integer_),
       lty = c(0,1,2,2), 
       col = c('black','green','blue','red'),
       cex = 0.8)

# test statistics 
df_July <- df %>% 
  filter(df$Time > as.POSIXct("2012-06-30 23:00:00", tz="UTC") & df$Time < as.POSIXct("2012-08-01 00:00:00", tz="UTC"))
df_July <- df_July[1:744,1:6]
df_July <- na.omit(df_July)

df_Jan <- df %>% 
  filter(df$Time > as.POSIXct("2011-12-31 23:00:00", tz="UTC") & df$Time < as.POSIXct("2012-02-01 00:00:00", tz="UTC"))
df_Jan <- df_Jan[1:744,1:6]
df_Jan <- na.omit(df_Jan)

mean(df_Jan$VULCAN)
mean(df_July$VULCAN)
