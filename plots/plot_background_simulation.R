library(readxl)
library(dplyr)

# reading the data
df <- read_excel("E:/Eeshan/evaluation/CCGG_NACP_all_sites_minus_background.xlsx",
                 sheet = "background_compare_Jan",
                 col_types = c('date','numeric','numeric','numeric'))


df[df <= 0] <- NA
df <- na.omit(df)

par(mar = c(5, 4, 5, 4) + 0.3) 

plot(df$Time,
     df$Background_BU,
     type = "l",
     xaxt = 'n',
     yaxt = 'n',
     xlab = "",
     ylab = "",
     ylim = range(max(df$Background_UU),
                  min(df$Background_UU),
                  max(df$Background_BU),
                  min(df$Background_BU),
                  max(df$Background_AMT),
                  min(df$Background_AMT)),
     col = 'black',
     main= expression('Background CO'[2]*' across three sites'),
     cex.main = 1)

lines(df$Time,
      df$Background_UU,
      type = 'l',
      col = 'red')

lines(df$Time,
      df$Background_AMT,
      type = 'l',
      col = 'blue')

axis(2, 
     cex.axis=1, 
     las = 1,
     at = pretty(range(max(df$Background_UU),
                       min(df$Background_UU),
                       max(df$Background_BU),
                       min(df$Background_BU),
                       max(df$Background_AMT),
                       min(df$Background_AMT))))

mtext(expression('Simulated Background CO'[2]*' (ppm)'), 
      side=2, 
      line=2.6, 
      cex=1)

axis.POSIXct(1, at = seq(from = as.POSIXct(df$Time[1]), 
                         to = as.POSIXct(df$Time[744]), 
                         length.out=7),
             #labels = seq(df$Time[1], df$Time[744], length.out=10),
             format = "%m/%d", 
             las = 1,
             cex.axis = 1)

# add legends to the plot with the line-type
legend("bottomleft",
       legend = c("Back_BU","Back_UU", "Back_AMT"),
       lty=c(1,1,1),
       pch=c(NA,NA,NA),
       cex=0.8,
       #pt.cex = 0.8,
       #horiz = T,
       #y.intersp = 0.1,
       #merge = FALSE,
       col=c("black","red", "blue"))

# check stats
mean(df$Background_AMT)
mean(df$Background_BU)
mean(df$Background_UU)

# plotting histogram
hist(df$Background_BU,
     xlab = expression('Background CO'[2]*' (ppm)'),
     col = rgb(1,0,0,0.5),
     breaks = 50,
     main = expression('Distribution of Simulated Background CO'[2]*' across three sites'),
     cex.main = 1,
     ylim = c(0,80))

hist(df$Background_UU,
     breaks = 50, 
     col = rgb(0,0,1,0.5), 
     add = T)

hist(df$Background_AMT,
     breaks = 50, 
     col = rgb(0,1,0,0.5), 
     add = T)

legend("topleft", 
       legend=c("BU","UU", "AMT"), 
       col=c(rgb(1,0,0,0.5), rgb(0,0,1,0.5), rgb(0,1,0,0.5)), 
       pt.cex=2, 
       pch=15 )
