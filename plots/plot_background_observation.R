library(readxl)
library(dplyr)

# reading the data
df <- read_excel("E:/Eeshan/evaluation/CCGG_NACP_all_sites_minus_background.xlsx",
                 sheet = "background_compare_BU_UU",
                 col_types = c('date','numeric','numeric'))


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
     col = 'black',
     main= "Comparison Between Background Observational \nCO2 at Two Different Sites",
     cex.main = 1)

lines(df$Time,
      df$Background_UU,
      type = 'l',
      col = 'red')

axis(2, 
     cex.axis=1, 
     las = 1,
     at = pretty(range(max(df$Background_UU),
                       min(df$Background_UU),
                       max(df$Background_BU),
                       min(df$Background_BU))))

mtext(expression('Background CO'[2]*' (ppm)'), 
      side=2, 
      line=2.6, 
      cex=1)

axis.POSIXct(1, at = seq(from = as.POSIXct(df$Time[1]), 
                         to = as.POSIXct(df$Time[1414]), length.out=7),
             #labels = seq(df$Time[1], df$Time[744], length.out=10),
             format = "%m/%d/%Y", las = 1,
             cex.axis = 1)

# add legends to the plot with the line-type
legend("topright",
       legend = c("Back_BU","Back_UU"),
       lty=c(1,1),
       pch=c(NA,NA),
       cex=0.8,
       #pt.cex = 0.8,
       #horiz = T,
       #y.intersp = 0.1,
       #merge = FALSE,
       col=c("black","red"))

# test statistics 
df_July <- df %>% 
  filter(df$Time > as.POSIXct("2014-06-30 23:00:00", tz="UTC") & df$Time < as.POSIXct("2014-08-01 00:00:00", tz="UTC"))

df_Jan <- df %>% 
  filter(df$Time > as.POSIXct("2013-12-31 23:00:00", tz="UTC") & df$Time < as.POSIXct("2014-02-01 00:00:00", tz="UTC"))

# plotting histogram
hist(df$Background_BU,
     xlab = expression('Background CO'[2]*' (ppm)'),
     col = rgb(1,0,0,0.5),
     breaks = 50,
     main = expression('Distribution of Observational Background CO'[2]*' across two sites'),
     cex.main = 1,
     ylim = c(0,120))

hist(df$Background_UU,
     breaks = 50, 
     col = rgb(0,0,1,0.5), 
     add = T)

legend("topright", 
       legend=c("BU","UU"), 
       col=c(rgb(1,0,0,0.5), rgb(0,0,1,0.5)), 
       pt.cex=2, 
       pch=15 )

rect(390,-1,410,110,
     border = "black",
     lwd = 2) #red rectangle

bu <- sprintf(table(cut(df$Background_BU,breaks=c(0,390,410,Inf)))/length(df$Background_BU),
              fmt = '%#.2f')
uu <- sprintf(table(cut(df$Background_UU,breaks=c(0,390,405,Inf)))/length(df$Background_UU),
              fmt = '%#.2f')

legend('topleft',
       legend = bu,
       cex = 0.6,
       horiz = T,
       col = rgb(1,0,0,0.5))

legend('left',
       legend = uu,
       cex = 0.6,
       horiz = T,
       col = rgb(0,0,1,0.5))


# monthly histogram
# plotting histogram
hist(df_Jan$Background_BU,
     xlab = expression('Background CO'[2]*' (ppm)'),
     col = rgb(1,0,0,0.5),
     breaks = 50,
     main = expression('Distribution of Observational Background CO'[2]*' across two sites'),
     cex.main = 1,
     ylim = c(0,10))

hist(df$Background_UU,
     breaks = 50, 
     col = rgb(0,0,1,0.5), 
     add = T)

legend("topright", 
       legend=c("BU","UU"), 
       col=c(rgb(1,0,0,0.5), rgb(0,0,1,0.5)), 
       pt.cex=2, 
       pch=15 )

rect(390,-1,410,110,
     border = "black",
     lwd = 2) #red rectangle

bu <- sprintf(table(cut(df$Background_BU,breaks=c(0,390,410,Inf)))/length(df$Background_BU),
              fmt = '%#.2f')
uu <- sprintf(table(cut(df$Background_UU,breaks=c(0,390,405,Inf)))/length(df$Background_UU),
              fmt = '%#.2f')

legend('topleft',
       legend = bu,
       cex = 0.6,
       horiz = T,
       col = rgb(1,0,0,0.5))

legend('left',
       legend = uu,
       cex = 0.6,
       horiz = T,
       col = rgb(0,0,1,0.5))


