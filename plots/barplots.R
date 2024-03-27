#install.packages("readxl")
#library(sirad)
library(xlsx)
library(readxl)
library(openxlsx)
library(lubridate)
library(dplyr)

df <- read_excel("E:/Eeshan/evaluation/results/AEESP_barplots.xlsx",
                 sheet = "JJA")

# Create a matrix with the data for both sets of bar plots
#BU
data_matrix <- matrix(c(df$NMB[1:4],
                        df$NMB[5:8],
                        df$NMB[9:12]),
                      nrow = 4)
# Nahant
# data_matrix <- matrix(c(df$NMB[13:16],
#                         df$NMB[17:20],
#                         df$NMB[21:24]),
#                       nrow = 4)
# Thompson Island
# data_matrix <- matrix(c(df$NMB[25:28],
#                         df$NMB[29:32],
#                         df$NMB[33:36]),
#                       nrow = 4)

# Create bar plots for both sets
barplot(data_matrix,
        beside = TRUE,
        main = "Normalized Mean Bias",
        #xlab = "Model",
        ylab = "%",
        col = c("red", "blue", "green", "grey", 
                "red", "blue", "green", "grey", 
                "red", "blue", "green", "grey"),
        width = 0.2,
        ylim = c(-35,35))

# Add x-axis titles below each set of bar plots
axis(side = 1, 
     at = c(0.6, 1.6, 2.6), 
     labels = c("D01", "D02", "D03"), 
     tick = FALSE, 
     line = -15.5)

# Add a dotted black line at a specific y-axis position
abline(h = -25, 
       col = "black", 
       lty = 3)

abline(h = 25, 
       col = "black", 
       lty = 3)

# Add a word over the line
text(x = 0.1, 
     y = -24, 
     labels = "Threshold", 
     pos = 4, 
     col = "black")

# Add a word over the line
text(x = 0.1, 
     y = 24, 
     labels = "Threshold", 
     pos = 4, 
     col = "black")

# Add legend
# legend("topright",
#        y = -35,
#        legend = c("EDGAR", "FFDAS", "ODIAC", "Vulcan"),
#        fill = c("red", "blue", "green", "grey"),
#        border = NA)


