#install.packages("readxl")
#library(sirad)
library(xlsx)
library(readxl)
library(openxlsx)
library(lubridate)
library(dplyr)

df <- read_excel("E:/Eeshan/evaluation/results/results_barplots.xlsx",
                 sheet = "JJA_Summary")

df <- df[25]
colnames(df) <- 'NMB'

# Create a matrix with the data for fixed background
#d01 in d03
data_matrix <- matrix(c(df$NMB[6:9],
                        df$NMB[25:28]),
                      nrow = 4)

data_matrix <- as.numeric(data_matrix)
data_matrix <- round(data_matrix, 1)

# Pair the elements
pair_1 <- c(data_matrix[1], data_matrix[5])
pair_2 <- c(data_matrix[2], data_matrix[6])
pair_3 <- c(data_matrix[3], data_matrix[7])
pair_4 <- c(data_matrix[4], data_matrix[8])

# Set bar width
bar_width <- 0.4

# Calculate the positions for the bars
bar_positions <- barplot(matrix(c(pair_1, pair_2, pair_3, pair_4), nrow = 2),
                         beside = TRUE,
                         width = bar_width,
                         col = c("red", "red", "blue", "blue", 'green', 'green', 'grey', 'grey'),
                         density = c(NA, 20, NA, 20, NA, 20, NA, 20),
                         angle = c(NA, 135, NA, 135, NA, 135, NA, 135),
                         ylim = range(c(-50,50)),
                         xlab = "",
                         ylab = "NMB (%)")

# Add x-axis titles below each set of bar plots
axis(side = 1, 
     at = c(0.8, 2, 3.2, 4.4), 
     labels = c("EDGAR", "FFDAS", "ODIAC", "Vulcan"), 
     tick = T)

# Add a dotted black line at a specific y-axis position
abline(h = -25, 
       col = "black", 
       lty = 3)

abline(h = 25, 
       col = "black", 
       lty = 3)

abline(h = 0, 
       col = "black", 
       lty = 1)

# Add a word over the line
text(x = 0.2, 
     y = -23, 
     labels = "Threshold", 
     pos = 4, 
     col = "black")

# Add a word over the line
text(x = 0.2, 
     y = 27, 
     labels = "Threshold", 
     pos = 4, 
     col = "black")

# Add legend
# legend("topright",
#        y = -35,
#        legend = c("EDGAR", "FFDAS", "ODIAC", "Vulcan"),
#        fill = c("red", "blue", "green", "grey"),
#        border = NA)


