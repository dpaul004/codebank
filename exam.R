## Creatinng Groups ##
coke <- read.csv("Coke.csv")

cells <- seq(from = 339, to = 341.2, by = 0.2)
center <- seq(from = 339.1, to = 341.1, by = 0.2)

Fill_Volume <- coke$Fill
# Cut() places each fill volume into its associated cell.
Fill_Volume <- cut(Fill_Volume, cells, include.lowest=TRUE, right = FALSE)
# table() followed by prop.table() calculates proportions in each cell.
Fill_Volume <- prop.table(table(Fill_Volume))
# Include the cell center in the data frame.
Fill_Volume <- data.frame(Fill_Volume, center)

count <- Fill_Volume$Freq*length(coke$Fill)
Fill_Volume <- data.frame(Fill_Volume, count)

mean <- sum(Fill_Volume$Freq*Fill_Volume$center)
mean
delta2 <- (Fill_Volume$center - mean)**2
std <- sqrt(sum(delta2*Fill_Volume$Freq))
std

