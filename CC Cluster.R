require(corrplot)
require(cluster)
require(useful)
require(Hmisc)
require(plot3D)
library(HSAUR)
library(MVA)
library(HSAUR2)
library(fpc)
library(mclust)
library(lattice)
library(car)

parm <- "pred"
setwd("~/Documents/Models/CancunS")

df <- read.csv('test.csv', header=T)
df <- df[,-1]

clusterresults <- kmeans(df,5)
clusterresults$size
rsquare <- clusterresults$betweenss/clusterresults$totss
rsquare
str(clusterresults)
plot(clusterresults, data=numsubr)
dev.off()
dissE <- daisy(df)
names(dissE)
dE2   <- dissE^2
sk2   <- silhouette(clusterresults$cluster, dE2)
str(sk2)
plot(sk2)
