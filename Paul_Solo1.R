#Packages
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
library(corrplot)

# Parameters

#Functions

# load the data
setwd("~/Dropbox/MSPA/450/Assignments")
load("apphappyData.RData")
ls()

numdata <- apphappy.3.num.frame
dim(numdata)
head(numdata,2)

### Creating subsets ###

numsubr <- subset(numdata, select=
                    c("q24r1","q24r2","q24r3","q24r4","q24r5","q24r6","q24r7","q24r8","q24r9",
                      "q24r10","q24r11","q24r12",
                      "q25r1","q25r2","q25r3","q25r4","q25r5","q25r6","q25r7","q25r8","q25r9",
                      "q25r10","q25r11","q25r12",
                      "q26r3","q26r4","q26r5","q26r6","q26r7","q26r8","q26r9","q26r10","q26r11",
                      "q26r12","q26r13","q26r14","q26r15","q26r16","q26r17","q26r18"))

numsub <- subset(numdata, select=
                   c("q24r1","q24r2","q24r3","q24r4","q24r5","q24r6","q24r7","q24r8","q24r9",
                     "q24r10","q24r11","q24r12",
                     "q25r1","q25r2","q25r3","q25r4","q25r5","q25r6","q25r7","q25r8","q25r9",
                     "q25r10","q25r11","q25r12",
                     "q26r3","q26r4","q26r5","q26r6","q26r7","q26r8","q26r9","q26r10","q26r11",
                     "q26r12","q26r13","q26r14","q26r15","q26r16","q26r17","q26r18"))

## The following code focusses on EDA
apply(numsub,2,table)

# The correlation plot
mcor <- cor(numsub)
corrplot(mcor, method=c("shade"), tl.col="black", tl.cex=0.8)

# 
attach(numsub)
hist3D(z=table(q24r11, q24r10), border="black")

# The PCA Plot
dev.off()
pca <-princomp(numsub)
summary(pca)
plot(pca$scores[,1],pca$scores[,2])

names(pca)
str(pca)
head(pca$scores)
##  Create cuts:
pcadf <- as.data.frame(pca$scores)
pca1 <- cut(pcadf$Comp.1, 10)
pca2 <- cut(pcadf$Comp.2, 10)

##  Calculate joint counts at cut levels:
z <- table(pca1, pca2)

##  Plot as a 3D histogram:
hist3D(z=z, border="black")
summary(pca)

# Find the optimal number of cluseters based on a scree pplot off Within Sum of Squares
dev.off()
wssplot <- function(data, nc, seed=1234) {
  
  wss <- (nrow(data) - 1) * sum(apply(data,2,var))
  
  for (i in 2:nc) {
    set.seed(seed)
    wss[i] <- sum(kmeans(data, centers=i,nstart=10)$withinss)
  }
  
  plot(1:nc, wss, xlab="Nr of Clusters", ylab="Within Group SS", type="b")
}

wssplot(numsubr, 15)

# Create Base Model with 5 clusters and basis variable
km_5 <- kmeans(numsubr, centers=5, nstart=50)
km_5$size
rsquare <- km_5$betweenss/km_5$totss
rsquare

plot(km_5, numsubr)

dissE <- daisy(numsubr)
dE2 <- dissE^2
sK2 <- silhouette(km_5$cluster, dE2)
str(sK2)
dev.off()
plot(sK2)

# Create derived variables
attach(numsub)
numsub$q24a <- log((q24r2 + q24r3 + q24r6)/3)
numsub$q24b <- log((q24r7 + q24r8)/2)
numsub$q24c <- 7 - ((q24r4 + q24r9)/2)
numsub$q24d <- log((q24r10 + q24r11 + q24r12)/3)
numsub$q25a <- log((q25r1 + q25r2+ q25r3 + q25r4 + q25r5)/5)
numsub$q25b <- 7 - (q25r6)
numsub$q25c <- log((q25r7 + q25r8 + q25r9 + q25r10 + q25r11)/5)
numsub$q26a <- log((q26r6 + q26r8 + q26r10 + q26r12 + q26r17)/5)
numsub$q26b <- log((q26r7 + q26r14 + q26r16)/3)
numsub$q26c <- log((q26r3 + q26r11 + q26r13)/3)

numsub_n <- subset(numsub, select=c('q24a', 'q24b', 'q24c', 'q24d', 'q25a', 'q25b',
                                    'q25c',  'q26a', 'q26b', 'q26c'))

# The PCA Plot with the derived variables
dev.off()
pca <-princomp(numsub_n)
summary(pca)
plot(pca$scores[,1],pca$scores[,2])

names(pca)
str(pca)
head(pca$scores)
##  Create cuts:
pcadf <- as.data.frame(pca$scores)
pca1 <- cut(pcadf$Comp.1, 10)
pca2 <- cut(pcadf$Comp.2, 10)

##  Calculate joint counts at cut levels:
z <- table(pca1, pca2)

##  Plot as a 3D histogram:
hist3D(z=z, border="black")

# Create Improved Model with 6 clusters and derived variable
set.seed(1234)
km_5 <- kmeans(numsub_n, centers=5, nstart=50)
km_5$size
rsquare <- km_5$betweenss/km_5$totss
rsquare

plot(km_5, numsub_n)

dissE <- daisy(numsub_n)
dE2 <- dissE^2
sK2 <- silhouette(km_5$cluster, dE2)
str(sK2)
dev.off()
plot(sK2)

# Create a data frame for typying tools
clusters <- km_5$cluster
t_data <- cbind(subset(numdata, select=c('q1', 'q48', 'q49', 'q54', 'q55', 'q56', 'q57')), 
                       numsub_n, clusters)

t_data$q24a <- (exp(t_data$q24a))
t_data$q24b <- (exp(t_data$q24b))
t_data$q24c <- (7 - (t_data$q24c))
t_data$q24d <- (exp(t_data$q24d))
t_data$q25a <- (exp(t_data$q25a))
t_data$q25b <- (7 - (t_data$q25b))
t_data$q25c <- (exp(t_data$q25c))
t_data$q26a <- (exp(t_data$q26a))
t_data$q26b <- (exp(t_data$q26b))
t_data$q26c <- (exp(t_data$q26c))

round(aggregate(t_data, by=list(t_data$clusters), mean),1)

###############################################
## Hierarchical Clustering with derived data ##
###############################################
numsub.dist = dist(numsub_n)
require(maptree)
hclustmodel <- hclust(dist(numsub_n), method = 'ward.D2')
names(hclustmodel)
plot(hclustmodel)

cut.5 <- cutree(hclustmodel, k=5)
plot(silhouette(cut.5, numsub.dist))
head(cut.5)

# Calculate SSW and SST for the above clusters.
require(proxy)
numsubmat <- as.matrix(numsub_n)
overallmean <- matrix(colMeans(numsubmat), nrow=1)
TSS <- sum(dist(numsubmat, overallmean, method='euclidean')^2)
TSS

# Calculate WSS bsed on 5 clusters
calc_d <- function(x) {
  x_mat <- as.matrix(x)
  overallmean <- matrix(colMeans(x_mat), nrow=1)
  TSS <- sum(dist(x_mat, overallmean, method='euclidean')^2)
}

combdata <- cbind(numsub_n, cut.5)
WSS <- sum(tapply(1:nrow(combdata), cut.5, function(x) calc_d(combdata[x,1:10])))

rsquare <- 1 - WSS/TSS
rsquare

#################################
# PAM method
###############################
clusterresultsPAM <-pam(numsub_n,5)
summary(clusterresultsPAM)
str(clusterresultsPAM$silinfo)
plot(clusterresultsPAM, which.plots=1)
plot(clusterresultsPAM, which.plots=2)

###################
## Model based clustering
##################
library(mclust)
fit <- Mclust(numsub_n,5)
plot(fit,data=numsub_n, what="density") # plot results
#plot(fit,data=numsub2, what="BIC") # plot results

summary(fit) # display the best model

dev.off()
dissE <- daisy(numsub_n)
names(dissE)
dE2   <- dissE^2
sk2   <- silhouette(fit$classification, dE2)
str(sk2)
plot(sk2)

###############################################
## comparison of cluster results  #############
###############################################
clstat <- cluster.stats(numsub.dist, km_5$cluster, clusterresultsPAM$cluster)
names(clstat)
clstat$corrected.rand

clstat <- cluster.stats(numsub.dist, km_5$cluster, cut.5)
clstat$corrected.rand
