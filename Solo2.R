# Solo 2 Project - Conjoint Analysis
library(bayesm)
library(dummies)

# Step 1: Load the Data
setwd("~/Dropbox/MSPA/450/Assignments/Solo2")
load("stc-cbc-respondents-v3(1).RData")
load("efCode.RData") 
ls()

stc_dc <- read.csv("stc-dc-task-cbc-v3(1).csv", header=T)
extra <- read.csv("extra-scenarios(1).csv", header=T)

# Step 2: Creating the Predictor Variables
task_mat <- as.matrix(stc_dc[, c('screen', 'RAM', 'processor', 'price', 'brand')])
X.mat <- efcode.attmat.f(task_mat)

price_v <- stc_dc$price - mean(stc_dc$price)
X.mat.brandbyprice <- X.mat[,9:11] * price_v
X.matrix <- cbind(X.mat, X.mat.brandbyprice)

# Test
det(t(X.matrix) %*% (X.matrix))

y_data <- resp.data.v3[,4:39]
apply(y_data, 2, function(x) sum(is.na(x)))
y_data <- as.matrix(y_data)
apply(resp.data.v3[4:39], 2, function(x){tabulate(na.omit(x))}) 
#zowner <- ifelse((resp.data.v3$vList3 %in% c(1,2)),1,0)
zowner <- 1 * ( ! is.na(resp.data.v3$vList3) )

lgtdata <- NULL
for (i in 1:424) {
  lgtdata[[i]] <- list(y=y_data[i,], X=X.matrix)
}

# Modelling MCMC (test)
lgtdata100 <- lgtdata[1:100]
mcmctest <- list(R=100000, keep=5)
data1 <- list(p=3, lgtdata=lgtdata)
final_run_1 <- rhierMnlDP(Data=data1, Mcmc=mcmctest)

bd1 <-final_run_1$betadraw
colMeans(apply(bd1[,,3000:8000], c(1,2), sd))

hist(bd1[1,1,],50)

k <- 45
par_now <- par()
par(mfrow=c(2,2))
for (i in round(runif(4,1,14))) {
  plot(1:length(bd1[k,i,]), bd1[k,i,])
}
par(par_now)

plot(1:length(bd1[1,1,]), bd1[1,1,])
plot(density(bd1[1,1,15000:20000],width=2))
summary(bd1[1,1,15000:20000])
apply(bd1[,,15000:20000],c(2),mean)
table(final_run_1$Istardraw)

# Creating the part worth DF
pw <- data.frame(t(apply(bd1[,,15000:20000],c(2),mean)))
names(pw) <- c('scr8', 'scr10', 'ram16', 'ram32', 'prc2', 'prc2.5', 'price299', 'price399', 
               'Samesong', 'Pear', 'Gaggle', 'Price_Samesong', 'Price_Pear', 'Price_Gaggle')

pw$scr5 <- -1*pw$scr8 + (-1)*pw$scr10
pw$ram8 <- -1*pw$ram16 + (-1)*pw$ram32
pw$prc1.5 <- -1*pw$prc2 + (-1)*pw$prc2.5
pw$price199 <- -1*pw$price299 + (-1)*pw$price399
pw$STC <- -1*pw$Samesong + (-1)*pw$Pear + (-1)*pw$Gaggle
pw$Price_STC <- -1*pw$Price_Samesong + (-1)*pw$Price_Pear + (-1)*pw$Price_Gaggle

pw_df_1 <- pw[c('scr5', 'scr8', 'scr10', 'ram8', 'ram16', 'ram32', 'prc1.5', 'prc2', 'prc2.5', 'price199', 
                  'price299', 'price399', 'STC', 'Samesong', 'Pear', 'Gaggle', 'Price_STC', 
                  'Price_Samesong', 'Price_Pear', 'Price_Gaggle')]
pw_table <- data.frame(names(pw_df_1), t(pw_df_1), t(exp(pw_df_1)))

dev.off()
barplot(as.matrix(pw_df_1[,1:20]), names.arg=names(pw_df_1), 
        cex.names=0.8, col='lightblue', width=1, las=2, ylim=c(-3,3), main='Part Worth Model 1', pos=0)

# Predicting the 36 scenarios with this model
beta_1 <- apply(bd1[,,15000:20000], c(1,2), mean)
xbeta <- X.matrix %*% t(beta_1)
xbeta2 <- matrix(xbeta, ncol=3, byrow=T)
expxbeta2 <- exp(xbeta2)
rsumvec <- rowSums(expxbeta2)
pchoice <- expxbeta2/rsumvec
custchoice <- max.col(pchoice)

# Confusion Matrix
ydatavec <- as.vector(t(y_data))
table(custchoice, ydatavec)

# Model Prediction Accuracy
sum(diag(table(custchoice, ydatavec)))/sum(table(custchoice, ydatavec))
[1]# 0.8835168

#               ydatavec
#custchoice    1    2    3
#1           3677  415  222
#2            233 3760  200
#3            294  414 6049

require("pROC")
dev.off()
roctest <- roc(ydatavec, custchoice, plot=TRUE)    ### ROC curve
auc(roctest)                       #### Area Under the Curve

# Predicting the extra 2 scenarios with this model
X.ex.matrix <- as.matrix(extra)

ex_xbeta <- X.ex.matrix %*% t(beta_1)
ex_xbeta.m <- matrix(ex_xbeta, ncol=3, byrow=T)
exp_ex.xbeta <- exp(ex_xbeta.m)
ex_rsumvec <- rowSums(ex_xbeta.m)
ex_choice.mat <- exp_ex.xbeta/ex_rsumvec
ex_pchoice <- max.col(ex_choice.mat)
ex_df <- as.data.frame(matrix(ex_pchoice, ncol=2, byrow=T))
names(ex_df) <- c('ChoiceSet1', 'ChoiceSet2')
apply(ex_df, 2, table)

# Analysis Resoults of betas
#apply(bd1[,,800:100], c(2), mean)
#apply(bd1[,,800:100], c(1,2), mean)

# Adding the Z parameter to the model
zownertest <- matrix(scale(zowner, scale=FALSE), ncol=1)
mcmctest <- list(R=100000, keep=5)
data2 <- list(p=3, lgtdata=lgtdata, Z=zownertest)
final_run_2 <- rhierMnlDP(Data=data2, Mcmc=mcmctest)

bd2 <-final_run_2$betadraw
colMeans(apply(bd2[,,3000:8000], c(1,2), sd))

hist(bd2[1,1,],50)

k <- 164
par_now <- par()
par(mfrow=c(2,2))
for (i in round(runif(4,1,14))) {
  plot(1:length(bd2[k,i,]), bd2[k,i,], main=i)
}
par(par_now)

plot(1:length(bd1[1,1,]), bd1[1,1,])
plot(density(bd2[1,1,15000:20000],width=2))
summary(bd2[1,1,15000:20000])
apply(bd2[,,15000:20000],c(2),mean)
table(final_run_2$Istardraw)

# Creating the part worth DF
pw <- data.frame(t(apply(bd2[,,15000:20000],c(2),mean)))
names(pw) <- c('scr8', 'scr10', 'ram16', 'ram32', 'prc2', 'prc2.5', 'price299', 'price399', 
               'Samesong', 'Pear', 'Gaggle', 'Price_Samesong', 'Price_Pear', 'Price_Gaggle')

pw$scr5 <- -1*pw$scr8 + (-1)*pw$scr10
pw$ram8 <- -1*pw$ram16 + (-1)*pw$ram32
pw$prc1.5 <- -1*pw$prc2 + (-1)*pw$prc2.5
pw$price199 <- -1*pw$price299 + (-1)*pw$price399
pw$STC <- -1*pw$Samesong + (-1)*pw$Pear + (-1)*pw$Gaggle
pw$Price_STC <- -1*pw$Price_Samesong + (-1)*pw$Price_Pear + (-1)*pw$Price_Gaggle

pw_df_2 <- pw[c('scr5', 'scr8', 'scr10', 'ram8', 'ram16', 'ram32', 'prc1.5', 'prc2', 'prc2.5', 'price199', 
                'price299', 'price399', 'STC', 'Samesong', 'Pear', 'Gaggle', 'Price_STC', 
                'Price_Samesong', 'Price_Pear', 'Price_Gaggle')]
pw_table <- data.frame(names(pw_df_2), t(pw_df_2), t(exp(pw_df_2)))

dev.off()
barplot(as.matrix(pw_df_2[,1:20]), names.arg=names(pw_df_2), 
        cex.names=0.8, col='lightblue', width=1, las=2, ylim=c(-3,3), main='Part Worth Model 2', pos=0)

# Additional Analysis - Deltadraws
dim(final_run_2$Deltadraw)
apply(final_run_2$Deltadraw[15000:20000,], 2, mean)
apply(final_run_2$Deltadraw[15000:20000,], 2, quantile, probs=c(0.1, 0.25, 0.5, 0.75, 0.9))

pw_dd <- data.frame(t(apply(final_run_2$Deltadraw[15000:20000,],2,mean)))
names(pw_dd) <- c('scr8', 'scr10', 'ram16', 'ram32', 'prc2', 'prc2.5', 'price299', 'price399', 
               'Samesong', 'Pear', 'Gaggle', 'Price_Samesong', 'Price_Pear', 'Price_Gaggle')

pw_dd$scr5 <- -1*pw_dd$scr8 + (-1)*pw_dd$scr10
pw_dd$ram8 <- -1*pw_dd$ram16 + (-1)*pw_dd$ram32
pw_dd$prc1.5 <- -1*pw_dd$prc2 + (-1)*pw_dd$prc2.5
pw_dd$price199 <- -1*pw_dd$price299 + (-1)*pw_dd$price399
pw_dd$STC <- -1*pw_dd$Samesong + (-1)*pw_dd$Pear + (-1)*pw_dd$Gaggle
pw_dd$Price_STC <- -1*pw_dd$Price_Samesong + (-1)*pw_dd$Price_Pear + (-1)*pw_dd$Price_Gaggle

pw_dd_df_2 <- pw_dd[c('scr5', 'scr8', 'scr10', 'ram8', 'ram16', 'ram32', 'prc1.5', 'prc2', 'prc2.5', 'price199', 
                'price299', 'price399', 'STC', 'Samesong', 'Pear', 'Gaggle', 'Price_STC', 
                'Price_Samesong', 'Price_Pear', 'Price_Gaggle')]
pw_dd_table <- data.frame(names(pw_dd_df_2), t(pw_dd_df_2), t(exp(pw_dd_df_2)))

dev.off()
barplot(as.matrix(t(pw_dd_table[,2])), names.arg=pw_dd_table[,1], 
        cex.names=0.8, col='lightblue', width=1, las=2, ylim=c(-1,1), main='Deltadraw', pos=0)


# Customer Choice Prediction
betameans <- apply(final_run_2$betadraw[,,800:1000], c(1,2), mean)
xbeta <- X.matrix %*% t(betameans)
xbeta2 <- matrix(xbeta, ncol=3, byrow=T)

xbeta2exp <- exp(xbeta2)
rsumvec <- rowSums(xbeta2exp)
pchoicemat <- xbeta2exp/rsumvec
custchoice <- max.col(pchoicemat)

ydatavec <- as.vector(t(y_data))
str(ydatavec)
table(custchoice, ydatavec)

# Model Prediction Accuracy
sum(diag(table(custchoice, ydatavec)))/sum(table(custchoice, ydatavec))

require("pROC")
dev.off()
roctest <- roc(ydatavec, custchoice, plot=TRUE)    ### ROC curve
auc(roctest)                       #### Area Under the Curve


# Predicting the two extra choice sets
beta_2 <- apply(final_run_2$betadraw[,,15000:20000], c(1,2), mean)
X.ex.matrix <- as.matrix(extra)
ex_xbeta <- X.ex.matrix %*% t(beta_2)
ex_xbeta.m <- matrix(ex_xbeta, ncol=3, byrow=T)
exp_ex.xbeta <- exp(ex_xbeta.m)
ex_rsumvec <- rowSums(ex_xbeta.m)
ex_choice.mat <- exp_ex.xbeta/ex_rsumvec
ex_pchoice <- max.col(ex_choice.mat)
ex_df <- as.data.frame(matrix(ex_pchoice, ncol=2, byrow=T))
names(ex_df) <- c('ChoiceSet1', 'ChoiceSet2')
apply(ex_df, 2, table)



m <- matrix(custchoice, nrow=36, byrow=F)
m2 <- t(m)
apply(m2, 2, function(x) tabulate(na.omit(x)))

