# PREDICT 422 Practical Machine Learning

# Course Project - Example R Script File

# OBJECTIVE: A charitable organization wishes to develop a machine learning
# model to improve the cost-effectiveness of their direct marketing campaigns
# to previous donors.

# 1) Develop a classification model using data from the most recent campaign that
# can effectively capture likely donors so that the expected net profit is maximized.

# 2) Develop a prediction model to predict donation amounts for donors - the data
# for this will consist of the records for donors only.

#Packages
library(ggplot2)
library(gridExtra)
library(tree)
library(rpart)
library(glmnet)
library(gam)
library(randomForest)
library(h2o)
library(pROC)
library(leaps)
library(e1071)
library(gbm)

# Parameters
trnsfrm = "Y"
var_sel = "N"
eda = "N"
cat_xfm = "Y"
prt = "N"

#Functions
#Validate model on the valid set
validate_clf_model <- function(post.valid, c.valid, prt) {
  
  profit <- cumsum(14.5 * as.numeric(as.character(c.valid[order(post.valid, decreasing=T)])) - 2)
  n.mail.valid <- which.max(profit) # number of mailings that maximizes profits
  
  if (prt == "Y") {
    plot(profit, xlab = "Number of Mailings", ylab="Net Profit") # see how profits change as more mailings are made
    abline(v=which.max(profit), lty = 2, col = "red")
    legend ("bottomright", legend = c(paste("Total Mails: ", n.mail.valid, sep=""),
                                      paste("Profit: ", max(profit), sep="")), col = c('black', "blue"), pch = c(19, 2))
    
    roc1 <- roc(c.valid, post.valid)
    plot(roc1, main=c(paste("AUC: ", round(roc1$auc,4), sep="")))
  } else {
    print(c(n.mail.valid, max(profit))) # report number of mailings and maximum profit
  }

  cutoff <- sort(post.valid, decreasing=T)[n.mail.valid+1] # set cutoff based on n.mail.valid
  chat.valid <- ifelse(post.valid > cutoff, 1, 0) # mail to everyone above the cutoff
  addmargins(table(chat.valid, c.valid))
}

# load the data
setwd("~/Dropbox/MSPA/422/Project")
charity <- read.csv("charity.csv") # load the "charity.csv" file

region <- as.factor(as.matrix(charity[,2:5]) %*% as.matrix(c(1,2,3,4)))
charity[,2] <- region
charity <- charity[,-(3:5)]
names(charity)[2] <- 'region'

if (cat_xfm == "Y") {
  charity$home <- as.factor(charity$home)
  charity$hinc <- as.factor(charity$hinc)
  charity$genf <- as.factor(charity$genf)
}

charity$donr <- as.factor(charity$donr)

# EDA
if (eda == "Y") {
  charity.eda <- charity[charity$part=="train",]
  
  p <- ggplot(charity.eda)
  h_p1 <- p + geom_bar(aes(home, fill=donr))
  h_p2 <- h_p1 + theme(panel.background=element_rect(fill="lightyellow", color="lightgreen"))
  
  r_p1 <- p + geom_bar(aes(region, fill=donr))
  r_p2 <- r_p1 + theme(panel.background=element_rect(fill="lightyellow", color="lightgreen"))
  
  inc_p1 <- p + geom_bar(aes(hinc, fill=donr))
  inc_p2 <- inc_p1 + theme(panel.background=element_rect(fill="lightyellow", color="lightgreen"))
  
  g_p1 <- p + geom_bar(aes(genf, fill=donr))
  g_p2 <- g_p1 + theme(panel.background=element_rect(fill="lightyellow", color="lightgreen"))
  
  grid.arrange(h_p2, r_p2, inc_p2, g_p2, nrow=2, ncol=2)
  
  pairs(~ avhv + incm + inca + plow + npro + tgif + lgif + rgif + tdon + tlag  + agif, data=charity.eda,
        pch=21, bg=c('red', 'green')[unclass(charity.eda$donr)])
  
  # Find Interactions for classification model
  tree_model <- tree(donr ~., data=charity.eda[,-c(1,20,21)])
  plot(tree_model)
  text(tree_model, pretty=0)
  
  cv_tree_model <- cv.tree(tree_model, FUN=prune.tree)
  plot(cv_tree_model$size, cv_tree_model$dev, type="b")
  
  pruned_model <- prune.misclass(tree_model, best=8)
  plot(pruned_model)
  text(pruned_model, pretty=0)
  
  # Find interaction for linear model
  tree_model <- tree(damt ~., data=charity.eda[charity.eda$donr == "1",-c(1,19,21)])
  plot(tree_model)
  text(tree_model, pretty=0)
  
  cv_tree_model <- cv.tree(tree_model, FUN=prune.tree)
  plot(cv_tree_model$size, cv_tree_model$dev, type="b")
  
  pruned_model <- prune.tree(tree_model, best=6)
  plot(pruned_model)
  text(pruned_model, pretty=0)
  
}

# The following indicator variables will be added based on the classification tree model
chld_i <- I(charity$chld < .5)
hinc_i <- I(charity$hinc %in% c(1,2,6,7))
region_i <- I(charity$region %in% c(0,3,4))
wrat_i <- I(charity$wrat < 3.5)
tdon_i <- I(charity$tdon < 24.5)
int_1 <- chld_i * hinc_i * region_i * wrat_i * tdon_i

# Add Interaction Variables for linear model
rgif_i <- I(charity$rgif < 14.5)
lgif_1 <- I((charity$lgif < 9.5) & (charity$region %in% c(0,1,2)))
lgif_2 <- I((charity$lgif >= 9.5) & (charity$lgif < 42.5))

# Model Preparation Starts Here
# The data
data <- cbind(charity[,1:18], chld_i, hinc_i, region_i, wrat_i, tdon_i, int_1, 
              rgif_i, lgif_1, lgif_2, charity[,19:21])

# predictor transformations
charity.t <- data
if (trnsfrm == "Y") {
  charity.t$avhv <- log(charity.t$avhv)
  charity.t$incm <- log(charity.t$incm)
  charity.t$plow <- charity.t$plow/100
  charity.t$tgif <- log(charity.t$tgif)
  charity.t$lgif[charity.t$lgif > 250] <- 200
  charity.t$rgif[charity.t$rgif > 75] <- 60
  if (cat_xfm == "Y") {
    charity.t$tdon <- as.factor(unclass(cut(charity.t$tdon,10))) 
  }
}

# add further transformations if desired
# for example, some statistical methods can struggle when predictors are highly skewed

# set up data for analysis

data.train <- charity.t[charity.t$part=="train",]
x.train <- data.train[,2:27]
c.train <- data.train$donr
n.train.c <- length(c.train) # 3984
y.train <- data.train[c.train==1,]$damt # damt for observations with donr=1
n.train.y <- length(y.train) # 1995

data.valid <- charity.t[charity.t$part=="valid",]
x.valid <- data.valid[,2:27]
c.valid <- data.valid$donr
n.valid.c <- length(c.valid) # 2018
y.valid <- data.valid[c.valid==1,]$damt # damt for observations with donr=1
n.valid.y <- length(y.valid) # 999

data.test <- charity.t[charity.t$part=="test",]
n.test <- dim(data.test)[1] # 2007
x.test <- data.test[,2:27]

# Standardizing Variables
if (cat_xfm == "Y") {
  cols <- which(names(x.train) %in% c('wrat', 'avhv', 'incm', 'inca', 'plow', 'npro', 'tgif', 
                                      'lgif', 'rgif', 'tlag', 'agif'))
} else {
  x.train$region <- as.numeric(as.character(x.train$region))
  x.valid$region <- as.numeric(as.character(x.valid$region))
  x.test$region <- as.numeric(as.character(x.test$region))
  
  x.train <- x.train[,1:17]
  x.valid <- x.valid[,1:17]
  x.test <- x.test[,1:17]
  cols <- 1:ncol(x.train)
}

x.train.mean <- apply(x.train[,cols], 2, mean)
x.train.sd <- apply(x.train[,cols], 2, sd)

x.train.std <- x.train
x.train.std[,cols] <- t((t(x.train[,cols])-x.train.mean)/x.train.sd) # standardize to have zero mean and unit sd

apply(x.train.std[,cols], 2, mean) # check zero mean
apply(x.train.std[,cols], 2, sd) # check unit sd

data.train.std.c <- data.frame(x.train.std[,1:23], donr=c.train) # to classify donr
data.train.std.y <- data.frame(x.train.std[c.train==1,], damt=y.train) # to predict damt when donr=1

x.valid.std <- x.valid
x.valid.std[,cols] <- t((t(x.valid[,cols])-x.train.mean)/x.train.sd) # standardize using training mean and sd
data.valid.std.c <- data.frame(x.valid.std, donr=c.valid) # to classify donr
data.valid.std.y <- data.frame(x.valid.std[c.valid==1,], damt=y.valid) # to predict damt when donr=1

x.test.std <- x.test
x.test.std[,cols] <- t((t(x.test[,cols])-x.train.mean)/x.train.sd) # standardize using training mean and sd
data.test.std <- data.frame(x.test.std)

##### CLASSIFICATION MODELING ######

#Call:  glm(formula = donr ~ region + home + chld + hinc + wrat + avhv + 
#             incm + npro + tgif + lgif + tdon + tlag + agif + chld_i + 
#             wrat_i + tdon_i + int_1 + X5 + X11 + X12 + X13 + X15 + X17 + 
#             X18 + X19 + X21 + X22 + X23 + X24 + X26 + X31 + X32 + X37 + 
#             X41 + X42 + X47 + X50 + X53 + X55, family = binomial("logit"), 
#           data = temp)


# logistic model
if (var_sel == "Y") {
  model <- glm(donr ~ ., data.train.std.c, family=binomial("logit"))
  step(model, direction="both")
}

# Base Model (GLM)
model.glm <- glm(formula = donr ~ region + home + chld + hinc + wrat + incm + 
                   plow + npro + tgif + tdon + tlag + chld_i + wrat_i + tdon_i + 
                   int_1, family = binomial("logit"), data = data.train.std.c)

post.valid.glm <- predict(model.glm, data.valid.std.c, type="response") # n.valid post probs

# calculate ordered profit function using average donation = $14.50 and mailing cost = $2
validate_clf_model(post.valid.glm, c.valid, "N")

# Try glm with higher order polynomial
for (i in 1:10) {
  print(i)
  model <- glm(formula = donr ~ region + home + chld + hinc + poly(wrat, i) +  
                   poly(incm, i) + poly(plow,i) + poly(npro, i) + poly(tgif, i) +  poly(lgif, i) + 
                   tdon + poly(tlag, i)  + chld_i + poly(agif, i) +
                   wrat_i + tdon_i + int_1, family = binomial("logit"), data = data.train.std.c)
  
  post.valid <- predict(model, data.valid.std.c, type="response") # n.valid post probs
  
  # calculate ordered profit function using average donation = $14.50 and mailing cost = $2
  validate_clf_model(post.valid, c.valid, "N")
  
}

# GLM with degree=8 polynomial
i=7
model.glm.8 <- glm(formula = donr ~ region + home + chld + hinc + poly(wrat, i) +  
               poly(incm, i) + poly(plow,i) + poly(npro, i) + poly(tgif, i) + poly(lgif, i) + 
               tdon + poly(tlag, i) + poly(agif, i) + chld_i + 
               wrat_i + tdon_i + int_1, family = binomial("logit"), data = data.train.std.c)

post.valid.glm.8 <- predict(model.glm.8, data.valid.std.c, type="response") # n.valid post probs

# calculate ordered profit function using average donation = $14.50 and mailing cost = $2
validate_clf_model(post.valid.glm.8, c.valid, "Y")

# Tree Model
model.tree <- rpart(formula = donr ~ region + home + chld + hinc + wrat + avhv + 
                   incm + npro + tgif + lgif + tdon + tlag + agif, 
                   data = data.train.std.c, control = rpart.control(cp = 0))

plotcp(model.tree)
printcp(model.tree)

model.tree <- rpart(formula = donr ~ region + home + chld + hinc + wrat + avhv + 
                      incm + npro + tgif + lgif + tdon + tlag + agif, 
                    data = data.train.std.c, control = rpart.control(cp = 0.0055))

post.valid.tree <- predict(model.tree, data.valid.std.c, type="class") # n.valid post probs

# calculate ordered profit function using average donation = $14.50 and mailing cost = $2
validate_clf_model(as.numeric(as.character(post.valid.tree)), c.valid, "Y")
# 1801.0 10564.5

# linear discriminant analysis
library(MASS)

model.lda1 <- lda(donr ~ region + home + chld + hinc + genf + wrat + incm + inca + plow + 
                  npro + tgif + lgif + rgif + tdon + tlag + agif, data.train.std.c) 
# include additional terms on the fly using I()

# Note: strictly speaking, LDA should not be used with qualitative predictors,
# but in practice it often is if the goal is simply to find a good predictive model

post.valid.lda1 <- predict(model.lda1, data.valid.std.c)$posterior[,2] # n.valid.c post probs
validate_clf_model(post.valid.lda1, c.valid, "Y")

# GAM
for (i in 1:10) {
  print(i)
  model.gam <- gam(donr ~ region + home + chld + hinc + s(wrat,i) + 
                     s(incm,i) + s(npro,i) + s(tgif,i) + s(lgif,i) + tdon + s(tlag,i) + 
                     s(agif,i) + chld_i + 
                     wrat_i + tdon_i + int_1, family = binomial, data = data.train.std.c)
  
  post.valid.gam <- predict(model.gam, data.valid.std.c, type="response") # n.valid post probs
  
  # calculate ordered profit function using average donation = $14.50 and mailing cost = $2
  validate_clf_model(post.valid.gam, c.valid, "N")
}

model.gam.7 <- gam(donr ~ region + home + chld + hinc + s(wrat,7) + 
                   s(incm,7) + s(npro,7) + s(tgif,7) + s(lgif,7) + tdon + s(tlag,7) + s(agif,7) + chld_i + 
                   wrat_i + tdon_i + int_1, family = binomial, data = data.train.std.c)

post.valid.gam.7 <- predict(model.gam.7, data.valid.std.c, type="response") # n.valid post probs

# calculate ordered profit function using average donation = $14.50 and mailing cost = $2
validate_clf_model(post.valid.gam.7, c.valid, "Y")
# Results

# n.mail Profit  Model
# 1329   11624.5 LDA1
# 1291   11642.5 Log1

# GLMNET with Ridge, LASSO and PCA 
tr_x <- model.matrix(  ~ ., data = data.train.std.c[,-c(25:27)])[,-1]
tr_y <- data.train.std.c$donr

vl_x <- model.matrix(  ~ ., data = data.valid.std.c[,-c(25:27)])[,-1]
vl_y <- data.valid.std.c$donr

#Ridge
grid <- 10^seq(10, -2, length=100)
ridge.mod <- glmnet(tr_x, tr_y, family=c("binomial"), alpha=0, lambda=grid)
plot(ridge.mod, xvar="lambda", label=T)

cv.out <- cv.glmnet(tr_x, tr_y, family=c("binomial"), alpha=0)
plot(cv.out)
bestlam <- cv.out$lambda.min
maxlam <- cv.out$lambda.1se

post.valid.rdg <- predict(ridge.mod, s=bestlam, newx=vl_x, type="response") # n.valid post probs

# calculate ordered profit function using average donation = $14.50 and mailing cost = $2
validate_clf_model(post.valid.rdg, c.valid, "Y")

#Lasso
lasso.mod <- glmnet(tr_x, tr_y, family=c("binomial"), alpha=1, lambda=grid)
plot(lasso.mod, xvar="lambda", label=T)

cv.out <- cv.glmnet(tr_x, tr_y, family=c("binomial"), alpha=1)
plot(cv.out)
bestlam <- cv.out$lambda.min
maxlam <- cv.out$lambda.1se

post.valid.las <- predict(lasso.mod, s=bestlam, newx=vl_x, type="response") # n.valid post probs

# calculate ordered profit function using average donation = $14.50 and mailing cost = $2
validate_clf_model(post.valid.las, c.valid, "Y")

#Random Forest
model.rf <- randomForest(donr ~ region + home + chld + hinc + wrat + incm + npro + tgif + lgif + 
                         tdon + tlag + agif, ntree=1200, importance=T,
                         data = data.train.std.c)

post.valid.rf <- predict(model.rf, data.valid.std.c, type="response") # n.valid post probs

# calculate ordered profit function using average donation = $14.50 and mailing cost = $2
validate_clf_model(as.numeric(as.character(post.valid.rf)), c.valid, "Y")

# Boosting
model.gbm <- gbm(as.numeric(as.character(donr)) ~ region + home + chld + hinc + wrat +
                  incm + npro + tgif + lgif + tdon + tlag + agif,
                  n.trees=5000, shrinkage=0.01, interaction.depth=1,
                  data = data.train.std.c)

post.valid.gbm <- predict(model.gbm, data.valid.std.c, type="response", n.trees=5000) # n.valid post probs

# calculate ordered profit function using average donation = $14.50 and mailing cost = $2
validate_clf_model(post.valid.gbm, c.valid, "Y")

# SVM
tune.svm <- tune(svm, donr ~ region + home + chld + hinc + wrat +
                   incm + npro + tgif + lgif + tdon + tlag + agif + chld_i + 
                   wrat_i + tdon_i + int_1,
                   kernel="radial", 
                   ranges=list(cost=c(0.001, 0.01, 0.1, 1, 5, 10, 100), gamma=c(0.5,1,2,3,4)),
                 data = data.train.std.c)
summary(tune.svm)

post.valid.svm <- predict(tune.svm$best.model, data.valid.std.c, type="response") # n.valid post probs

# calculate ordered profit function using average donation = $14.50 and mailing cost = $2
validate_clf_model(as.numeric(as.character(post.valid.svm)), c.valid, "Y")

#H2O
ncols <- which(names(data.train.std.c) %in% c('region', 'home', 'chld', 'hinc', 'wrat', 'incm', 'npro', 
                                             'tgif', 'lgif', 'tdon', 'tlag', 'agif', 'donr'))
localH2O <- h2o.init(nthread=-1,max_mem_size="4G")
train.hex <- as.h2o(localH2O, data.train.std.c[,ncols])
predictors <- 1:(ncol(train.hex) - 1)
response <- ncol(train.hex)

valid.hex <- as.h2o(localH2O, data.valid.std.c[,ncols])
post.valid.h2o <- rep(0, nrow(valid.hex))

for(j in 1:20){
  print(j)
  model <- h2o.deeplearning(x=predictors,
                            y=response,
                            data=train.hex,
                            classification=T,
                            activation="RectifierWithDropout",
                            hidden=c(1024,512,256),
                            hidden_dropout_ratio=c(0.5,0.5,0.5),
                            input_dropout_ratio=0.05,
                            epochs=20,
                            l1=1e-5,
                            l2=1e-5,
                            rho=0.99,
                            epsilon=1e-8,
                            train_samples_per_iteration=500,
                            max_w2=10,
                            seed=1)
  post.valid.h2o <- post.valid.h2o + as.data.frame(h2o.predict(model,valid.hex))$X1
  print(j)
}

post.valid.h2o <- post.valid.h2o/j
validate_clf_model(post.valid.h2o, c.valid, "Y")

# select model.gam.7 since it has maximum profit in the validation sample

post.test <- predict(model.gam.7, data.test.std, type="response") # post probs for test data

# Oversampling adjustment for calculating number of mailings for test set
profit.gam.7 <- cumsum(14.5 * as.numeric(as.character(c.valid[order(post.valid.gam.7, decreasing=T)])) - 2)

n.mail.valid <- which.max(profit.gam.7)
tr.rate <- .1 # typical response rate is .1
vr.rate <- .5 # whereas validation response rate is .5
adj.test.1 <- (n.mail.valid/n.valid.c)/(vr.rate/tr.rate) # adjustment for mail yes
adj.test.0 <- ((n.valid.c-n.mail.valid)/n.valid.c)/((1-vr.rate)/(1-tr.rate)) # adjustment for mail no
adj.test <- adj.test.1/(adj.test.1+adj.test.0) # scale into a proportion
n.mail.test <- round(n.test*adj.test, 0) # calculate number of mailings for test set

cutoff.test <- sort(post.test, decreasing=T)[n.mail.test+1] # set cutoff based on n.mail.test
chat.test <- ifelse(post.test>cutoff.test, 1, 0) # mail to everyone above the cutoff
table(chat.test)
#    0    1 
# 1711  296
# based on this model we'll mail to the 296 highest posterior probabilities

# See below for saving chat.test into a file for submission

##### PREDICTION MODELING ######

if (eda == "Y") {
  charity.eda <- data.train.std.y
  pairs(damt ~ avhv + incm + inca + plow + npro + tgif + lgif + rgif + tdon + tlag  + agif, data=charity.eda)
}

if (var_sel == "Y") {
  model.step <- lm(damt ~ ., data.train.std.y)
  step(model.step, direction="both")
  
  model.full <- regsubsets(damt ~ ., data.train.std.y) 
}

#Base Linear Model
model.ls1 <- lm(damt ~ region + home + chld + hinc + genf + wrat + plow +
                  avhv + incm + inca + tgif + rgif + agif + wrat_i + rgif_i + lgif_1 +lgif_2, 
                data.train.std.y)

summary(model.ls1)
pred.valid.ls1 <- predict(model.ls1, newdata = data.valid.std.y) # validation predictions
mean((y.valid - pred.valid.ls1)^2) # mean prediction error
sd((y.valid - pred.valid.ls1)^2)/sqrt(n.valid.y) # std error

#Linear Model with Higher Polynomial ## Poly, 4 performs the best with 1.4 0.15 0.65
for (i in 1:6) {
  model.ls <- lm(log(damt) ~ region + home + chld + hinc + genf + poly(wrat, 6) + poly(plow,i) +
                 poly(incm, 2) + poly(inca,i) + poly(tgif,i) + poly(rgif,i) +
                 poly(agif,i) + wrat_i + rgif_i + lgif_1 + lgif_2, 
                 data.train.std.y)
  
  pred.valid.ls <- exp(predict(model.ls, newdata = data.valid.std.y)) # validation predictions
  m <- (mean((y.valid - pred.valid.ls)^2)) # mean prediction error
  sdv <- (sd((y.valid - pred.valid.ls)^2)/sqrt(n.valid.y)) # std error
  print(c(i, m, sdv, summary(model.ls)$adj.r.squared))
}

model.ls4 <- lm(log(damt) ~ region + home + chld + hinc + genf + poly(wrat, 6) + poly(plow,4) +
                  poly(incm, 2) + poly(inca,4) + poly(tgif,4) + poly(rgif,4) +
                  poly(agif,4) + wrat_i + rgif_i + lgif_1 + lgif_2, 
               data.train.std.y)

summary(model.ls4)
pred.valid.ls4 <- exp(predict(model.ls4, newdata = data.valid.std.y)) # validation predictions
(mean((y.valid - pred.valid.ls4)^2)) # mean prediction error - 1.32
(sd((y.valid - pred.valid.ls4)^2)/sqrt(n.valid.y)) # std error - .146

# influence.measures(model.ls4)
# table(rstudent(model.ls4) > 3)
# residualPlot(model.ls4)

# GLMNET with Ridge, LASSO 
tr_x <- model.matrix(  ~ region + home + chld + hinc + genf + poly(wrat, 6) + 
                         poly(incm, 4) + poly(inca,4) + poly(tgif,4) + poly(rgif,4) + 
                         poly(agif,4) + wrat_i + rgif_i + lgif_1 + lgif_2, 
                         data = data.train.std.y)[,-1]
tr_y <- data.train.std.y$damt

vl_x <- model.matrix(  ~ region + home + chld + hinc + genf + poly(wrat, 6) + 
                         poly(incm, 4) + poly(inca,4) + poly(tgif,4) + poly(rgif,4) + 
                         poly(agif,4) + wrat_i + rgif_i + lgif_1 + lgif_2
                       , data = data.valid.std.y)[,-1]
vl_y <- data.valid.std.y$damt

#Ridge
grid <- 10^seq(10, -2, length=100)
ridge.mod <- glmnet(tr_x, tr_y, family=c("gaussian"), alpha=0, lambda=grid)
plot(ridge.mod, xvar="lambda", label=T)

cv.out <- cv.glmnet(tr_x, tr_y, family=c("gaussian"), alpha=0)
plot(cv.out)
bestlam <- cv.out$lambda.min
maxlam <- cv.out$lambda.1se

pred.valid.rdg <- unclass(predict(ridge.mod, s=bestlam, newx=vl_x)) # n.valid post probs
mean((y.valid - pred.valid.rdg)^2) # mean prediction error
sd((y.valid - pred.valid.rdg)^2)/sqrt(n.valid.y) # std error

#Lasso
lasso.mod <- glmnet(tr_x, tr_y, family=c("gaussian"), alpha=1, lambda=grid)
plot(lasso.mod, xvar="lambda", label=T)

cv.out <- cv.glmnet(tr_x, tr_y, family=c("gaussian"), alpha=1)
plot(cv.out)
bestlam <- cv.out$lambda.min
maxlam <- cv.out$lambda.1se

pred.valid.las <- unclass(predict(lasso.mod, s=bestlam, newx=vl_x)) # n.valid post probs
mean((y.valid - pred.valid.las)^2) # mean prediction error
sd((y.valid - pred.valid.las)^2)/sqrt(n.valid.y) # std error

# GAM Model
for (i in 1:10) {
  print(i)
  model.gam <- gam(damt ~ region + home + chld + hinc + genf + s(wrat, i) + s(plow,i) +
                   s(incm, i) + s(inca,i) + s(tgif,i) + s(rgif,i) +
                   s(agif,i) + wrat_i + rgif_i + lgif_1 + lgif_2, 
                   family = gaussian, data = data.train.std.y)
  
  pred.valid.gam <- predict(model.gam, data.valid.std.y) # n.valid post probs
  m <- (mean((y.valid - pred.valid.gam)^2)) # mean prediction error
  sdv <- (sd((y.valid - pred.valid.gam)^2)/sqrt(n.valid.y)) # std error
  print(c(i, m, sdv))
}

# Boosting
i = 4
model.gbm <- gbm(damt ~ region + home + chld + hinc + genf + wrat + plow +
                 incm + inca + tgif + rgif + agif,
                 n.trees=5000, shrinkage=0.01, interaction.depth=1,
                 data = data.train.std.y)

pred.valid.gbm <- predict(model.gbm, data.valid.std.y, n.trees=5000) # n.valid post probs
mean((y.valid - pred.valid.gbm)^2) # mean prediction error
sd((y.valid - pred.valid.gbm)^2)/sqrt(n.valid.y) # std error

# Results
yhat.test <- exp(predict(model.ls4, newdata = data.test.std)) # test predictions

# FINAL RESULTS
# Save final results for both classification and regression

length(chat.test) # check length = 2007
length(yhat.test) # check length = 2007
chat.test[1:10] # check this consists of 0s and 1s
yhat.test[1:10] # check this consists of plausible predictions of damt

ip <- data.frame(id=chat=chat.test, yhat=yhat.test) # data frame with two variables: chat and yhat
write.csv(ip, file="DP.csv", row.names=FALSE) # use your initials for the file name

# submit the csv file in Angel for evaluation based on actual test donr and damt values
