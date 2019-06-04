library(e1071)

set.seed(1)
x <- matrix(rnorm(20*2), ncol=2)
y <- c(rep(-1,10), rep(1,10))
x[y==1,] <- x[y==1,] + 1
plot(x, col=(3-y))

dat <- data.frame(x=x, y=as.factor(y))
svmfit <- svm(y ~ ., data=dat, kernel="linear", cost=.01, scale=F)
table(svmfit$fitted, dat$y)
plot(svmfit, dat)

svmfit$index
summary(svmfit)

set.seed(1)
tune.out <- tune(svm, y ~ ., data=dat, kernel="linear", 
                 ranges=list(cost=c(0.001, 0.01, 0.1, 1, 5, 10, 100)))
summary(tune.out)

bestmod <- tune.out$best.model
summary(bestmod)

xtest <- matrix(rnorm(20*2), ncol=2)
ytest <- sample(c(-1,1), 20, rep=T)
xtest[ytest==1,] <- xtest[ytest==1,] + 1
testdat <- data.frame(x=xtest, y=as.factor(ytest))

ypred <- predict(bestmod, testdat)
table(pred=ypred, truth=testdat$y)

svmfit <- svm(y ~ ., data=dat, kernel="linear", cost=.01, scale=F)
ypred <- predict(svmfit, testdat)
table(pred=ypred, truth=testdat$y)

x[y==1,] <- x[y==1,] + 0.5
plot(x, col=(y+5)/2, pch=19)

dat <- data.frame(x=x, y=as.factor(y))
svmfit <- svm(y ~ ., data=dat, kernel="linear", cost=1e5, scale=F)
summary(svmfit)
plot(svmfit, dat)

svmfit <- svm(y ~ ., data=dat, kernel="linear", cost=1, scale=F)
summary(svmfit)
plot(svmfit, dat)

# SVM

set.seed(1)
x <- matrix(rnorm(200*2), ncol=2)
x[1:100,] <- x[1:100,] + 2
x[101:150,] <- x[101:150,] -2
y <- c(rep(1,150), rep(2,50))
dat <- data.frame(x=x, y=as.factor(y))
plot(x, col=y)

train <- sample(200,100)

svmfit <- svm(y ~ ., data=dat[train,], kernel="radial", cost=1, gamma=2)
table(svmfit$fitted, dat[train,"y"])
summary(svmfit)
plot(svmfit, dat[train,])

set.seed(1)
tune.out <- tune(svm, y ~ ., data=dat[train,], kernel="radial", 
                 ranges=list(cost=c(0.001, 0.01, 0.1, 1, 5, 10, 100), gamma=c(0.5,1,2,3,4)))
summary(tune.out)

table(true=dat[-train,"y"], predict(tune.out$best.model, newdata=dat[-train,]))

# ROCR
library(ROCR)
rocplot <- function(pred, truth, ...) {
  predob <- prediction(pred, truth)
  perf <- performance(predob, "tpr", "fpr")
  plot(perf,...) 
}

svmfit.opt <- svm(y ~ ., data=dat[train,], kernel="radial", cost=1, gamma=2, decision.values=T)
fitted <- attributes(predict(svmfit.opt, newdata=dat[train,], decision.values=TRUE))$decision.values
rocplot(fitted, dat[train,"y"], main="Training Data")

svmfit.flex <- svm(y ~ ., data=dat[train,], kernel="radial", cost=1, gamma=50, decision.values=T)
fitted <- attributes(predict(svmfit.flex, newdata=dat[train,], decision.values=TRUE))$decision.values
rocplot(fitted, dat[train,"y"], add=T, col="red")

fitted <- attributes(predict(svmfit.opt, newdata=dat[-train,], decision.values=TRUE))$decision.values
rocplot(fitted, dat[-train,"y"], main="Training Data")
fitted <- attributes(predict(svmfit.flex, newdata=dat[-train,], decision.values=TRUE))$decision.values
rocplot(fitted, dat[-train,"y"], add=T, col="red")

# SVM Multiple Classes
set.seed(1)
x <- rbind(x, matrix(rnorm(50*2), ncol=2))
y <- c(y, rep(0,50))
x[y==0,2] <- x[y==0,2] + 2
dat <- data.frame(x=x, y=as.factor(y))
plot(x, col=(y+1))

svmfit <- svm(y ~ ., data=dat, kernel="radial", cost=10, gamma=1)
summary(svmfit)
plot(svmfit, dat)

# Gene Data
library(ISLR)
names(Khan)

table(Khan$ytrain)
table(Khan$ytest)

dat <- data.frame(x=Khan$xtrain, y=as.factor(Khan$ytrain))
out <- svm(y ~., data=dat, kernel="linear", cost=10)
summary(out)
table(out$fitted, dat$y)

dat.te <- data.frame(x=Khan$xtest, y=as.factor(Khan$ytest))
pred.te <- predict(out,dat.te)
table(pred.te, dat.te$y)



