library(tree)

parm <- "none"
setwd("~/Dropbox/MSPA/411/Assignment/Heloc")

train <- read.csv('heloc.csv', header=T)
impute_cols <- names(which(colSums(apply(train,2,is.na)) != 0))

tree_model <- tree(TARGET_FLAG ~., data=train[,-1])
plot(tree_model)
text(tree_model, pretty=0)

cv_tree_model <- cv.tree(tree_model, FUN=prune.tree)
plot(cv_tree_model$size, cv_tree_model$dev, type="b")

pruned_model <- prune.misclass(tree_model, best=5)
plot(pruned_model)
text(pruned_model, pretty=0)



test <- read.csv('insurance_test.csv', header=T)

# Clean up train and test data
train$INCOME <- as.numeric(gsub('[$,]', '', train$INCOME))
train$HOME_VAL <- as.numeric(gsub('[$,]', '', train$HOME_VAL))
train$BLUEBOOK <- as.numeric(gsub('[$,]', '', train$BLUEBOOK))
train$OLDCLAIM <- as.numeric(gsub('[$,]', '', train$OLDCLAIM))

test$INCOME <- as.numeric(gsub('[$,]', '', test$INCOME))
test$HOME_VAL <- as.numeric(gsub('[$,]', '', test$HOME_VAL))
test$BLUEBOOK <- as.numeric(gsub('[$,]', '', test$BLUEBOOK))
test$OLDCLAIM <- as.numeric(gsub('[$,]', '', test$OLDCLAIM))

# Missing and Outlier Imputation
train[which(is.na(train$AGE)),c('AGE')] <- 43
train[which(is.na(train$YOJ)),c('YOJ')] <- 10

train[which((is.na(train$INCOME) | (train$INCOME == 0)) & (train$JOB == '')),c('INCOME')] <- 100000
train[which((is.na(train$INCOME) | (train$INCOME == 0)) & (train$JOB == 'Clerical')),c('INCOME')] <- 30000
train[which((is.na(train$INCOME) | (train$INCOME == 0)) & (train$JOB == 'Doctor')),c('INCOME')] <- 135000
train[which((is.na(train$INCOME) | (train$INCOME == 0)) & (train$JOB == 'Home Maker')),c('INCOME')] <- 0
train[which((is.na(train$INCOME) | (train$INCOME == 0)) & (train$JOB == 'Lawyer')),c('INCOME')] <- 75000
train[which((is.na(train$INCOME) | (train$INCOME == 0)) & (train$JOB == 'Manager')),c('INCOME')] <- 72000
train[which((is.na(train$INCOME) | (train$INCOME == 0)) & (train$JOB == 'Professional')),c('INCOME')] <- 65000
train[which((is.na(train$INCOME) | (train$INCOME == 0)) & (train$JOB == 'Student')),c('INCOME')] <- 0
train[which((is.na(train$INCOME) | (train$INCOME == 0)) & (train$JOB == 'z_Blue Collar')),c('INCOME')] <- 50000

train[which((is.na(train$HOME_VAL) | (train$HOME_VAL == 0)) & (train$JOB == '')),c('HOME_VAL')] <- 173000
train[which((is.na(train$HOME_VAL) | (train$HOME_VAL == 0)) & (train$JOB == 'Clerical')),c('HOME_VAL')] <- 95000
train[which((is.na(train$HOME_VAL) | (train$HOME_VAL == 0)) & (train$JOB == 'Doctor')),c('HOME_VAL')] <- 240000
train[which((is.na(train$HOME_VAL) | (train$HOME_VAL == 0)) & (train$JOB == 'Home Maker')),c('HOME_VAL')] <- 72000
train[which((is.na(train$HOME_VAL) | (train$HOME_VAL == 0)) & (train$JOB == 'Lawyer')),c('HOME_VAL')] <- 155000
train[which((is.na(train$HOME_VAL) | (train$HOME_VAL == 0)) & (train$JOB == 'Manager')),c('HOME_VAL')] <- 165000
train[which((is.na(train$HOME_VAL) | (train$HOME_VAL == 0)) & (train$JOB == 'Professional')),c('HOME_VAL')] <- 150000
train[which((is.na(train$HOME_VAL) | (train$HOME_VAL == 0)) & (train$JOB == 'Student')),c('HOME_VAL')] <- 0
train[which((is.na(train$HOME_VAL) | (train$HOME_VAL == 0)) & (train$JOB == 'z_Blue Collar')),c('HOME_VAL')] <- 150000

train[which(is.na(train$CAR_AGE)),c('CAR_AGE')] <- 7

#Impute test set
test[which(is.na(test$AGE)),c('AGE')] <- 43
test[which(is.na(test$YOJ)),c('YOJ')] <- 10

test[which((is.na(test$INCOME) | (test$INCOME == 0)) & (test$JOB == '')),c('INCOME')] <- 100000
test[which((is.na(test$INCOME) | (test$INCOME == 0)) & (test$JOB == 'Clerical')),c('INCOME')] <- 30000
test[which((is.na(test$INCOME) | (test$INCOME == 0)) & (test$JOB == 'Doctor')),c('INCOME')] <- 135000
test[which((is.na(test$INCOME) | (test$INCOME == 0)) & (test$JOB == 'Home Maker')),c('INCOME')] <- 0
test[which((is.na(test$INCOME) | (test$INCOME == 0)) & (test$JOB == 'Lawyer')),c('INCOME')] <- 75000
test[which((is.na(test$INCOME) | (test$INCOME == 0)) & (test$JOB == 'Manager')),c('INCOME')] <- 72000
test[which((is.na(test$INCOME) | (test$INCOME == 0)) & (test$JOB == 'Professional')),c('INCOME')] <- 65000
test[which((is.na(test$INCOME) | (test$INCOME == 0)) & (test$JOB == 'Student')),c('INCOME')] <- 0
test[which((is.na(test$INCOME) | (test$INCOME == 0)) & (test$JOB == 'z_Blue Collar')),c('INCOME')] <- 50000

test[which((is.na(test$HOME_VAL) | (test$HOME_VAL == 0)) & (test$JOB == '')),c('HOME_VAL')] <- 173000
test[which((is.na(test$HOME_VAL) | (test$HOME_VAL == 0)) & (test$JOB == 'Clerical')),c('HOME_VAL')] <- 95000
test[which((is.na(test$HOME_VAL) | (test$HOME_VAL == 0)) & (test$JOB == 'Doctor')),c('HOME_VAL')] <- 240000
test[which((is.na(test$HOME_VAL) | (test$HOME_VAL == 0)) & (test$JOB == 'Home Maker')),c('HOME_VAL')] <- 72000
test[which((is.na(test$HOME_VAL) | (test$HOME_VAL == 0)) & (test$JOB == 'Lawyer')),c('HOME_VAL')] <- 155000
test[which((is.na(test$HOME_VAL) | (test$HOME_VAL == 0)) & (test$JOB == 'Manager')),c('HOME_VAL')] <- 165000
test[which((is.na(test$HOME_VAL) | (test$HOME_VAL == 0)) & (test$JOB == 'Professional')),c('HOME_VAL')] <- 150000
test[which((is.na(test$HOME_VAL) | (test$HOME_VAL == 0)) & (test$JOB == 'Student')),c('HOME_VAL')] <- 0
test[which((is.na(test$HOME_VAL) | (test$HOME_VAL == 0)) & (test$JOB == 'z_Blue Collar')),c('HOME_VAL')] <- 150000

test[which(is.na(test$CAR_AGE)),c('CAR_AGE')] <- 7

# Remove outliers from train
train <- train[-which(train$CAR_AGE < 0),]

# Additional features - train
train$aux1 <- 0
train$aux1[(train$AGE < 25)] <- 1
train$aux1[(train$AGE >= 25 & train$AGE < 55)] <- 2
train$aux1[(train$AGE >= 55)] <- 3

train$aux2 <- ifelse(((train$CAR_TYPE == 'Sports Car') & (train$RED_CAR == 'yes')), 1, 0)
train$aux3 <- ifelse(((train$REVOKED == 'No') & (train$MVR_PTS > 10)), 1, 0)
train$aux4 <- train$YOJ * train$INCOME

# Additional features - test
test$aux1 <- 0
test$aux1[(test$AGE < 25)] <- 1
test$aux1[(test$AGE >= 25 & test$AGE < 55)] <- 2
test$aux1[(test$AGE >= 55)] <- 3

test$aux2 <- ifelse(((test$CAR_TYPE == 'Sports Car') & (test$RED_CAR == 'yes')), 1, 0)
test$aux3 <- ifelse(((test$REVOKED == 'No') & (test$MVR_PTS > 10)), 1, 0)
test$aux4 <- test$YOJ * test$INCOME

feature.names <- names(train)[3:(ncol(train))]
for (f in feature.names) {
  if (class(train[[f]])=="factor") {
    levels <- levels(ordered(unique(train[[f]], test[[f]])))
    train[[f]] <- as.integer(factor(train[[f]], levels=levels))
    test[[f]]  <- as.integer(factor(test[[f]],  levels=levels))
  }
}

write.table(train,"train.csv",sep=',',col.names=T,row.names=F)
write.table(test,"test.csv",sep=",",col.names=T,row.names=F)

train <- train[,-1]
train$TARGET <- log(train$TARGET)

nam <- names(train[,2:ncol(train)])
for (itm in nam) {
  train[,c(itm)] <- (train[,c(itm)] - mean(train[,c(itm)]))/sd(train[,c(itm)])
}

idx <- test$INDEX
test <- test[,-c(1,2)]

spl <- rbinom(nrow(train),size=1,prob=0.7)
trn <- train[spl==1,][,]
xcv <- train[spl==0,][,]

model <- lm(TARGET ~., data=trn)
n_val <- predict(model,newdata=xcv)

print(summary(abs(exp(n_val) - exp(xcv$TARGET))/exp(xcv$TARGET)))
