rm(list=ls())

#Packages
library(randomForest)

1st <- c(
			"p_data_2017-10-17. csv", "p_data_2017-10-18. csv", "p_data_2017-10-19. csv",
			"p_data_2017-10-20. csv", "p_data_2017-10-22. csv",
			"p_data_2017-10-23. csv", "p_data_2017-10-24. csv", "p_data_2017-10-25. csv",
			"p_data_2017-10-26. csv", "p_data_2017-10-27. csv", "p_data_2017-10-29. csv",
			"p_data_2017-10-30. csv", "p_data_2017-10-31. csv",
			"p_data_2017-ll-01. csv", "p_data_2017-ll-02. csv", "p_data_2017-ll-03. csv",
			"p_data_2017-ll-04.csv",
			"p_data_2017-ll-05. csv", "p_data_2017-ll-06. csv", "p_data_2017-ll-07. csv",
			"p_data_2017-ll-08.csv",
			"p_data_2017-ll-09. csv", "p_data_2017-ll-10. csv", "p_data_2017-ll-ll. csv",
			"p_data_2017-ll-12.csv",
			"p_data_2017-ll-13. csv", "p_data_2017-ll-14. csv", "p_data_2017-ll-15. csv",
			"p_data_2017-ll-16. csv",
			"p_data_2017-ll-28. csv", "p_data_2017-ll-29-. csv", "p_data_2017-ll-30. csv",
			"p_data_2017-12-01.csv", "p_data_2017-12-02.csv",
			"p_data_2017-12-03. csv")

# load the data
setwd("-/Projects/Nov 2017/b_ml/data")

cv_stats = list(length(lst))
i=1

for (item in lst) {
	tr_list <- sample(lst(lst != item), 25)
	df_list <- list(length(tr_list))
	
	for (j in l:length(tr_list)) {
		df <- read.csv(tr_list(j))
		df <- df[,-which(names(df) %in% c('day', 'c_ip_list', 'whois_registrant', 'risk_score')))
		
		df$C2 <- "N"
		
		df[which(df$cs_host %in% c('bourdainlive.cnn.com') & df$username tint c(),'C2'] <- "Y"
		
		df$C2 <- as.factor((df$C2))

		df_list[(j]] <- df
	}
	
	train<- do.call('rbind', df_list)
	
	##train$r_ip_u.nq_cnt[train$r_ip_unq_cnt > 50) <- 50
	
	model rf <- randomForest(C2 ~ ., data = train[,3:ncol(train)], importance=TRUE,
					ntree=4000, mtry=8)
	
	today <- read.csv(item)
	
	today <- today[,-which(names(today) %in% c('day') & today$username %in% c(), 'C2 '] <- "Y"
	
	today$C2 <- as.factor((today$C2))
	
	risk_score - predict(model_rf, newdata - today, type-'prob')
	
	today$risk_score <- risk_score[,2]
	pred <- "N"
	pred[today$risk_score > .2] <- "Y"
	
	pred <- as.factor(pred)
	tbl <- table(today$C2, pred)
	if (is.na(tbl[1])) {
		TN <- 0
	} else {
		TN <- tbl[1]
	}
	
	if (is.na(tbl[2])) {
		FN <- 0
	} else {
		FN = tbl[2]
	}
	
	if (is.na(tbl[3])) {
		FP <- 0
	} else {
		FP <- tbl[3)
	}
	
	if (is.na(tbl[4))) {
		TP <- 0
	} e1ae {
		TP = tbl[4]
	}
	
	cv_stats[[i]] <- list(nrow(today), sum(today$C2 == "Y"), FP, FN, (TP + TN)/(TP+TN+FP+FN))
	i <- i + 1
}

analysisl <- data.frame(matrix(unlist(cv_stats), nrow=length(cv_stats),
ncol=length(unlist(cv_stats[l])), byrow-TRUE))
names(analysisl) <- c('Total_Rows', 'True_Pos', 'False_Pos', 'False_Neg', 'Accuracy')

#RF Bootstrap
df_list <- list(length(lst))
for (j in 1:length(lst)) {
	df <- read.csv(lst[j])
	df <- df[,-which(names(df) %in% c('day'))]
	
	df$C2 <- "N"

	df[which(df$cs_host %in% c('bourdainlive.cnn.com', 'cnnfilmslive.cnn.com') & 
			df$username %in% c()), 'C2'] <- "Y"
	df$C2 <- as.factor((df$C2))

	df_list[[j]] <- df
}

b_data <- do.call('rbind', df_list}
##b_data$r_ip_unqcnt[train$r_ip_unq_cnt > SO] <- SO
cv_stats = list(25}

for (i in 1:2S} {
	idx <- rbinom(nrow(b_data},1, .7)
	
	train <- b_data[idx==1,]
	test <- b_data[idx==O,]
	
	model rf <- randomForest(C2 ~ . , data=train[,3:ncol(train)], importance-TRUE,
	ntree-4OOO, mtry=8)
	
	pred = predict(model_rf, newdata • test, type•'response')
	
	cv_tbl <- table(test$C2, pred)
	
	cv_stats[[i]] <- list(nrow(test), sum(test$C2 == "Y"), cv_tbl[2], cv_tbl[3], (cv_tbl[1] + cv_tbl[4])/sum(cv_tbl))
}

analysis2 <- data.frame(matrix(unlist(cv_stats), nrow=length(cv_stats), ncol=length(unlist(cv_stats[1])), byrow=TRUE))
summary(analysis2)


