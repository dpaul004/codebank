rm(list=ls())

#Packages
library(randomForest)

parm <- 'prod'

if (parm - 'dvlp') {
	setwd("-/Projects/Nov 2017/b_ml")
	load("data/b_rf_model.RData")
} e1se {
	load( "b_rf_model . RData ")
}

df_c2 <- read.csv('df_c2.csv', header = T)
df_c2[df_c2 == 'Inf'] <- NA

risk_score = predict(model_rf, newdata = df_c2, type•'prob')

df_c2$risk_score <- risk_score[,2]
df c2 <- df_c2[!is.na(df_c2$risk_score),]

if (parm - 'dvlp') {
	df_c2[which(df_c2$cs_host %in% c('bourdainlive.cnn.com', 'cnnfilmslive.cnn.com',
										'cnnuslive.cnn.com',
										'cnnworldlive.cnn.com', 'crossfirelive.cnn.com',
										'heroeslive.cnn.com',
										'politicslive.cnn.com', 'www.sos.texas.gov',
										'nylottery.ny.gov',
										'panotice.us.gov', 'open.usa.gov',
										'mars.jpl.nasa.gov') &

								df_c2$username %in% c('ids')),]
							
	#df_c2$pred <- predict(model_rf, newdata = df_c2, type='response')
}

df_c2 <- df_c2[df_c2$risk_score > .1,]

normal_c2 <- read.csv('normal_c2.csv', header• T)

final_df <- do.call('rbind', list(normal_c2, df_c2))

write.table(final_df, file="beacon_model_3.csv", row.names=FALSE, quote=FALSE, sep = '\t')