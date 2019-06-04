#Packages
rm(list=ls())
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
library(graphics)
library(tm)
library(SnowballC)
library(cluster)

# Parameters

#Functions

# load the data
setwd("/Users/dipanjanpaul/Dropbox/MSPA/452/Assg4/DipanjanPaul")
c_comp_data <- read.delim("CreditKarma_CC_IndividualReviews.txt", header = T)
lst = list(nrow(c_comp_data))

for (i in 1:nrow(c_comp_data)) {
  cmt <- strsplit(as.character(c_comp_data$Review[i]), "[<]|[>]|[[]|[]]|[\\\\]")
  cmt <- cmt[[1]][which(lapply(cmt[[1]], nchar) > 5)]
  
  lst[[i]] <-cbind(rep(as.character(c_comp_data[i,]$Credit.Card), length(cmt)), 
                   rep(c_comp_data[i,]$Star.Rating, length(cmt)), 
                   rep(c_comp_data[i,]$X..Marked.Helpful, length(cmt)), 
                   rep(c_comp_data[i,]$X..Marked.Not.Helpful, length(cmt)), 
                   rep(as.character(c_comp_data[i,]$Review.Date), length(cmt)),
                   cmt)
}

final_data <- as.data.frame(do.call("rbind", lst))
names(final_data) <- c('Credit.Card', 'Star.Rating', 'Marked.Helpful', 'Marked.Not.Helpful', 'Review.Date', 'Review')

positive <- final_data[final_data$Star.Rating == '5',]
negative <- final_data[final_data$Star.Rating == '1',]

## Find Positive Words 
nar <- VCorpus(VectorSource(positive$Review))
nar_tm <- tm_map(nar, removePunctuation)
nar_tm <- tm_map(nar_tm, removeNumbers)
nar_tm <- tm_map(nar_tm, stripWhitespace)
nar_tm <- tm_map(nar_tm, content_transformer(tolower))
nar_tm <- tm_map(nar_tm, removeWords, stopwords("English"))
nar_tm <- tm_map(nar_tm, removeWords, c('will', 'just', 'dont', 'can', 'didnt',
                                        "amp"   ,   "app" ,     "appli", "boa", "cktext" ,  "discov",   "doubl",
                                        "easi", "xai"))
nar_tm <- tm_map(nar_tm, removeWords, c('credit', 'card'))
nar_tm <- tm_map(nar_tm, stemDocument)
inspect(nar_tm[1:5])
print(nar_tm[[1]][1])

dtm <- DocumentTermMatrix(nar_tm)
dtms <- removeSparseTerms(dtm, 0.95)
positive_words <- dtms$dimnames$Terms

## Find Negative Words 
nar <- VCorpus(VectorSource(negative$Review))
nar_tm <- tm_map(nar, removePunctuation)
nar_tm <- tm_map(nar_tm, removeNumbers)
nar_tm <- tm_map(nar_tm, stripWhitespace)
nar_tm <- tm_map(nar_tm, content_transformer(tolower))
nar_tm <- tm_map(nar_tm, removeWords, stopwords("English"))
nar_tm <- tm_map(nar_tm, removeWords, c('will', 'just', 'dont', 'can', 'didnt',
                                        "amp"   ,   "app" ,     "appli", "boa", "cktext" ,  "discov",   "doubl",
                                        "easi", "xai", "arbitrarili", "bank" , "canada" , "day" , "sjw"   ,   "noth"  ,
                                        "tar"  ,       "taugt"  ,     "treati",  "websit"    ,  "xato" ))
nar_tm <- tm_map(nar_tm, removeWords, c('credit', 'card'))
nar_tm <- tm_map(nar_tm, stemDocument)
inspect(nar_tm[1:5])
print(nar_tm[[1]][1])

dtm <- DocumentTermMatrix(nar_tm)
dtms <- removeSparseTerms(dtm, 0.95)
negative_words <- dtms$dimnames$Terms

# Final Lists
positive_final <- as.data.frame(positive_words[-which(positive_words %in% negative_words)])
negative_final <- as.data.frame(negative_words[-which(negative_words %in% positive_words)])

write.csv(positive_final, "positive_words.txt", row.names = FALSE, quote = FALSE)
write.csv(negative_final, "negative_words.txt", row.names = FALSE, quote = FALSE)
