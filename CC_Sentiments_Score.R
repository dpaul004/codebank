#Packages
rm(list = ls())
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
c_comp_data <- read.delim("creditkarma_reviews_updated.txt", header = T)
lst = list(nrow(c_comp_data))

for (i in 1:nrow(c_comp_data)) {
  cmt <- strsplit(as.character(c_comp_data$Review[i]), "[<]|[>]|[[]|[]]|[\\\\]")
  cmt <- cmt[[1]][which(lapply(cmt[[1]], nchar) > 5)]
  
  lst[[i]] <-cbind(rep(as.character(c_comp_data[i,]$Credit.Card), length(cmt)), 
                   rep(c_comp_data[i,]$X..Marked.Helpful, length(cmt)), 
                   rep(c_comp_data[i,]$X..Marked.Not.Helpful, length(cmt)), 
                   rep(as.character(c_comp_data[i,]$Review.Date), length(cmt)),
                   cmt)
}

final_data <- as.data.frame(do.call("rbind", lst))
names(final_data) <- c('Credit.Card', 'Marked.Helpful', 'Marked.Not.Helpful', 'Review.Date', 'Review')

## EDA
dim(final_data)
length(table(final_data$Credit.Card))

## Text Analysis Code 
nar <- VCorpus(VectorSource(final_data$Review))
nar_tm <- tm_map(nar, removePunctuation)
nar_tm <- tm_map(nar_tm, removeNumbers)
nar_tm <- tm_map(nar_tm, stripWhitespace)

nar_tm <- tm_map(nar_tm, content_transformer(tolower))
nar_tm <- tm_map(nar_tm, removeWords, stopwords("English"))
nar_tm <- tm_map(nar_tm, removeWords, c('will', 'just', 'dont', 'can', 'didnt'))
nar_tm <- tm_map(nar_tm, removeWords, c('credit', 'card'))

nar_tm <- tm_map(nar_tm, stemDocument)
inspect(nar_tm[1:5])
print(nar_tm[[1]][1])

initial.tdm <- TermDocumentMatrix(nar_tm)
examine.tdm <- removeSparseTerms(initial.tdm, sparse = 0.98)
top.words <- Terms(examine.tdm)
print(top.words) 

more_stop_words <- c("account", "also","card","even","ive","xai","cktext","compani", "tri",
                     "anoth",  "appli", "busi", "capit", "cktext", "compani", "ive", "purchas", "realli", 
                     "receiv") 
nar_tm <- tm_map(nar_tm, removeWords, more_stop_words)

some_proper_nouns_to_remove <- c("dick","ginger","hollywood","jack","jill","john","karloff",
    "kudrow","orson","peter","tcm","tom","toni","welles","william","wolheim")
nar_tm <- tm_map(nar_tm, removeWords, some_proper_nouns_to_remove)

total_words <- (length(names(nar_tm)))

dtm <- DocumentTermMatrix(nar_tm)

freq <- sort(colSums(as.matrix(dtm)), decreasing=T)
head(freq, 100)
freq_df <- data.frame(names(freq), freq)
head(freq_df, 20)
head(table(freq), 20)

findFreqTerms(dtm, lowfreq=500)

lst <- findFreqTerms(dtm, lowfreq=500)
findAssocs(dtm, as.character(freq_df[1:30,1]), corlimit=0.5)

dtms <- removeSparseTerms(dtm, 0.98)
d <- dist(t(dtms), method='euclidian')
fit <- hclust(d=d, method='ward.D2')
plot(fit, hang=-1)

plot.new()
plot(fit, hang=-1)
groups <- cutree(fit, k=18)
rect.hclust(fit, k=18, border='red')

## Develop sentiment score for each credit card

# read in positive and negative word lists from Hu and Liu (2004)
bytecode.convert <- function(x) {iconv(enc2utf8(x), sub = "byte")}
positive.data.frame <- read.table(file = "Hu_Liu_positive_word_list.txt",
                                  header = FALSE, colClasses = c("character"), row.names = NULL, 
                                  col.names = "positive.words")
positive.data.frame$positive.words <- 
  bytecode.convert(positive.data.frame$positive.words)

negative.data.frame <- read.table(file = "Hu_Liu_negative_word_list.txt",
                                  header = FALSE, colClasses = c("character"), row.names = NULL, 
                                  col.names = "negative.words")  
negative.data.frame$negative.words <- 
  bytecode.convert(negative.data.frame$negative.words)

Hu.Liu.positive.dictionary <- c(as.character(positive.data.frame$positive.words))

reviews.tdm.Hu.Liu.positive <- TermDocumentMatrix(nar_tm, list(dictionary = Hu.Liu.positive.dictionary))
examine.tdm <- removeSparseTerms(reviews.tdm.Hu.Liu.positive, 0.99)
top.words <- Terms(examine.tdm)
print(top.words)  

card_df <- data.frame(as.character(final_data$Credit.Card), t(as.matrix(examine.tdm)))

Hu.Liu.frequent.positive <- findFreqTerms(reviews.tdm.Hu.Liu.positive, 25)
# this provides a list positive words occurring at least 25 times
# a review of this list suggests that all make sense (have content validity)
# test.positive.dictionary <- Dictionary(Hu.Liu.frequent.positive)
test.positive.dictionary <- c(as.character(Hu.Liu.frequent.positive))

# .... now for the negative words
# Hu.Liu.negative.dictionary <- Dictionary(negative.data.frame$negative.words)
Hu.Liu.negative.dictionary <- c(as.character(negative.data.frame$negative.words))
reviews.tdm.Hu.Liu.negative <- TermDocumentMatrix(nar_tm, 
                                                  list(dictionary = Hu.Liu.negative.dictionary))
examine.tdm <- removeSparseTerms(reviews.tdm.Hu.Liu.negative, 0.99)

card_df <- data.frame(card_df, -1*t(as.matrix(examine.tdm)))

library(reshape)
card_df <- rename(card_df, c(as.character.final_data.Credit.Card.='CreditCard'))
card_df$CreditCard <- gsub("[0-9]", '', card_df$CreditCard)
card_df_agg <- aggregate(card_df[,2:22], by=list(card_df$CreditCard), sum)
card_df_agg$total_score <- rowSums(card_df_agg[,2:22])
card_df_agg <- rename(card_df_agg, c(Group.1='CreditCard'))
card_df_agg <- card_df_agg[,c('CreditCard', 'total_score')]

write.csv(card_df_agg, "card_sent_score.csv", row.names = F)

top.words <- Terms(examine.tdm)
print(top.words)    
Hu.Liu.frequent.negative <- findFreqTerms(reviews.tdm.Hu.Liu.negative, 15) 

## Find Good and Bad CC
freq_positive <- sort(colSums(as.matrix(reviews.tdm.Hu.Liu.positive)), decreasing=T)
head(freq_positive, 10)
## Top Card with good Review
final_data[names(head(freq_positive, 10)), 'Credit.Card']

freq_negative <- sort(colSums(as.matrix(reviews.tdm.Hu.Liu.negative)), decreasing=T)
head(freq_negative, 10)
## Top Card with good Review
final_data[names(head(freq_negative, 10)), 'Credit.Card']


