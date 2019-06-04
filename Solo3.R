library(pROC)
require(caTools)
require(ROCR)
require(ResourceSelection)
library(xgboost)

parm='N'
setwd("/Users/dipanjanpaul/Dropbox/MSPA/450/Assignments/Solo3")

load("XYZ_complete_customer_data_frame.RData")
c_df <- complete.customer.data.frame

# Outliers with high pre sales
if (parm == "Y") {
  summary(c_df$PRE2009_SALES)
  dim(c_df[(c_df$PRE2009_SALES > 25000),])
  table(c_df[(c_df$PRE2009_SALES > 25000),]$SUM_MAIL_16)
  table(c_df[(c_df$PRE2009_SALES > 25000),]$RESPONSE16)
  mean(c_df[(c_df$PRE2009_SALES > 25000),]$YTD_SALES_2009)
}

# Remove rows
c_df <- c_df[-which(c_df$PRE2009_SALES > 25000),]

# Check
if (parm == "Y") {
  tapply(c_df$PRE2009_SALES, c_df$RESPONSE16, summary)
  table(c_df[(c_df$PRE2009_SALES < 100),c('RESPONSE16')])
  
  # Response Stats
  table(c_df$RESPONSE16)
}


# Find all columns with more than 75% of the rows as unknown and remove them from the model,
unk <- apply(c_df[,-1], 2, function(x) sum(x %in% c('U',''))/length(x))
unk_cols <- names(which(unk > .8))
#unk_cols_fin <- names(apply(c_df[,unk_cols], 2, function(x) sum(x=='Y')/length(x[x != 'U'])) < 0.4)
c_df <- c_df[,-which(names(c_df) %in% unk_cols)]

#str(c_df, list.len=ncol(c_df))

c_df[which(c_df$EXAGE == 'U' | c_df$EXAGE == ''), c('EXAGE')] <- 
  mean(as.numeric(c_df[which(c_df$EXAGE != 'U' & c_df$EXAGE != ''), c('EXAGE')]))
c_df$EXAGE <- as.numeric(c_df$EXAGE)

# Reformat character fields as factor
ix <- which(names(c_df) == 'CHANNEL_ACQUISITION')
for (i in ix:(ix+24)) {
  c_df[, i] <- as.factor(c_df[, i])
}

ix <- which(names(c_df) == 'INC_WIOUTSCS_V4')
for (i in ix:(ix+1)) {
  c_df[, i] <- as.factor(c_df[, i])
}

ix <- which(names(c_df) == 'FIPSSTCD')
for (i in ix:(ix+9)) {
  c_df[, i] <- as.factor(c_df[, i])
}

# Reformat numeric fields as numeric
c_df$INC_SCS_AMT_V4 <- as.numeric(c_df$INC_SCS_AMT_V4)
c_df$INC_WOUTSCS_AMT_4 <- as.numeric(c_df$INC_WOUTSCS_AMT_4)

# Reformat character fields as factor
ix <- which(names(c_df) == 'POPUND10')
for (i in ix:(ix+90)) {
  c_df[, i] <- as.numeric(c_df[, i])
}

ix <- which(names(c_df) == 'CUR_ST_EST_FAM_INC')
for (i in ix:(ix+1)) {
  c_df[, i] <- as.factor(c_df[, i])
}

ix <- which(names(c_df) == 'CENSUS_SEG1')
for (i in ix:(ix+7)) {
  c_df[, i] <- as.factor(c_df[, i])
}

#c_df <- c_df[-which(c_df$EXAGE == 'U',]

ix <- which(names(c_df) == 'ADULT1_G')
for (i in ix:(ix+12)) {
  c_df[, i] <- as.factor(c_df[, i])
}

c_df$LOR1 <- as.numeric(c_df$LOR1)
c_df$NUM_CHILD <- as.numeric(c_df$NUM_CHILD)

ix <- which(names(c_df) == 'C_00_03')
for (i in ix:(ix+4)) {
  c_df[, i] <- as.factor(c_df[, i])
}

c_df$AD2_ESTAGE <- as.numeric(c_df$AD2_ESTAGE)
c_df$AD3_ESTAGE <- as.numeric(c_df$AD3_ESTAGE)

## Ignore Other Adults

ix <- which(names(c_df) == 'HH_DMR')
for (i in ix:(ix+45)) {
  c_df[, i] <- as.factor(c_df[, i])
}

ix <- which(names(c_df) == 'MAILPREF')
for (i in ix:(ix+1)) {
  c_df[, i] <- as.factor(c_df[, i])
}

ix <- which(names(c_df) == 'NEWCAR')
for (i in ix:(ix+25)) {
  c_df[, i] <- as.factor(c_df[, i])
}

ix <- which(names(c_df) == 'DATEDEED')
for (i in ix:(ix+106)) {
  c_df[, i] <- as.factor(c_df[, i])
}

# Delete rows that have a missing values in most columns
summary(c_df)
c_df <- c_df[!(c_df$GEOPIXELCODE %in% c('U','')),]
x <- (apply(c_df, 2, function(x) sum(x %in% c('U',''))))

# Create df for cost modelling
cols <- c('ACCTNO', 'TOTAMT16', 'SUM_MAIL_16', 'ANY_MAIL_16', 'RESPONSE16')
prf_df <- c_df[which(c_df$LEARNING_TEST == 'TEST'),]
prf_df <- subset(prf_df, select = cols)


## New Variables
c_df$CC_YES <- I((c_df$VISA_REG =="Y") | (c_df$MC_REG  == 'Y') | (c_df$AMEX_REG == 'Y') | (c_df$OTHER_REG_CC == 'Y'))
c_df$CC_YES <- as.factor(as.numeric(c_df$CC_YES))

rmcols <- c('ZIP', 'ZIP4', 'ZIP9_Supercode', 'AMEX_REG', 'OTHER_REG_CC', 'VISA_REG', 'MC_REG', 'SPORTS_RELATED',
            'NSM', 'PIXEL', 'PIXELGEO', 'ESTAGE', 'ETHNIC_MATCH', 'RRC', 'ADULT1_R', 'CRRT', 'DBPC', 'BLOCK_ID',
            'ADULT2_G', 'AD2AGE', 'ADULT2_R', 'ADULT3_G', 'AD3AGE', 'ADULT3_R', 'INC_WITHSCS_V4',
            "ADULT4_G", "AD4_ESTAGE", "AD4AGE",  "HH_DMR",          "HH_MULTI",        "PRESCHLD",       
            "CA00_03K",        "CA04_06K",        "CA07_09K",        "CA10_12K",        "CA13_18K",       
            "SSN",             "PHONEMATCH",      "CHANNEL_DOMINANCE", "DATEDEED",        "SALES",          
            "TRANSTYP",        "LOAN_AMT",        "LOAN_TYP",        "LOAN_KND",        "YEAR_BLT",       
            "ZCREDIT",         "ZCRAFTS",         "ZGOURMET",        "ZCOMPUTR",        "ZHITECH",        
            "ZONLINE",         "ZSPENDER",        "ZDONORS",         "ZPETS",           "ZMOB",           
            "ZFITNESS",        "ZOUTDOOR",        "ZTRAVANY",        "ZINVESTR",        "ZGARDEN",        
            "ZSPORTS",         "ZMUSIC",          "ZREAD",           "ZCHLDPRD",        "ZCLOTHNG",       
            "ZMOBMULT",        "ZHEALTH",         "ZHMDECOR",        "ZKITCHEN",        "ZMOBBOOK",       
            "ZMOBCLTH",        "ZMOBFIN",         "ZMOBJWL",         "ZPBCARE",         "ZPRCHONL",       
            "ZTRAVDOM",
            "FIPSSTCD", "CBSA_CD", "RURAL_URBAN_CODE", "ETHNIC_COUNTRY",  "HOMEOWNR", "AD3_ESTAGE",
            "DM_FOODS", "DM_GARDN", "DM_UP", "MGZ_FOOD",  "MGZ_GRDN",  "MGZ_RELG", "MGZ_FEM", "CON_GEN",
            "CON_RELG", "PHOTO", "TIMEZONE", "MAILPREF", "AD2_ESTAGE",
            "DPBC", "LAT", "LONG", "GEOPIXELCODE", "DUS", "ESTHMVL", "ECHVINX",
            "ESTLOANTOVALRNG", "FILLER", "TRACT", "ETHNIC_DETAIL", "CENTSCH", "STINCIND", 
            "CTINCIND", "ESTAERNG", "ESTMORTPAYRN", "ESTMORTAMTRNG", "M_HH_LEVEL", 
            "M_GRPTYPE_MEDIAN")

c_df <- c_df[, -which(names(c_df) %in% rmcols)]

num_cols <- rep("", 1)
j <- 1
for (i in 1:244) {
  if (is.numeric(c_df[,i])) {
    num_cols[j] <- names(c_df)[i]
    j <- j+1
  }
} 

X <- subset(c_df, select=num_cols)

mu.x <- matrix(rep(apply(X,FUN=mean,MARGIN=2),dim(X)[1]),nrow=dim(X)[1],byrow=TRUE);
sd.x <- matrix(rep(apply(X,FUN=sd,MARGIN=2),dim(X)[1]),nrow=dim(X)[1],byrow=TRUE);
pca_df <- (X - mu.x)/sd.x

pca <- princomp(pca_df)
num_df <- pca$scores[,1:10]

c_df <- c_df[,-which(names(c_df) %in% num_cols)]

# c_df$SUM_MAIL_QTR1 <- c_df$SUM_MAIL_1 + c_df$SUM_MAIL_2 + c_df$SUM_MAIL_3 + c_df$SUM_MAIL_4
# c_df$SUM_MAIL_QTR2 <- c_df$SUM_MAIL_5 + c_df$SUM_MAIL_6 + c_df$SUM_MAIL_7 + c_df$SUM_MAIL_4
# c_df$SUM_MAIL_QTR3 <- c_df$SUM_MAIL_9 + c_df$SUM_MAIL_10 + c_df$SUM_MAIL_11 + c_df$SUM_MAIL_12
# c_df$SUM_MAIL_QTR4 <- c_df$SUM_MAIL_13 + c_df$SUM_MAIL_14 + c_df$SUM_MAIL_15
# 
# c_df$ANY_MAIL_QTR1 <- c_df$ANY_MAIL_1 + c_df$ANY_MAIL_2 + c_df$ANY_MAIL_3 + c_df$ANY_MAIL_4
# c_df$ANY_MAIL_QTR2 <- c_df$ANY_MAIL_5 + c_df$ANY_MAIL_6 + c_df$ANY_MAIL_7 + c_df$ANY_MAIL_4
# c_df$ANY_MAIL_QTR3 <- c_df$ANY_MAIL_9 + c_df$ANY_MAIL_10 + c_df$ANY_MAIL_11 + c_df$ANY_MAIL_12
# c_df$ANY_MAIL_QTR4 <- c_df$ANY_MAIL_13 + c_df$ANY_MAIL_14 + c_df$ANY_MAIL_15

c_df$TOTAL_MAIL_INT <- apply(c_df[,which(names(c_df) %in% c('TOTAL_MAIL_1',
                                                              'TOTAL_MAIL_2',
                                                              'TOTAL_MAIL_3',
                                                              'TOTAL_MAIL_4',
                                                              'TOTAL_MAIL_5',
                                                              'TOTAL_MAIL_6',
                                                              'TOTAL_MAIL_7',
                                                              'TOTAL_MAIL_8',
                                                              'TOTAL_MAIL_9',
                                                              'TOTAL_MAIL_10',
                                                              'TOTAL_MAIL_11',
                                                              'TOTAL_MAIL_12',
                                                              'TOTAL_MAIL_13',
                                                              'TOTAL_MAIL_14',
                                                              'TOTAL_MAIL_15'))], 
                               1, function(x) lm(x ~ ., data=data.frame(x, 1:15))$coefficients[1])

c_df$TOTAL_MAIL_SLOPE <- apply(c_df[,which(names(c_df) %in% c('TOTAL_MAIL_1',
                                                            'TOTAL_MAIL_2',
                                                            'TOTAL_MAIL_3',
                                                            'TOTAL_MAIL_4',
                                                            'TOTAL_MAIL_5',
                                                            'TOTAL_MAIL_6',
                                                            'TOTAL_MAIL_7',
                                                            'TOTAL_MAIL_8',
                                                            'TOTAL_MAIL_9',
                                                            'TOTAL_MAIL_10',
                                                            'TOTAL_MAIL_11',
                                                            'TOTAL_MAIL_12',
                                                            'TOTAL_MAIL_13',
                                                            'TOTAL_MAIL_14',
                                                            'TOTAL_MAIL_15'))], 
                             1, function(x) lm(x ~ ., data=data.frame(x, 1:15))$coefficients[2])

c_df$QTY_QTR1 <- c_df$QTY1 + c_df$QTY2 + c_df$QTY3 + c_df$QTY4
c_df$QTY_QTR2 <- c_df$QTY5 + c_df$QTY6 + c_df$QTY7 + c_df$QTY4
c_df$QTY_QTR3 <- c_df$QTY9 + c_df$QTY10 + c_df$QTY11 + c_df$QTY12
c_df$QTY_QTR4 <- c_df$QTY13 + c_df$QTY14 + c_df$QTY15

c_df$TOTAMT_INT <- apply(c_df[,which(names(c_df) %in% c('TOTAMT1',
                                                          'TOTAMT2',
                                                          'TOTAMT3',
                                                          'TOTAMT4',
                                                          'TOTAMT5',
                                                          'TOTAMT6',
                                                          'TOTAMT7',
                                                          'TOTAMT8',
                                                          'TOTAMT9',
                                                          'TOTAMT10',
                                                          'TOTAMT11',
                                                          'TOTAMT12',
                                                          'TOTAMT13',
                                                          'TOTAMT14',
                                                          'TOTAMT15'))], 
                           1, function(x) lm(x ~ ., data=data.frame(x, 1:15))$coefficients[1])

c_df$TOTAMT_SLOPE <- apply(c_df[,which(names(c_df) %in% c('TOTAMT1',
                                                            'TOTAMT2',
                                                            'TOTAMT3',
                                                            'TOTAMT4',
                                                            'TOTAMT5',
                                                            'TOTAMT6',
                                                            'TOTAMT7',
                                                            'TOTAMT8',
                                                            'TOTAMT9',
                                                            'TOTAMT10',
                                                            'TOTAMT11',
                                                            'TOTAMT12',
                                                            'TOTAMT13',
                                                            'TOTAMT14',
                                                            'TOTAMT15'))], 
                             1, function(x) lm(x ~ ., data=data.frame(x, 1:15))$coefficients[2])

c_df$RESPONSE_QTR1 <- c_df$RESPONSE1 + c_df$RESPONSE2 + c_df$RESPONSE3 + c_df$RESPONSE4
c_df$RESPONSE_QTR2 <- c_df$RESPONSE5 + c_df$RESPONSE6 + c_df$RESPONSE7 + c_df$RESPONSE4
c_df$RESPONSE_QTR3 <- c_df$RESPONSE9 + c_df$RESPONSE10 + c_df$RESPONSE11 + c_df$RESPONSE12
c_df$RESPONSE_QTR4 <- c_df$RESPONSE13 + c_df$RESPONSE14 + c_df$RESPONSE15

rmcols <- c("SUM_MAIL_1",        
            "SUM_MAIL_2",         "SUM_MAIL_3",         "SUM_MAIL_4",         "SUM_MAIL_5",        
            "SUM_MAIL_6",         "SUM_MAIL_7",         "SUM_MAIL_8",         "SUM_MAIL_9",        
            "SUM_MAIL_10",        "SUM_MAIL_11",        "SUM_MAIL_12",        "SUM_MAIL_13",       
            "SUM_MAIL_14",        "SUM_MAIL_15",        "SUM_MAIL_16",        "ANY_MAIL_1",        
            "ANY_MAIL_2",         "ANY_MAIL_3",         "ANY_MAIL_4",         "ANY_MAIL_5",        
            "ANY_MAIL_6",         "ANY_MAIL_7",         "ANY_MAIL_8",         "ANY_MAIL_9",        
            "ANY_MAIL_10",        "ANY_MAIL_11",        "ANY_MAIL_12",        "ANY_MAIL_13",       
            "ANY_MAIL_14",        "ANY_MAIL_15",        "ANY_MAIL_16",        "TOTAL_MAIL_1",      
            "TOTAL_MAIL_2",       "TOTAL_MAIL_3",       "TOTAL_MAIL_4",       "TOTAL_MAIL_5",      
            "TOTAL_MAIL_6",       "TOTAL_MAIL_7",       "TOTAL_MAIL_8",       "TOTAL_MAIL_9",      
            "TOTAL_MAIL_10",      "TOTAL_MAIL_11",      "TOTAL_MAIL_12",      "TOTAL_MAIL_13",     
            "TOTAL_MAIL_14",      "TOTAL_MAIL_15",      "TOTAL_MAIL_16",      "QTY",               
            "TOTAMT",             "QTY1",               "QTY2",              
            "QTY3",               "QTY4",               "QTY5",               "QTY6",              
            "QTY7",               "QTY8",               "QTY9",               "QTY10",             
            "QTY11",              "QTY12",              "QTY13",              "QTY14",             
            "QTY15",              "QTY16",              "TOTAMT1",           
            "TOTAMT2",            "TOTAMT3",            "TOTAMT4",            "TOTAMT5",           
            "TOTAMT6",            "TOTAMT7",            "TOTAMT8",            "TOTAMT9",           
            "TOTAMT10",           "TOTAMT11",           "TOTAMT12",           "TOTAMT13",          
            "TOTAMT14",           "TOTAMT15",           "TOTAMT16",           "RESPONSE0",         
            "RESPONSE1",          "RESPONSE2",          "RESPONSE3",          "RESPONSE4",         
            "RESPONSE5",          "RESPONSE6",          "RESPONSE7",          "RESPONSE8",         
            "RESPONSE9",          "RESPONSE10",         "RESPONSE11",         "RESPONSE12",        
            "RESPONSE13",         "RESPONSE14",         "RESPONSE15")

df <- c_df[,-which(names(c_df) %in% rmcols)]
df <- cbind(df, num_df)
df$RESPONSE16 <- as.factor(df$RESPONSE16)

df$CUR_ST_EST_FAM_INC <- as.numeric(df$CUR_ST_EST_FAM_INC)
# df$CENSUS_SEG1 <- as.numeric(df$CENSUS_SEG1)
# df$CENSUS_SEG2 <- as.numeric(df$CENSUS_SEG2)
# df$CENSUS_SEG3 <- as.numeric(df$CENSUS_SEG3)
# df$CENSUS_SEG4 <- as.numeric(df$CENSUS_SEG4)
# 
# df$DM_GIFTS <- as.numeric(df$DM_GIFTS)
# df$DM_BOOKS <- as.numeric(df$DM_BOOKS)
# df$DM_CRAFT <- as.numeric(df$DM_CRAFT)
# df$DM_FEM  <- as.numeric(df$DM_FEM)
# df$DM_MALE <- as.numeric(df$DM_MALE)
# df$DM_GEN <- as.numeric(df$DM_GEN)
# df$SWEEPS <- as.numeric(df$SWEEPS)
# df$DOITSELF <- as.numeric(df$DOITSELF)
df$OCCUPATION <- as.numeric(df$OCCUPATION)
# df$CHILDPROB <- as.numeric(df$CHILDPROB)
# df$CA00_03I <- as.numeric(df$CA00_03I)
# df$CA04_06I <- as.numeric(df$CA04_06I)
# df$CA07_09I <- as.numeric(df$CA07_09I)
# df$CA10_12I <- as.numeric(df$CA10_12I)
# df$CA13_18I <- as.numeric(df$CA13_18I)
df$IMPACT <- as.numeric(df$IMPACT)

rmcols <- c( "ZGOLFERP",          "ZDONORSP",          "ZPETSP",           
             "ZARTSP",            "ZMOBP",             "ZFITNESP",          "ZOUTDOOP",         
             "ZTRAVANP",          "ZINVESTP",          "ZAUTOOWP",          "ZGARDENP",         
             "ZCOLLECP",          "ZCRUISEP",          "ZSPORTSP",          "ZSWEEPSP",         
             "ZPOLITIP",          "ZMUSICP",           "ZREADP",            "ZCHLDPRP",         
             "ZDIYP",             "ZSELFIPP",          "ZRELIGOP",          "ZGRANDPP",         
             "ZCLOTHNP",          "ZDONENVP",          "ZMUTUALP",          "ZWGHTCOP",         
             "ZPRCHPHP",          "ZPRCHTVP",          "ZMOBMULP",          "ZCREDPLP",         
             "ZDOGSP",            "ZCATSP",            "ZHEALTHP",          "ZAUTOINP",         
             "ZSKIINGP",          "ZASTRLGP",          "ZBOATSP",           "ZCELLP",           
             "ZCOMMCOP",          "ZHMDECOP",          "ZHOMEENP",          "ZKITCHEP",         
             "ZMOBAVP",           "ZMOBBOOP",          "ZMOBCLTP",          "ZMOBFINP",         
             "ZMOBGIFP",          "ZMOBGRDP",          "ZMOBJWLP",          "ZMUSCLAP",         
             "ZMUSCNTP",          "ZMUSCRSP",          "ZMUSOLDP",          "ZMUSROCP",         
             "ZPBCAREP",          "ZPHOTOP",           "ZPRCHONP",          "ZTENNISP",         
             "ZTRAVDOP",          "ZTRAVFOP",          "ZVOLUNTP",
             "INC_WIOUTSCS_V4", "FIPSCNTY", "MCD_CCD", "CENSUS_SEG1", "CENSUS_SEG2", "CENSUS_SEG3", "CENSUS_SEG4",
             "ADULT1", "MARRIED", "LANGUAGE", "RELIGION", "ETHNIC_GROUP", "E_TECH", "DM_CRAFT", 
             "DM_FEM", "DM_GEN", "CON_HLTH", "NEWS", "RESPODDS", "RESPMISC", "OCCUPATION_GROUP", "IND_ED",
             "CHILDPROB", "CA00_03I", "CA04_06I", "CA07_09I", "CA10_12I", "CA13_18I", "NEWCAR", "USEDCAR",
             "SHOW_ME_MONEY", "GO_WITH_FLOW", "NO_TIME_PRESENT", "NEVER_EMPTY_HANDED", "ON_THE_ROAD",
             "LOOK_ATME_NOW", "WORK_PLAY_HARD", "PENNY_SAVED_EARNED", "ALL_INTHE_NAME", "EMAIL_RECEPTIVE",
             "AD_MAGAZINE", "AD_NEWSPAPER", "AD_RADIO", "AD_TV", "ESTMORTPAYRNG", "M_GLOBAL_Z4",
             "CHANNEL_ACQUISITION", "BUYER_STATUS", "BLOCK", "LEVEL_LAT_LONG", "ADULT1_G", "ADD_TYPE", 
             "C_00_03", "C_04_06", "C_07_09", "C_10_12", "DM_GIFTS", "DM_BOOKS", "DM_MALE", "MGZ_HLTH",
             "MGZ_HLTH", "MGZ_MALE", "MGZ_FAM", "CON_POLT", "SWEEPS", "DOITSELF", "ITMM", "AD_WEB",
             "BUY_AMERICAN", "STOP_SMELL_ROSES")

df <- df[,-which(names(df) %in% rmcols)]
#df$PRE2009_SALES <- log(df$PRE2009_SALES + 0.0001)
#df$PRE2009_TRANSACTIONS <- log(df$PRE2009_TRANSACTIONS + 0.0001)
#df$TOTAMT0 <- log (df$TOTAMT0 + 0.0001)

trn <- df[which(df$LEARNING_TEST == 'LEARNING'), ]
trn <- trn[, -which(names(trn) %in% c('ACCTNO', 'LEARNING_TEST'))]

tst <- df[which(df$LEARNING_TEST == 'TEST'), ]
tst <- tst[, -which(names(tst) %in% c('ACCTNO', 'LEARNING_TEST'))]

## EDA and Data Mining Activities Ends Here,.. Modelling starts ##

## Base Model - GLM
model <- glm(RESPONSE16 ~ ., family = binomial("logit"), data = trn)

if (parm=="Y") {
  step(model, direction="both")
}

model.glm <- glm(formula = RESPONSE16 ~ CUR_ST_EST_FAM_INC + C_13_18 + OCCUPATION + 
      IMPACT + PRE2009_SALES + PRE2009_TRANSACTIONS + QTY0 + TOTAMT0 + 
      TOTAL_MAIL_INT + TOTAL_MAIL_SLOPE + QTY_QTR1 + QTY_QTR2 + 
      QTY_QTR3 + QTY_QTR4 + TOTAMT_INT + TOTAMT_SLOPE + RESPONSE_QTR1 + 
      RESPONSE_QTR2 + RESPONSE_QTR3 + RESPONSE_QTR4 + Comp.1 + 
      Comp.2 + Comp.3 + Comp.4 + Comp.5 + Comp.6 + Comp.7 + Comp.8 + 
      Comp.9, family = binomial("logit"), data = trn)

pred2 <- predict(model.glm, newdata=tst, type="response")
head(pred2)
str(pred2)

pred2round <- round(pred2,0)
xtabs(~RESPONSE16 + pred2round, data = tst)

#### accuracy = (8888+505)/(8888  + 124 + 344 + 505) = 95.25%####

### ROCR curve
ROCRpred <- prediction(pred2,tst$RESPONSE16)
ROCRperf <- performance(ROCRpred, 'tpr', 'fpr')
plot(ROCRperf, colorize = TRUE, text.adj = c(-0.2,1.7))
abline(a=0,b=1)
auc(tst$RESPONSE16,pred2)

## 
library(randomForest)
model.rf <- randomForest(RESPONSE16 ~ ., ntree=400,importance=T,data = trn)
pred2 <- predict(model.rf, newdata=tst, type="response")

## XGB Model
trn_response <- as.numeric(as.character(trn$RESPONSE16))
trn_M <- as.matrix(apply(trn[,-which(names(trn) %in% c('RESPONSE16', 'C_13_18'))], 2, as.numeric))
tst_M <- as.matrix(apply(tst[,-which(names(tst) %in% c('RESPONSE16', 'C_13_18'))], 2, as.numeric))

param <- list("objective" = "binary:logistic", max.depth = 2, gamma = 0, eta = 0.05, 
              min_child_weight=1, "eval_metric" = "logloss", subsample = .8, colsample_bytree=1)

model.xgb.cv <- xgb.cv(params=param, data = trn_M, label = trn_response, nround=500, nfold=4)

bst = xgboost(param=param, data = trn_M, label = trn_response, nrounds = 400, verbose=0)

pred <- predict(bst, newdata=tst_M)
pred2round <- round(pred,0)
xtabs(~RESPONSE16 + pred2round, data = tst)

roc1 <- roc(tst$RESPONSE16, pred)
plot(roc1, main=c(paste("AUC: ", round(roc1$auc,4), sep="")))

## Profitability Calculation Model
prf_df$pred <- pred2
prf_df$prof <- prf_df$pred*(0.1*prf_df$TOTAMT16 - 2)
prf_df <- prf_df[order(prf_df$pred, decreasing = T),]
prf_df$pred_rnd <- round(prf_df$pred,0)

profit <- round(cumsum(prf_df$prof), 2)
n.mail.valid <- which.max(profit)

plot(profit, xlab = "Number of Mailings", ylab="Net Profit") # see how profits change as more mailings are made
abline(v=which.max(profit), lty = 2, col = "red")
legend ("bottomright", legend = c(paste("Total Mails: ", n.mail.valid, sep=""),
                                  paste("Profit: ", max(profit), sep="")), col = c('black', "blue"), pch = c(19, 2))

## Scoring
hist(prf_df[((prf_df$SUM_MAIL_16 == 0) & (prf_df$pred_rnd == 1)),]$prof, 25, 
     main='Non targeted but positively predicted customers', xlab = 'Net Profit', col = 'lightblue')

head(prf_df[((prf_df$SUM_MAIL_16 == 0) & (prf_df$pred_rnd == 1)),which(names(prf_df) %in% c('ACCTNO', 'prof'))])

## Comparing net Revenue
sum(prf_df[((prf_df$pred_rnd == 1)),]$prof)
sum(prf_df[((prf_df$RESPONSE16 == 1)),]$prof) - dim(prf_df[(prf_df$SUM_MAIL_16 > 0) & (prf_df$RESPONSE16 == 0),])[1] * 2



