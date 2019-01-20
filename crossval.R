

library(caret)

zillow = read.csv("Data Science ZExercise_TRAINING_CONFIDENTIAL1.csv")
names(zillow)
summary(zillow)
require(boot)


pre_zillow = zillow
# pre_zillow = subset(zillow, select = c("PropertyID", "censusblockgroup"))
pre_zillow$PropertyID=NULL
pre_zillow$censusblockgroup=NULL
pre_zillow$Usecode=NULL

## time like data
pre_zillow$TransDate=pre_zillow$TransDate # NULL

pre_zillow$BuiltYear=pre_zillow$BuiltYear

pre_zillow$BGMedYearBuilt=pre_zillow$BGMedYearBuilt
pre_zillow[is.na(pre_zillow[,"BGMedYearBuilt"]), "BGMedYearBuilt"] <- mean(pre_zillow[,"BGMedYearBuilt"], na.rm = TRUE)

pre_zillow$Latitude=pre_zillow$Latitude
pre_zillow$Longitude=pre_zillow$Longitude

pre_zillow$ZoneCodeCounty=NULL#zillow$ZoneCodeCounty

## factor like
pre_zillow$BedroomCnt=pre_zillow$BedroomCnt
pre_zillow$BedroomCnt=pre_zillow$BedroomCnt
pre_zillow$StoryCnt = pre_zillow$StoryCnt


pre_zillow$ViewType=NULL

## missing values
# pre_zillow$GarageSquareFeet=pre_zillow$GarageSquareFeet
pre_zillow$GarageSquareFeet=pre_zillow$GarageSquareFeet
pre_zillow[is.na(pre_zillow[,"GarageSquareFeet"]), "GarageSquareFeet"] <- mean(pre_zillow[,"GarageSquareFeet"], na.rm = TRUE)

# pre_zillow$BGMedRent=NULL#pre_zillow$BGMedRent
pre_zillow$BGMedRent=pre_zillow$BGMedRent
pre_zillow[is.na(pre_zillow[,"BGMedRent"]), "BGMedRent"] <- mean(pre_zillow[,"BGMedRent"], na.rm = TRUE)

# pre_zillow$BGMedHomeValue=NULL#pre_zillow$BGMedHomeValue
pre_zillow$BGMedHomeValue=pre_zillow$BGMedHomeValue
pre_zillow[is.na(pre_zillow[,"BGMedHomeValue"]), "BGMedHomeValue"] <- mean(pre_zillow[,"BGMedHomeValue"], na.rm = TRUE)

## quantative variables
pre_zillow$FinishedSquareFeet=pre_zillow$FinishedSquareFeet
pre_zillow$LotSizeSquareFeet=pre_zillow$LotSizeSquareFeet
pre_zillow$BGPctOwn=pre_zillow$BGPctOwn
pre_zillow$BGPctVacant=pre_zillow$BGPctVacant
pre_zillow$BGMedIncome=pre_zillow$BGMedIncome
pre_zillow$BGPctKids=pre_zillow$BGPctKids
pre_zillow$BGMedAge=pre_zillow$BGMedAge

write.csv(pre_zillow, file = "preprocessed.csv")


pre_zillow$TransDate = as.factor(format(as.Date(zillow$TransDate, format = "%m/%d/%Y"),"%m"))
pre_zillow$BuiltYear = as.factor(cut(pre_zillow$BuiltYear, breaks=c(0, 1940, 1960, 1980, 1990, 2000, 2010, Inf)))
pre_zillow$BGMedYearBuilt = as.factor(cut(pre_zillow$BGMedYearBuilt, breaks=c(0, 1970, 1990, 2000, Inf)))

pre_zillow$BathroomCnt=as.factor(cut(pre_zillow$BathroomCnt, breaks=c(0, 5, Inf)))
pre_zillow$BedroomCnt=as.factor(cut(pre_zillow$BedroomCnt, breaks=c(0, 5, Inf)))
pre_zillow$StoryCnt=as.factor(cut(pre_zillow$StoryCnt, breaks=c(0, 2, Inf)))

summary(pre_zillow)

# glm.fit=glm(SaleDollarCnt~.,data=training_data);
library(randomForest)
library(e1071)

cv.errors10=rep(0, 10)
cv.errors10med=rep(0, 10)
cv.errors101=rep(0, 10)
cv.errors10med1=rep(0, 10)
cv.errors102=rep(0, 10)
cv.errors10med2=rep(0, 10)
counter = 1
cv_splits <- createFolds(seq(1:dim(pre_zillow)[1]), k = 10, returnTrain = TRUE)
for(train_set in cv_splits){
  ts = unlist(train_set, use.names = FALSE)
  training_data = pre_zillow[ts, ]
  testing_data = pre_zillow[-ts, ]
  glm.fit1=glm(SaleDollarCnt~.,data=training_data);
  glm.fit2=randomForest(SaleDollarCnt~.,data=training_data,ntree=4);
  # glm.fit=svm(SaleDollarCnt~.,data=training_data,kernel = "linear", cost = 10, scale = FALSE)
  
  y_hat1=predict(glm.fit1, testing_data)
  y_hat2=predict(glm.fit2, testing_data)
  y_hat=(y_hat1+y_hat2)/2
  y=testing_data$SaleDollarCnt
  print (mean(abs(y_hat-y)/y));print (median(abs(y_hat-y)/y))
  cv.errors10[counter] = mean(abs(y_hat-y)/y)
  cv.errors10med[counter] = median(abs(y_hat-y)/y)
  cv.errors101[counter] = mean(abs(y_hat1-y)/y)
  cv.errors10med1[counter] = median(abs(y_hat1-y)/y)
  cv.errors102[counter] = mean(abs(y_hat2-y)/y)
  cv.errors10med2[counter] = median(abs(y_hat2-y)/y)
  counter = counter + 1
}
print (summary(glm.fit1))
print (mean(cv.errors101))
print (mean(cv.errors102))
write.csv(pre_zillow, file = "preprocessed.csv")


# bstSparse <- xgboost(data = train$data, label = train$label, max.depth = 2, eta = 1, nthread = 2, nrounds = 2, objective = "binary:logistic")
library(xgboost)
labels = training_data$SaleDollarCnt
new_tr = training_data
training_data$SaleDollarCnt=NULL

ts_label = testing_data$SaleDollarCnt
new_ts = testing_data
testing_data$SaleDollarCnt=NULL

dtrain <- xgb.DMatrix(data = new_tr, label=labels) 
dtest <- xgb.DMatrix(data = new_ts, label=ts_label)


params <- list(booster = "gbtree", objective = "reg:linear", eta=0.3, gamma=0, max_depth=6, min_child_weight=1, subsample=1, colsample_bytree=1)
xgbcv <- xgb.cv( params = params, data = dtrain, nrounds = 2, nfold = 10, showsd = T, stratified = T, print.every.n = 10, early.stop.round = 20, maximize = F)
