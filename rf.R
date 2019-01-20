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
pre_zillow$TransDate=NULL
pre_zillow$BuiltYear=NULL
pre_zillow$BGMedYearBuilt=NULL

pre_zillow$Latitude=NULL
pre_zillow$Longitude=NULL
pre_zillow$ZoneCodeCounty=NULL

## factor like
pre_zillow$BedroomCnt=NULL
pre_zillow$BathroomCnt=NULL
pre_zillow$StoryCnt=NULL
pre_zillow$ViewType=NULL

## missing values
pre_zillow$GarageSquareFeet=NULL#pre_zillow$GarageSquareFeet
pre_zillow$BGMedRent=NULL#pre_zillow$BGMedRent
pre_zillow$BGMedHomeValue=NULL#pre_zillow$BGMedHomeValue


## quantative variables
pre_zillow$FinishedSquareFeet=pre_zillow$FinishedSquareFeet
pre_zillow$LotSizeSquareFeet=pre_zillow$LotSizeSquareFeet
pre_zillow$BGPctOwn=pre_zillow$BGPctOwn
pre_zillow$BGPctVacant=pre_zillow$BGPctVacant
pre_zillow$BGMedIncome=pre_zillow$BGMedIncome
pre_zillow$BGPctKids=pre_zillow$BGPctKids
pre_zillow$BGMedAge=pre_zillow$BGMedAge



library(randomForest)
library(mlbench)
library(caret)

data(Sonar)
dataset <- Sonar


# Create model with default paramters
control <- trainControl(method="repeatedcv", number=10, repeats=3)
seed <- 7
metric <- "RMSE"
set.seed(seed)
mtry <- sqrt(ncol(pre_zillow)-1)
tunegrid <- expand.grid(.mtry=mtry)
rf_default <- train(SaleDollarCnt~., data=pre_zillow, method="rf", metric=metric, tuneGrid=tunegrid, trControl=control)
print(rf_default)


# Random Search
control <- trainControl(method="repeatedcv", number=10, repeats=3, search="random")
set.seed(seed)
mtry <- sqrt(ncol(x))
rf_random <- train(Class~., data=dataset, method="rf", metric=metric, tuneLength=15, trControl=control)
print(rf_random)
plot(rf_random)


control <- trainControl(method="repeatedcv", number=10, repeats=3, search="grid")
set.seed(seed)
tunegrid <- expand.grid(.mtry=c(1:15))
rf_gridsearch <- train(Class~., data=dataset, method="rf", metric=metric, tuneGrid=tunegrid, trControl=control)
print(rf_gridsearch)
plot(rf_gridsearch)



# Algorithm Tune (tuneRF)
set.seed(seed)
bestmtry <- tuneRF(x, y, stepFactor=1.5, improve=1e-5, ntree=500)
print(bestmtry)


# Algorithm Tune (tuneRF)
set.seed(seed)
bestmtry <- tuneRF(x, y, stepFactor=1.5, improve=1e-5, ntree=500)


control <- trainControl(method="repeatedcv", number=10, repeats=3, search="grid")
tunegrid <- expand.grid(.mtry=c(sqrt(ncol(x))))
modellist <- list()
for (ntree in c(1000, 1500, 2000, 2500)) {
  set.seed(seed)
  fit <- train(Class~., data=dataset, method="rf", metric=metric, tuneGrid=tunegrid, trControl=control, ntree=ntree)
  key <- toString(ntree)
  modellist[[key]] <- fit
}
# compare results
results <- resamples(modellist)
summary(results)
dotplot(results)


# train model
control <- trainControl(method="repeatedcv", number=10, repeats=3)
tunegrid <- expand.grid(.mtry=c(1:15), .ntree=c(1000, 1500, 2000, 2500))
set.seed(seed)
custom <- train(Class~., data=dataset, method=customRF, metric=metric, tuneGrid=tunegrid, trControl=control)
summary(custom)
plot(custom)
print(bestmtry)