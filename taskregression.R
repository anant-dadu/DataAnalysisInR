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



summary(pre_zillow)
regplot=function(x,y,...){
  fit=lm(y~x)
  plot(x,y,...)
  abline(fit,col="red")
}
par(mfrow=c(2,2))
# regplot(pre_zillow$BGMedAge,pre_zillow$SaleDollarCnt)
# regplot(pre_zillow$FinishedSquareFeet,pre_zillow$SaleDollarCnt)
# regplot(pre_zillow$GarageSquareFeet,pre_zillow$SaleDollarCnt)
# regplot(pre_zillow$LotSizeSquareFeet,pre_zillow$SaleDollarCnt)
# regplot(pre_zillow$BGMedRent,pre_zillow$SaleDollarCnt)
# pre_zillow=na.omit(pre_zillow)
summary(pre_zillow)
# glm.fit10=glm(SaleDollarCnt~.+GarageSquareFeet:FinishedSquareFeet,data=pre_zillow);
glm.fit11=glm(SaleDollarCnt~.,data=pre_zillow);
cv.glm(data=pre_zillow,glmfit=glm.fit10,K=5)$delta
cv.glm(pre_zillow,glm.fit11,K=10)$delta

require(randomForest)
rf12=randomForest(SaleDollarCnt ~ . , data = pre_zillow)
library(rfUtilities)
rf.crossValidation(rf12, pre_zillow, p = 0.1, n = 99, seed = NULL)
