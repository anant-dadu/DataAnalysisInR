x=c(2,7,5)
y=seq(from=4,length=3,by=3)
?seq
y
x+y
x/y
x^y
x[2]
x[2:3] ## starting at 2 ending at 3->[2,3]
x[-2] ## remove the element at index 2 from x
x[-c(1,2)]
z=matrix(seq(1,12), 4, 3) ## 2-d array column order
z
z[3:4, 2:3]
z[,2:3]
z[,1]
z[,1, drop=FALSE]
dim(z)
ls()
rm(y)
ls()
### generating random data, graphics
x=runif(50) ## random uniform
y=rnorm(50) ## random normal
plot(x,y)
par(mfrow=c(1,1))
plot(x,y)
hist(y)
## Reading in data
Auto = read.csv("Data Science ZExercise_TRAINING_CONFIDENTIAL1.csv")
names(Auto)
dim(Auto)
class(Auto)
summary(Auto)
plot(BedroomCnt, SaleDollarCnt)
attach(Auto) ## variables in Auto dataframe
BedroomCnt = as.factor(BedroomCnt)
