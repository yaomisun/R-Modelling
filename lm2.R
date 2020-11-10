
#read data
myData=read.table("uscrime.txt", stringsAsFactors=FALSE, header=TRUE)
#myData
#fit to linear regression model
lm_crime<-lm(Crime~.,data=myData)

lm_crime
summary(lm_crime)
library(MASS)
#boxed<-boxcox(lm_crime)
#boxed
AIC(lm_crime)
plot(lm_crime)
#build another lm by using low p value predictors:
lm2 <- lm(Crime~M+Ed+Ineq+Prob, data = myData)
lm2
summary(lm2)
AIC(lm2)
plot(lm2)

#rebuild mode by choosing high coefficients preditors

lm3 <- lm(Crime~M+Ed+Po1+Po2+LF+U1+U2+Ineq+Prob, data = myData)
lm3
summary(lm3)
AIC(lm3)
plot(lm3)

#build a new datapoint to predict by using lm model
newData<-data.frame(M = 14.0
                    ,So = 0
                    ,Ed = 10.0
                    ,Po1 = 12.0
                    ,Po2 = 15.5
                    ,LF = 0.640
                    ,M.F = 94.0
                    ,Pop = 150
                    ,NW = 1.1
                    ,U1 = 0.120
                    ,U2 = 3.6
                    ,Wealth = 3200
                    ,Ineq = 20.1
                    ,Prob = 0.04
                    ,Time = 39.0)
pred_new<-predict(lm3, newData)
pred_new

qqnorm(myData$Crime)

#validate the quailty of the fit
library(DAAG)

#do 4 folder cross validation with the model
set.seed(42)
lmcv<-cv.lm(myData,lm_crime,m=4)

#do 4 folder cross validation with the model
set.seed(42)
lmcv2<-cv.lm(myData,lm2,m=4)




#lmcv
#summary(lmcv)
summary(lmcv)$sigma^2
rmse <- sqrt(attr(lmcv,"ms"))
rmse
