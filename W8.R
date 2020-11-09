setwd("F:/Geth/Homework/W8")
#read data
myData=read.table("uscrime.txt", stringsAsFactors=FALSE, header=TRUE)
set.seed(123)
library(MASS)
# Fit the full model 
lm <- lm(Crime ~., data = myData)


# Stepwise regression model
Step <-step(lm,  scale = 0,
               direction = "both",
               trace = 1, keep = NULL, steps = 1000, use.start = FALSE,
               k = 2)
summary(Step)

#build m model based on stepwise suggestion with 8 varibale selection:
lm_step<-lm(Crime~M+Ed+Po1+M.F+U1+U2+Ineq+Prob, data = myData)
summary(lm_step)

#first scale data
scaled <-scale(myData[,1:15],scale=TRUE)#try LASSO where alpha=1
lasso<-glmnet::glmnet(as.matrix(scaled), as.matrix(myData$Crime), family="gaussian", alpha = 1, nlambda = 100)
summary(lasso)
plot(lasso, xvar = "dev", label = TRUE)

coef.exact = coef(x=scaled, y=as.matrix(myData$Crime),lasso, s=1,exact = TRUE)
coef.apprx = coef(lasso, s=1,exact = FALSE)
cbind2(coef.exact, coef.apprx)



cvLASSO<-glmnet::cv.glmnet(scaled, as.matrix(myData$Crime), type.measure="deviance")
print(cvLASSO)
summary(cvLASSO)
plot(cvLASSO)
plot(cvLASSO, xvar = "dev", label = TRUE)
coef.exact = coef(x=scaled, y=as.matrix(myData$Crime),cvLASSO, s=1,exact = TRUE)
coef.exact

#choose M, ED, Po1, U2,Ineq.
lm_lasso<-lm(Crime~M+Ed+Po1+U2+Ineq, data = myData)
summary(lm_lasso)

set.seed(123)
#now try to build elastic net and try the best one 
index<-1:10
error<-double(10) #mean error
for (ov in index) {
  al<-ov*0.1
  tmp<-glmnet::cv.glmnet(scaled, as.matrix(myData$Crime), type.measure="deviance", family="gaussian", alpha = al)
  error[ov]<- mean(tmp$cvsd)
}
error
#choose alpha=0.4
cv_EN<-glmnet::cv.glmnet(scaled, as.matrix(myData$Crime), type.measure="mse", family="gaussian", alpha = 0.4)
summary(cv_EN)
plot(cv_EN)
coef.exact = coef(x=scaled, y=as.matrix(myData$Crime),cv_EN, s=0.4,exact = TRUE)
coef.exact

#choose ED, Po1, Po2, U2,Ineq, Prob
#build m model based on suggestion with 6 varibale selection:
lm_EN<-lm(Crime~Ed+Po1+Po2+U2+Ineq+Prob, data = myData)
summary(lm_EN)