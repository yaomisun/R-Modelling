# clear variables
rm(list = ls()) 


# loading data into R
mData <- read.table("breast-cancer-wisconsin.data.txt", stringsAsFactors=FALSE, header=FALSE, sep=",")
summary(mData)
missData<-which(mData$V7=="?")
missData

#find the percentage of missing amount
percent<-length(missData)/nrow(mData)
percent

#check if there is bias in V7 missing data
m_nomissing<-mData[-missData,]
m_missing<-mData[missData,]
m_missing
table(mData$V11)
table(m_nomissing$V11)
table(m_missing$V11)
sum(mData$V11==2)/nrow(mData)
sum(m_nomissing$V11==2)/nrow(mData)
sum(m_missing$V11==2)/nrow(mData)

# see from above in missing set the result 2 missing heavier than normal, so it is baised.

#find mode for V7
mode<- function(x) {
       ux <- unique(x)
      ux[which.max(tabulate(match(x, ux)))]
}
apply(mData,2,mode)

mode2<-table(unlist(mData$V7))
mode2

#replace missing data
mData[mData$V7=="?"]<-1
#convert V11 the response to 0 and 1
mData$V11[mData$V11==4] <-0
mData$V11[mData$V11==2] <-1

useData<-mData[,-c(1)]
  useData
#build lm for missing data,
lm<-lm(V7~.,data=useData)
lm
summary(lm)
#build another lm by using low p value predictors:
lm2 <- lm(V7~V2+V3+V4+V8+V9, data = useData)
lm2
summary(lm2)

#use V7 as response, and only user missing data rows as data frame
var<- c( "V1", "V2", "V3", "V4", "V5", "V6", "V8", "V9")
finalda<-m_missing[var]
finalda
Pred_lm<-predict(lm2, finalda)
Pred_lm
round(Pred_lm)

pertub<-rnorm(length(missData), 0,1)
pertub
round(Pred_lm+pertub)
