
####Q3.1
library(outliers)
#load data
myData=read.table("uscrime.txt", header=TRUE)
myData
d.rows = nrow(myData) 

#
set.seed(123)
test1<- grubbs.test(myData$Crime, type = 10, opposite = FALSE, two.sided = FALSE)

test1

#  Grubbs test for one outlier

data:  myData$Crime
G = 2.81287, U = 0.82426, p-value = 0.07887
alternative hypothesis: highest value 1993 is an outlier
test2<- grubbs.test(myData$Crime, type = 11, opposite = FALSE, two.sided = TRUE)
test2
#
  Grubbs test for two opposite outliers

data:  myData$Crime
G = 4.26877, U = 0.78103, p-value < 2.2e-16
alternative hypothesis: 342 and 1993 are outliers

boxplot(Time~Crime,data=myData, main="Crime Data", 
  col="lightgray")

#Q6.2
library(cran)
myData=read.table("temps.txt", header=TRUE)
mySummer<-myData[c(1:92),]

year <-1:20 #year 1995 to 2014
mulist <-double(19) #Set up an empty vector to hold the 100 tries

for(v in year){ 
 colyear<-paste("X",1995+v, sep="")
print(colyear)
#print(mySummer[,v+1])
mulist[v]<-mean(mySummer[,v+1])
print(mulist[v])
}

for(v in year){ 
 print(mulist[v])
}

#for all the summer
totalmean<-mean(mulist)
totalmean

cusum(X, k, h, initial = 0, reset = TRUE)
 try<- cusum(mulist, totalmean, h=5, reset=TRUE)
summary(try)


