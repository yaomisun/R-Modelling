
####Q3.1
library(kknn)
#load data
myData=read.table("credit_card_data-headers.txt", header=TRUE)
myData
d.rows = nrow(myData) 

#Splite data
#Randomly shuffle the data
myData<-myData[sample(nrow(myData)),]
## 80% of the train size
train_size <- floor(0.8 * nrow(myData))
## set the seed to make your partition reproducible
set.seed(123)
train_ind <- sample(seq_len(nrow(myData)), size = train_size)
train <- myData[train_ind, ]
test <- myData[-train_ind, ]

library(data.table)
#train training set on 10-Fold validation by different models
model <- cv.kknn(formula = R1 ~ ., train, kcv =10,  kernel = "optimal", distance = 1,scale=TRUE)
model2 <- cv.kknn(formula = R1 ~ ., train, kcv =10,  kernel = "rectangular", distance = 1,scale=TRUE)
model3 <- cv.kknn(formula = R1 ~ ., train, kcv =10,  kernel = "triangular", distance = 1,scale=TRUE)
model4 <- cv.kknn(formula = R1 ~ ., train, kcv =10,  kernel = "epanechnikov", distance = 1,scale=TRUE)
model5 <- cv.kknn(formula = R1 ~ ., train, kcv =10,  kernel = "biweight", distance = 1,scale=TRUE)
model6 <- cv.kknn(formula = R1 ~ ., train, kcv =10,  kernel = "triweight", distance = 1,scale=TRUE)
model7 <- cv.kknn(formula = R1 ~ ., train, kcv =10,  kernel = "inv", distance = 1,scale=TRUE)
model8 <- cv.kknn(formula = R1 ~ ., train, kcv =10,  kernel = "gaussian", distance = 1,scale=TRUE)
model9 <- cv.kknn(formula = R1 ~ ., train, kcv =10,  kernel = "cos", distance = 1,scale=TRUE)
model10 <- cv.kknn(formula = R1 ~ ., train, kcv =10,  kernel = "rank", distance = 1,scale=TRUE)

model
model2
model3
model4
model5
model6
model7
model8
model9
model10

cv1 = data.table(model[[1]])
 
 print('Cross Validation Accuracy model')
  print(table(cv1$y == cv1$yhat))
  print(prop.table(table(cv1$y == cv1$yhat)))
 
>  print('Cross Validation Accuracy model')
[1] "Cross Validation Accuracy model"


FALSE  TRUE 
  288   235 
>   print(prop.table(table(cv1$y == cv1$yhat)))

    FALSE      TRUE 
0.5506692 0.4493308 



trainresult=train.kknn(formula = R1 ~ ., data = myData, kmax = 100, kernel = "inv", scale = TRUE)
#test it on test set
testout<- kknn(formula = R1 ~ ., train, test, distance = 1, kernel = "inv", scale = TRUE) 
testout
summary(testout)
fitted(testout)
table(predict(trainresult, test), test$R1)


##splitting the data into training, validation, and test data sets
set.seed(1)
settings = c(train = .7, test = .15, validate = .15)

spliting = sample(cut(
  seq(nrow(myData)), 
  nrow(myData)*cumsum(c(0,settings)),
  labels = names(settings)
))

res = split(myData, spliting)
sapply(res, nrow)/nrow(myData)
res


#question 4.2:
library(graphics)
irisset=read.table("iris.txt", header=TRUE)
#irisset
irisdata<-irisset[,1:4]
#irisdata


set.seed(123)

range<-2:10 #K from 2 to 10
set.seed(123)
range<-2:10 #K from 2 to 10
loopindex <-100 #Run the K Means algorithm 100 times
avg.totw <-integer(length(range)) #hold all of points
for(v in range){ 
 v.totw <-integer(loopindex) #Set up an empty vector to hold the 100 tries
 for(i in 1:loopindex){
 k.temp <-kmeans(irisdata,centers=v) 
 v.totw[i] <-k.temp$tot.withinss #Store the total withinss
 }
 avg.totw[v-1] <-mean(v.totw) #Average the 100 total withinss
}
plot(range,avg.totw,type="b", main="Total Within SS by Various K",
 ylab="Average Total Within Sum of Squares",
 xlab="Value of K")


final1<-kmeans(irisdata, centers=4, iter.max=300, nstart=4,algorithm = "Hartigan-Wong",trace=FALSE)
print(final1)
final1$center

final2<-kmeans(irisdata, centers=4, iter.max=300, nstart=4,algorithm = "Lloyd",trace=FALSE)
print(final2)

final3<-kmeans(irisdata, centers=4, iter.max=300, nstart=4,algorithm = "MacQueen",trace=FALSE)
print(final3)
final2$center
final3$center

