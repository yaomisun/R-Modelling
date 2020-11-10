
#read data
myData=read.table("uscrime.txt", stringsAsFactors=FALSE, header=TRUE)
#myData
library(tree)
Ctree<-tree(Crime~.,data=myData)
summary(Ctree)
#visualize 
plot(Ctree)
text(Ctree)

Ctree$frame
Ctree$where

#predict
PredTree<-predict(Ctree)
PredTree
plot(PredTree,myData$Crime)

#computes the root mean squared error between two numeric vectors
rmse_cTree<-sqrt(mean((PredTree-myData$Crime)^2))
rmse_cTree

prune.tree(Ctree)$size
prune.tree(Ctree)$dev

set.seed(42)
cv.tree(Ctree)
cv.tree(Ctree)$dev


#prune the tree to 4 leaves
Ctree_P<-prune.tree(Ctree, best=4)
Ctree_P
plot(Ctree_P)
text(Ctree_P)

#for randome forest
library(randomForest)
set.seed(42)
#choose number of predictors: 1+log(n) or n/3, which I choose 5
Pred_No<-5
C_RF<-randomForest(Crime~.,data=myData, mtry=Pred_No,importance=TRUE,ntree=500)
C_RF

importance(C_RF)

Pred_RF<-predict(C_RF, myData)
Pred_RF

rmse_RF<-sqrt(mean((Pred_RF-myData$Crime)^2))
rmse_RF
#10.3
myData=read.table("germancredit.txt", sep=" ")
head(myData)
#myData

#match 1 and 2 in data file to 0 and 1(binary)
myData$V21[myData$V21==1] <-0
myData$V21[myData$V21==2] <-1
head(myData)


#make training and test set
Credit_Train<-myData[1:800,]
Credit_Test<-myData[801:1000,]

Credit_logit<-glm(V21~.,family=binomial(link="logit"),data=Credit_Train)
summary(Credit_logit)

Pred_Credit<-predict(Credit_logit, Credit_Test, type = "response")
Pred_Credit

#use
library(pROC)
roc(Credit_Test$V21, round(Pred_Credit))

#set threshold
TH<-0.8
run_TH<-as.integer(Pred_Credit>TH)
Conf_mtx<-as.matrix(table(run_TH,Credit_Test$V21))
Conf_mtx
#choose false positive

Conf_mtx[2,1]

index<-1:95
finalFP=1000;
final_TH=0;
for (ov in index) {
   TH<-ov*0.01
   run_TH<-as.integer(Pred_Credit>TH)
   Conf_mtx<-as.matrix(table(run_TH,Credit_Test$V21))
   if(Conf_mtx[2,1]<finalFP)
   {
     finalFP<-Conf_mtx[2,1]
     final_TH<-TH
   }
}
cat("lowest False Positive ", finalFP, " get when threshold is ", final_TH)

