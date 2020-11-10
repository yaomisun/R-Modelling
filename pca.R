
#read data
myData=read.table("uscrime.txt", stringsAsFactors=FALSE, header=TRUE)
#myData

#one way to examine correction in the data, random choose some predictors to show some have relationship, some not
library(GGally)
ggpairs(myData, columns=c("Po1", "Po2", "U1", "Ineq","M"))

#PCA on the motrix of scaled predictors
PCA<-prcomp(myData[,1:15], scale. = TRUE)
summary(PCA)

#pca$rotation is the matrix of eigenvectors for rotation back to original
Rotation<-PCA$rotation
Rotation

#use screenplot to plot the variances of each prinicipal components (variance=pca$sdev^2) to help deciding
# the number of prinicipal components to use.
screeplot(PCA,type="lines", col="purple")

#can choose first 5 principal components
PC<-PCA$x[,1:5]
PC
#build linear regression model with the first 5 principal components
#add the response columns back to data for lm to run
pcData<-cbind(PC,myData[,16])
pcamodel<-lm(V6~.,data=as.data.frame(pcData))
summary(pcamodel)

#foreach picked principal components
#first rotation back
#then unscale  (scaled-original mean)/orginal sdev
index<-1:6 #first one is intercept
orgindex<-1:15
rotateback <-double(7) #Set up an empty vector to hold the rotateback value for original data ov
unscale <-double(7) #Set up an empty vector to hold the unscale value for original data ov
#in pca 
#Center refers to the mean and 
#and scale refers to standard deviation of the original variables based on our data points.
finala<-double(16) #final original predictor's coeffications
interceptpart<-double(16) #intercept added together
finalintercept=0;
for (ov in orgindex) {
  for(v in index){ 
    rotateback[v]<-pcamodel$coefficients[v]%*%Rotation[v,ov]
    unscale[v]<-(rotateback[v]-PCA$center[ov])/PCA$scale[ov]
    cat("unscaled coeffi for predictor", ov, " at component",v," is ",unscale[v], "\n" )
    
    if(v==1)
    {
      interceptpart[ov]=interceptpart[ov]+unscale[v]
    }
    else
      finala[ov]=finala[ov]+unscale[v]
  }
  cat("final coeffi for predictor", ov, "is",finala[ov], "\n" )
  cat("final intercept for predictor", ov, "is",interceptpart[ov], "\n" )
  finalintercept=finalintercept+interceptpart[ov]
}
cat("final intercept for all predictors is ", finalintercept)

y=-8652.466-99.9766*14-261.7326*0-101.7107*10+12.32713*12+8.527472*15.5-3339.723*0.64-141.4435*94-5.596622*150-7.468968*1.1-2640.796*0.12-112.0494*3.6-27.24985*3200-20.11809*20.1+217.647*0.04-14.89475*39
y


library(MASS)
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


#the soultion:
#find the vector of PC coefficients from model
