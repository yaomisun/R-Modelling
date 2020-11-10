
#read data
myData=read.table("temps.txt", header=TRUE)
#myData
##convert to time series
#first convert to vector
t_Vec<-as.vector(unlist(myData[,2:21]))
#t_Vec
plot(t_Vec)
#convert to time series
#frequency: becasue we have 123 data points each year.
t_ts<-ts(t_Vec, start=1996, frequency=123)
#use holtwinter
#let hlot winter to determine the best alpha (level),
#beta (trend) and gamma(seasonality),
#
temp_hm<-HoltWinters(t_ts,alpha = NULL, beta=NULL, gamma=NULL, seasonal="multiplicative")
summary(temp_hm)
plot(temp_hm)
fitted_o<-temp_hm$fitted
View(fitted_o)

year <-1:20 #year 1995 to 2014
for (variable in year) {
    #myValue[row, variable]<-fitted_o[variable:123]
    write.table(fitted_o[variable:123],file="fittedvalues.csv",append=TRUE, sep=",")
    
}
myValue
head(fitted_o)
tail(fitted_o)

temp_sf<-matrix(fitted_o[,1],nrow=123)
temp_sf

#write to csv
write.table(fitted_o,file="fittedvalues.csv",sep=",")