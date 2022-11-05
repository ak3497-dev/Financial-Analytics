#FA Assignment 1
#MBA21071 - ADITYA KODTE
#---------------------------------------------------------------------
rm(list=ls())   #Clear the memory
setwd("C:\\Users\\kodte\\Dropbox\\PC\\Documents\\Study\\Term 5\\FA\\Assignment 1")
data=read.csv("Dataset.csv", header=T)
#Separating Price series
price=data$Portfolio.Index
n=length(price)

#Finding simple return
ret1=diff(price)/lag(price)
write.csv(ret1,"Return.csv")
ret1=ret1[1:(n-1)]

#Finding Continuously compounded return
ret2=diff(log(price))

# Plot Price and Return data
par("mar")
par(mar=c(6.1,5.1,5.1,3.1))
plot(ret1)
plot(price)
plot(ret1,type = "l")     #Line Plot
plot(ret1,type="l",xlab = "Time",ylab = "Return",main="Portfolio Return")
plot(price,type="l",xlab = "Time",ylab = "Price",main="Portfolio Price")

#Summary statistics of data
summary(ret1)
quantile_1=quantile(ret1,c(0,0.1,0.2,0.25,.3,.4,.5,.6,.7,0.75,.8,.9,1))
quantile_1

# All Basic statistics
library(fBasics)
basic_sum=basicStats(ret1)
basic_sum

#Make Histogram
hist(ret1)
hist(ret1,breaks=200,freq=F)
hist(ret1,main="Histogram for Portfolio Returns",xlab="Returns",breaks=500)

#Make Kernel density Plots
rg_1=range(ret1)
x_axis=seq(rg_1[1],rg_1[2],.001)
y1=dnorm(x_axis,mean(ret1),sd(ret1))
lines(x_axis,y1)

den1=density(ret1,kernel="gaussian",adjust=1)  #Kernel density
plot(den1,main="Kernel density plot",xlab="Return",ylab="Density")

#Skewness, Kurtosis and Normality Test
skew_1=skewness(ret1)
skew_1
kurt_1=kurtosis(ret1)
kurt_1

# Test of Normality (Jarque-Bera Statistic)
normalTest(ret1,method="jb")

# Normal Q-Q Plot
qqnorm(ret1)

#Testing if mean return is statistically zero
t.test(ret1,mu=0)
tstat=mean(ret1)/(stdev(ret1)/sqrt(length(ret1)))
tstat

library(fUnitRoots)
#Step 1: Check Stationary 
adfTest(ret1,lags=10,type=c("ct"))
adfTest(ret1,lags=10,type=c("nc"))
adfTest(ret1,lags=10,type=c("c"))
#Step 1 Pass: Series is Stationary

#Step 2: Check Series can be forecasted or not
#Test 1: Autocorrelation function (upto 10 lags)
acf(ret1,10)
aa=acf(ret1,10)
aa
#Lag 1,6,7,9 are significant

#Test 2: Joint test of autocorrelation
Box.test(ret1,lag=20,type="Box-Pierce")
#p-value: 5.797e-06
Box.test(ret1,lag=20,type="Ljung")
#p-value = 5.419e-06
#Test 2 result: Null is rejected and the series can be forecasted using all lags jointly upto given lags

#Step 3: #Choose Model
#1. AR Model
# Identification of order:
# Method 1: pacf
pacf(ret1,10)
a1 = pacf(ret1, 10)
a1
#Lags 1,6,7,9 are significant
 
#Method 2: Information Criteria
library(forecast)
auto.arima(ret1, max.p=10,max.q=0,d=0,ic="aic")
#ARIMA(1,0,0) with non-zero mean
auto.arima(ret1, max.p=10,max.q=0,d=0,ic="bic")
#ARIMA(1,0,0) with non-zero mean 
#AR(1) is significant

#Step 4: Fit the model
out_1=arima(ret1,c(1,0,0),include.mean = T)
out_1

#Step 5: Check Good Fit
Box.test(out_1$residuals,lag=10,type="Ljung")
#p-value = 0.001444 which is less than 1% and hence significant.
#AR(1) is not a good fit

#Step 6: Reiterate From Step 4 For AR(9)
out_1=arima(ret1,c(9,0,0),include.mean = T)
out_1
Box.test(out_1$residuals,lag=10,type="Ljung")
#p-value = 0.976 which is greater than 10% and insignificant. Hence, a good fit

#Step 7: Predict The Series
predict(out_1,1)

l=length(ret1) #3405 
n=3300
forc_r=0   #This will store the forecast
#l-n+1 = 106
for (i in 1:(l-n+1)) {
  out_5=arima(ret1[i:(i+n-1)],order=c(9,0,0),include.mean = T)
  forc=predict(out_5,1)
  forc_r[i]=forc$pred[1]
}
write.csv(forc_r,"Ret_Forecast_AR(9).csv")

#2: MA Model
#Step:1-> Identification of order:
# Method 1: acf
acf(ret1,10)
#1,6,7

#Method 2: Information Criteria
library(forecast)
auto.arima(ret1, max.p=0,max.q=10,d=0,ic="aic")
#ARIMA(0,0,1) with non-zero mean
auto.arima(ret1, max.p=0,max.q=10,d=0,ic="bic")
#ARIMA(0,0,0) with zero mean

#Step:2-> Fit the model
# Fit MA(1) model (MA(1) model without intercept)
out_2=arima(ret1,c(0,0,1),include.mean = T)
out_2

#Step:3-> Check the good fit
Box.test(out_2$residuals,lag=10,type="Ljung")
#NOT A GOOD MODEL SINCE P VALUE LESS THAN 1%

#Step 4:-> Reiterate step 2 & 3
out_2=arima(ret1,c(0,0,7),include.mean = T)
out_2

Box.test(out_2$residuals,lag=10,type="Ljung")
# p-value = 0.5681

#Step 5:-> 
#Step 7: Predict The Series
predict(out_2,1)

l=length(ret1) #3405 
n=3300
forc_r=0   #This will store the forecast
#l-n+1 = 106
for (i in 1:(l-n+1)) {
  out_6=arima(ret1[i:(i+n-1)],order=c(0,0,7),include.mean = T)
  forc=predict(out_6,1)
  forc_r[i]=forc$pred[1]
}
write.csv(forc_r,"Ret_Forecast_MA(7).csv")
