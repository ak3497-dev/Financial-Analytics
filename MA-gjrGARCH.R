#MA Modelling
library(fBasics)
rm(list=ls())   #Clear the memory
setwd("C:\\Users\\kodte\\Dropbox\\PC\\Documents\\Study\\Term 5\\FA\\Mid Term Practice\\MA-gjrGARCH")
data=read.csv("Dataset.csv", header=T)
#Separating Price series
price=data$Nifty
n=length(price)

#Finding simple return
ret1=diff(price)/lag(price)
write.csv(ret1,"Return.csv")
ret1=ret1[1:(n-1)]

#Finding Continuously compounded return
ret2=diff(log(price))

# Test of Normality (Jarque-Bera Statistic)
normalTest(ret1,method="jb") #p Value: < 2.2e-16 
# Normal Q-Q Plot
qqnorm(ret1)

#Identify the order of MA model
acf(ret1,10)
aa = acf(ret1,10)
aa
# Another way for autoselection of MA model
library(forecast)
auto.arima(ret1, max.p=0,max.q=10,d=0,ic="aic") #ARIMA(0,0,1) with non-zero mean
auto.arima(ret1, max.p=0,max.q=10,d=0,ic="bic") #ARIMA(0,0,0) with zero mean

# Fit MA(0) model without intercept
out_1=arima(ret1,c(0,0,0),include.mean = F)
out_1
Box.test(out_1$residuals,lag=10,type="Ljung") #p-value = 1.122e-05
#Not A GOOD MODEL SINCE P VALUE LESS THAN 1%

# Fit MA(1) model with intercept
out_2=arima(ret1,c(0,0,1),include.mean = T)
out_2
Box.test(out_2$residuals,lag=10,type="Ljung") #p-value = 6.978e-05
#Result is significant so discard the model

# Fit MA(8) model with intercept
out_3=arima(ret1,c(0,0,8),include.mean = T)
out_3
Box.test(out_3$residuals,lag=10,type="Ljung") #p-value = 0.9928
#p value is 99.28% hence good fit

l=length(ret1) #4397 
n=4300
forc_r=0   #This will store the forecast
#l-n+1 = 98
for (i in 1:(l-n+1)) {
  out_5=arima(ret1[i:(i+n-1)],order=c(0,0,8),fixed = c(NA,0,0,0,0,NA,NA,NA,NA))
  forc=predict(out_5,1)
  forc_r[i]=forc$pred[1]
}
write.csv(forc_r,"Ret_Forecast_MA(8).csv")
#---------------------------------------------------------------------------------------
#Fit MA(8) model with lags 1,6,7,8
out_4=arima(ret1,c(0,0,8),fixed = c(NA,0,0,0,0,NA,NA,NA,NA))
out_4
Box.test(out_1$residuals,lag=10,type="Ljung")
#p value is 90.77% hence good fit

l=length(ret1) #4397 
n=4300
forc_r=0   #This will store the forecast
#l-n+1 = 98
for (i in 1:(l-n+1)) {
  out_5=arima(ret1[i:(i+n-1)],order=c(0,0,8),fixed = c(NA,0,0,0,0,NA,NA,NA,NA))
  forc=predict(out_5,1)
  forc_r[i]=forc$pred[1]
}
write.csv(forc_r,"Ret_Forecast_MA(1_6_7_8)_specific lag.csv")
#----------------------------
library(rugarch)
spec1=ugarchspec(variance.model = list(model = "gjrGARCH", garchOrder = c(1, 1)),mean.model = list(armaOrder = c(0, 8), include.mean = TRUE),distribution.model = "norm")
garch_fit_1 = ugarchfit(spec = spec1, data = ret1)
garch_fit_1
std_res_1=as.numeric(residuals(garch_fit_1,standardize=TRUE))
Box.test(std_res_1,lag=10,type="Ljung")
library(FinTS)
ArchTest(std_res_1,lags = 10)

#Generate forecast
#Recursive window
spec1=ugarchspec(variance.model = list(model = "gjrGARCH", garchOrder = c(1, 1)),mean.model = list(armaOrder = c(8, 0), include.mean = TRUE),distribution.model = "norm")
forc_1=ugarchroll(spec1,data = ret1,n.ahead = 1, n.start = 4300,refit.every = 96, refit.window = "recursive")
forc_1
forc_11=cbind(forc_1@forecast$density$Mu,forc_1@forecast$density$Sigma)
write.csv(forc_11,"Forcast_Recursive_gjrGARCH_08_norm.csv")

#Rolling Window
forc_2=ugarchroll(spec1,data = ret1,n.ahead = 1, n.start = 4300,refit.every = 1, refit.window = "moving",window.size = 4300)
forc_22=cbind(forc_2@forecast$density$Mu,forc_2@forecast$density$Sigma)
write.csv(forc_22,"Forecast_Rolling_gjrGARCH_08_norm.csv")
#------------------------------------------------------------------------------------------
#Including exo variable
#with exogenous variables
data=read.csv("Dataset_2.csv", header=T)
#Separating Price series
price=data$Nifty
#Or
price2=data[,2]
n=length(price)

#Finding simple return
ret1=diff(price)/lag(price)
ret1=ret1[1:(n-1)]
ret2=diff(data$SP500)/lag(data$SP500)
ret2=ret2[1:(n-1)]
ret3=diff(data$Crude)/lag(data$Crude)
ret3=ret3[1:(n-1)]
ret4=diff(data$EURUSD)/lag(data$EURUSD)
ret4=ret4[1:(n-1)]

library(rugarch)
#Recursive window
spec1=ugarchspec(variance.model = list(model = "gjrGARCH", garchOrder = c(1, 1),external.regressors=NULL),mean.model = list(armaOrder = c(8, 0), include.mean = TRUE,external.regressors=as.matrix(cbind(ret2,ret3,ret4))),distribution.model = "norm")
forc_1=ugarchroll(spec1,data = ret1,n.ahead = 1, n.start = 4300,refit.every = 96, refit.window = "recursive")
forc_1
forc_11=cbind(forc_1@forecast$density$Mu,forc_1@forecast$density$Sigma)
write.csv(forc_11,"Recursive_gjrGARCH_ARMA_8_Exo.csv")

#Rolling Window
spec1=ugarchspec(variance.model = list(model = "gjrGARCH", garchOrder = c(1, 1),external.regressors=NULL),mean.model = list(armaOrder = c(8, 0), include.mean = TRUE,external.regressors=as.matrix(cbind(ret2,ret3,ret4))),distribution.model = "norm")
forc_2=ugarchroll(spec1,data = ret1,n.ahead = 1, n.start = 4300,refit.every = 1, refit.window = "moving",window.size = 4300)
forc_22=cbind(forc_2@forecast$density$Mu,forc_2@forecast$density$Sigma)
write.csv(forc_22,"Rolling_gjrGARCH_ARMA_80_Exo.csv")
