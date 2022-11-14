#ARMA Model
library(fBasics)
rm(list=ls())   #Clear the memory
setwd("C:\\Users\\kodte\\Dropbox\\PC\\Documents\\Study\\Term 5\\FA\\Mid Term Practice\\ARMA-eGARCH")
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
normalTest(ret1,method="jb")
# Normal Q-Q Plot
qqnorm(ret1)

#ARMA Model
#identify the order of the model
# EACF
library(TSA)
eacf_out=eacf(ret1,15,15)
#ARMA(5,8) & ARMA (8,8)
# Another way for auto selection of ARMA model (Finding optimal order of model)
library(forecast)
auto.arima(ret1, max.p=10,max.q=10,d=0,ic="aic") #ARIMA(0,0,1) with non-zero mean 
auto.arima(ret1, max.p=10,max.q=10,d=0,ic="bic") #ARIMA(0,0,0) with zero mean 

# Fit ARMA(8,8) model with intercept
out_1=arima(ret1,c(8,0,8),include.mean = T)
out_1 #CONVERGENCE PROBLEM SOL: MULTIPLY THE RETUN VALUE BY 100 AND THEN FIT
#The warning is because the normal optimization for the MLE has reached the default maximum number of iterations before convergence. You can increase that by adding optim.control = list(maxit = ?) at the end of your ARIMA fit. Something like arima(x.ts,order=c(p,d,q),list(maxit = 1000)). I think the default for ? is 500 but you can increase it.
out_1=arima(ret1*100,c(8,0,8),include.mean = T)
out_1
#We cannot fit this model since even with percentage return its giving non convergence problem
Box.test(out_1$residuals,lag=10,type="Ljung") #p-value = 1
#A P-value of 1 says that the two sets of data are identical and that no change has been observed, supporting the null hypothesis that there have been no change.

#ARMA(5,8)
out_2=arima(ret1,c(5,0,8),include.mean = T)
out_2 
Box.test(out_2$residuals,lag=10,type="Ljung") p-value = 1

#Evaluating performance of the forecasted results
# Generating return forecasts using rolling window estimations (ARMA(5,8))
l=length(ret1) #4397 
n=4300
forc_r=0   #This will store the forecast
#l-n+1 = 98
for (i in 1:(l-n+1)) {
  out_5=arima(ret1[i:(i+n-1)],order=c(5,0,8),include.mean = T)
  forc=predict(out_5,1)
  forc_r[i]=forc$pred[1]
}
write.csv(forc_r,"Ret_Forecast_ARMA_5-8.csv")

# To fix the AR(2 and 3) coefficient to zero:
out_3=arima(ret2,order=c(5,0,0),fixed=c(NA,NA,0,0,NA,NA))
Box.test(out_3$residuals,lag=10,type="Ljung")
predict(out_3,1)
#-------------------------------------------------------------------------------------
#ARIMA Model
#Implementing ARMA model continuous compounded series
out_4=arima(ret2,c(1,0,1),include.mean = F)
out_4

#Implementing ARIMA model on log(price) series
out_4=arima(log(price),c(1,1,1),include.mean = F)
out_4

#Both are having same result
#Coefficients:
#  ar1     ma1
# 0.0192  0.0198
# s.e.  0.3462  0.3492
#yt = 0.0192y(t-1) + 0.0198E(t-1)+E(t)
Box.test(out_1$residuals,lag=10,type="Ljung")
#-------------------------------------------------------------------------------------
library(rugarch)
spec1=ugarchspec(variance.model = list(model = "eGARCH", garchOrder = c(1, 1)),mean.model = list(armaOrder = c(5, 8), include.mean = TRUE),distribution.model = "norm")
garch_fit_1 = ugarchfit(spec = spec1, data = ret1)
garch_fit_1
std_res_1=as.numeric(residuals(garch_fit_1,standardize=TRUE))
Box.test(std_res_1,lag=10,type="Ljung") #p-value = 0.3114
library(FinTS)
ArchTest(std_res_1,lags = 10) #p-value = 0.5589
#eGARCH ARMA(5,8) is a good model fit as there is no arch effect left (no heterskedasticity)

#Generate forecast
#Recursive window (EGARCH Normal)
spec1=ugarchspec(variance.model = list(model = "eGARCH", garchOrder = c(1, 1)),mean.model = list(armaOrder = c(8, 0), include.mean = TRUE),distribution.model = "norm")
forc_1=ugarchroll(spec1,data = ret1,n.ahead = 1, n.start = 4300,refit.every = 96, refit.window = "recursive")
forc_1
forc_11=cbind(forc_1@forecast$density$Mu,forc_1@forecast$density$Sigma)
write.csv(forc_11,"Forcast_Recursive_eGARCH_58_norm.csv")

#Rolling Window
forc_2=ugarchroll(spec1,data = ret1,n.ahead = 1, n.start = 4300,refit.every = 1, refit.window = "moving",window.size = 4300)
forc_22=cbind(forc_2@forecast$density$Mu,forc_2@forecast$density$Sigma)
write.csv(forc_22,"Forecast_Rolling_gjrGARCH_58_norm.csv")
#---------------------------------------------------------------------------------------------------------------------------------------------------------
#Including exogenous variable
#with exogenous variables
data=read.csv("Dataset_2.csv", header=T)
#Separating Price series
price=data$Nifty
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
spec1=ugarchspec(variance.model = list(model = "eGARCH", garchOrder = c(1, 1),external.regressors=NULL),mean.model = list(armaOrder = c(5, 8), include.mean = TRUE,external.regressors=as.matrix(cbind(ret2,ret3,ret4))),distribution.model = "norm")
forc_1=ugarchroll(spec1,data = ret1,n.ahead = 1, n.start = 4300,refit.every = 96, refit.window = "recursive")
forc_1
forc_11=cbind(forc_1@forecast$density$Mu,forc_1@forecast$density$Sigma)
write.csv(forc_11,"Recursive_eGARCH_ARMA_58_Exo.csv")

#Rolling Window
spec1=ugarchspec(variance.model = list(model = "eGARCH", garchOrder = c(1, 1),external.regressors=NULL),mean.model = list(armaOrder = c(5, 8), include.mean = TRUE,external.regressors=as.matrix(cbind(ret2,ret3,ret4))),distribution.model = "norm")
forc_2=ugarchroll(spec1,data = ret1,n.ahead = 1, n.start = 4300,refit.every = 1, refit.window = "moving",window.size = 4300)
forc_22=cbind(forc_2@forecast$density$Mu,forc_2@forecast$density$Sigma)
write.csv(forc_22,"Rolling_gjrGARCH_ARMA_58_Exo.csv")
