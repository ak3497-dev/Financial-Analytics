rm(list=ls())   #Clear the memory
setwd("C:\\Users\\kodte\\Dropbox\\PC\\Documents\\Study\\Term 5\\FA\\Mid Term Practice")
data=read.csv("Dataset.csv", header=T)
#Separating Price series
price=data$Nifty
n=length(price)
n
#Finding simple return
ret1=diff(price)/lag(price)
write.csv(ret1,"Simple Return.csv")
ret1=ret1[1:(n-1)]
ret1
#Finding Continuously compounded return
ret2=diff(log(price))
write.csv(ret1,"Cont Compounded Return.csv")
# All Basic statistics
library(fBasics)
basic_sum=basicStats(ret1)
basic_sum
# Test of Normality 
#1: (Jarque-Bera Statistic)
normalTest(ret1,method="jb")
#p Value: < 2.2e-16: Nifty series is not normally distributed
#2: Normal Q-Q Plot
qqnorm(ret1)
# Plot is not following diagonal: Nifty series is not normally distributed
#-------------------------------------------------------------------------------------
#Step1: 
library(fUnitRoots)
adfTest(price,lags=10,type=c("ct")) #0.558
adfTest(price,lags=10,type=c("c")) #0.9629
adfTest(price,lags=10,type=c("nc")) #0.9876
adfTest(ret1,lags=10,type=c("ct")) # 0.01 (Smaller than printed value)
adfTest(ret1,lags=10,type=c("nc")) # 0.01 (Smaller than printed value)
adfTest(ret1,lags=10,type=c("c")) # 0.01 (Smaller than printed value)

#Interpretation: Price Series is insignificant i.e Null accepted and hence, series is a unit root and non stationary. We cannot forecast this series
#Interpretation: Return series is significant i.e not a unit root and hence stationary
#--------------------------------------------------------------------------------------------
#Step2:
# Autocorrelation function (upto 10 lags)
acf(ret1,10)
#Interpretation: From the graph, lag 1, 6, 7, 8 have crossed the confidence band. These are significant lags.
LCI = -1.96/sqrt(n)
LCI
UCI = 1.96/sqrt(n)
UCI
aa=acf(ret1,10)
aa
#lag 1, 6, 7, 8 have crossed the confidence band
#Joint test of autocorrelation
Box.test(ret1,lag=20,type="Box-Pierce") #p-value = 4.04e-08 test statistics value: 73.97
Box.test(ret1,lag=20,type="Ljung") #p-value = 3.729e-08 test statistics value: 74.18
#Interpretation: Results are significant and hence there exist significant joint autocorrelation i.e the series can be forecasted using all lags jointly upto (given) 20 lags
#---------------------------------------------------------------------------------------------
#Step 3: AR Model Fitting
#3.1: Identification of Model order
#3.1.1: Partial Autocorrelation Function
pacf(ret1,10)
a1 = pacf(ret1, 10)
a1
#From both, 1,6,7,8 are significant lags
#3.1.2: Information criteria
library(forecast)
auto.arima(ret1, max.p=10,max.q=0,d=0,ic="aic") #ARIMA(2,0,0) with non-zero mean 
auto.arima(ret1, max.p=10,max.q=0,d=0,ic="bic") #ARIMA(0,0,0) with zero mean

#3.2: Fitting the model
# Fit AR(0) model
out_1=arima(ret1,c(0,0,0),include.mean = T)
out_1
out_2=arima(ret1,c(2,0,0),include.mean = T)
out_2
out_3=arima(ret1,c(1,0,0),include.mean = T)
out_3
out_4=arima(ret1,c(6,0,0),include.mean = T)
out_4
out_5=arima(ret1,c(7,0,0),include.mean = T)
out_5
out_6=arima(ret1,c(8,0,0),include.mean = T)
out_6
#----------------------------------------------------------------------------------
#Step 4: Check good fit (Model diagnostics)
Box.test(out_1$residuals,lag=10,type="Ljung") #p-value = 1.122e-05
#output is not greater than 10% and even close to zero hence autocorrelation still exist in the residuals.
Box.test(out_2$residuals,lag=10,type="Ljung") #p-value = 8.663e-05
Box.test(out_3$residuals,lag=10,type="Ljung") #p-value = 6.707e-05
Box.test(out_4$residuals,lag=10,type="Ljung") #p-value = 0.09281
Box.test(out_5$residuals,lag=10,type="Ljung") #p-value = 0.7642
Box.test(out_6$residuals,lag=10,type="Ljung") #p-value = 0.9954

#Interpretation: AR(8) is a good fit model since it has highest p-value of 99.54%
#------------------------------------------------------------------------------------
#Step 5: Forecast the series
predict(out_6,1)
predict(out_6)
# Generating return forecasts using rolling window estimations (AR(8))
l=length(ret1)
n=4300
#l-n+1 = 98 4397-4300+1=98
forc_r=0   #This will store the forecast
for (i in 1:(l-n+1)) {
  out_7=arima(ret1[i:(i+n-1)],order=c(8,0,0),include.mean = T)
  forc=predict(out_7,1)
  forc_r[i]=forc$pred[1]
}
write.csv(forc_r,"Ret_Forecast_AR(8).csv")
#------------------------------------------------------------------------------------
#Step 6: Apply ARCH test on residuals of AR/MA/ARMA model. In this case AR(8)
library(FinTS)
out_8=arima(ret1,c(8,0,0),include.mean = T) 
#first get this model fitted and then take residual
arch_1=ArchTest(out_8$residuals,lags = 10)
#=ArchTest(out_6$residuals,lags = 10) this is also fine as our out_6 was AR(8)
arch_1 #p-value < 2.2e-16
#Interpretaion: The above significant result of ARCH test indicates that the series is heteroskedastic and hence, volatility can be modeled for this series.
#------------------------------------------------------------------------------------
#Step 7: Model volatility using GARCH models
library(rugarch)
spec1=ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(1, 1)),mean.model = list(armaOrder = c(8, 0), include.mean = TRUE),distribution.model = "norm")
garch_fit_1 = ugarchfit(spec = spec1, data = ret1)
garch_fit_1
#------------------------------------------------------------------------------------
#Step 8: Check good fit. Apply Lujung Box & ARCH test on standardized residuals
#1: Ljung Box Test
#2: ARCH Test
std_res_1=as.numeric(residuals(garch_fit_1,standardize=TRUE))
Box.test(std_res_1,lag=10,type="Ljung") #p-value = 0.5571
ArchTest(std_res_1,lags = 10) #p-value = 0.8854
#insignificant results in both the tests
#No autocorrelation & no heteroskedasticity in standardized residuals indicate a good model fit for GARCH model
#------------------------------------------------------------------------------------
#Step 9: Generate forecast of returns & volatility
#9.1: Recursive window
spec1=ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(1, 1)),mean.model = list(armaOrder = c(8, 0), include.mean = TRUE),distribution.model = "norm")
forc_1=ugarchroll(spec1,data = ret1,n.ahead = 1, n.start = 4300,refit.every = 96, refit.window = "recursive")
forc_1
forc_11=cbind(forc_1@forecast$density$Mu,forc_1@forecast$density$Sigma)
write.csv(forc_11,"Recursive_GARCH_AR(8).csv")

#9.2: Rolling Window
forc_2=ugarchroll(spec1,data = ret1,n.ahead = 1, n.start = 4300,refit.every = 1, refit.window = "moving",window.size = 4300)
forc_22=cbind(forc_2@forecast$density$Mu,forc_2@forecast$density$Sigma)
write.csv(forc_22,"Rolling_GARCH_AR(8).csv")

#9.2: Other way of rolling window
l=length(ret1)
l1=4300
pred1=0
pred2=0
for(i in 1:(l-l1+1)){
  spec2=ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(1, 1)),mean.model = list(armaOrder = c(8, 0), include.mean = TRUE),distribution.model = "norm")
  garch_fit_3 = ugarchfit(spec = spec2, data = ret1[i:(l1+i-1)])
  forc_2 = ugarchforecast(garch_fit_3, n.ahead=1)
  pred1[i]=forc_2@forecast$seriesFor[[1]]
  pred2[i]=forc_2@forecast$sigmaFor[[1]]
}
pred_all=cbind(pred1,pred2)
write.csv(pred_all,"Roll_forecast_AR(8).csv")
#-----------------------------------------------------------------------------------------------