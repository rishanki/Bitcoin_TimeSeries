#package_installation
#install library
install.packages('TSA')
install.packages('fma')
library(TSA)

#set up library direction
setwd("_")

#convert your data in a time series format
dataset<-read.table(file="bitcoin_price_GT_combined.csv",sep=",",row.names=1,header=TRUE)
data <- ts(dataset$Close, frequency = 365 ,start = c(2014,1,1))

#original_Dataset_analysis
plot.ts(data)
acf(data, 300)
pacf(data)
eacf(data)
BoxCox.ar(data, 10)
adf.test(data)
y<-armasubsets(y=(data),  nar=3, nma=6)
plot(y)

#stationarity by taking (diff of log data)-method1
df <-diff(log(data))
plot(df)
acf(df, 100)
pacf(df, 200)
eacf(df)
adf.test(df)
y<-armasubsets(y=df,  nar=6, nma=10)
plot(y)
arima(df, order = c(6,0,6))

#stationary by seasonal decomposition-method2
install.packages("forecast")
library(forecast)
decompose_beer <-decompose(data, "multiplicative")
plot(decompose_beer)
adjust_beer = data/decompose_beer$seasonal
plot(adjust_beer)
df <-diff(log(adjust_beer))
plot(df)
acf(df, 10)
pacf(df,10)
eacf(df)
adf.test(df)
y<-armasubsets(y=df,  nar=6, nma=10)
plot(y)

df <-diff(log(data))
df2<-log(data)

#Chosen Model(stationary model)
fit3 <- arima(df, order=c(3,0,2))
fit3
#Competitive Model(non-staionary model)
fit3 <- arima(df2, order=c(2,1,2))
fit3


#residual Study
tsdisplay(residuals(fit3), lag.max=45, main='(1,1,1) Model Residuals')
Box.test(rstandard(fit3),type="Ljung-Box", lag=10, fitdf=2)
hist(rstandard(fit3))
qqplot(rstandard(fit3))
qqline(rstandard(fit3))
shapiro.test(rstandard(fit3))

#Forecast the values
fc <-plot(fit3,n.ahead=200)
fc

#Back Transformation to the original scale
fc$predict<-exp(fc$pred)
fc$upper<-exp(fc$upi)
fc$lower<-exp(fc$lpi)
fc
plot(fc)




