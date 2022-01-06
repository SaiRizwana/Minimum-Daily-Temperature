#MINIMUM DAILY TEMPERATURE

library(forecast)
library(tseries)
library(MLmetrics)
#Import data
data = read.csv("\\Users\\shake\\Desktop\\SAI\\Daily_min_temp_project.csv",fileEncoding="UTF-8-BOM")

#See the data
data
head(data)
tail(data)
colSums(is.na(data))

#Converting to time series
temp = ts(data[,2],start = c(1981,01,01),frequency = 365)
class(temp)
summary(temp)
plot(temp)
abline(reg = lm(temp~time(temp)),col = "red")
plot(decompose(temp))#there is seasonality in data

#Check if stationary
adf.test(temp)#stationary but there is seasonality in data
acf(temp) 
pacf(temp)
ggseasonplot(temp)


#1st order differencing
d.temp = diff(temp)
d.temp
plot(d.temp)
summary(d.temp)
#adf test
adf.test(temp)
adf.test(d.temp)
#auto arima for best fit
auto.arima(temp)#(5,0,0)
auto.arima(d.temp)#(4,0,1)
#arima models
arima(temp,order = c(1,0,1))
arima(temp,order = c(1,1,1))
arima(temp,order = c(1,2,1))
arima(temp,order = c(4,0,1))#least aic value i.e. 16779.72
#acf and pacf test for differenced data
acf(d.temp)
pacf(d.temp)

train <- ts(data[1:2556,2],start = c(1981,01,01) , frequency = 365)
test <- ts(data[2557:3650,2],start = c(1988,01,01) , frequency = 365)

d.train = diff(train)
plot(d.train)
adf.test(train)
adf.test(d.train)
acf(train)
pacf(train,lag=13)
acf(d.train)
pacf(d.train,lag=13)
auto.arima(train)
auto.arima(d.train)#aic is less


m1 = arima(train,order = c(4,0,1))#aic is less
m1
m2 = arima(train,order = c(5,0,0))
m2
m3 = arima(train,order = c(1,1,1))
m3

fit1 = predict(m1,1100)
fit1
fit2 = predict(m2,1100)
fit2
fit3 = predict(m3,1100)
fit3

MAPE(data$TEMP[2557:3650],fit1$pred)#0.280208
MAPE(data$TEMP[2557:3650],fit2$pred)#0.295643
MAPE(data$TEMP[2557:3650],fit3$pred)#0.2674671

fit4 = predict(m1,1500)
fit4
fcast = forecast(m1, h=30)
plot(fcast)

ts.plot(data$TEMP[2557:3650],fit$pred)

