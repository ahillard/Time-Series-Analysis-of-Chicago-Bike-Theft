install.packages("TSA")
library(TSA)

gp <- read.csv("C:/Users/ajhkt6/Desktop/Time Series Project/gasprice.csv")
th <- read.csv("C:/Users/ajhkt6/Desktop/Time Series Project/theft.csv")

gp$date <- as.Date(gp$Week,"%m/%d/%Y")
gp$month <- months(gp$date)
gp$year <- format(gp$date, format="%y")
avegp <- aggregate(Gas.Price ~ month + year, gp, mean)


th$date <- as.Date(th$Date, "%m/%d/%Y")
th$month <- months(th$date)
th$year <- format(th$date, format="%y")
sumth <- aggregate(Count ~ month + year, th, sum)


/*Reorganize Month in Excel*/

write.table(sumth$month, "C:/Users/ajhkt6/Desktop/Time Series Project/thmonth.xls", row.names=FALSE)
write.table(sumth$year, "C:/Users/ajhkt6/Desktop/Time Series Project/thyear.xls", row.names=FALSE)
write.table(sumth$Count, "C:/Users/ajhkt6/Desktop/Time Series Project/thCount.xls", row.names=FALSE)

write.table(avegp$month, "C:/Users/ajhkt6/Desktop/Time Series Project/gpmonth.xls", row.names=FALSE)
write.table(avegp$year, "C:/Users/ajhkt6/Desktop/Time Series Project/gpyear.xls", row.names=FALSE)
write.table(avegp$Gas.Price, "C:/Users/ajhkt6/Desktop/Time Series Project/gpmean.xls", row.names=FALSE)

tbm <- read.csv("thsum.csv")[1:168,]
gbm <- read.csv("avegp.csv")[1:175,]

tbm$date <- as.Date(as.character(tbm$date),format="%m/%d/%Y")
gbm$date <- as.Date(as.character(gbm$date),format="%m/%d/%Y")

theft <- ts(tbm$count, frequency=12, start=c(2001,1))
gas <- ts(gbm$average, frequency=12, start=c(2000,6))

par(mfrow=c(2,2))

plot(theft, xlab='Time in Months', ylab='Number of Bike Thefts', main='Plot of Average Bike Theft Time Series')
out <- BoxCox.ar(theft)

##transform data
theft2 <- theft^.4
plot(theft2),xlab='Time in Months', ylab='Number of Bike Thefts', main='Plot of Transformed Bike Theft Time Series')

adf.test(theft2)
##data is stationary

acf(theft2, lag.max=48)
pacf(theft2, lag.max=48)
##ACF shows slow linear decay at lag 12. Therefore, take seasonal difference. 

theft3 <- diff(theft2, lag=12)
acf(theft3, lag.max=200)
pacf(theft3, lag.max=200)

##FIT ONE Seasonal 2,1,0

fit000210 <- arima(theft2, order=c(0,0,0), season=list(order=c(2,1,0), period=12))
acf(rstandard(fit000210), lag.max=200, main='ACF for ARIMA (0,0,0) x (2,1,0)')
pacf(rstandard(fit000210), lag.max=200, main='PACF for ARIMA (0,0,0) x (2,1,0)')

plot(armasubsets(residuals(fit000210),nar=8,nma=3))

fit100210 <- arima(theft2, order=c(1,0,0), season=list(order=c(2,1,0), period=12))
acf(rstandard(fit100210), lag.max=200, main='ACF for ARIMA(1,0,0) x (2,1,0)')
pacf(rstandard(fit100210), lag.max=200, main='PACF for ARIMA(1,0,0) x (2,1,0)')

fit100210

##FIT TWO Seasonal 1,1,1

fit000111 <- arima(theft2, order=c(0,0,0), season=list(order=c(1,1,1), period=12))
acf(rstandard(fit000111), lag.max=60, main='ACF for ARIMA (0,0,0) x (1,1,1)')
pacf(rstandard(fit000111), lag.max=60, main='ACF for ARIMA (0,0,0) x (1,1,1)')

plot(armasubsets(residuals(fit000111),nar=8,nma=3))

fit200111 <- arima(theft2, order=c(2,0,0), season=list(order=c(1,1,1), period=12))
acf(rstandard(fit200111), lag.max=200, main='ACF for ARIMA(2,0,0) x (1,1,1)')
pacf(rstandard(fit200111), lag.max=200, main='PACF for ARIMA(2,0,0) x (1,1,1)')

fit200111

##all coefficients are significant

##Got rid of seasonal operator AR because not significant

fit200011 <- arima(theft2, order=c(2,0,0), season=list(order=c(0,1,1), period=12))
acf(rstandard(fit200011), lag.max=200, main='ACF for ARIMA(2,0,0) x (0,1,1)')
pacf(rstandard(fit200011), lag.max=200, main='PACF for ARIMA(2,0,0) x (0,1,1)')

fit200011

##SELECT FIT ONE BECAUSE OF DAMP ACF SINE WAVE
##CHECK RESIDUALS FOR ARIMA (1,0,0)x(2,1,0)

hist(residuals(fit100210), xlab='Residuals')
plot(density(residuals(fit100210)))
qqnorm(residuals(fit100210))
qqline(residuals(fit100210))
shapiro.test(residuals(fit100210))
runs(residuals(fit100210))

##FORECAST WITH SIMPLE ARIMA MODEL

out1 <- plot(fit100210, n.ahead=24, pch=19)

##TRANSFER FUNCTION

theft.gas <- ts.intersect(theft, gas)
plot(theft.gas, main='Plot of Theft and Gas Time Series')


##PREWHITEN DATA AND CHECK CCF FOR LAG B

par(mfrow=c(1,1))

plot(gas)
ccf(theft, gas)
##Spurious Cross Correlation 

tg.dif = ts.intersect(diff(theft2, 12), diff(diff(gas, 12)))
prewhiten(as.vector(tg.dif[,1]), as.vector(tg.dif[,2]), ylab='CCF')
prewhiten(as.vector(tg.dif[,2]), as.vector(tg.dif[,1]), ylab='CCF')
##Lag of 3 on CCF. Let's do b equals 3 and (i) is 1. No exponential decay

##CREATE ARIMA TRANSFER FUNCTION MODEL B=3, w=1

theft2
gaslag3 <- gas[5:172]

transfer <- arimax(theft2, order=c(1,0,0), seasonal=list(order=c(2,1,0), period=12), xtransf=data.frame(gaslag3), transfer=list(c(0,1)))
transfer <- arimax(theft2, order=c(1,0,0), seasonal=list(order=c(2,1,0), period=12), xtransf=data.frame(gaslag3), transfer=list(c(1,0)))

##CHOOSE TRANSFER WITH b=3, r=0, s=1

##CHECK FIT OF TRANSFER FUNCTION (EVERYTHING CHECKS OUT FINE)

par(mfrow=c(2,2))

plot(residuals(transfer))
acf(residuals(transfer), main='ACF of Residuals')
hist(residuals(transfer), xlab='Residuals', main='Histogram of Residuals')
plot(density(residuals(transfer)), main='Estimated Density Curve of Residuals')
qqnorm(residuals(transfer), main='NPP of Residuals')
qqline(residuals(transfer))
shapiro.test(residuals(transfer))
runs(residuals(transfer))

##FORECAST WITH TRANSFER FUNCTION

##Previously tbm[1:168] now [1:179]
tbm.validate <- read.csv("thsum.csv")
theft.validate <- ts(tbm.validate$count, frequency=12, start=c(2001,1))
theft2.validate <- theft.validate^.4

##Previously gbm[1:175] now full length is [1:187]
gbm.validate <- read.csv("avegp.csv")
gas.validate <- gbm.validate[,4][5:183]


tf<-filter(gas.validate,filter=0.90586168,method='recursive',side=1)*(0.01459462)
install.packages("forecast")
library(forecast)
forecast.arima <- arima(theft2,order=c(1,0,0), season=list(order=c(2,1,0), period=12), xreg=tf[1:(length(tf)-11)])
forecast.arima





out1 <- plot(fit100210, n.ahead=24, pch=19)
out2 <- predict(forecast.arima,n.ahead = 11, newxreg=tf[169:length(tf)])

real <- theft2.validate[169:179]

sum(abs(out1$pred[1:11]-real))
sum(abs(out2$pred[1:11]-real))


## http://stats.stackexchange.com/questions/169564/arimax-prediction-using-forecast-package?rq=1















