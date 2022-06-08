install.packages('TSA',type = 'binary')
library(TSA)

data <- read.csv('C:/Users/hp/Desktop/CPI.csv', header = F)
testing <- data[217:240,]
df <- data[73:216,]

# original time series plot
ts.plot(df$V2,ylab='CPI')

acf(df$V2, lag.max=50, plot=T, main='')
pacf(df$V2, lag.max=50, plot=T, main='')

# first order differencing
df_1 <- diff(df$V2)
ts.plot(df_1,ylab='diff(CPI)')

par(mfrow=c(1,1))
acf(df_1, lag.max=120, plot=T,main='')
pacf(df_1, lag.max=120, plot=T,main='')

# seasonal differencing s=12
df_12 <- diff(df$V2,12)
ts.plot(df_12,ylab='diff(CPI)',main='Seasonal Differencing S = 12')

par(mfrow=c(1,1))
acf(df_12, lag.max=120, plot=T,main='')
pacf(df_12, lag.max=120, plot=T,main='')

### ARIMA(1,1,1)
fit_1 <- arima(df$V2, order=c(1,1,1))
fit_1

# diagnostic
res <- fit_1$residuals
ts.plot(res,ylab='', main='Residuals')
std_res <- rstandard(fit_1)
ts.plot(std_res,ylab="", main="Standardized Residuals")
acf(res, lag.max=100, plot=T,main='')
pacf(res, lag.max=100, plot=T,main='')


### SARIMA(0,1,0)(3,0,0)12
fit_2 <- arima(df$V2, order=c(0,1,0),seasonal=list(order=c(3,0,0),period=12))
fit_2

# diagnostic
res <- fit_2$residuals
ts.plot(res,ylab="", main="Residuals")
std_res <- rstandard(fit_2)
ts.plot(std_res,ylab="", main="Standardized Residuals")
acf(res, lag.max=100, plot=T,main='')
pacf(res, lag.max=100, plot=T,main='')


### SARIMA(0,1,2)(3,0,0)12
fit_3 <- arima(df$V2, order=c(0,1,2),seasonal=list(order=c(3,0,0),period=12))
fit_3

# diagnostic
res <- fit_3$residuals
ts.plot(res,ylab="", main="Residuals")
std_res <- rstandard(fit_3)
ts.plot(std_res,ylab="", main="Standardized Residuals")
acf(res, lag.max=100, plot=T,main='')
pacf(res, lag.max=100, plot=T,main='')


## Ljung-Box Test
Box.test(res, lag=3, type="Ljung-Box")
names(Box.test(res, lag=3, type="Ljung-Box"))
B_text_p_value = c(0,0)
for(hh in 3:20){
  B_text_p_value[hh] = Box.test(res, lag=hh, type="Ljung-Box")$p.value
}
plot(3:20, B_text_p_value[3:20], type="p", 
     main="p values for Ljung-Box statistic", 
     xlab="lag", ylab="p value", ylim=c(0,1))
abline(h=0.05, lty=2, col=4)


# Prediction
x.pred = predict(fit_1, n.ahead=24)
x.pred
pred.U = x.pred$pred + 1.96*x.pred$se
pred.L = x.pred$pred - 1.96*x.pred$se

ts.plot(c(df$V2, rep(NA,24)),ylim = c(90,110),main = 'Prediction',ylab = '')
lines(144+(1:24), x.pred$pred, col=2)
lines(144+(1:24), pred.U, col=3, lty=2)
lines(144+(1:24), pred.L, col=3, lty=2)
points(144+(1:24), testing$V2,type = 'p')

# MSE
mse <- sqrt(sum((x.pred$pred - testing$V2)^2))
mse



