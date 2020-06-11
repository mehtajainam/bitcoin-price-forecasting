library(tseries)
library(forecast)
library(TSA)

setwd("C:/Users/Andrew/Documents/School/SP2020/31006 1 Time Series/Assignments/Course")
ts <- read.csv("price.csv")$Value
ts <- log(ts)

ho <- ts[4016:4045] # 2020/01
val <- ts[3985:4015] # 2019/12
train <- ts[3286:3984] # 2018-2019

periodogram(train)
(top10 <- order(periodogram(train)$spec, decreasing=T)[1:10]) # top 10 frequencies

# could automate this but it's ok

m1a <- forecast::arfima(train)
m1b <- HoltWinters(ts(m1a$residuals, frequency=top10[2])) # period=3

p1a <- forecast(m1a, 31)$mean
p1b <- predict(m1b, 31)

mean(abs(exp(as.vector(p1a) + as.vector(p1b)) - exp(val))) # MAE of ensemble
mean(abs(exp(as.vector(p1a)) - exp(val))) # MAE of ARFIMA

m2a <- forecast::arfima(train)
m2b <- HoltWinters(ts(m2a$residuals, frequency=top10[3])) # period=2

p2a <- forecast(m2a, 31)$mean
p2b <- predict(m2b, 31)

mean(abs(exp(as.vector(p2a) + as.vector(p2b)) - exp(val))) # MAE of ensemble
mean(abs(exp(as.vector(p2a)) - exp(val))) # MAE of ARFIMA

m3a <- forecast::arfima(train)
m3b <- HoltWinters(ts(m3a$residuals, frequency=top10[4])) # period=12

p3a <- forecast(m3a, 31)$mean
p3b <- predict(m3b, 31)

mean(abs(exp(as.vector(p3a) + as.vector(p3b)) - exp(val))) # MAE of ensemble
mean(abs(exp(as.vector(p3a)) - exp(val))) # MAE of ARFIMA

# period 4 validates best

m4a <- forecast::arfima(c(train, val))
m4b <- HoltWinters(ts(m4a$residuals, frequency=top10[4]))

p4a <- forecast(m4a, 30)$mean
p4b <- predict(m4b, 30)

mean(abs(exp(as.vector(p4a) + as.vector(p4b)) - exp(ho))) # MAE of ensemble
mean(abs(exp(as.vector(p4a)) - exp(ho))) # MAE of ARFIMA

#################################################################################################################

# intervention analysis using bitcoin crash

train2 <- ts[3102:3984] # 2017/07-2019

ts.plot(train2)

pulse <- 1 * (seq(train2) == which.max(train2))
step <- 1 * (seq(train2) >= which.max(train2))

(top10 <- order(periodogram(train2)$spec, decreasing=T)[1:10])
 
# low periodicity, ignore for now

auto.arima(train, trace=T)
auto.arima(train2, trace=T)

(mPulse <- arimax(train2, order=c(0,1,1), xtransf=data.frame(pulse, pulse), transfer=list(c(0,0), c(1,0)),
                 method="ML")) # using best arima from trace that doesn't produce NAs

ts.plot(fitted(mPulse))

# workaround to predict with arimax transfer function: use function as xreg in arima
tf_val <- (1 * (seq(1:(length(train2) + 31)) == which.max(train2))) * (mPulse$coef[2]) +
  filter(1 * (seq(1:(length(train2) + 31)) == which.max(train2)), filter=mPulse$coef[3],
         method='recursive', side=1) * (mPulse$coef[4])
ts.plot(tf_val)

mPulse_pred <- Arima(train2, order=c(0,1,1), xreg=tf_val[1:(length(tf_val) - 31)])
pPulse <- forecast(mPulse_pred, 31, xreg=tf_val[(length(tf_val) - 30):length(tf_val)])
mean(abs(as.vector(exp(pPulse$mean)) - exp(val)))

pulse_2 <- 1 * (seq(c(train2, val)) == which.max(train2))

(mPulse_2 <- arimax(c(train2, val), order=c(0,1,1), xtransf=data.frame(pulse_2, pulse_2), transfer=list(c(0,0), c(1,0)),
                  method="ML"))
tf_val_2 <- (1 * (seq(1:(length(pulse_2) + 30)) == which.max(train2))) * (mPulse_2$coef[2]) +
  filter(1 * (seq(1:(length(pulse_2) + 30)) == which.max(train2)), filter=mPulse_2$coef[3],
         method='recursive', side=1) * (mPulse_2$coef[4])
mPulse_pred_2 <- Arima(c(train2, val), order=c(0,1,1), xreg=tf_val_2[1:(length(tf_val_2) - 30)])
pPulse_2 <- forecast(mPulse_pred_2, 30, xreg=tf_val_2[(length(tf_val_2) - 29):length(tf_val_2)])
mean(abs(as.vector(exp(pPulse_2$mean)) - exp(ho)))
