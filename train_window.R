library(tseries)
library(forecast)

ts <- read.csv("price.csv")

test <- ts[3985:4015,]
ts <- ts[1:3984,] # end @ 2020
ts <- ts[2555:3984,] # start @ 2016
rownames(ts) <- NULL # reset index

start_index <- 1:1097 # 2016-2019
end_index <- dim(ts)[1]

# check best pred using arima
index_test <- function(index) {
  model <- auto.arima(ts$Value[index:end_index])
  pred <- forecast(model, 31)
  print(sprintf("Model %s fit", index))
  return(list(mse=mean((pred$mean - test$Value)**2), coef=list(p=model$arma[1], d=model$arma[6], q=model$arma[2])))
}

out <- lapply(start_index, FUN="index_test")

mse <- sapply(out, FUN=function(z) z$mse)
plot(1:length(mse), mse, type="l")
abline(h=min(mse) + sd(mse), col="red")

ts$Date[which.min(mse)]

arfima_index_test <- function(index) {
  model <- forecast::arfima(ts$Value[index:end_index])
  pred <- forecast(model, 31)
  print(sprintf("Model %s fit", index))
  return(mean((pred$mean - test$Value)**2)) # no coef this time
}

mse2 <- sapply(start_index, FUN="arfima_index_test")
plot(1:length(mse2), mse2, type="l")
abline(h=min(mse2) + sd(mse2), col="red")

ts$Date[which.min(mse2)]

# load("start_date.RData")

par(mfrow=c(2,2))

plot(1:dim(ts)[1], ts$Value, type="l", main="Price")
abline(v=which.min(mse), col="blue")
abline(v=which.min(mse2), col="green")

plot(1:dim(ts)[1], c(mse, rep(NA, dim(ts)[1] - length(mse))), type="l", main="ARIMA MSE")
abline(h=min(mse) + sd(mse), col="red")
abline(v=which.min(mse), col="blue")

plot(1:dim(ts)[1], c(mse2, rep(NA, dim(ts)[1] - length(mse2))), type="l", main="ARFIMA MSE")
abline(h=min(mse2) + sd(mse2), col="red")
abline(v=which.min(mse2), col="green")

# potential choices: June-July 2017, December 2017-January 2018, 2019 only