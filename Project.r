Data <- read.csv(file = 'Data.csv')

library(ggfortify)
library(ggplot2)
library(ggfortify)
library(forecast)

#ts_data is the time series of original closing values data
ts_data = ts(Data$Value,start = 2019, frequency = 365)

#task b. plot of ts_data, acf and pcf plots.Data appears to be non stationary
plot(ts_data)
acf(ts_data)
pacf(ts_data)
#first differnce is taken and acf pcf functions performed again
first_difference = diff(ts_data)
acf(first_difference)
pacf(first_difference)

#residuals checked to see if first_difference is stationary 
checkresiduals(ts_data)
checkresiduals(first_difference)

#task e. exponential smoothing used for original data and first_difference
ses(ts_data)
round(accuracy(ses(ts_data)), 2)
ses(first_difference)
round(accuracy(ses(first_difference)), 2)

#summaries of exponential smoothing models including the alpha used
summary(ses(first_difference))
summary(ses(ts_data))

#task f. fitted into the model. 
arima_model <- arima(first_difference, order = c(2, 0, 0), include.mean = TRUE)
summary(arima_model)

#task h. The residuals appear constant
residuals <- residuals(arima_model)
fitted.values <- fitted.values(arima_model)
plot(fitted.values, residuals, main = "Residuals vs. Fitted Values",
     xlab = "Fitted Values", ylab = "Residuals")
abline(h = 0, col = "red")
plot(residuals)
residuals <- residuals(arima_model)
plot(arima_model$residuals)
acf(arima_model$residuals)

#box cox transformation, lambda is determined.
lambda = BoxCox.lambda(first_difference)
print(lambda)

#box cox transformation performed, and model created
Data_boxcox = BoxCox(first_difference, lambda)
model_boxcox <- arima(Data_boxcox, order = c(2, 0, 0))
plot(model_boxcox$residuals)
plot(arima_model$residuals)

#task j. Forecasts created for both the boxcox transformed data and non transformed data
forecast(arima_model,h=754)
forecast1 = forecast(arima_model, h=754)
forecast1
plot(forecast1)
forecast2 = forecast(model_boxcox, h = 754)
plot(forecast2)