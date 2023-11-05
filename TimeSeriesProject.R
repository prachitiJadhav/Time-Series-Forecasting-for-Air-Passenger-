## USE FORECAST LIBRARY.

library(forecast)

## CREATE DATA FRAME FOR S&P500 STOCK PRICES. 

# Set working directory for locating files.
setwd("C:/Users/STSC/Desktop/Time Series/Group Project")


# Create data frame.
airpassenger.data <- read.csv("AirPassengers.csv")

# See the first and last 6 records of the file for S&P500 data.
head(airpassenger.data )
tail(airpassenger.data )

#convert to timeseries data
airpassenger.ts <- ts(airpassenger.data$Passengers, 
                 start = c(1949, 1), end = c(1960, 12), freq = 12)

# Use plot() function to create plot For AirPassenger. 
plot(airpassenger.ts, 
     xlab = "Time", ylab = "Passengers (in 00's)", 
     ylim = c(100, 700), xaxt = 'n',
     main = "Forecasting Air Passengers")
# Establish x-axis scale interval for time in months.
axis(1, at = seq(1949, 1960, 1), labels = format(seq(1949, 1960, 1)))

## TEST PREDICTABILITY OF Air passengers.

# Use Arima() function to fit AR(1) model 
# The ARIMA model of order = c(1,0,0) gives an AR(1) model.
airpassenger.ar1<- Arima(airpassenger.ts, order = c(1,0,0))
summary(airpassenger.ar1)

# Apply z-test to test the null hypothesis that beta 
# coefficient of AR(1) is equal to 1.
ar1 <- 0.9646
s.e. <- 0.0214
null_mean <- 1
alpha <- 0.05
z.stat <- (ar1-null_mean)/s.e.
z.stat
p.value <- pnorm(z.stat)
p.value
if (p.value<alpha) {
  "Reject null hypothesis"
} else {
  "Accept null hypothesis"
}

# Create first difference of Airpassenger data using diff() function.
diff.close.price <- diff(airpassenger.ts, lag = 1)
diff.close.price

# Develop data frame with Airpassenger, Airpassenger lag 1, and first
# differenced data.
diff.df <- data.frame(airpassenger.ts, c("", round(airpassenger.ts[1:143],2)), 
                      c("", round(diff.close.price,2)))

names(diff.df) <- c("AirPassenger", "AirPassenger Lag-1", 
                    "First Difference")
diff.df 

# Use Acf() function to identify autocorrealtion for first differenced
# Airpassenger and plot autocorrelation for different lags 
# (up to maximum of 12).
Acf(diff.close.price, lag.max = 12, 
    main = "Autocorrelation for Differenced Close Airpassenger")

###------------------------------------------------------------------------------------------

## CREATE TIME SERIES PARTITION.

# Define the numbers of months in the training and validation sets,
# nTrain and nValid, respectively.
# Total number of period length(ridership.ts) = 159.
# nvalid = 60 months for the last 12 months (January 2014 to December 2018).
# nTrain = 276 months, from January 1991 to December 2013.
nValid <- 24 
nTrain <- length(airpassenger.ts) - nValid
train.ts <- window(airpassenger.ts, start = c(1949, 1), end = c(1949, nTrain))
valid.ts <- window(airpassenger.ts, start = c(1949, nTrain + 1), 
                   end = c(1949, nTrain + nValid))
###------------------------------------------------------------------------------------------


## FIT AUTO ARIMA MODEL.

# Use auto.arima() function to fit ARIMA model.
# Use summary() to show auto ARIMA model and its parameters.
train.auto.arima <- auto.arima(train.ts)
summary(train.auto.arima)

# Apply forecast() function to make predictions for ts with 
# auto ARIMA model in validation set.  
train.auto.arima.pred <- forecast(train.auto.arima, h = nValid, level = 0)
train.auto.arima.pred


# Using Acf() function, create autocorrelation chart of auto ARIMA 
# model residuals.
Acf(train.auto.arima$residuals, lag.max = 12, 
    main = "Autocorrelations of Auto ARIMA Model Residuals")

# Plot ts data, trend and seasonality data, and predictions for validation period.
plot(train.auto.arima.pred, 
     xlab = "Time", ylab = "Air passenger (in 00s)", 
     ylim = c(100, 700), xaxt = "n", 
     bty = "l", xlim = c(1949, 1962.25), 
     main = "Auto ARIMA Model", lwd = 2, flty = 5) 
axis(1, at = seq(1949, 1962, 1), labels = format(seq(1949, 1962, 1)))
lines(train.auto.arima.pred$fitted, col = "blue", lwd = 2)
lines(valid.ts, col = "black", lwd = 2, lty = 1)
legend(1949,600, legend = c("Air passenger Time Series", 
                             "Auto ARIMA Forecast for Training Period",
                             "Auto ARIMA Forecast for Validation Period"), 
       col = c("black", "blue" , "blue"), 
       lty = c(1, 1, 5), lwd =c(2, 2, 2), bty = "n")

# Plot on the chart vertical lines and horizontal arrows
# describing training, validation, and future prediction intervals.
lines(c(1959, 1959), c(0, 700))
lines(c(1960, 1960), c(0, 700))
text(1958,600, "Training")
text(1959.5, 600, "Validation")
text(1962.2, 600, "Future")
arrows(1949, 650, 1958.9, 650, code = 3, length = 0.1,
       lwd = 1, angle = 30)
arrows(1959.1, 650, 1959.9, 650, code = 3, length = 0.1,
       lwd = 1, angle = 30)
arrows(1960.1, 650, 1962.3, 650, code = 3, length = 0.1,
       lwd = 1, angle = 30)



# Use accuracy() function to identify common accuracy measures 
# for validation period forecast:
# Auto ARIMA model.



round(accuracy(train.auto.arima.pred$mean, valid.ts), 3)


# Use auto.arima() function to fit ARIMA model for entire data set.
# use summary() to show auto ARIMA model and its parameters for entire data set.
auto.arima <- auto.arima(airpassenger.ts)
summary(auto.arima)

# Apply forecast() function to make predictions for ts with 
# auto ARIMA model for the future 12 periods. 
auto.arima.pred <- forecast(auto.arima, h = 12, level = 0)
auto.arima.pred

# Use Acf() function to create autocorrelation chart of auto ARIMA 
# model residuals.
Acf(auto.arima$residuals, lag.max = 12, 
    main = "Autocorrelations of Auto ARIMA Model Residuals")

# Plot historical data, predictions for historical data, and seasonal 
# ARIMA forecast for 12 future periods.
plot(airpassenger.ts, 
     xlab = "Time", ylab = "Air passengers (in 00s)", 
     ylim = c(100, 700), xaxt = "n", 
     bty = "l", xlim = c(1949, 1962.25), 
     main = "Auto ARIMA Model", lwd = 2, flty = 5) 
axis(1, at = seq(1949, 1962, 1), labels = format(seq(1949, 1962, 1)))
lines(auto.arima$fitted, col = "blue", lwd = 2)
lines(auto.arima.pred$mean, col = "blue", lty = 5, lwd = 2)
legend(1949,600, legend = c("Air passenger Time Series", 
                             "Auto ARIMA Forecast for Training Period",
                             "Auto ARIMA Forecast for Validation Period"), 
       col = c("black", "blue" , "blue"), 
       lty = c(1, 1, 5), lwd =c(2, 2, 2), bty = "n")

# plot on the chart vertical lines and horizontal arrows
# describing training and future prediction intervals.
lines(c(1961, 1961), c(0, 700))
text(1958,600, "data set")
#text(1959.5, 600, "Validation")
text(1962.2, 600, "Future")
arrows(1961, 650, 1960.9, 650, code = 3, length = 0.1,
       lwd = 1, angle = 30)
#arrows(1959.1, 650, 1959.9, 650, code = 3, length = 0.1,
   #    lwd = 1, angle = 30)
arrows(1949.1, 650, 1962.3, 650, code = 3, length = 0.1,
       lwd = 1, angle = 30)



# MEASURE FORECAST ACCURACY FOR ENTIRE DATA SET.

# Use accuracy() function to identify common accuracy measures for:

# (1) Auto ARIMA Model,
# (2) Seasonal naive forecast, and
# (3) Naive forecast.

round(accuracy(auto.arima.pred$fitted, airpassenger.ts), 3)
round(accuracy((snaive(airpassenger.ts))$fitted, airpassenger.ts), 3)
round(accuracy((naive(airpassenger.ts))$fitted, airpassenger.ts), 3)

###------------------------------------------------------------------------------------------


#1a.	Create time series data set sales.ts in R using the ts() function
## USE ts() FUNCTION TO CREATE TIME SERIES DATA SET.
## USE stl() FUNCTION TO PLOT TIME SERIES COMPONENTS 
## USE Acf() FUNCTION TO IDENTIFY AUTOCORRELATION

# Function ts() takes three arguments: start, end, and freq.
# With monthly data, frequency (freq) of periods in a season (year) is 12. 
# Arguments start and end are pairs: (season number, period number).

airpassenger.ts <- ts(airpassenger.data$Passengers,
                      start = c(1949, 1), end = c(1960, 12), freq = 12)
airpassenger.ts

#1b. Apply the plot() function to create a data plot with the historical 
#data, provide it in your report, and explain what time series components can 
#be visualized in this plot.  

## Use plot() to plot time series data  
plot(airpassenger.ts, 
     xlab = "TIME", ylab = "PASSENGERS", ylim = c(100, 700), bty = "l",
     xaxt = "n", xlim = c(1949, 1960), main = "Forecasting Air Passengers", 
     lwd = 1, col="black") 
#Establish X- Axis scale interval for time in months
axis(1, at = seq(1949, 1960, 1), labels = format(seq(1949, 1960, 1)))
airpassenger.stl <- stl(airpassenger.ts, s.window = "periodic")
autoplot(airpassenger.stl, main = "PLOTS")
## CREATE DATA PARTITION.
## PLOT DATA PARTITION.
# Define the numbers of months in the training and validation sets,
# nTrain and nValid, respectively.
nValid <- 24
nTrain <- length(airpassenger.ts) - nValid
train.ts <- window(airpassenger.ts, start = c(1949, 1), end = c(1949, nTrain))
valid.ts <- window(airpassenger.ts, start = c(1949, nTrain + 1), 
                   end = c(1949, nTrain + nValid))
train.ts
valid.ts

## 2b. 
# FIT REGRESSION MODEL WITH (1) LINEAR TREND (2) QUADRATIC (POLYNOMIAL) TREND, 
## (3) SEASONALITY, (4) LINEAR TREND AND SEASONALITY, AND
## (5) QUADRATIC TREND AND SEASONALITY.
## IDENTIFY FORECAST FOR VALIDATION PERIOD FOR EACH MODEL.

## (1) LINEAR TREND MODEL.
# Use tslm() function to create linear trend model.
train.lin <- tslm(train.ts ~ trend)

# See summary of quadratic trend model and associated parameters.
summary(train.lin)

# Apply forecast() function to make predictions for ts data in
# validation set.  
train.lin.pred <- forecast(train.lin, h = nValid, level = 0)
train.lin.pred

## (2) QUADRATIC TREND MODEL.
# Use tslm() function to create quadratic (polynomial) trend model.
train.quad <- tslm(train.ts ~ trend + I(trend^2))

# See summary of quadratic trend model and associated parameters.
summary(train.quad)

# Apply forecast() function to make predictions for ts data in
# validation set.  
train.quad.pred <- forecast(train.quad, h = nValid, level = 0)
train.quad.pred

## (3) SEASONALITY MODEL.
# Use tslm() function to create seasonal model.
train.season <- tslm(train.ts ~ season)

# See summary of seasonal model and associated parameters.
summary(train.season)

# Apply forecast() function to make predictions for ts with 
# seasonality data in validation set.  
train.season.pred <- forecast(train.season, h = nValid, level = 0)
train.season.pred

## (4) LINEAR TREND AND SEASONALITY MODEL.
# Use tslm() function to create linear trend and seasonal model.
train.lin.trend.season <- tslm(train.ts ~ trend  + season)

# See summary of linear trend and seasonality model and associated parameters.
summary(train.lin.trend.season)

# Apply forecast() function to make predictions for ts with 
# trend and seasonality data in validation set.  
train.lin.trend.season.pred <- 
  forecast(train.lin.trend.season, h = nValid, level = 0)
train.lin.trend.season.pred


## (5) QUADRATIC TREND AND SEASONALITY MODEL.
# Use tslm() function to create quadratic trend and seasonal model.
train.quad.trend.season <- tslm(train.ts ~ trend + I(trend^2) + season)

# See summary of quadratic trend and seasonality model and associated parameters.
summary(train.quad.trend.season)

# Apply forecast() function to make predictions for ts with 
# trend and seasonality data in validation set.  
train.quad.trend.season.pred <- 
  forecast(train.quad.trend.season, h = nValid, level = 0)
train.quad.trend.season.pred

## 2c. 
# Use accuracy() function to identify common accuracy measures
# for the developed forecast in the validation period.

round(accuracy(train.lin.pred$mean, valid.ts),3)
round(accuracy(train.quad.pred$mean, valid.ts),3)
round(accuracy(train.season.pred$mean, valid.ts),3)
round(accuracy(train.lin.trend.season.pred$mean, valid.ts),3)
round(accuracy(train.quad.trend.season.pred$mean, valid.ts),3)


## 3a. 
# FIT REGRESSION MODEL WITH QUADRATIC TREND AND SEASONALITY, 
# WITH LINEAR TREND AND SEASONALITY FOR ENTIRE DATASET. 
# FORECASTDATA, AND MEASURE ACCURACY.

## (1) LINEAR TREND AND SEASONALITY MODEL.
# Use tslm() function to create linear trend and seasonality model.
## (2) LINEAR TREND MODEL.
# Use tslm() function to create linear trend model.
lin.season <- tslm(airpassenger.ts ~ trend + season)

# See summary of linear trend equation and associated parameters.
summary(lin.season)

# Apply forecast() function to make predictions for ts with linear trend and seasonality data in 12 future periods.
lin.season.pred <- forecast(lin.season, h = 12, level = 0)
lin.season.pred

# Use tslm() function to create regression model with linear trend 
lin <- tslm(airpassenger.ts ~ trend)

# See summary of linear trend equation and associated parameters.
summary(lin)

# Apply forecast() function to make predictions for ts with linear trend data in 12 future periods.
lin.pred <- forecast(lin, h = 12, level = 0)
lin.pred

# Use tslm() function to create regression model with quadratic trend and seasonality.
quad.season <- tslm(airpassenger.ts ~ trend + I(trend^2) + season)

# See summary of quadratic trend and seasonality equation and associated parameters.
summary(quad.season)

# Apply forecast() function to make predictions for ts with quadratic trend and seasonality data in 12 future periods.
quad.season.pred <- forecast(quad.season, h = 12, level = 0)
quad.season.pred

# Apply forecast() function to make predictions for ts with 
# trend and seasonality data in the future 12 months. 
quad.season <- forecast(quad.season, h = 12, level = 0)
quad.season

round(accuracy(lin.season.pred$fitted, airpassenger.ts),3)
round(accuracy(lin.pred$fitted, airpassenger.ts),3)
round(accuracy(quad.season.pred$fitted, airpassenger.ts),3)

round(accuracy((naive(airpassenger.ts))$fitted, airpassenger.ts), 3)
round(accuracy((snaive(airpassenger.ts))$fitted, airpassenger.ts), 3)

###------------------------------------------------------------------------------------------
## HOLT'S EXPONENTIAL SMOOTHING WITH PARTITIONED DATA.

# Create Holt-Winter's (HW) exponential smoothing for partitioned data.
# Use ets() function with model = "ZZZ"
hw.ZZZ <- ets(train.ts, model = "ZZZ")
hw.ZZZ

# Use forecast() function to make predictions using this HW model with 
# validation period (nValid). 
hw.ZZZ.pred <- forecast(hw.ZZZ, h = nValid, level = 0)
hw.ZZZ.pred

## ACCURACY MEASURES FOR HW MODEL WITH AUTOMATED SELECTION OF MODEL OPTIONS.
round(accuracy(hw.ZZZ.pred$mean, valid.ts), 3)

## FORECAST WITH HOLT-WINTER'S MODEL USING ENTIRE DATA SET INTO
## THE FUTURE FOR 12 PERIODS.

# Create Holt-Winter's (HW) exponential smoothing for full Airpassenger data set. 
# Use ets() function with model = "ZZZ"
HW.ZZZ <- ets(airpassenger.ts, model = "ZZZ")
HW.ZZZ

# Use forecast() function to make predictions using this HW model for
# 12 month into the future.
HW.ZZZ.pred <- forecast(HW.ZZZ, h = 12 , level = 0)
HW.ZZZ.pred

# Identify performance measures for HW forecast.
round(accuracy(HW.ZZZ.pred$fitted, airpassenger.ts), 3)
round(accuracy((naive(airpassenger.ts))$fitted, airpassenger.ts), 3)
round(accuracy((snaive(airpassenger.ts))$fitted, airpassenger.ts), 3)

###------------------------------------------------------------------------------------------

# Identify performance measures compare.
round(accuracy(quad.season.pred$fitted, airpassenger.ts), 3)
round(accuracy(auto.arima.pred$fitted, airpassenger.ts), 3)
round(accuracy(HW.ZZZ.pred$fitted, airpassenger.ts), 3)


