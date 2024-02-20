## USE FORECAST LIBRARY.

library(forecast)
library(zoo)

## CREATE DATA FRAME. 

# Set working directory for locating files.
setwd("/Users/sahithipriya/Downloads")

# Create data frame.
wholesaletrade.data <- read.csv("motorvehicles.csv")

# See the first 6 records of the file.
head(wholesaletrade.data)
tail(wholesaletrade.data)

## USE ts() FUNCTION TO CREATE TIME SERIES DATA SET.
## USE stl() FUNCTION TO PLOT TIME SERIES COMPONENTS 
## USE Acf() FUNCTION TO IDENTIFY AUTOCORRELATION

# Function ts() takes three arguments: start, end, and freq.
# With monthly data, frequency (freq) of periods in a season (year) is 12. 
# With quarterly data, frequency in a season (year) is equal to 4.
# Arguments start and end are pairs: (season number, period number).
trade.ts <- ts(wholesaletrade.data$Trade, 
               start = c(1992, 1), end = c(2022, 12), freq = 12)
trade.ts

# Use stl() function to plot times series components of the original data. 
# The plot includes original data, trend, seasonal, and reminder 
# (level and noise component).
trade.stl <- stl(trade.ts, s.window = "periodic")
autoplot(trade.stl, main = "Monthly Wholesale Trade Time Series Components")

# Use Acf() function to identify autocorrelation and plot autocorrelation
# for different lags.
autocor <- Acf(trade.ts, lag.max = 12, 
               main = "Autocorrelation for Monthly Wholesale Trade for Motor Vehicles and
                              their parts")

# Display autocorrelation coefficients for various lags.
Lag <- round(autocor$lag, 0)
ACF <- round(autocor$acf, 3)
data.frame(Lag, ACF)

## CREATE TIME SERIES PARTITION.

# Define the numbers of months in the training and validation sets,
# nTrain and nValid, respectively.
# nvalid = 36 months  (January 2020 to December 2022).
# nTrain = 336 months, from January 1992 to December 2019.
nValid <- 36
nTrain <- length(trade.ts) - nValid
train.ts <- window(trade.ts, start = c(1992, 1), end = c(1992, nTrain))
valid.ts <- window(trade.ts, start = c(1992, nTrain + 1), 
                   end = c(1992, nTrain + nValid))
train.ts
valid.ts

# Create Holt-Winter's (HW) exponential smoothing for partitioned data.
# Use ets() function with model = "ZZZ", i.e., automatic selection of
# error, trend, and seasonality options.
# Use optimal alpha, beta, & gamma to fit HW over the training period.
hw.ZZZ <- ets(train.ts, model = "ZZZ")
hw.ZZZ 

# Use forecast() function to make predictions using this HW model with 
# validation period (nValid). 
# Show predictions in tabular format.
hw.ZZZ.pred <- forecast(hw.ZZZ, h = nValid, level = 0)
hw.ZZZ.pred


# Plot HW predictions for original data, automatic selection of the 
# model and optimal smoothing parameters.
plot(hw.ZZZ.pred$mean, 
     xlab = "Time", ylab = "Monthly Wholesale Trade for Motor Vehicles and their parts", 
     ylim = c(10000, 60000), 
     bty = "l", xlim = c(1992, 2024.25), xaxt = "n",
     main = "Holt-Winter's Model with Automatic Selection of Model Options", 
     lty = 5, col = "blue", lwd = 2) 
axis(1, at = seq(1992, 2024, 1), labels = format(seq(1992, 2024, 1)))
lines(hw.ZZZ.pred$fitted, col = "blue", lwd = 2)
lines(trade.ts)
legend(1992,60000, 
       legend = c("Monthly Wholesale Trade", 
                  "Holt-Winter's Model for Training Partition",
                  "Holt-Winter's Model for Validation Partition"), 
       col = c("black", "blue" , "blue"), 
       lty = c(1, 1, 2), lwd =c(1, 2, 2), bty = "n")

# Plot on the chart vertical lines and horizontal arrows
# describing training, validation, and future prediction intervals.
lines(c(2020, 2020), c(0, 60000))
lines(c(2022, 2022), c(0, 60000))
text(2008, 50000, "Training")
text(2020.5, 50000, "Validation")
text(2024.2, 50000, "Future")
arrows(1995, 40000, 2019.9, 40000, code = 3, length = 0.1,
       lwd = 1, angle = 30)
arrows(2020.1, 40000, 2021.9, 40000, code = 3, length = 0.1,
       lwd = 1, angle = 30)
arrows(2023.1, 40000, 2024.3, 40000, code = 3, length = 0.1,
       lwd = 1, angle = 30)

## FORECAST WITH HOLT-WINTER'S MODEL USING ENTIRE DATA SET INTO
## THE FUTURE FOR 12 PERIODS.

# Create Holt-Winter's (HW) exponential smoothing for full trade data set. 
# Use ets() function with model = "ZZZ", to identify the best HW option
# and optimal alpha, beta, & gamma to fit HW for the entire data period.
HW.ZZZ <- ets(trade.ts, model = "ZZZ")
HW.ZZZ 

# Use forecast() function to make predictions using this HW model for
# 12 month into the future.
HW.ZZZ.pred <- forecast(HW.ZZZ, h = 12 , level = 0)
HW.ZZZ.pred

# plot HW predictions for original data, optimal smoothing parameters.
plot(HW.ZZZ.pred$mean, 
     xlab = "Time", ylab = "Monthly Wholesale Trade for Motor Vehicles and their parts", ylim = c(10000, 60000), 
     bty = "l", xlim = c(1992, 2024.25), xaxt = "n",
     main = "Holt-Winter's Automated Model for Entire Data Set and Forecast for Future 12 Periods", 
     lty = 2, col = "blue", lwd = 2) 
axis(1, at = seq(1992, 2024, 1), labels = format(seq(1992, 2024, 1)))
lines(HW.ZZZ.pred$fitted, col = "blue", lwd = 2)
lines(trade.ts)
legend(1992,60000, 
       legend = c("Wholesale Trade", 
                  "Monthy Wholesale Trade for Entire Data Set",
                  "Holt-Winter's Model Forecast, Future 12 Periods"), 
       col = c("black", "blue" , "blue"), 
       lty = c(1, 1, 2), lwd =c(1, 2, 2), bty = "n")

# Plot on chart vertical lines and horizontal arrows describing
# entire data set and future prediction intervals.
lines(c(2023, 2023), c(0, 60000))
text(2013, 50000, "Data Set")
text(2024.2, 50000, "Future")


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

# Plot Auto arima for original data, automatic selection of the 
# model and optimal smoothing parameters.
plot(train.auto.arima.pred$mean, 
     xlab = "Time", ylab = "Monthly Wholesale Trade for Motor Vehicles and their parts", ylim = c(10000, 60000), 
     bty = "l", xlim = c(1992, 2024.25), xaxt = "n",
     main = "Auto ARIMA model for partition", 
     lty = 5, col = "blue", lwd = 2) 
axis(1, at = seq(1992, 2024, 1), labels = format(seq(1992, 2024, 1)))
lines(train.auto.arima.pred$fitted, col = "blue", lwd = 2)
lines(trade.ts)
legend(1992,60000, 
       legend = c("Wholesale Trade", 
                  "Auto ARIMA model for Training Partition",
                  "Auto ARIMA Model for Validation Partition"), 
       col = c("black", "blue" , "blue"), 
       lty = c(1, 1, 2), lwd =c(1, 2, 2), bty = "n")

# Plot on the chart vertical lines and horizontal arrows
# describing training, validation, and future prediction intervals.
lines(c(2020, 2020), c(0, 60000))
lines(c(2022, 2022), c(0, 60000))
text(2002, 50000, "Training")
text(2021, 50000, "Validation")
text(2024.2, 50000, "Future")

# Use auto.arima() function to fit ARIMA model for entire data set.
# use summary() to show auto ARIMA model and its parameters for entire data set.
auto.arima <- auto.arima(trade.ts)
summary(auto.arima)

# Apply forecast() function to make predictions for ts with 
# auto ARIMA model for the future 12 periods. 
auto.arima.pred <- forecast(auto.arima, h = 12, level = 0)
auto.arima.pred

# Use Acf() function to create autocorrelation chart of auto ARIMA 
# model residuals.
Acf(auto.arima$residuals, lag.max = 12, 
    main = "Autocorrelations of Auto ARIMA Model Residuals")

# Plot historical data, predictions for historical data, and Auto ARIMA 
# forecast for 12 future periods.
plot(trade.ts, 
     xlab = "Time", ylab = "Monthly Wholesale Trade for Motor Vehicles and their parts", 
     ylim = c(10000, 60000), xaxt = "n", 
     bty = "l", xlim = c(1992, 2024), lwd = 2,
     main = "Auto ARIMA Model for Entire Dataset") 
axis(1, at = seq(1992, 2024, 1), labels = format(seq(1992, 2024, 1)))
lines(auto.arima$fitted, col = "blue", lwd = 2)
lines(auto.arima.pred$mean, col = "blue", lty = 5, lwd = 2)
legend(1992,60000, legend = c("Wholesale Trade", 
                            "Auto ARIMA Forecast", 
                            "Auto ARIMA Forecast for 12 Future Periods"), 
       col = c("black", "blue" , "blue"), 
       lty = c(1, 1, 5), lwd =c(2, 2, 2), bty = "n")

# plot on the chart vertical lines and horizontal arrows
# describing training and future prediction intervals.
# lines(c(2004.25 - 3, 2004.25 - 3), c(0, 2600)
lines(c(2023, 2023), c(0, 60000))
text(2005, 50000, "Entire Data Set")
text(2024.5, 50000, "Future")


## FIT REGRESSION MODEL WITH LINEAR TREND AND SEASONALITY. 
## USE Acf() FUNCTION TO IDENTIFY AUTOCORRELATION FOR RESIDUALS.
## PLOT RESIDUALS.

# Use tslm() function to create linear trend and seasonal model.
train.lin.season <- tslm(train.ts ~ trend + season)

# See summary of linear trend equation and associated parameters.
summary(train.lin.season)

# Apply forecast() function to make predictions for ts with 
# linear trend and seasonal model in validation set.  
train.lin.season.pred <- forecast(train.lin.season, h = nValid, level = 0)
train.lin.season.pred

# Plot ts data, linear trend and seasonality data, and predictions 
# for validation period.
plot(train.lin.season.pred$mean, 
     xlab = "Time", ylab = "Monthly Wholesale Trade for Motor Vehicles and their parts", ylim = c(10000, 60000), 
     bty = "l", xlim = c(1992, 2024.25), xaxt = "n",
     main = "Regression with Linear Trend and Seasonality", 
     lwd = 2, lty = 2, col = "blue") 
axis(1, at = seq(1992, 2024, 1), labels = format(seq(1992, 2024, 1)))
lines(train.lin.season.pred$fitted, col = "blue", lwd = 2)
lines(train.ts, col = "black", lwd = 2, lty = 1)
lines(valid.ts, col = "black", lwd = 2, lty = 1)
legend(1992,60000, legend = c("Monthly Wholesale Trade Time Series", "Regression for Training Data",
                            "Forecast for Validation Data"), 
       col = c("black", "blue" , "blue"), 
       lty = c(1, 1, 5), lwd =c(2, 2, 2), bty = "n")

# Plot on the chart vertical lines and horizontal arrows
# describing training, validation, and future prediction intervals.
lines(c(2020, 2020), c(0, 60000))
lines(c(2022, 2022), c(0, 60000))
text(2002, 50000, "Training")
text(2020.5, 50000, "Validation")
text(2024.2, 50000, "Future")

# Use tslm() function to create linear trend and seasonal model.
trade.lin.season <- tslm(trade.ts ~ trend + season)

# See summary of linear trend equation and associated parameters.
summary(trade.lin.season)

# Plot residuals of the predictions with trend and seasonality.
plot(train.lin.season.pred$residuals, 
     xlab = "Time", ylab = "Residuals", 
     ylim = c(-20000, 20000), bty = "l",
     xlim = c(1992, 2024.25), xaxt = "n",
     main = "Regresssion Residuals for Training and Validation Data", 
     col = "black", lwd = 2) 
axis(1, at = seq(1992, 2024, 1), labels = format(seq(1992, 2024, 1)))
lines(valid.ts - train.lin.season.pred$mean, col = "orange", lwd = 2, lty = 1)

# Plot on the chart vertical lines and horizontal arrows
# describing training, validation, and future prediction intervals.
lines(c(2020, 2020), c(-20000, 20000))
lines(c(2022, 2022), c(-20000, 20000))
text(2002, 20000, "Training")
text(2021, 20000, "Validation")
text(2023.5, 20000, "Future")

# Use Acf() function to identify autocorrelation for the model residuals 
# (training and validation sets), and plot autocorrelation for different 
# lags (up to maximum of 12).

Acf(train.lin.season.pred$residuals, lag.max = 12, 
    main = "Autocorrelation for Monthly Wholesale Trade Training Residuals")
Acf(valid.ts - train.lin.season.pred$mean, lag.max = 12, 
    main = "Autocorrelation for Monthly Wholesale Trade Validation Residuals")

## USE Arima() FUNCTION TO CREATE AR(1) MODEL FOR TRAINING RESIDUALS.
## CREATE TWO-LEVEL MODEL WITH LINEAR TREND AND SEASONALITY MODEL 
## AND AR(1) RESIDUALS.
## PLOT DATA AND IDENTIFY ACCURACY MEASURES.

# Use Arima() function to fit AR(1) model for training residuals. The Arima model of 
# order = c(1,0,0) gives an AR(1) model.
# Use summary() to identify parameters of AR(1) model. 
res.ar1 <- Arima(train.lin.season$residuals, order = c(1,0,0))
summary(res.ar1)
res.ar1$fitted

# Use forecast() function to make prediction of residuals in validation set.
res.ar1.pred <- forecast(res.ar1, h = nValid, level = 0)
res.ar1.pred

# Develop a data frame to demonstrate the training AR model results 
# vs. original training series, training regression model, 
# and its residuals.  
train.df <- round(data.frame(train.ts, train.lin.season$fitted, 
                             train.lin.season$residuals, res.ar1$fitted, res.ar1$residuals), 3)
names(train.df) <- c("Monthly Wholesale Trade", "Regression", "Residuals",
                     "AR.Model", "AR.Model.Residuals")
train.df

# Plot residuals of the predictions for training data before AR(1).
plot(train.lin.season.pred$residuals, 
     xlab = "Time", ylab = "Residuals", 
     ylim = c(-20000, 20000), bty = "l",
     xlim = c(1992, 2024), xaxt = "n", 
     main = "Regresssion Residuals for Training Data before AR(1)", 
     col = "brown", lwd = 3) 
axis(1, at = seq(1992, 2024, 1), labels = format(seq(1992, 2024, 1)))

# Plot on the chart vertical lines and horizontal arrows
# describing training, validation, and future prediction intervals.
lines(c(2020, 2020), c(-20000, 20000))
text(2002, 20000, "Training")

# Plot residuals of the residuals for training data after AR(1).
plot(res.ar1$residuals, 
     xlab = "Time", ylab = "Residuals", 
     ylim = c(-20000, 20000), bty = "l",
     xlim = c(1992, 2024), xaxt = "n",
     main = "Residuals of Residuals for Training Data after AR(1)", 
     col = "brown", lwd = 3) 
axis(1, at = seq(1992, 2024, 1), labels = format(seq(1992, 2024, 1)))

# Plot on the chart vertical lines and horizontal arrows
# describing training, validation, and future prediction intervals.
lines(c(2020, 2020), c(-20000, 20000))
text(2002, 20000, "Training")

# Use Acf() function to identify autocorrelation for the training 
# residual of residuals and plot autocorrelation for different lags 
# (up to maximum of 12).
Acf(res.ar1$residuals, lag.max = 12, 
    main = "Autocorrelation for Trade Training Residuals of Residuals")

# Create two-level model's forecast with linear trend and seasonality 
# regression + AR(1) for residuals for validation period.

# Create data table with validation data, regression forecast
# for validation period, AR(1) residuals for validation, and 
# two level model results. 
valid.two.level.pred <- train.lin.season.pred$mean + res.ar1.pred$mean

valid.df <- round(data.frame(valid.ts, train.lin.season.pred$mean, 
                             res.ar1.pred$mean, valid.two.level.pred),3)
names(valid.df) <- c("Monthly Wholesale Trade", "Reg.Forecast", 
                     "AR(1)Forecast", "Combined.Forecast")
valid.df

# plot ts data, linear trend and seasonality data, and predictions 
# for validation period.
plot(valid.two.level.pred, 
     xlab = "Time", ylab = "Monthly Wholesale Trade for Motor Vehicles and their parts", ylim = c(10000, 60000), 
     bty = "l", xlim = c(1992, 2024.25), xaxt = "n",
     main = "Two-Level Forecast: Regression with Trend
             and Seasonlity + AR(1) for Residuals", lwd = 2,
     col = "blue", lty = 2) 
axis(1, at = seq(1992, 2024, 1), labels = format(seq(1992, 2024, 1)))
lines(train.lin.season.pred$fitted, col = "blue", lwd = 2)
lines(train.ts, col = "black", lwd = 2, lty = 1)
lines(valid.ts, col = "black", lwd = 2, lty = 1)
legend(1992,60000, legend = c("Monthly Wholesale Trade", "Regression for Training Data",
                            "Two Level Forecast for Validation Data"), 
       col = c("black", "blue" , "blue"), 
       lty = c(1, 1, 5), lwd =c(2, 2, 2), bty = "n")

# Plot on the chart vertical lines and horizontal arrows
# describing training, validation, and future prediction intervals.
lines(c(2020, 2020), c(0, 60000))
lines(c(2022, 2022), c(0, 60000))
text(2002, 50000, "Training")
text(2021, 50000, "Validation")
text(2024.2, 50000, "Future")

## FIT REGRESSION MODEL WITH LINEAR TREND AND SEASONALITY 
## FOR ENTIRE DATASET. FORECAST AND PLOT DATA, AND MEASURE ACCURACY.

# Use tslm() function to create linear trend and seasonality model.
lin.season <- tslm(trade.ts ~ trend + season)

# See summary of linear trend equation and associated parameters.
summary(lin.season)

# Apply forecast() function to make predictions with linear trend and seasonal 
# model into the future 12 months.  
lin.season.pred <- forecast(lin.season, h = 12, level = 0)
lin.season.pred

# Use Acf() function to identify autocorrelation for the model residuals 
# for entire data set, and plot autocorrelation for different 
# lags (up to maximum of 12).
Acf(lin.season.pred$residuals, lag.max = 12, 
    main = "Autocorrelation of Regression model's Residuals")

# Use Arima() function to fit AR(1) model for regression residuals.
# The ARIMA model order of order = c(1,0,0) gives an AR(1) model.
# Use forecast() function to make prediction of residuals into the future 12 months.
residual.ar1 <- Arima(lin.season$residuals, order = c(1,0,0))
residual.ar1.pred <- forecast(residual.ar1, h = 12, level = 0)

# Use summary() to identify parameters of AR(1) model.
summary(residual.ar1)

# Use Acf() function to identify autocorrelation for the residuals of residuals 
# and plot autocorrelation for different lags (up to maximum of 12).
Acf(residual.ar1$residuals, lag.max = 12, 
    main = "Autocorrelation for Residuals of Residuals for Entire Data Set")

# Identify forecast for the future 12 periods as sum of linear trend and 
# seasonal model and AR(1) model for residuals.
lin.season.ar1.pred <- lin.season.pred$mean + residual.ar1.pred$mean
lin.season.ar1.pred

# Create a data table with linear trend and seasonal forecast 
# for 12 future periods,
# AR(1) model for residuals for 12 future periods, and combined 
# two-level forecast for 12 future periods. 
table.df <- round(data.frame(lin.season.pred$mean, 
                             residual.ar1.pred$mean, lin.season.ar1.pred),3)
names(table.df) <- c("Reg.Forecast", "AR(1)Forecast","Combined.Forecast")
table.df

# Plot historical data, predictions for historical data, and forecast 
# for 12 future periods.
plot(trade.ts, 
     xlab = "Time", ylab = "Monthly Wholesale Trade for Motor Vehicles and their parts", 
     ylim = c(10000, 60000), xaxt = "n",
     bty = "l", xlim = c(1992, 2024.25), lwd = 2,
     main = "Two-Level Forecast: Regression with Trend and Seasonlity + AR(1)
     for Residuals") 
axis(1, at = seq(1992, 2024, 1), labels = format(seq(1992, 2024, 1)))
lines(lin.season$fitted + residual.ar1$fitted, col = "blue", lwd = 2)
lines(lin.season.ar1.pred, col = "blue", lty = 5, lwd = 2)
legend(1992,60000, legend = c("Monthly Wholsale Trade Series for Training and Valiadaton Periods", 
                            "Two-Level Forecast for Training and Valiadtion Periods", 
                            "Two-Level Forecast for 12 Future Periods"), 
       col = c("black", "blue" , "blue"), 
       lty = c(1, 1, 5), lwd =c(2, 2, 2), bty = "n")

# plot on the chart vertical lines and horizontal arrows
# describing training, validation, and future prediction intervals.
lines(c(2020, 2020), c(0, 60000))
lines(c(2023, 2023), c(0, 60000))
text(2002, 50000, "Training")
text(2021.5, 50000, "Validation")
text(2024.2, 50000, "Future")

# Use accuracy() function to identify common accuracy measures for:
# (1) Holt-Winter's Model with Automatic Selection of Model Options
#     and Parameters,,
# (2) Auto ARIMA Model,
# (3) two-level model (linear trend and seasonality model 
#     + AR(1) model for residuals) and,
# (4) Seasonal naive forecast
round(accuracy(hw.ZZZ.pred$fitted, trade.ts), 3)
round(accuracy(auto.arima.pred$fitted, trade.ts), 3)
round(accuracy(train.lin.season$fitted + residual.ar1$fitted, trade.ts), 3)
round(accuracy((naive(trade.ts))$fitted, trade.ts), 3)

