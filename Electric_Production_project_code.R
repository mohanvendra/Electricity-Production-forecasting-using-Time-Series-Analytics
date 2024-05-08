library(forecast)
library(zoo)

setwd("C:/Users/STSC/Desktop/BAN 673/project")


# Importing the dataset and looking into the data
electric.data <- read.csv('Electric_Production.csv')
head(electric.data)

#using ts() function converting the csv to timeseries data

electric.data.ts <- ts(electric.data$Value,
                       start=c(1985,1), end=c(2017,12),freq=12)
head(electric.data.ts)
length(electric.data.ts)

##### historical Plot
plot(electric.data.ts, 
     xlab = "Time", ylab = "Production of electricity(kWh)", xaxt = "n",
     ylim = c(50, 150), bty = "l",
     xlim = c(1985, 2018), main = "Historical plot") 
axis(1, at = seq(1985, 2018, 1), labels = format(seq(1985, 2018, 1)))
legend(1985,150, legend = c("Production"), col = c("black"), lty = c(1),
       lwd = c(1), bty = "n")

# Treand, Seasonality and Level plot
electric.st1 <- stl(electric.data.ts, s.window = 'periodic')
autoplot(electric.st1, main = "plot graph")


############## predictability

#### approach 1 -Arima-AR(1)

electric.predictability.ar1<- Arima(electric.data.ts, order = c(1,0,0))
summary(electric.predictability.ar1)

ar1 <- 0.8734
s.e. <- 0.0245
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
###### approach 2 fpr predcitability

first.diff.data <- diff(electric.data.ts, lag = 1)
first.diff.data

Acf(first.diff.data, lag.max = 12, 
    main = "Autocorrelation for Electric Production")

########## autocorrelation plot
autocorr<- Acf(electric.data.ts,lag.max=12,
               main = "Autocorrelation for Sales data")

##########Partitioning the data into training and validation
# total data 1985 to 2017
# training 324 records from 1985 to 2011
# validation 72 records from 2012 to 2017
nValid <- 72
nTrain <- length(electric.data.ts) - nValid
train.ts <- window(electric.data.ts, start = c(1985, 1), end = c(1985, nTrain))
valid.ts <- window(electric.data.ts, start = c(1985, nTrain + 1), 
                   end = c(1985, nTrain + nValid))
head(train.ts)
head(valid.ts)

######### two level Forecasting  with trailing MA ##########
trend.seas <- tslm(train.ts ~ trend+I(trend^2)+season)
summary(trend.seas)
trend.seas.pred <- forecast(trend.seas, h = nValid, level = 0)
trend.seas.pred

#### deciding on window width ######
ma.trailing_3 <- rollmean(train.ts, k = 3, align = "right")
ma.trailing_8 <- rollmean(train.ts, k = 8, align = "right")
ma.trailing_12 <- rollmean(train.ts, k = 12, align = "right")

ma.trail_3.pred <- forecast(ma.trailing_3, h = nValid, level = 0)
ma.trail_3.pred
ma.trail_8.pred <- forecast(ma.trailing_8, h = nValid, level = 0)
ma.trail_8.pred
ma.trail_12.pred <- forecast(ma.trailing_12, h = nValid, level = 0)
ma.trail_12.pred

plot(electric.data.ts, 
     xlab = "Time", ylab = "Exports", xaxt = "n",
     ylim = c(50, 150), bty = "l",
     xlim = c(1985, 2022), main = "Historical plot") 
axis(1, at = seq(1985, 2022, 1), labels = format(seq(1985, 2022, 1)))
lines(ma.trailing_3, col = "blue", lwd = 2, lty = 2)
lines(ma.trail_3.pred$mean, col = "blue", lwd = 2, lty = 2)
lines(ma.trailing_8, col = "brown", lwd = 2, lty = 2)
lines(ma.trail_8.pred$mean, col = "brown", lwd = 2, lty = 2)
lines(ma.trailing_12, col = "green", lwd = 2, lty = 2)
lines(ma.trail_12.pred$mean, col = "green", lwd = 2, lty = 2)

legend(1985,140, legend = c("Production", 
                               "K=3",
                               "K=6", 
                               "K=12"), 
       col = c("black", "blue", "brown", "green"), 
       lty = c(1, 1, 1, 2), lwd =c(1, 2, 2, 2), bty = "n")
#legend(1990,26000, legend = c("Exports"), col = c("black"), lty = c(1),
#       lwd = c(1), bty = "n")

lines(c(2012, 2012), c(0, 3500))
lines(c(2018, 2018), c(0, 3500))
text(2002, 150, "Training")
text(2015, 150, "Validation")
text(2021, 150, "Future")

round(accuracy(ma.trail_3.pred$mean, valid.ts),3)
round(accuracy(ma.trail_8.pred$mean, valid.ts),3)
round(accuracy(ma.trail_12.pred$mean, valid.ts),3)
############# Two -level using window width = 3 ########
# using window width 3
trend.seas.res <- trend.seas$residuals
trend.seas.res

# Apply trailing MA for residuals with window width k = 3
# for training partition.
ma.trail.res <- rollmean(trend.seas.res, k = 3, align = "right")
ma.trail.res

# Create residuals forecast for validation period.
ma.trail.res.pred <- forecast(ma.trail.res, h = nValid, level = 0)
ma.trail.res.pred

fst.2level.train <- trend.seas$fitted.values + ma.trail.res


fst.2level <- trend.seas.pred$mean + ma.trail.res.pred$mean
fst.2level
########## plotting predictions 
plot(electric.data.ts, 
     xlab = "Time", ylab = "Production in kWh", xaxt = "n",
     ylim = c(50, 150), bty = "l",
     xlim = c(1985, 2019), main = "Historical data vs predictions for validation") 
axis(1, at = seq(1985, 2019, 1), labels = format(seq(1985, 2019, 1)))
lines(fst.2level.train, col = "blue", lwd = 2, lty = 2)
lines(fst.2level, col = "blue", lwd = 2, lty = 2)
legend(1985,140, legend = c("Production"), col = c("black"), lty = c(1),
       lwd = c(1), bty = "n")

lines(c(2012, 2012), c(0, 3500))
lines(c(2018, 2018), c(0, 3500))
text(2002, 150, "Training")
text(2015, 150, "Validation")
############ Two - level with quadratic trend
######### with only trend
trend <- tslm(train.ts ~ trend+I(trend^2))
summary(trend)
trend.pred <- forecast(trend, h = nValid, level = 0)
trend.pred
# using window width 3
trend.res <- trend$residuals
trend.res

# Apply trailing MA for residuals with window width k = 3
# for training partition.
ma.trail.trend.res <- rollmean(trend.res, k = 3, align = "right")
ma.trail.trend.res

# Create residuals forecast for validation period.
ma.trail.trend.res.pred <- forecast(ma.trail.trend.res, h = nValid, level = 0)
ma.trail.trend.res.pred

fst.2level.train <- trend$fitted.values + ma.trail.trend.res


fst.2level <- trend.pred$mean + ma.trail.trend.res.pred$mean
fst.2level

round(accuracy(trend.seas.pred$mean + ma.trail.res.pred$mean, valid.ts), 3)
round(accuracy(trend.pred$mean + ma.trail.trend.res.pred$mean, valid.ts), 3)
round(accuracy((snaive(valid.ts))$fitted, valid.ts), 3)

###### for entire data
tot.trend.seas <- tslm(electric.data.ts ~ trend + I(trend^2)  + season)
summary(tot.trend.seas)

# Create regression forecast for future 12 periods.
tot.trend.seas.pred <- forecast(tot.trend.seas, h = 12, level = 0)
tot.trend.seas.pred

# Identify and display regression residuals for entire data set.
tot.trend.seas.res <- tot.trend.seas$residuals
tot.trend.seas.res

# Use trailing MA to forecast residuals for entire data set.
tot.ma.trail.res <- rollmean(tot.trend.seas.res, k = 3, align = "right")
tot.ma.trail.res

# Create forecast for trailing MA residuals for future 12 periods.
tot.ma.trail.res.pred <- forecast(tot.ma.trail.res, h = 12, level = 0)
tot.ma.trail.res.pred

# Develop 2-level forecast for future 12 periods by combining 
# regression forecast and trailing MA for residuals for future
# 12 periods.
tot.fst.2level.train <- tot.trend.seas$fitted.values + tot.ma.trail.res
tot.fst.2level <- tot.trend.seas.pred$mean + tot.ma.trail.res.pred$mean
tot.fst.2level

######## accuracy measures #########
round(accuracy(tot.trend.seas.pred$fitted + tot.ma.trail.res.pred$fitted, electric.data.ts), 3)
round(accuracy((snaive(electric.data.ts))$fitted, electric.data.ts), 3)
round(accuracy((naive(electric.data.ts))$fitted, electric.data.ts), 3)

######### holt winters ##########
HW.ZZZ <- ets(train.ts, model = "ZZZ")
HW.ZZZ 

# Use forecast() function to make predictions using this HW model for
# 12 month into the future.
HW.ZZZ.pred <- forecast(HW.ZZZ, h = nValid , level = 0)
HW.ZZZ.pred

plot(electric.data.ts, 
     xlab = "Time", ylab = "Production", xaxt = "n",
     ylim = c(50, 150), bty = "l",
     xlim = c(1985, 2023), main = "Historical data vs predictions for validation data") 
axis(1, at = seq(1985, 2023, 1), labels = format(seq(1985, 2023, 1)))
lines(HW.ZZZ$fitted, col = "blue", lwd = 2, lty = 2)
lines(HW.ZZZ.pred$mean, col = "Green", lwd = 2, lty = 2)
legend(1985,140, legend = c("Production"), col = c("black"), lty = c(1),
       lwd = c(1), bty = "n")

lines(c(2012, 2012), c(0, 3500))
lines(c(2018, 2018), c(0, 3500))
text(2000, 140, "Training")
text(2015, 140, "Validation")

########### entire data

############### zzz for entire dataset
HW.ZZZ.entire <- ets(electric.data.ts, model = "ZZZ")
HW.ZZZ.entire 

# Use forecast() function to make predictions using this HW model for
# 12 month into the future.
HW.ZZZ.entire.pred <- forecast(HW.ZZZ.entire, h = 12 , level = 0)
HW.ZZZ.entire.pred

########## plotting future predictions

plot(electric.data.ts, 
     xlab = "Time", ylab = "Production", xaxt = "n",
     ylim = c(50, 150), bty = "l",
     xlim = c(1985, 2023), main = "Historical data with future predictions") 
axis(1, at = seq(1985, 2023, 1), labels = format(seq(1985, 2023, 1)))
lines(HW.ZZZ.entire$fitted, col = "blue", lwd = 2, lty = 2)
lines(HW.ZZZ.entire.pred$mean, col = "Green", lwd = 2, lty = 2)
legend(1985,150, legend = c("Production"), col = c("black"), lty = c(1),
       lwd = c(1), bty = "n")

lines(c(2012, 2012), c(0, 350))
lines(c(2018, 2018), c(0, 350))
text(2000, 140, "Training")
text(2015, 140, "Validation")
text(2020,140, "Future")

############ accuracy
round(accuracy((snaive(electric.data.ts))$fitted, electric.data.ts), 3)
round(accuracy((naive(electric.data.ts))$fitted, electric.data.ts), 3)
round(accuracy(HW.ZZZ.entire.pred$fitted, electric.data.ts), 3)

###### auto.arima()
train.auto.arima <- auto.arima(train.ts)
summary(train.auto.arima)

# Apply forecast() function to make predictions for ts with 
# auto ARIMA model in validation set.  
train.auto.arima.pred <- forecast(train.auto.arima, h = nValid, level = 0)
train.auto.arima.pred


######## plotting the predictions
plot(electric.data.ts, 
     xlab = "Time", ylab = "Production", xaxt = "n",
     ylim = c(50, 150), bty = "l",
     xlim = c(1985, 2023), main = "Historical data with future predictions") 
axis(1, at = seq(1985, 2023, 1), labels = format(seq(1985, 2023, 1)))

lines(train.auto.arima$fitted, col = "blue", lwd = 2, lty = 2)
lines(train.auto.arima.pred$mean, col = "blue", lwd = 2, lty = 2)
legend(1985,150, legend = c("Production"), col = c("black"), lty = c(1),
       lwd = c(1), bty = "n")

lines(c(2012, 2012), c(0, 350))
lines(c(2018, 2018), c(0, 350))
text(2000, 140, "Training")
text(2015, 140, "Validation")

##### entire data

entire.auto.arima <- auto.arima(electric.data.ts)
summary(entire.auto.arima)

# Apply forecast() function to make predictions for ts with 
# auto ARIMA model in validation set.  
entire.auto.arima.pred <- forecast(entire.auto.arima, h = 12, level = 0)
entire.auto.arima.pred

entire.300.arima <- Arima(electric.data.ts, order = c(3,0,0),seasonal = c(0,1,1)) 
summary(entire.300.arima)

entire.300.arima.pred <- forecast(entire.300.arima, h = 12, level = 0)
entire.300.arima.pred

####### plotting the predictions
plot(electric.data.ts, 
     xlab = "Time", ylab = "Production", xaxt = "n",
     ylim = c(50, 150), bty = "l",
     xlim = c(1985, 2023), main = "Historical data with future predictions") 
axis(1, at = seq(1985, 2023, 1), labels = format(seq(1985, 2023, 1)))
lines(entire.300.arima$fitted, col = "brown", lwd = 2, lty = 2)
lines(entire.300.arima.pred$mean, col = "brown", lwd = 2, lty = 2)
lines(entire.auto.arima$fitted, col = "blue", lwd = 2, lty = 2)
lines(entire.auto.arima.pred$mean, col = "blue", lwd = 2, lty = 2)
legend(1985,150, legend = c("Production", 
                               "ARIMA(2,1,1)",
                               "ARIMA(3,0,0)"), 
       col = c("black", "blue", "brown"), 
       lty = c(1, 1, 1), lwd =c(1, 2, 2), bty = "n")
lines(c(2012, 2012), c(0, 350))
lines(c(2018, 2018), c(0, 350))
text(2007, 140, "Training")
text(2015, 140, "Validation")
text(2020,140, "Future")
########## accuracy measures

round(accuracy(entire.auto.arima.pred$fitted, electric.data.ts), 3)
round(accuracy(entire.300.arima.pred$fitted, electric.data.ts), 3)
round(accuracy((snaive(electric.data.ts))$fitted, electric.data.ts), 3)
round(accuracy((naive(electric.data.ts))$fitted, electric.data.ts), 3)

