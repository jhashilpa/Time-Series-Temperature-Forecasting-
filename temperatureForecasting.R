library(forecast)
library(zoo)
library(MASS)
#install.packages("tidyverse")
library(tidyverse)
#install.packages('ggplot')
#install.packages("ggplot2",dependencies = TRUE)
#install.packages("utf8")
library(utf8)
library(ggplot2)
#install.packages("lubridata")
#install.packages("Kendall")
#install.packages('astsa')
#Stationary Test
library(tseries)# for stationary test
library(lubridate)
#remove.packages('prophet')

# Read the file Create data frame.
weather.data <- read.csv("DailyDelhiClimate.csv")

#Removing NA values

# See the first 6 records of the file.
head(weather.data)


#.............................................Data Pre-processing starts...........................................
#Remove NA values if any
weather.data<-na.omit(weather.data)
#Took out Month and Year from the date Column
weather.data$date=mdy(weather.data$date) 
weather.data$Month=month(weather.data$date)
weather.data$Year=year(weather.data$date)
weather.data$dat=day(weather.data$date)

#Aggregate the daily weather data by Month. Calculated the Mean temperature for each Month and Year combination
weatherPerMonth=aggregate(cbind(meantemp,humidity,wind_speed,meanpressure )~Month + Year,
                          weather.data,mean)
head(weatherPerMonth)

#Converted to corresponding ts object
temperature.ts <- ts(weatherPerMonth$meantemp, 
                     start = c(2013, 1), end = c(2017,4), freq = 12)
humidity.ts <- ts(weatherPerMonth$humidity, 
                  start = c(2013, 1), end = c(2017,4), freq = 12)
windSpeed.ts <- ts(weatherPerMonth$wind_speed, 
                   start = c(2013, 1), end = c(2017,4), freq = 12)
pressure.ts  <- ts(weatherPerMonth$meanpressure, 
                   start = c(2013, 1), end = c(2017,4), freq = 12)
#After Aggregation
temperature.ts

#.............................................Data Pre-processing ends...........................................

#.............................................Data Visualization  starts..........................................


# Plotting Relationship B/w Temperature and other variables
qplot(wind_speed,meantemp,data=weather.data)
qplot(humidity,meantemp,data=weather.data)
qplot(meanpressure,meantemp,data=weather.data)
cor(weather.data[,2:5])
# There is moderate correlation between Temperature and Humidity

#Decomposing the time series Components 
plot(temperature.ts)
temperature.stl <- stl(temperature.ts, s.window = "periodic")
autoplot(temperature.stl, main = "Weather Time Series Components")

#For further analysis
autocor <- Acf(temperature.ts, lag.max = 12, main = "Autocorrelation for Weather Time Series")


seasonplot(temperature.ts, year.labels = TRUE, year.labels.left=TRUE,labelgap = .2, col=1:80, pch=19, main = "Seasonal Variation in Temperature", xlab = "Month", ylab = "ML")

boxplot(temperature.ts ~ cycle(temperature.ts), xlab = "Month", ylab = "ML", main = "Monthly Variation in Temperature")
#ALl the findings has been shared in the report


#.............................................Data Visualization  ends..........................................

#.............................................Stationarity and Predictibility check  starts..........................................
#Check stationarity of the dataset

adf.test(temperature.ts)

#Predictability Test
#Method 1
test.ar1<- Arima(temperature.ts, order = c(1,0,0))
summary(test.ar1)

z.stat <- (0.8297 - 1)/0.0794
z.stat
p.value <- pnorm(z.stat)
p.value


#Method 2
diff.revenue <- diff(temperature.ts, lag = 1)
diff.revenue
Acf(diff.revenue, lag.max = 12, 
    main = "Autocorrelation for First Differenced  Temperature Values")


#..................................................Predictability Test ends................................



#.................................................Regression Model starts....................................
regression.model1 <- tslm(temperature.ts ~ trend+season)
summary(regression.model1)
regression.model1.pred <- forecast(regression.model1, h = 12, level = 0)
head(regression.model1.pred)
round(accuracy(regression.model1.pred$fitted, temperature.ts), 3)



regression.model2 <- tslm(temperature.ts ~ trend+I(trend^2)+season)
summary(regression.model2)
regression.model2.pred <- forecast(regression.model2, h = 12, level = 0)
head(regression.model2.pred)
round(accuracy(regression.model2.pred$fitted, temperature.ts), 3)
checkresiduals(regression.model2$residuals)


#External regressor Model
regression.external.model <- tslm(temperature.ts ~ trend+I(trend^2)+season + humidity.ts)
summary(regression.external.model)

#Fetch humidity values of corresponding months of last year to pass as a predictor
test.data=humidity.ts[time(humidity.ts) >= 2016.333 & time(humidity.ts) <=2017.25]

forecast_param <- data.frame(trend = c(53:64), humidity.ts = test.data)
forecast_param


regression.external.model.pred <- forecast(regression.external.model, newdata = forecast_param, level = 0)
head(regression.external.model)

round(accuracy(regression.external.model.pred$fitted, temperature.ts), 3)

#.......................................Two Level Forecasting Model(Regression+AR(3) starts...........................

trend.season <- tslm(temperature.ts ~ trend + I(trend^2) + season)
summary(trend.season)

trend.season.pred <- forecast(trend.season, h = 12, level = 0)
trend.season.pred

# Use AR(3) function to fit AR(3) model for regression residuals.
residual.ar3<- Arima(trend.season$residuals, order = c(3,0,0))
summary(residual.ar3)
residual.ar3pred <- forecast(residual.ar3, h = 12, level = 0)

#Combined Forecast 
trend.season.ar3.pred <- trend.season.pred$mean + residual.ar3.pred$mean
trend.season.ar3.pred

Acf(residual.ar3$residuals, lag.max = 12, 
    main = "Autocorrelation for Temperature Residuals of Residuals for Entire Data Set")
#Only Noise
checkresiduals(residual.ar3$residuals)


#Model with Two level forecasting
round(accuracy(trend.season$fitted + residual.ar3$fitted, temperature.ts), 3)

#This model is chosen finally amongst all  the Regression models
plot(trend.season.ar3.pred, 
     xlab = "Year", ylab = "Temperature", 
     ylim = c(5, 60), 
     main = "Two Level Forecasting model for the dataset", 
     xlim = c(2013, 2018), lty = 2, bty = "l", lwd = 2, col = "blue")
axis(1, at = seq(2013, 2018, 1), labels = format(seq(2013, 2018, 1)))
lines(trend.season.pred$fitted, lty = 1, lwd = 2, col = "blue")
lines(temperature.ts, col = "black", lty = 1, lwd = 2)


#...............................................Regression Models end............................................

#..............................................Arima Model begins............................................


#Using Auto Arima Model 

auto.arima.model <- forecast::auto.arima(temperature.ts,trace=TRUE,
                                         approximation = FALSE,stepwise = FALSE)
summary(auto.arima.model)
checkresiduals(auto.arima.model)
#Check the mean and variance of the residuals
var(auto.arima.model$residuals)
mean(auto.arima.model$residuals)

auto.arima.pred <- forecast(auto.arima.model, h = 12, level = 0)
auto.arima.pred

round(accuracy(auto.arima.pred$fitted, temperature.ts), 5)

# Manual Selection of PARAMETERS

#Create PACF and ACF plot
plot(temperature.ts)
par(mar = rep(2, 4))
tsdisplay(temperature.ts)
#Based on above plots and outputs corresponding  values of p,d,q  will be explored and tested

arima.model1<- Arima(temperature.ts, order = c(2,0,0),seasonal=c(2,0,0)) 
summary(arima.model1)
checkresiduals(arima.model1)



arima.model2<- Arima(temperature.ts, order = c(2,0,2),seasonal=c(2,1,2)) 
summary(arima.model2)
arima.model2.pred <- forecast(arima.model2, h = 12)
arima.model2.pred$mean
round(accuracy(arima.model2$fitted, temperature.ts), 3)
#This model is chosen finally amongst all  the ARIMA models
plot(arima.model2.pred$mean, 
     xlab = "Year", ylab = "Temperature", 
     ylim = c(5, 60), 
     main = "ARIMA(2,0,2)(2,1,2) model for the dataset", 
     xlim = c(2013, 2018), lty = 2, bty = "l", lwd = 2, col = "blue")
axis(1, at = seq(2013, 2018, 1), labels = format(seq(2013, 2018, 1)))
lines(arima.model2.pred$fitted, lty = 1, lwd = 2, col = "blue")
lines(temperature.ts, col = "black", lty = 1, lwd = 2)

checkresiduals(arima.model2)
var(arima.model2$residuals)
mean(arima.model2$residuals)

#Arima Model(2,0,2)(2,1,2) with external regressors
arima.model3<- Arima(temperature.ts, order = c(2,0,2),seasonal=c(2,1,2),xreg = ex.regressors) 
summary(arima.model3)
checkresiduals(arima.model3)
var(arima.model3$residuals)
mean(arima.model3$residuals)

#Auto Arima model with External Regressor Model
ex.regressors= cbind(humidity.ts)
auto.arima.exreg.model=auto.arima(temperature.ts,xreg = ex.regressors,trace=TRUE,
                                  approximation = FALSE,stepwise = FALSE)

summary(auto.arima.exreg.model)
checkresiduals(auto.arima.exreg.model)
mean(auto.arima.exreg.model$residuals)
var(auto.arima.exreg.model$residuals)


round(accuracy(auto.arima.exreg.model$fitted, temperature.ts), 3)

#...............................................Arima Model ends...............................................

#...............................................HOLT Winter's starts.............................................

HW.ZZZ <- ets(temperature.ts, model = "ZZZ")
HW.ZZZ 

# Use forecast() function to make predictions using this hw model for
# 12 month into the future.
HW.ZZZ.pred <- forecast(HW.ZZZ, h = 12 , level = 0)
HW.ZZZ.pred

round(accuracy(HW.ZZZ$fitted, temperature.ts), 3)
checkresiduals(HW.ZZZ$residuals)

Acf(HW.ZZZ.pred$residuals, lag.max = 12, 
    main = "Autocorrelation for Temeperature  Residuals")


#Two level Forecasting(HoltWinter(A,A,A)+AR(3))

HW.AAA <- ets(temperature.ts, model = "AAA")
HW.AAA

HW.AAA.pred <- forecast(HW.AAA, h = 12 , level = 0)
HW.AAA.pred

residual.ar3.model2 <- Arima(HW.AAA$residuals, order = c(3,0,0))
summary(residual.ar3.model2)
residual.ar3.model2.pred <- forecast(residual.ar3.model2, h = 12, level = 0)
checkresiduals(residual.ar3.model2$residuals)

#Combined Forecast 
hw.AAA.total.pred <- HW.AAA.pred$mean + residual.ar3.model2.pred$mean
hw.AAA.total.pred




#Model with Two level forecasting
round(accuracy(HW.AAA$fitted + residual.ar3.model2$fitted, temperature.ts), 3)

plot(hw.AAA.total.pred, 
     xlab = "Time", ylab = "Revenue (in Millions)", 
     ylim = c(5, 50), 
     main = "Two-Level Forecasting -ets(A,A,A)+AR(3)", 
     xlim = c(2013, 2018),flty = 2, bty = "l", lwd = 2, col = "blue")
axis(1, at = seq(2013, 2018, 1), labels = format(seq(2013, 2018, 1)))
lines(HW.AAA.pred$fitted, lty = 1, lwd = 2, col = "blue")
lines(temperature.ts, col = "black", lty = 1, lwd = 2)

#This model is chosen finally amongst all  the Holt WInter's models


#......................................................HOLT Winter ends here.................................................


#..................................................Monthly forecast to Daily Forecast............................

#Arima Model(2,0,2)(2,1,2) outperforms other Models in terms of accuracy metrics
#This is the arima.model2 created above in the codebase

final.model.pred=arima.model2.pred$mean

# To convert Monthly Temperatures into Daily temperatures
#1.We will maintain separate dataFrames for each Month and will store the weightage of each day(daily Mean temperature/Monthly mean temp)
#2. To get Daily Forecast We will multiply this weightage value of each day with the fprecasted Monthly Tempearture

# We will be using the most recent year to calculate the weightage of each day for each month.
#For ex. To calculate daily temperature value of May 2017 We will use the mean temperature of May 2016 and so on.
previousyear.meanTemp=temperature.ts[time(temperature.ts) >= 2016.333 & time(temperature.ts) <=2017.250]
previousyear.meanTemp

#....................Forecast daily temperatures for May 2017..........................................
#Get the daily Temperature series for May Month and year 2016
may.df <- subset(weather.data, Month == 5 & Year==2016)
#Grab specific columns( Mean temperature and other Date attributes)
may.df=may.df[,c(2,6,7,8)]
head(may.df)
#Calculate weightage of each day-(contribution towards the mean )
may.df <- mutate(may.df, perc = meantemp /mean(meantemp )) 
head(may.df)
#Get the forecasted Mean temperature for May 2017
may.pred=final.model.pred[time(final.model.pred) >= 2017.333 & time(final.model.pred) <2017.4]
print(may.pred)
#TO get Forecast for daily temperatures Multiply the forecasted mean temp with the corresponding weightages
may.df=mutate(may.df,forecastedMeanTemp=perc*may.pred)
may.df

#.............................Forecasting daily Temperatures for February Month and year 2018...................
feb.df <- subset(weather.data, Month == 2 & Year==2017)
#Grab specific columns( Mean temperature and other Date attributes)
feb.df=feb.df[,c(2,6,7,8)]
head(feb.df)
#Calculate weightage of each day-(contribution towards the mean )
feb.df <- mutate(feb.df, perc = meantemp /mean(meantemp )) 
head(feb.df)

#Get the forecasted Mean temperature for Feb 2018
feb.pred=final.model.pred[time(final.model.pred) >= 2018.083 & time(final.model.pred) <2018.085]
print(feb.pred)
#TO get Forecast for daily temperatures Multiply the forecasted mean temp with the corresponding weightages
feb.df=mutate(feb.df,forecastedMeanTemp=perc*feb.pred)
feb.df

 #Similar logic could be applied for different months and daily temperatures can be calcluated for each Month and Year
