#Loading Libraries
library(prophet)
library(tidyverse)
library(dplyr)
library(lubridate)
library(reshape)
library(forecast)
library(magrittr)

#Reading and viewing the data
raw_data=read.csv(file.choose())
raw_data

#Transform the Date variables
ds <- raw_data$Date
ds <- as.character(ds)
ds <- ymd(ds)
regressor <- log(raw_data$Fib_supp_MT)

#Transform y variables
logy <- log(raw_data$Export_in_USD)
logy
raw_data <- mutate(raw_data, y = logy)
raw_data <- mutate(raw_data, ds = ds)
raw_data <- mutate(raw_data, regressor = regressor)

#Select the column to analyze
df <- data.frame(raw_data)
y=ts(df$y,
     start = c(2012, 1),
     frequency = 12)
df


#Initializing the Prophet Model
m=prophet(yearly.seasonality = TRUE,
          weekly.seasonality = FALSE,
          daily.seasonality = FALSE,
          seasonality.mode = 'multiplicative',
          seasonality.prior.scale = 12,
          changepoint.prior.scale = 0.01)
m <- add_regressor(m, 'regressor')

#Fitting the Prophet Model to the Training Data Set
m=fit.prophet(m, df)


#Forecast Future Values Using the Fitted Model
future <- make_future_dataframe(m, periods = 36, freq = 'month', include_history = TRUE)
future <- left_join(future, df %>% dplyr::select(ds, regressor), by = "ds") #the value of the additional regressor for the 36 months is not incorporated yet
future

#Conducting forecasts
prophet.fcst=predict(m, future)


#Plotting the forecasts
dyplot.prophet(m, prophet.fcst)
prophet_plot_components(m, prophet.fcst)

plot(m, prophet.fcst,
     type = "l",
     main = "Optimized Prophet Forecast for 3 years",
     xlabel = "Year",
     ylabel = "natural log of exports in USD",
     bg = "white")


#Determining accuracy of forecasts
predictions=tail(prophet.fcst$yhat, nrow(test))
predictions
accuracy(predictions, test$y)

