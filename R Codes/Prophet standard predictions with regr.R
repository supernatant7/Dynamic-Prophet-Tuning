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
df=data.frame(raw_data)
df

#Plotting the raw data
plot.ts(df$y)
ggplot(df, aes(x = ds, y = y)) + geom_line()
y=ts(df$y,
     start = c(2012, 1),
     frequency = 12)
ggseasonplot(x=y,
             main="Seasonality Graph",
             polar=TRUE)

#Decompose the data set to determine the trend, seasonality, and error
decomposition_multiplicative=decompose(x=y,
                                       type = "multiplicative")
plot(decomposition_multiplicative)


#Setting the training and test data set
training=df %>% 
  filter(ds <"2020-06-01")
test=df %>%
  filter(ds>="2020-06-01")


#Initializing the Prophet Model
m=prophet(yearly.seasonality = TRUE,
          weekly.seasonality = FALSE,
          daily.seasonality = FALSE,
          seasonality.mode = 'multiplicative')
m <- add_regressor(m, 'regressor')

#Fitting the Prophet Model to the Training Data Set
m=fit.prophet(m, training)

m$seasonality.mode
m$seasonality.prior.scale
m$changepoint.prior.scale


#Forecast Future Values Using the Fitted Model
future=make_future_dataframe(m, periods = nrow(test), freq = 'month', include_history = TRUE)
future <- left_join(future, df %>% dplyr::select(ds, regressor), by = "ds")


#Conducting forecasts
prophet.fcst=predict(m, future)


#Plotting the forecasts
dyplot.prophet(m, prophet.fcst)
prophet_plot_components(m, prophet.fcst)

plot(m, prophet.fcst)

#Determining accuracy of forecasts
predictions=tail(prophet.fcst$yhat, nrow(test))
predictions
accuracy(predictions, test$y)

