# Load libraries
library(prophet)
library(dplyr)
library(lubridate)
library(readr)

# Load and prepare data
raw_data <- read.csv("C:\\Users\\admin\\Documents\\CodesandDataSets\\ComprehensiveData.csv")
raw_data$ds <- mdy(raw_data$Date)
raw_data$y <- log(raw_data$Export_in_USD)

# Define initial train length and forecast horizon
initial_train_length <- floor(nrow(raw_data) * 0.7)
horizon <- 12  # Forecast one year at a time

# Define ranges for parameters
seasonality_scale_seq <- seq(1, 10, by = 1.0)
changepoint_scale_seq <- seq(0.1, 1.0, by = 0.1)

# Initialize results list
results_list <- list()

# Nested loop over parameter ranges without parallel processing
for (seasonality_prior_scale in seasonality_scale_seq) {
  for (changepoint_prior_scale in changepoint_scale_seq) {
    
    rmse_values <- vector()
    mae_values <- vector()
    mape_values <- vector()
    
    for (i in seq(initial_train_length, nrow(raw_data) - horizon, by = 1)) {
      training <- raw_data[1:i, ]
      test <- raw_data[(i + 1):(i + horizon), ]
      
      # Initialize and fit Prophet model with dynamic parameters
      m <- prophet(
        yearly.seasonality = TRUE,
        seasonality.mode = 'multiplicative',
        seasonality.prior.scale = seasonality_prior_scale,
        changepoint.prior.scale = changepoint_prior_scale
      )
      m <- fit.prophet(m, training)
      
      # Manually create the future dataframe to ensure it only includes the horizon periods
      last_date <- max(training$ds)  # Get the last date from the training dataset
      future_dates <- seq(from = last_date + months(1), by = "months", length.out = horizon)  # Generate future dates correctly for monthly data
      future <- data.frame(ds = future_dates)  # Create the future dataframe
      
      # Conducting forecasts
      forecast <- predict(m, future)
      
      # Extract the forecast subset and calculate accuracy
      forecast_subset <- forecast$yhat[(nrow(future) - horizon + 1):nrow(future)]
      accuracy_metrics <- forecast::accuracy(forecast_subset, test$y)
      
      # Collect metrics
      rmse_values <- c(rmse_values, accuracy_metrics[1, "RMSE"])
      mae_values <- c(mae_values, accuracy_metrics[1, "MAE"])
      mape_values <- c(mape_values, accuracy_metrics[1, "MAPE"])
    }
    
    # Collect all metrics across all iterations for current parameters
    results_list[[length(results_list) + 1]] <- data.frame(
      SeasonalityPriorScale = seasonality_prior_scale,
      ChangepointPriorScale = changepoint_prior_scale,
      RMSE = mean(rmse_values),
      MAE = mean(mae_values),
      MAPE = mean(mape_values)
    )
  }
}

# Results dataframe
results_df <- do.call(rbind, results_list)

# File path
results_file_path <- "Test_1_cores_windows_no regr no par t3.csv"

# Save results to CSV file, append if it exists
if (file.exists(results_file_path)) {
  write.csv(results_df, results_file_path, row.names = FALSE, append = TRUE)
} else {
  write.csv(results_df, results_file_path, row.names = FALSE)
}

