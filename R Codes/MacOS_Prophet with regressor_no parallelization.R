# Load necessary libraries
library(prophet)
library(dplyr)
library(lubridate)
library(readr)


# Load and prepare data
raw_data <- read.csv("/Users/macbookpro/Documents/Codes and Data Sets/Comprehensive Data.csv")
raw_data$ds <- mdy(raw_data$Date)
raw_data$y <- log(raw_data$Export_in_USD)
raw_data$Fib_supp_MT_log <- log(raw_data$Fib_supp_MT)

# Check if there are still any NaNs in Fib_supp_MT_log after the transformation
if (any(is.nan(raw_data$Fib_supp_MT_log))) {
  stop("NaNs detected in Fib_supp_MT_log after initial transformation.")
}

# Define initial train length and forecast horizon
initial_train_length <- floor(nrow(raw_data) * 0.7)
horizon <- 12  # Forecast one year at a time

# Define ranges for parameters
seasonality_scale_seq <- seq(1, 10, by = 1.0)
changepoint_scale_seq <- seq(0.1, 1.0, by = 0.1)

# Create a data frame with all combinations of parameters
param_grid <- expand.grid(seasonality_prior_scale = seasonality_scale_seq, changepoint_prior_scale = changepoint_scale_seq)

# Initialize a list to store results
results_list <- list()

# Loop over the parameter grid
for (param_idx in 1:nrow(param_grid)) {
  seasonality_prior_scale <- param_grid$seasonality_prior_scale[param_idx]
  changepoint_prior_scale <- param_grid$changepoint_prior_scale[param_idx]
  
  # Initialize vectors to store metrics
  rmse_values <- vector("numeric")
  mae_values <- vector("numeric")
  mape_values <- vector("numeric")
  
  tryCatch({
    for (i in seq(initial_train_length, nrow(raw_data) - horizon, by = 1)) {
      training <- raw_data[1:i, ]
      test <- raw_data[(i + 1):(i + horizon), ]
      
      # Initialize and fit Prophet model with dynamic parameters
      m <- prophet(yearly.seasonality = TRUE, seasonality.mode = 'multiplicative', seasonality.prior.scale = seasonality_prior_scale, changepoint.prior.scale = changepoint_prior_scale)
      m <- add_regressor(m, 'Fib_supp_MT_log')
      m <- fit.prophet(m, training)
      
      # Manually create the future dataframe
      last_date <- max(training$ds)  # Get the last date from the training dataset
      future_dates <- seq(from = last_date + months(1), by = "months", length.out = horizon)  # Generate future dates correctly for monthly data
      future <- data.frame(ds = future_dates)  # Create the future dataframe
      future$Fib_supp_MT_log <- raw_data$Fib_supp_MT_log[(i + 1):(i + horizon)]
      
      # Forecasting
      forecast <- predict(m, future)
      
      # Extract the forecast subset and calculate accuracy
      forecast_subset <- forecast$yhat[(nrow(future) - horizon + 1):nrow(future)]
      accuracy_metrics <- forecast::accuracy(forecast_subset, test$y)
      
      # Collect metrics
      rmse_values <- c(rmse_values, accuracy_metrics[1, "RMSE"])
      mae_values <- c(mae_values, accuracy_metrics[1, "MAE"])
      mape_values <- c(mape_values, accuracy_metrics[1, "MAPE"])
    }
  }, error = function(e) {
    message("Error in model fitting or forecasting: ", e$message)
    next  # Skip to the next iteration
  })
  
  # Calculate mean of metrics across all iterations for current parameters
  result <- data.frame(
    SeasonalityPriorScale = seasonality_prior_scale,
    ChangepointPriorScale = changepoint_prior_scale,
    RMSE = mean(rmse_values, na.rm = TRUE),
    MAE = mean(mae_values, na.rm = TRUE),
    MAPE = mean(mape_values, na.rm = TRUE)
  )
  
  results_list <- append(results_list, list(result))
}

# Results dataframe
results_df <- do.call(rbind, results_list)

# Save results to CSV
results_file_path <- "Results with regr no par.csv"
write.csv(results_df, results_file_path, row.names = FALSE)
