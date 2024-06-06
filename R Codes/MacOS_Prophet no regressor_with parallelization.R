# Load necessary libraries
library(prophet)
library(dplyr)
library(lubridate)
library(readr)
library(foreach)
library(doFuture)
library(doRNG)


# Register doFuture as the parallel backend
registerDoFuture()
future::plan(future::multisession, workers = 12)  # Use multisession instead of multicore

# Load and prepare data
raw_data <- read.csv("/Users/macbookpro/Documents/Codes and Data Sets/Comprehensive Data.csv")
raw_data$ds <- mdy(raw_data$Date)
raw_data$y <- log(raw_data$Export_in_USD)

# Check for NAs after transformation
if (any(is.na(raw_data$ds))) {
  stop("Error: 'ds' column contains NAs after date transformation")
}
if (any(is.na(raw_data$y))) {
  stop("Error: 'y' column contains NAs after log transformation")
}

# Define initial train length and forecast horizon
initial_train_length <- floor(nrow(raw_data) * 0.7)
horizon <- 12  # Forecast one year at a time

# Define ranges for parameters
seasonality_scale_seq <- seq(1, 10, by = 1.0)
changepoint_scale_seq <- seq(0.1, 1.0, by = 0.1)

# Create a data frame with all combinations of parameters
param_grid <- expand.grid(seasonality_prior_scale = seasonality_scale_seq, changepoint_prior_scale = changepoint_scale_seq)

# Parallel foreach loop over the parameter grid
results_list <- foreach(param_idx = 1:nrow(param_grid), .combine = rbind, .packages = c("prophet", "dplyr", "lubridate")) %dorng% {
  seasonality_prior_scale <- param_grid$seasonality_prior_scale[param_idx]
  changepoint_prior_scale <- param_grid$changepoint_prior_scale[param_idx]
  
  library(prophet)  # Ensure prophet is loaded in the parallel worker
  library(dplyr)
  library(lubridate)
  
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
      m <- fit.prophet(m, training)
      
      # Manually create the future dataframe
      last_date <- max(training$ds)
      future_dates <- seq(from = last_date + months(1), by = "months", length.out = horizon)
      future <- data.frame(ds = future_dates)
      
      # Forecasting
      forecast <- predict(m, future)
      
      # Extract the forecast subset and calculate accuracy
      forecast_subset <- forecast$yhat[(nrow(future) - horizon + 1):nrow(future)]
      
      # Check for NA values in forecast
      if (any(is.na(forecast_subset))) {
        warning("Forecast contains NA values")
        next
      }
      
      # Calculate accuracy metrics
      accuracy_metrics <- forecast::accuracy(forecast_subset, test$y)
      
      # Collect metrics
      rmse_values <- c(rmse_values, accuracy_metrics[1, "RMSE"])
      mae_values <- c(mae_values, accuracy_metrics[1, "MAE"])
      mape_values <- c(mape_values, accuracy_metrics[1, "MAPE"])
    }
  }, error = function(e) {
    message("Error in model fitting or forecasting: ", e$message)
    return(NULL)  # Return NULL to allow the foreach to continue
  })
  
  # Calculate mean of metrics across all iterations for current parameters
  data.frame(
    SeasonalityPriorScale = seasonality_prior_scale,
    ChangepointPriorScale = changepoint_prior_scale,
    RMSE = mean(rmse_values, na.rm = TRUE),
    MAE = mean(mae_values, na.rm = TRUE),
    MAPE = mean(mape_values, na.rm = TRUE)
  )
}

# Results dataframe
results_df <- do.call(rbind, results_list)

# Save results to CSV
results_file_path <- "Results.csv"
write.csv(results_df, results_file_path, row.names = FALSE)

# Reset future plan to sequential to clean up
future::plan("sequential")
