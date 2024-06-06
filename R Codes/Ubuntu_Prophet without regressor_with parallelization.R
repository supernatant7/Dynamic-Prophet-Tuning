# Load libraries
library(prophet)
library(dplyr)
library(lubridate)
library(readr)
library(foreach)
library(doParallel)


# Load and prepare data
raw_data <- read.csv("/home/nathanphetamine/Documents/Econometrics/Codes and Data Sets/Comprehensive Data.csv")

# Convert Date column to date format and handle potential errors
raw_data$ds <- as.Date(raw_data$Date, format = "%Y-%m-%d")

# Check for NA values in the date column
if (any(is.na(raw_data$ds))) {
  stop("NA values detected in the Date column after conversion.")
}

raw_data$y <- log(raw_data$Export_in_USD)

# Define initial train length and forecast horizon
initial_train_length <- floor(nrow(raw_data) * 0.7)
horizon <- 12  # Forecast one year at a time

# Define ranges for parameters
seasonality_scale_seq <- seq(1, 10, by = 1.0)
changepoint_scale_seq <- seq(0.1, 1.0, by = 0.1)

# Setup parallel processing
num_cores <- detectCores() - 1  # Leave one core free to ensure system stability
registerDoParallel(cores = num_cores)  # Register parallel backend to use multiple cores

# Nested loop over parameter ranges using parallel foreach
results_list <- foreach(seasonality_prior_scale = seasonality_scale_seq, .combine = rbind) %:%
  foreach(changepoint_prior_scale = changepoint_scale_seq, .combine = rbind) %dopar% {
    library(prophet)  # Load prophet within each parallel worker
    library(dplyr)
    library(lubridate)  # Ensure lubridate is loaded in the parallel worker
    
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
    data.frame(
      SeasonalityPriorScale = seasonality_prior_scale,
      ChangepointPriorScale = changepoint_prior_scale,
      RMSE = mean(rmse_values),
      MAE = mean(mae_values),
      MAPE = mean(mape_values)
    )
  }

# Results dataframe
results_df <- do.call(rbind, results_list)

# File path
results_file_path <- "Results.csv"

# Save results to CSV file, append if it exists
if (file.exists(results_file_path)) {
  write.csv(results_df, results_file_path, row.names = FALSE, append = TRUE)
} else {
  write.csv(results_df, results_file_path, row.names = FALSE)
}

# Stop parallel processing
stopImplicitCluster()
