library(tidyverse)
library(tidymodels)
library(vroom)
library(forecast)
library(patchwork)
library(modeltime)
library(timetk)

# Set working directory
setwd("~/Desktop/Stats348/Store-Item-Demand")

# Load data
train <- vroom("train.csv")
test <- vroom("test.csv")

# Filter for specific store-item combinations
storeItemTrain_1 <- train %>%
  filter(store == 3, item == 23)
storeItemTest_1 <- test %>%
  filter(store == 3, item == 23)

storeItemTrain_2 <- train %>%
  filter(store == 9, item == 47)
storeItemTest_2 <- test %>%
  filter(store == 9, item == 47)

# Function to prepare data, fit ARIMA, and generate forecasts
fit_forecast_arima <- function(train_data, test_data) {
  # Time split
  split <- initial_time_split(train_data, prop = 0.8)
  
  # Recipe
  arima_recipe <- recipe(sales ~ ., data = training(split)) %>%
    step_rm(item, store) %>%
    step_date(date, features = c("doy", "decimal")) %>%
    step_mutate(sinDOY = sin(date_doy), cosDOY = cos(date_doy)) %>%
    step_rm(date_doy)
  
  # ARIMA model
  arima_model <- arima_reg() %>%
    set_engine("auto_arima")
  
  # Workflow
  arima_wf <- workflow() %>%
    add_recipe(arima_recipe) %>%
    add_model(arima_model) %>%
    fit(data = training(split))
  
  # Cross-validation
  cv_results <- modeltime_calibrate(
    arima_wf,
    new_data = testing(split)
  )
  
  # Refit to the whole dataset
  full_fit <- cv_results %>%
    modeltime_refit(data = train_data)
  
  # Forecast on test data
  forecasts <- full_fit %>%
    modeltime_forecast(
      new_data = test_data,
      actual_data = train_data
    )
  
  list(cv_results = cv_results, forecasts = forecasts, split = split)
}

# Fit and forecast for both store-item combinations
results_1 <- fit_forecast_arima(storeItemTrain_1, storeItemTest_1)
results_2 <- fit_forecast_arima(storeItemTrain_2, storeItemTest_2)

# Cross-validation plot for Store 3, Item 23
cv_plot_1 <- results_1$cv_results %>%
  modeltime_forecast(
    new_data = testing(results_1$split),
    actual_data = training(results_1$split)
  ) %>%
  plot_modeltime_forecast(.interactive = FALSE) +
  ggtitle("Cross-Validation Predictions: Store 3, Item 23")

# Cross-validation plot for Store 9, Item 47
cv_plot_2 <- results_2$cv_results %>%
  modeltime_forecast(
    new_data = testing(results_2$split),
    actual_data = training(results_2$split)
  ) %>%
  plot_modeltime_forecast(.interactive = FALSE) +
  ggtitle("Cross-Validation Predictions: Store 9, Item 47")

# Forecast plot for Store 3, Item 23
forecast_plot_1 <- results_1$forecasts %>%
  plot_modeltime_forecast(.interactive = FALSE) +
  ggtitle("3-Month Forecast: Store 3, Item 23")

# Forecast plot for Store 9, Item 47
forecast_plot_2 <- results_2$forecasts %>%
  plot_modeltime_forecast(.interactive = FALSE) +
  ggtitle("3-Month Forecast: Store 9, Item 47")

# Combine plots in a 4-panel layout
final_plot <- (cv_plot_1 | forecast_plot_1) / (cv_plot_2 | forecast_plot_2)
print(final_plot)

