library(tidyverse)
library(tidymodels)
library(vroom)
library(forecast)
library(patchwork)
library(modeltime)
library(timetk)


setwd("~/Desktop/Stats348/Store-Item-Demand")

train <- vroom("train.csv")
test <- vroom("test.csv")

storeItem1 <- train %>%
  filter(store==1, item==1)

storeItem2 <- train %>% 
  filter(store == 9, item==47 )

ts_plot1 <- storeItem1 %>%
  ggplot(aes(x = as.Date(date), y = sales)) +
  geom_line(color = "blue") +
  labs(title = "Store 1, Item 1 - Time Series", x = "Date", y = "Sales") +
  theme_minimal()
acf1_month1 <- forecast::ggAcf(storeItem1$sales, lag.max = 30) +
  ggtitle("ACF - 1 Month Lag") +
  theme_minimal()
acf2_years1 <- forecast::ggAcf(storeItem1$sales, lag.max = 2 * 365) +
  ggtitle("ACF - 2 Years Lag") +
  theme_minimal()

ts_plot2 <- storeItem2 %>%
  ggplot(aes(x = as.Date(date), y = sales)) +
  geom_line(color = "red") +
  labs(title = "Store 9, Item 47 - Time Series", x = "Date", y = "Sales") +
  theme_minimal()
acf1_month2 <- forecast::ggAcf(storeItem2$sales, lag.max = 30) +
  ggtitle("ACF - 1 Month Lag") +
  theme_minimal()
acf2_years2 <- forecast::ggAcf(storeItem2$sales, lag.max = 2 * 365) +
  ggtitle("ACF - 2 Years Lag") +
  theme_minimal()

panel_plot <- (ts_plot1 | acf1_month1 | acf2_years1) /
  (ts_plot2 | acf1_month2 | acf2_years2)

print(panel_plot)

  
storeItem <- train %>%
  filter(store==3, item==23)

my_recipe <- recipe(formula= sales~., data=train) %>%
  step_date(date, features = c("dow", "month", "year")) %>%
  step_dummy(all_nominal_predictors()) %>%
  step_zv(all_predictors()) %>%
  step_normalize(all_numeric_predictors()) %>%
  step_date(date, features = "doy") %>%
  step_range(date_doy, min = 0, max = pi) %>%
  step_mutate(sinDOY = sin(date_doy), cosDOY = cos(date_doy))
  
  
rf_model <- rand_forest(
  mode = "regression",
  trees = tune(),
  min_n = tune()
) %>%
  set_engine("ranger")

rf_workflow <- workflow() %>%
  add_recipe(my_recipe) %>%
  add_model(rf_model)

smape <- metric_set(smape)

cv_folds <- vfold_cv(storeItem, v = 5)

rf_grid <- grid_regular(
  trees(range = c(50, 500)),
  min_n(range = c(2, 10)),
  levels = 5
)

rf_results <- tune_grid(
  rf_workflow,
  resamples = cv_folds,
  grid = rf_grid,
  metrics = smape
)

best_rf <- rf_results %>%
  show_best(metric = "smape", n = 1) 

print(best_rf)

final_rf <- finalize_workflow(rf_workflow, best_rf)

final_cv_results <- fit_resamples(
  final_rf,
  resamples = cv_folds,
  metrics = smape
)

final_metrics <- collect_metrics(final_cv_results)
print(final_metrics)

  
  
  
  
  