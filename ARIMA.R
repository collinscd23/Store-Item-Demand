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

my_recipe <- recipe(formula= sales~., data=train) %>%
  step_date(date, features = c("dow", "month", "year")) %>%
  step_dummy(all_nominal_predictors()) %>%
  step_zv(all_predictors()) %>%
  step_normalize(all_numeric_predictors()) %>%
  step_date(date, features = "doy") %>%
  step_range(date_doy, min = 0, max = pi) %>%
  step_mutate(sinDOY = sin(date_doy), cosDOY = cos(date_doy))

