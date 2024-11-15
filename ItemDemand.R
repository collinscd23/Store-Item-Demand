library(tidyverse)
library(tidymodels)
library(vroom)
library(forecast)
library(patchwork)

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

  
  
  
  
  
  
  
  
  
  
  
  