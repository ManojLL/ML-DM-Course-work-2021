library(tidyverse)
library(readxl)
library(lubridate)
library(zoo)
library(tidymodels)
library(neuralnet)
library(knitr)
library(MLmetrics)

# read data set from excel sheet
exchangeUSD_data <- read_excel("ExchangeUSD.xlsx")%>%
  janitor::clean_names() %>%
  mutate(date_in_ymd = ymd(yyyy_mm_dd)) %>%
  select(-1) %>%
  select(date_in_ymd,everything())

# print the data 
head(exchangeUSD_data)
#get bird view of the data set
summary(exchangeUSD_data)
head(exchangeUSD_data)
#get col name of data set
#plot(exchangeUSD_data$`USD/EUR`)
class(exchangeUSD_data$`date_in_ymd`)
class(exchangeUSD_data$`usd_eur`)

#make data frame using exchange_data column one and three
df <- data.frame(exchangeUSD_data[,1],exchangeUSD_data[,3])
names(df)[1]<- paste("date")
names(df)[2] <- paste("USD")
df

# split into train and testing data set
actual_rate <- df
previous_one_day <- lag(actual_rate$USD,k=1)
previous_two_day <- lag(actual_rate$USD,k=2)
previous_tree_day <- lag(actual_rate$USD,k=3)
new_data_vector <- cbind(actual_rate,previous_one_day,previous_two_day,previous_tree_day)
colnames(new_data_vector) <- c("date","ad","m1","m2","m3")
new_data_vector <- na.exclude(new_data_vector)
new_data_vector

normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x))) }

unnormalize <- function(x, min, max) {
  return( (max - min)*x + min ) }

normalize_data = new_data_vector%>%
  mutate(across(2:5,~normalize(.x)))
normalize_data

train_data <- normalize_data[1:400,]
test_data <- normalize_data[401:491,]
test_data
min_train <- min(new_data_vector[1:400,3])
max_train <- max(new_data_vector[1:400,3])
# Get the min and max of the original testing values
min_test <- min(new_data_vector[401:491,3])
max_test <- max(new_data_vector[401:491,3])



relevant_pred_stat <- function(true_value, predicted_value, model_kind,at) {
  rbind((tibble(truth = true_value,
                prediction = predicted_value) %>%
           metrics(truth,prediction) %>%
           mutate(type = model_kind)),(tibble(truth = true_value,
                                              prediction = predicted_value) %>%
                                         mape(truth,prediction) %>%
                                         mutate(type = model_kind)))
}

model_two_hidden_layers = function(hidden,sec_hidden) {
  nn_model_true = neuralnet(ad ~ m3, data=train_data, hidden=c(
    hidden,sec_hidden),act.fct = "tanh", linear.output=TRUE)
  train_results = compute(nn_model_true,test_data[,3:5])
  truthcol =  new_data_vector[401:491,2]
  predcol = unnormalize(train_results$net.result,min_train,max_train)[,1]
  relevant_pred_stat(truthcol,predcol,
                     "Two Hidden Layers") %>%
    mutate(hiddel_layers = paste0(hidden, " and ",sec_hidden),
           input_set = "m2") %>%
    filter(.metric != "rsq")
}
  

model_one_hidden_layer = function(hidden) {
  nn_model_true = neuralnet(ad ~ m3, data=train_data,hidden=hidden,act.fct = "tanh", linear.output=TRUE)
  train_results = compute(nn_model_true,test_data[,3:5])
  truthcol =  new_data_vector[401:491,2]
  predcol = unnormalize(train_results$net.result,min_train,max_train)[,1]
  relevant_pred_stat(truthcol,predcol,
                     "one Hidden Layers") %>%
    mutate(hiddel_layers = paste0(hidden),
           input_set = "m2") %>%
    filter(.metric != "rsq")
}

results_two_hidden_layers = bind_rows(
  lapply(1:10, function(n) {
    bind_rows(
      lapply(1:5, function(m) {
        model_two_hidden_layers(n,m)
      })
    )
  })) %>%
  janitor::clean_names()


result_one_hidden_layer = bind_rows(
  lapply(1:10, function(n) {
   model_one_hidden_layer(n)
  })) %>%
  janitor::clean_names()

set_a_models_two_layers = results_two_hidden_layers %>%
  select(-estimator) %>%
  pivot_wider(names_from = metric, values_from = estimate) %>%
  arrange(rmse)
kable(set_a_models_two_layers[1:10,])

set_a_models_one_layer = result_one_hidden_layer%>%
  select(-estimator) %>%
  pivot_wider(names_from = metric, values_from = estimate) %>%
arrange(rmse)
kable(set_a_models_one_layer[1:10,])

set_a_models = rbind(set_a_models_one_layer,set_a_models_two_layers)
view(set_a_models)
