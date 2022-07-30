# import library
library(tidyverse)
library(caret)
library(ggplot2)
library(mlbench)
library(rpart)

# set directory
getwd()
setwd("D:/dataset")

# load data
df_housing <- glimpse(read.csv("housing.csv"))
View(df_housing)

# check missing value
df_housing <- na.omit(df_housing)
mean(complete.cases(df_housing))
View(df_housing)

# cleansing
unique(df_housing$ocean_proximity)
## df_housing$ocean_proximity <- as.numeric(as.factor(df_housing$ocean_proximity))
df_housing$ocean_proximity <- with(df_housing, match(ocean_proximity, unique(ocean_proximity)))
df_housing$longitude <- NULL
df_housing$latitude <- NULL 
glimpse(df_housing)

# split data 
set.seed(42)
n <- nrow(df_housing)
id <- sample(1:n, size = 0.8*n)
train_df <- df_housing[id, ]
test_df <- df_housing[-id, ]

# train model
# lm_model
lm_model <- train(median_house_value ~ .,
                  data = train_df,
                  method = "lm")
lm_model

# rpart , knn
ctrl <- trainControl(method = "cv",
                     number = 10, #k
                     verboseIter = TRUE) 

rpart_model <- train( median_house_value ~ .,
                       data = train_df,
                       method = "rpart",
                       trControl = ctrl)
rpart_model

knn_model <- train( median_house_value ~ .,
                    data = train_df,
                    method = "knn",
                    metric = "RMSE",
                    trControl = ctrl)
knn_model

# scoring(prediction)
p_medv <- predict(lm_model, newdata = test_df)
test_df$p_medv <- p_medv
View(test_df)

# evaluate model (regression metrics)
# RMSE : root mean squared error
test_rmse <- sqrt(mean((test_df$median_house_value-p_medv)**2))
test_rmse

# plot predicted vs actual
ggplot(test_df, aes(x= test_df$p_medv, y= test_df$median_house_value)) +
  geom_point() +
  theme_minimal() +
  geom_smooth(method =  "lm", col = "red") +
  labs(x='Predicted Values', y='Actual Values', title='Median House Value Predicted vs. Actual Values')

# save model
saveRDS(lm_model, file = "lmmodel_housingUSA.RDS")




