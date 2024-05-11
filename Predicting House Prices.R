library(tidyverse)
library(rpart)
library(ggplot2)
library(Metrics)
#step 1: Import the dataset and perform EDA
housing_data <- read.csv("housing.csv")
head(housing_data)
str(housing_data)
# Box plot of bedrooms vs. price
ggplot(housing_data, aes(x = as.factor(bedrooms), y = price)) +
  geom_boxplot() +
  labs(x = "Bedrooms", y = "Price")
# Scatter plot of square footage vs. price
ggplot(housing_data, aes(x = sqft_living, y = price)) +
  geom_point() +
  labs(x = "Square Footage", y = "Price")
#Step 2: Preprocess the dataset
# Check for missing values
colSums(is.na(housing_data))
# Step 3: Split the dataset into training and testing sets

set.seed(123)  # Set seed for reproducibility

# Split the dataset into training and testing sets
train_indices <- sample(nrow(housing_data), nrow(housing_data) * 0.7)  # 70% for training
train_data <- housing_data[train_indices, ]
test_data <- housing_data[-train_indices, ]

# Verify the split
nrow(train_data)  # Number of rows in the training set
nrow(test_data)   # Number of rows in the testing set
# Step 4: Train a regression model

# Train a linear regression model
lm_model <- lm(price ~ ., data = train_data)

# Train a decision tree regression model
library(rpart)
dt_model <- rpart(price ~ ., data = train_data)
# Step 5: Evaluate the model's performance

# Make predictions on the testing set
lm_predictions <- predict(lm_model, newdata = test_data)
dt_predictions <- predict(dt_model, newdata = test_data)

# Evaluate the model's performance
lm_mae <- mae(test_data$price, lm_predictions)
dt_mae <- mae(test_data$price, dt_predictions)

lm_mse <- mse(test_data$price, lm_predictions)
dt_mse <- mse(test_data$price, dt_predictions)

lm_rmse <- rmse(test_data$price, lm_predictions)
dt_rmse <- rmse(test_data$price, dt_predictions)

lm_r2 <- summary(lm_model)$r.squared


# Print the evaluation metrics
cat("Linear Regression:\n")
cat("MAE:", lm_mae, "\n")
cat("MSE:", lm_mse, "\n")
cat("RMSE:", lm_rmse, "\n")
cat("R-squared:", lm_r2, "\n\n")
cat("Decision Tree Regression:\n")
cat("MAE:", dt_mae, "\n")
cat("MSE:", dt_mse, "\n")
cat("RMSE:", dt_rmse, "\n")
# Step 6: Use the trained model for prediction

# Suppose you have a new house with the following features
new_house <- data.frame(sqft_living = 1500, bedrooms = 3, bathrooms = 2,floors =2, yr_built = 2005)

# Predict the price of the new house using the linear regression model
predicted_price <- predict(lm_model, newdata = new_house)

# Print the predicted price
cat("Predicted Price:", predicted_price, "\n")