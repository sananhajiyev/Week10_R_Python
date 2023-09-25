# Description: crimes.csv dataset reflects reported incidents of crime that occurred in US. You are a 
# data scientist helping US to find important patterns about how to decrease and prevent crime rate.

# Your analysis must be able to address the following requests:

# 1. Find multicollinearity by applying VIF; 
# 2. Standardize features; 
# 3. Split data into train and test sets using seed=123; 
# 4. Build linear regression model. p value of variables should be max 0.05; 
# 5. Calculate RMSE and Adjusted R-squared; 
# 6. Check overfitting.


library(dplyr)
library(caret)
library(car)
library(lmtest)
library(Metrics)

df <- read.csv('crimes.csv')

# Reorder the dataframe
df <- df[, c(ncol(df), 1:(ncol(df)-1))]

X <- df[, -1]
y <- df[, 1]

# VIF for features
vif_result <- car::vif(lm(y ~ ., data = df))
vif_result

# Standardize the features
X_scaled <- scale(X)
X
X_scaled

set.seed(123)

split_index <- createDataPartition(y, p = 0.8, list = FALSE)

X_train <- X_scaled[split_index, ]
X_test <- X_scaled[-split_index, ]
y_train <- y[split_index]
y_test <- y[-split_index]

model <- lm(y_train ~ ., data = data.frame(y_train, X_train))

#-------------------------------------------------------------------------------
# Calculate RMSE
y_pred <- predict(model, newdata = data.frame(X_test))
rmse <- Metrics::rmse(y_test, y_pred)

# Calculate Adjusted R-squared
adjusted_r_squared <- 1 - (1 - summary(model)$r.squared) * ((length(y_test) - 1) / (length(y_test) - length(coefficients(model)) - 1))

# Calculate R-squared for the training set
r_squared_train <- summary(model)$r.squared

# Calculate R-squared for the test set
y_pred <- predict(model, newdata = data.frame(X_test))
r_squared_test <- 1 - (sum((y_test - y_pred)^2) / sum((y_test - mean(y_test))^2))
#-------------------------------------------------------------------------------

# Check for overfitting
if (r_squared_train > r_squared_test) {
  print("Warning: Potential overfitting detected.")
} else {
  print("No significant overfitting detected.")
}
