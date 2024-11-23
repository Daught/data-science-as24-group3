##########################################################
# Data Science - AS24
# DS-Assignment: Part1
# Part 1: Logistic Regression
# Magdalena Hardegger magdalena.hardegger@students.fhnw.ch
# Firat Turan firat.turan@students.fhnw.ch
# Sascha Frossard sascha.frossard@students.fhnw.ch
# Sebastian Fernandez sebastian.fernandez@students.fhnw.ch
##########################################################
# Assignment for DataScience Course at Olten, Switzerland
##########################################################


##############   1. Preliminaries                                               ##########################

# Install
if(!require('corrplot')) install.packages('corrplot', dependencies = TRUE)
if(!require('ggplot2')) install.packages('ggplot2', dependencies = TRUE)
if(!require('tidyverse')) install.packages('tidyverse', dependencies = TRUE)
if(!require('fastDummies')) install.packages('fastDummies', dependencies = TRUE)
if(!require('GGally')) install.packages('GGally', dependencies = TRUE)
if(!require('dplyr')) install.packages('dplyr', dependencies = TRUE)
if(!require("caret")) install.packages("caret", dependencies = TRUE)
if(!require("VIM")) install.packages("VIM", dependencies = TRUE)
if(!require("xgboost")) install.packages("xgboost", dependencies = TRUE)
if(!require("glmnet")) install.packages("glmnet", dependencies = TRUE)
if(!require("earth")) install.packages("earth", dependencies = TRUE)
if(!require("gbm")) install.packages("gbm", dependencies = TRUE)
if(!require("doParallel")) install.packages("doParallel", dependencies = TRUE)
if(!require("randomForest")) install.packages("tree", dependencies = TRUE)
if(!require("vif")) install.packages("car", dependencies = TRUE)

# Load packages
library('dplyr')
library('caret')
library('GGally')
library('fastDummies')
library('tidyverse')
library('ggplot2')
library('corrplot')
library('VIM')  # For kNN imputation
library('xgboost')
library('glmnet')
library('earth')
library('gbm')
library('xgboost')
library('doParallel')
library('car')
library('randomForest')

# load data set
cat("Sourcing helper functions...\n")
source("./submission/Group3-Assignment1-DataPreprocessing.R", echo = TRUE, print.eval = TRUE)

cat("Running preprocessing...\n")
export_type <- "export"   # Example parameter for analysis type 
                            #       export = writes csv to ./submission/data/LCdata_preprocessed.csv
                            #       return = returns the preprocessed dataframe
filter_value <- 100         # Example parameter for filtering

dataset_file_path <- perform_data_preprocessing(dataset_file_path, export_type, filter_value)
data <- read.csv2(dataset_file_path, header = TRUE, row.names=NULL, sep=";")

LC_Data <- data # make a copy of the original data set, so that we dont mess with it
static_seed_value = 1
train_proportion <- 0.8

##############   Step 2 - Train Test Split Data                                 ##########################
set.seed(static_seed_value)
train_indices <- sample(1:nrow(LC_Data), size = floor(train_proportion * nrow(data))) # Create indices for training data

# Split the data
train <- LC_Data[train_indices, ]  # Training data (70%)
test <- LC_Data[-train_indices, ]  # Testing data (30%)
watchlist = list(train=train, test=test)


##############   Final Model (XGBBoost)                                         ##########################

# Step 1: Define the grid of hyperparameters for tuning
xgbGrid <- expand.grid(
  nrounds = c(250),
  max_depth = c(6),
  eta = c(0.3),
  gamma = c(0),
  colsample_bytree = c(1.0),
  min_child_weight = c(5),
  subsample = c(0.6)
)

# Step 2: Train the XGBoost model
candidate_xgb_model_full <- train(
  int_rate ~ .,
  data = train,
  method = "xgbTree",
  trControl = trainControl(
    method = "repeatedcv",
    number = 5,
    repeats = 1,
    verboseIter = TRUE
  ),
  tuneGrid = xgbGrid,
  verbose = 1
)

# Display model summary
candidate_xgb_model_full
candidate_xgb_model_full$bestTune  # Optimal hyperparameters

# Step 3: Predict interest rates on the test data
test$predicted_int_rate <- predict(candidate_xgb_model_full, newdata = test)

# Step 4: Evaluate performance on the test set
# Mean Absolute Error (MAE)
test_mae <- mean(abs(test$int_rate - test$predicted_int_rate))

# Mean Squared Error (MSE)
test_mse <- mean((test$int_rate - test$predicted_int_rate)^2)

# Root Mean Squared Error (RMSE)
test_rmse <- sqrt(test_mse)

# R-squared
test_sst <- sum((test$int_rate - mean(test$int_rate))^2)  # Total Sum of Squares
test_sse <- sum((test$int_rate - test$predicted_int_rate)^2)  # Sum of Squared Errors
test_r_squared <- 1 - (test_sse / test_sst)

# Display performance metrics for test set
cat("Test Set Metrics:\n")
cat("MAE:", test_mae, "\nMSE:", test_mse, "\nRMSE:", test_rmse, "\nR-squared:", test_r_squared, "\n")

# Step 5: Generate predictions for the full dataset (including joint applications)
LC_Data$predicted_int_rate <- predict(candidate_xgb_model_full, newdata = LC_Data)

# Evaluate performance on the full dataset
# Mean Absolute Error (MAE)
full_mae <- mean(abs(LC_Data$int_rate - LC_Data$predicted_int_rate))

# Mean Squared Error (MSE)
full_mse <- mean((LC_Data$int_rate - LC_Data$predicted_int_rate)^2)

# Root Mean Squared Error (RMSE)
full_rmse <- sqrt(full_mse)

# R-squared
full_sst <- sum((LC_Data$int_rate - mean(LC_Data$int_rate))^2)  # Total Sum of Squares
full_sse <- sum((LC_Data$int_rate - LC_Data$predicted_int_rate)^2)  # Sum of Squared Errors
full_r_squared <- 1 - (full_sse / full_sst)

# Display performance metrics for the full dataset
cat("Full Dataset Metrics:\n")
cat("MAE:", full_mae, "\nMSE:", full_mse, "\nRMSE:", full_rmse, "\nR-squared:", full_r_squared, "\n")




##############   Step 3 - Apply learning algorithms to data                     ##########################
##############   3.1 Model: Simple Linear Regression                            ##########################

# Step 1: Train a regression model (e.g., linear regression) using all features
candidate_lm_model_full <- lm(int_rate ~ ., data = train) 
summary(candidate_lm_model_full)  # MSE: 10.47649
vif(candidate_lm_model_full)  # Check Variance Inflation Factor (VIF) for multicollinearity

# Step 2: Train the regression model with subset selection to identify the best subset of predictors
candidate_lm-model_subset <- regsubsets(int_rate ~ ., data = train) 
summary(candidate_lm)  # Not helpful due to multiple categorical features

# Step 3: Refine the model by dropping insignificant features or features with high multicollinearity

# Refined Model 1: Remove features with high VIF or poor contribution
candidate_lm_model_refined_1 <- lm(int_rate ~ . - il_util - collections_12_mths_ex_med - pub_rec, data = train) 
summary(candidate_lm_model_refined_1)  # MSE: 10.47565
vif(candidate_lm_model_refined_1)  # Check for remaining multicollinearity

# Refined Model 2: Remove 'home_ownership' as it shows no significance
candidate_lm_model_refined_2 <- lm(int_rate ~ . - home_ownership - collections_12_mths_ex_med - pub_rec, data = train) 
summary(candidate_lm_model_refined_2)  # MSE: 10.56537

# Refined Model 3: Remove 'mths_since_last_record' due to high VIF
candidate_lm_model_refined_3 <- lm(int_rate ~ . - mths_since_last_record - home_ownership - collections_12_mths_ex_med - pub_rec, data = train) 
summary(candidate_lm_model_refined_3)  # MSE: 10.71717

# Refined Model 4: Remove 'purpose' as it has 14 categories, making interpretation difficult
candidate_lm_model_refined_4 <- lm(int_rate ~ . - purpose - mths_since_last_record - home_ownership - collections_12_mths_ex_med - pub_rec, data = train) 
summary(candidate_lm_model_refined_4)  # MSE: 11.53331



##############   3.2 Model: Random Forest                                       ##########################
### Step (a): Fit a Random Forest using all input variables
set.seed(static_seed_value)  # Set seed for reproducibility
candidate_rf_model <- randomForest(
  int_rate ~ ., 
  data = train, 
  mtry = 6, 
  importance = TRUE, 
  ntree = 600
)
candidate_rf_model # Output model summary

### Step (b): Calculate the OOB (Out-of-Bag) error
# Variable Importance Plot
varImpPlot(candidate_rf_model)  # Visualize variable importance
candidate_rf_model # OOB error can be read from the model output

### Step (c): Optimize the Random Forest parameters (ntree and mtry)
plot(candidate_rf_model)  # Plot model error vs. number of trees to assess convergence

### Step (d): Evaluate the model on the test set
# Predict on the test set
candidate_rf_model.predict.test <- predict(candidate_rf_model, newdata = test)

# Calculate Test RMSE
candidate_rf_model.test.MSE <- mean((candidate_rf_model.predict.test - test$int_rate)^2)  # Mean Squared Error
candidate_rf_model.test.RMSE <- sqrt(candidate_rf_model.test.MSE)  # Root Mean Squared Error

# Display the Test MSE and RMSE
candidate_rf_model.test.MSE  # Test Mean Squared Error
candidate_rf_model.test.RMSE  # Test Root Mean Squared Error



##############   3.3 Model: Boosting, XGBoost                                   ##########################

# Step 1: Define the grid of hyperparameters for tuning
xgbGrid <- expand.grid(
  nrounds = c(50, 75, 100, 150, 200, 250, 500, 1000, 1500, 2000, 5000),
  max_depth = c(6, 9, 12, 15, 21),
  eta = c(0.3),
  gamma = c(0),
  colsample_bytree = c(1.0),
  min_child_weight = c(5),
  subsample = c(0.6)
)

# Step 2: Train the XGBoost model
candidate_xgb_model_full <- train(
  int_rate ~ .,
  data = train,
  method = "xgbTree",
  trControl = trainControl(
    method = "repeatedcv",
    number = 5,
    repeats = 1,
    verboseIter = TRUE
  ),
  tuneGrid = xgbGrid,
  verbose = 1
)

# Display model summary
candidate_xgb_model_full
candidate_xgb_model_full$bestTune  # Optimal hyperparameters

# Step 3: Predict interest rates on the test data
test$predicted_int_rate <- predict(candidate_xgb_model_full, newdata = test)

# Step 4: Evaluate performance on the test set
# Mean Absolute Error (MAE)
test_mae <- mean(abs(test$int_rate - test$predicted_int_rate))

# Mean Squared Error (MSE)
test_mse <- mean((test$int_rate - test$predicted_int_rate)^2)

# Root Mean Squared Error (RMSE)
test_rmse <- sqrt(test_mse)

# R-squared
test_sst <- sum((test$int_rate - mean(test$int_rate))^2)  # Total Sum of Squares
test_sse <- sum((test$int_rate - test$predicted_int_rate)^2)  # Sum of Squared Errors
test_r_squared <- 1 - (test_sse / test_sst)

# Display performance metrics for test set
cat("Test Set Metrics:\n")
cat("MAE:", test_mae, "\nMSE:", test_mse, "\nRMSE:", test_rmse, "\nR-squared:", test_r_squared, "\n")

# Step 5: Generate predictions for the full dataset (including joint applications)
LC_Data$predicted_int_rate <- predict(candidate_xgb_model_full, newdata = LC_Data)

# Evaluate performance on the full dataset
# Mean Absolute Error (MAE)
full_mae <- mean(abs(LC_Data$int_rate - LC_Data$predicted_int_rate))

# Mean Squared Error (MSE)
full_mse <- mean((LC_Data$int_rate - LC_Data$predicted_int_rate)^2)

# Root Mean Squared Error (RMSE)
full_rmse <- sqrt(full_mse)

# R-squared
full_sst <- sum((LC_Data$int_rate - mean(LC_Data$int_rate))^2)  # Total Sum of Squares
full_sse <- sum((LC_Data$int_rate - LC_Data$predicted_int_rate)^2)  # Sum of Squared Errors
full_r_squared <- 1 - (full_sse / full_sst)

# Display performance metrics for the full dataset
cat("Full Dataset Metrics:\n")
cat("MAE:", full_mae, "\nMSE:", full_mse, "\nRMSE:", full_rmse, "\nR-squared:", full_r_squared, "\n")



##############   3.4 Feature Importance Matrix (XGBBoost)                       ##########################

# Step 1: Clean and prepare training data using k-Nearest Neighbors imputation
train_knn <- kNN(train, imp_var = FALSE)  # Perform KNN-based imputation on training data

# Separate features (X) and target variable (y) for training
y_train <- data.matrix(subset(train_knn, select = int_rate))  # Target variable
X_train <- data.matrix(subset(train_knn, select = -int_rate))  # Predictor variables

# Step 2: Clean and prepare test data
test_clean <- na.omit(test)  # Remove rows with missing values in the test set

# Separate features (X) and target variable (y) for testing
y_test <- data.matrix(subset(test_clean, select = int_rate))  # Target variable
X_test <- data.matrix(subset(test_clean, select = -int_rate))  # Predictor variables

# Step 3: Evaluate model performance on test data
postResample(y_pred, y_test)  # Evaluate predictions (e.g., RMSE, R-squared, MAE)

# Step 4: Generate the feature importance matrix
importance_matrix <- xgb.importance(feature_names = colnames(X_train), model = candidate_xgb_model_full)

# Step 5: Plot feature importance
xgb.plot.importance(
  importance_matrix,
  rel_to_first = TRUE,  # Display relative importance to the most important feature
  xlab = "Relative Importance"
)



##############   3.5 Fine-Tuning Task (Cross-Validation)                        ##########################

# Step 1: Split data into training and testing sets
set.seed(static_seed_value)  # Set seed for reproducibility
tData <- sample(1:nrow(LC_Cleaned), 0.1 * nrow(LC_Cleaned))
trainData <- LC_Cleaned[tData, ]
testData <- LC_Cleaned[-tData, ]

# Step 2: Set up parallel processing
num_cores <- parallel::detectCores() - 1  # Use all but one core
cl <- makePSOCKcluster(num_cores)
registerDoParallel(cl)

# Helper function to calculate Mean Squared Error (MSE)
calculate_mse <- function(predictions, actuals) {
  mse <- mean((actuals - predictions)^2)
  return(mse)
}

# Step 3: Set up cross-validation
cv_control <- trainControl(
  method = "repeatedcv", 
  number = 5,             # 5-fold cross-validation
  repeats = 3,            # Repeat 3 times
  verboseIter = TRUE,     # Show progress
  allowParallel = TRUE    # Enable parallel processing
)

### Step 4: Model Training and Fine-Tuning ###

##############   3.5.1 Linear Model                                             ##########################
set.seed(static_seed_value)
lm_model <- train(
  int_rate ~ ., 
  data = trainData, 
  method = "lm", 
  trControl = cv_control
)
lm_predictions <- predict(lm_model, testData)
lm_mse <- calculate_mse(lm_predictions, testData$int_rate)
cat("Linear Model MSE:", lm_mse, "\n")


##############   3.5.2 Ridge Regression                                         ##########################
ridge_model <- train(
  int_rate ~ ., 
  data = trainData, 
  method = "glmnet", 
  tuneGrid = expand.grid(alpha = 0, lambda = 10^seq(-4, 0, length = 10)), 
  trControl = cv_control
)
ridge_predictions <- predict(ridge_model, testData)
ridge_mse <- calculate_mse(ridge_predictions, testData$int_rate)
cat("Ridge Regression MSE:", ridge_mse, "\n")


##############   3.5.3 LASSO Regression                                         ##########################
lasso_model <- train(
  int_rate ~ ., 
  data = trainData, 
  method = "glmnet", 
  tuneGrid = expand.grid(alpha = 1, lambda = 10^seq(-4, 0, length = 10)), 
  trControl = cv_control
)
lasso_predictions <- predict(lasso_model, testData)
lasso_mse <- calculate_mse(lasso_predictions, testData$int_rate)
cat("LASSO Regression MSE:", lasso_mse, "\n")


##############   3.5.4 MARS Model                                               ##########################
set.seed(static_seed_value)
mars_model <- train(
  int_rate ~ ., 
  data = trainData, 
  method = "earth", 
  tuneGrid = expand.grid(degree = c(1, 2), nprune = seq(2, 50, by = 5)), 
  trControl = cv_control
)
mars_predictions <- predict(mars_model, testData)
mars_mse <- calculate_mse(mars_predictions, testData$int_rate)
cat("MARS Model MSE:", mars_mse, "\n")


##############   3.5.5 Gradient Boosting Machine (GBM)                          ##########################
set.seed(static_seed_value)
gbm_grid <- expand.grid(
  interaction.depth = c(3, 5, 7), 
  n.trees = seq(50, 200, by = 50), 
  shrinkage = 0.1, 
  n.minobsinnode = 10
)
gbm_model <- train(
  int_rate ~ ., 
  data = trainData, 
  method = "gbm", 
  tuneGrid = gbm_grid, 
  trControl = cv_control, 
  verbose = FALSE
)
gbm_predictions <- predict(gbm_model, testData)
gbm_mse <- calculate_mse(gbm_predictions, testData$int_rate)
cat("Gradient Boosting MSE:", gbm_mse, "\n")


##############   3.5.6 Extreme Gradient Boosting (XGBoost)                      ##########################
set.seed(static_seed_value)
xgb_grid <- expand.grid(
  nrounds = seq(100, 2000, by = 50), 
  max_depth = c(5, 6, 7), 
  eta = c(0.1, 0.3), 
  gamma = 0, 
  colsample_bytree = c(0.8, 1.0), 
  min_child_weight = c(1, 5), 
  subsample = 0.8
)
xgb_model <- train(
  int_rate ~ ., 
  data = trainData, 
  method = "xgbTree", 
  tuneGrid = xgb_grid, 
  trControl = cv_control, 
  verbose = TRUE
)
xgb_predictions <- predict(xgb_model, testData)
xgb_mse <- calculate_mse(xgb_predictions, testData$int_rate)
cat("XGBoost MSE:", xgb_mse, "\n")

# Step 5: Save the best model
best_model <- gbm_model  # Update based on the model with the lowest MSE
saveRDS(best_model, "best_model.rds")

# Step 6: Stop parallel processing
stopCluster(cl)



##############   Step 5 - Post-Processing (apply business rules)                ##########################
# Predict interest rate for the joint applications dataset using the model trained on individual applications
LC_Data_application_type_joint <- LC_Data_application_type_joint %>%
  mutate(predicted_int_rate = predict(lm_all, newdata = LC_Data_application_type_joint))

# Apply business rules for final interest rate on joint applications
LC_Data_application_type_joint <- LC_Data_application_type_joint %>%
  mutate(
    final_int_rate = case_when(
      # Rule 1: JOINT & Source Verified
      verification_status_joint == "Source Verified" ~ predicted_int_rate,
      
      # Rule 2: JOINT & Verified - increase interest rate by 2%
      verification_status_joint == "Verified" ~ predicted_int_rate * 1.02,
      
      # Rule 3: JOINT & Not Verified - set to NA to indicate decline
      verification_status_joint == "Not Verified" ~ NA_real_,
      
      # Default case, if needed (e.g., to handle any unexpected values)
      TRUE ~ predicted_int_rate
    )
  )
# Filter out rows with NA in final_int_rate (since they are marked as declined and don't have a rate to compare)
comparison_data <- LC_Data_application_type_joint %>% filter(!is.na(final_int_rate))

# Calculate Mean Absolute Error (MAE)
mae <- mean(abs(comparison_data$int_rate - comparison_data$final_int_rate))

# Calculate Mean Squared Error (MSE)
mse <- mean((comparison_data$int_rate - comparison_data$final_int_rate)^2)

# Calculate Root Mean Squared Error (RMSE)
rmse <- sqrt(mse)

# Print the comparison metrics
cat("Mean Absolute Error (MAE):", mae, "\n")
cat("Mean Squared Error (MSE):", mse, "\n")
cat("Root Mean Squared Error (RMSE):", rmse, "\n")
