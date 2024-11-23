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


##############   1. Preliminaries                                                       ##########################

# Install



# Load packages



##############   Step 2: Environment                                                   ##########################
model_file_path = 'Group03_Final-Model_XGBoost.RData'
test_data_file_path = ''
static_seed_value = 1


# Check if the variable 'dataset_file_path' already exists
dataset_file_path <- test_data_file_path
if (!exists("dataset_file_path") || is.null(dataset_file_path) || dataset_file_path == "") {
  dataset_file_path <- "./ressources/LCdata.csv"
  cat("Using default dataset file path:", dataset_file_path, "\n")
} else {
  cat("Using existing dataset file path:", dataset_file_path, "\n")
}

##############   Step 3: Data Preprocessing                                             ##########################

# Load functions from another R script
cat("Sourcing helper functions...\n")
source("./submission/Group3-Assignment1-DataPreprocessing.R", echo = TRUE, print.eval = TRUE)

cat("Running preprocessing...\n")
export_type <- "return"   # Example parameter for analysis type 
                            #       export = writes csv to ./submission/data/LCdata_preprocessed.csv
                            #       return = returns the preprocessed dataframe
filter_value <- 100         # Example parameter for filtering

LC_Final <- perform_data_preprocessing(dataset_file_path, export_type, filter_value)


##############   Step 4: Predict                                                          ##########################

# Load the model
cat("Load model...\n")
model <- load(model_file_path)

cat("Predict...\n")
LC_Final$predicted_int_rate <- predict(model, newdata = LC_Final)


##############   Step 5: Evaluate                                                          ##########################

# Step 4: Evaluate performance on the test set
# Mean Absolute Error (MAE)
test_mae <- mean(abs(LC_Final$int_rate - LC_Final$predicted_int_rate))

# Mean Squared Error (MSE)
test_mse <- mean((LC_Final$int_rate - LC_Final$predicted_int_rate)^2)

# Root Mean Squared Error (RMSE)
test_rmse <- sqrt(test_mse)

# R-squared
test_sst <- sum((LC_Final$int_rate - mean(LC_Final$int_rate))^2)  # Total Sum of Squares
test_sse <- sum((LC_Final$int_rate - LC_Final$predicted_int_rate)^2)  # Sum of Squared Errors
test_r_squared <- 1 - (test_sse / test_sst)

# Display performance metrics for test set
cat("Test Set Metrics:\n")
cat("MAE:", test_mae, "\nMSE:", test_mse, "\nRMSE:", test_rmse, "\nR-squared:", test_r_squared, "\n")
