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

##############   Step 1: Environment                                                   ##########################
model_file_path       <- './submission/data/Group03_Final-Model_XGBoost.rds'
test_data_file_path   <- '' # Gwen, please specify your test-data here!
static_seed_value     <- 1

# Check if the variable 'dataset_file_path' is set, else take default dataset
if (!exists("dataset_file_path") || is.null(dataset_file_path) || dataset_file_path == "") {
  dataset_file_path <- "./ressources/LCdata.csv"
  cat("Using default dataset file path:", dataset_file_path, "\n")
} else {
  dataset_file_path <- test_data_file_path
  cat("Using testing dataset file path:", dataset_file_path, "\n")
}


##############   Step 2: Data Preprocessing                                             ##########################

# Load preprocessing-function from another R script
cat("Sourcing helper functions...\n")
source("./submission/Group3-Assignment1-DataPreprocessing.R", echo = TRUE, print.eval = TRUE)

cat("Running preprocessing...\n")
export_type <- "return"   # Example parameter for analysis type 
                            #       export = writes csv to ./submission/data/LCdata_preprocessed.csv
                            #       return = returns the preprocessed dataframe
LC_Processed <- perform_data_preprocessing(dataset_file_path, export_type)

LC_Processed_Data = LC_Processed$data
LC_Processed_Application_Joint = LC_Processed$data_application_type_joint

##############   Step 3: Predict                                                          ##########################

# Load the model
if (!exists("model_file_path") || is.null(model_file_path) || model_file_path == "") {
  cat("Unable to load an 'rds' model, variable 'model_file_path' not set, aborting..\n")
  return()
} else {
  model <- readRDS(model_file_path)
  cat("Load model from file path:", model_file_path, "\n")
}

cat("Predict...\n")
LC_Processed_Data$predicted_int_rate <- predict(model, newdata = LC_Processed_Data)


##############   Step 4: Evaluate performance on the test set                              ##########################
test_mae        <- mean(abs(LC_Processed_Data$int_rate - LC_Processed_Data$predicted_int_rate))   # Mean Absolute Error (MAE)
test_mse        <- mean((LC_Processed_Data$int_rate - LC_Processed_Data$predicted_int_rate)^2)    # Mean Squared Error (MSE)
test_rmse       <- sqrt(test_mse)                                                                 # Root Mean Squared Error (RMSE)
test_sst        <- sum((LC_Processed_Data$int_rate - mean(LC_Processed_Data$int_rate))^2)         # Total Sum of Squares
test_sse        <- sum((LC_Processed_Data$int_rate - LC_Processed_Data$predicted_int_rate)^2)     # Sum of Squared Errors
test_r_squared  <- 1 - (test_sse / test_sst)                                                      # R-squared

cat("Test Set Metrics:\n")
cat("MAE:", test_mae, "\nMSE:", test_mse, "\nRMSE:", test_rmse, "\nR-squared:", test_r_squared, "\n")


##############   Step 5 - Post-Processing (apply business rules)                ##########################


LC_Processed_Application_Joint$predicted_int_rate <- predict(model, newdata = LC_Processed_Application_Joint)
colnames(LC_Processed_Data)[colnames(LC_Processed_Data) == "predicted_int_rate"] <- "final_predicted_int_rate"

# Initialize variables to store the best results
best_mse <- Inf
best_multiplier <- NA

# Define the range of multipliers to test
multipliers <- seq(0.5, 2.0, by = 0.01) # Adjust range and step size as needed

# Loop over each multiplier
for (multiplier in multipliers) {
  # Apply business rules with the current multiplier
  LC_Processed_Application_Joint <- LC_Processed_Application_Joint %>%
    mutate(
      final_predicted_int_rate = case_when(
        verification_status_joint == "Source Verified" ~ LC_Processed_Application_Joint$predicted_int_rate,
        verification_status_joint == "Verified" ~ LC_Processed_Application_Joint$predicted_int_rate * multiplier,
        verification_status_joint == "Not Verified" ~ NA_real_,
        TRUE ~ LC_Processed_Application_Joint$predicted_int_rate
      )
    )
  
  # Filter out rows with NA in final_predicted_int_rate
  comparison_data <- LC_Processed_Application_Joint %>% filter(!is.na(final_predicted_int_rate))
  
  merged_data <- merge(comparison_data, LC_Processed_Data, by = "common_column")
  
  # Calculate MSE for the current multiplier
  mse <- mean((merged_data$int_rate - merged_data$final_predicted_int_rate)^2, na.rm = TRUE)
  
  # Update the best MSE and corresponding multiplier if current MSE is lower
  if (mse < best_mse) {
    best_mse <- mse
    best_multiplier <- multiplier
  }
}

# Output the best multiplier and its corresponding MSE
cat("Best Multiplier:", best_multiplier, "\n")
cat("Best MSE:", best_mse, "\n")
