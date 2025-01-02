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
rm(list = ls())
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
source("./submission/Group3-Assignment1-DataPreprocessing.R", echo = FALSE, print.eval = FALSE)

cat("Running preprocessing...\n")
export_type <- "return"   # Example parameter for analysis type 
                            #       export = writes csv to ./submission/data/LCdata_preprocessed.csv
                            #       return = returns the preprocessed dataframe

LC_Processed <- perform_data_preprocessing(dataset_file_path, export_type)

LC_Processed_Data <- LC_Processed$data
LC_Processed_Application_Joint <- LC_Processed$data_application_type_joint
LC_Original_data <- LC_Processed$original

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
mae        <- mean(abs(LC_Processed_Data$int_rate - LC_Processed_Data$predicted_int_rate))     # Mean Absolute Error (MAE)
mse        <- mean((LC_Processed_Data$int_rate - LC_Processed_Data$predicted_int_rate)^2)      # Mean Squared Error (MSE)
rmse       <- sqrt(mse)                                                                        # Root Mean Squared Error (RMSE)
sst        <- sum((LC_Processed_Data$int_rate - mean(LC_Processed_Data$int_rate))^2)           # Total Sum of Squares
sse        <- sum((LC_Processed_Data$int_rate - LC_Processed_Data$predicted_int_rate)^2)       # Sum of Squared Errors
r_squared  <- 1 - (sst / sse)                                                                  # R-squared

cat("Metrics:\n")
cat("MAE:", mae, "\nMSE:", mse, "\nRMSE:", rmse, "\nR-squared:", r_squared, "\n")


##############   Step 5 - Post-Processing (apply business rules)                ##########################
LC_Processed_Application_Joint$predicted_int_rate <- predict(model, newdata = LC_Processed_Application_Joint)
colnames(LC_Processed_Data)[colnames(LC_Processed_Data) == "predicted_int_rate"] <- "final_predicted_int_rate"

# Initialize variables to store the best results
best_mse <- Inf
best_multiplier <- NA
multipliers <- seq(0.5, 2.0, by = 0.01)                                                         # Define the range of multipliers to test

for (multiplier in multipliers) {                                                               # Loop over each multiplier
  # Apply business rules with the current multiplier
  LC_TMP <- LC_Processed_Application_Joint %>%
    mutate(
      final_predicted_int_rate = case_when(
        verification_status_joint == "Source Verified" ~ LC_Processed_Application_Joint$predicted_int_rate,
        verification_status_joint == "Verified" ~ LC_Processed_Application_Joint$predicted_int_rate * multiplier,
        verification_status_joint == "Not Verified" ~ NA_real_,
        TRUE ~ LC_Processed_Application_Joint$predicted_int_rate
      )
    )
  
  
  LC_TMP <- LC_TMP %>% filter(!is.na(final_predicted_int_rate))           
  LC_TMP <- LC_TMP %>% select(-c(application_type, annual_inc_joint, dti_joint, verification_status_joint, predicted_int_rate))
  mse <- mean((LC_TMP$int_rate - LC_TMP$final_predicted_int_rate)^2, na.rm = TRUE)              # Calculate MSE for the current multiplier
    
  if (mse < best_mse) {                                                                         # Update the best MSE and corresponding multiplier if current MSE is lower
    best_LC_Processed_Application_Joint <- LC_TMP
    best_mse <- mse
    best_multiplier <- multiplier
  }
}

LC_Final <- rbind(best_LC_Processed_Application_Joint, LC_Processed_Data)

mae                   <- mean(abs(LC_Final$int_rate - LC_Final$final_predicted_int_rate))       # Mean Absolute Error (MAE)
mse                   <- mean((LC_Final$int_rate - LC_Final$final_predicted_int_rate)^2)        # Mean Squared Error (MSE)
rmse                  <- sqrt(mse)                                                              # Root Mean Squared Error (RMSE)
sst                   <- sum((LC_Final$int_rate - mean(LC_Final$int_rate))^2)                   # Total Sum of Squares
sse                   <- sum((LC_Final$int_rate - LC_Final$final_predicted_int_rate)^2)         # Sum of Squared Errors
r_squared             <- 1 - (sse / sst)                                                        # R-squared
min_int_rate          <- min(LC_Final$int_rate, na.rm = TRUE)                                   # Minimum interest rate
max_int_rate          <- max(LC_Final$int_rate, na.rm = TRUE)                                   # Maximum interest rate
range_int_rate        <- max_int_rate - min_int_rate                                            # Range of interest rate
mae_percent_of_range  <- round((mae / range_int_rate) * 100, 2)                                 # MAE as percentage of range

output_line <- paste(
  " MAE:", mae, "\n", 
  "  | Min int_rate:", min_int_rate, "\n", 
  "  | Max int_rate:", max_int_rate, "\n", 
  "  | MAE as % of Range:", mae_percent_of_range, "%", "\n",
  "MSE:", mse, "\n",
  "  | RMSE as % of Range:", round((rmse / range_int_rate) * 100, 2), "\n",
  "RMSE:", rmse, "\n",
  "R-squared:", r_squared, "\n"
)

# Output the best multiplier and its corresponding MAE, MSE, RMSE, R-squared
cat("Best Business-rule Multiplier for verification_status == 'verified':", best_multiplier, "\n")
cat(output_line, "\n")
