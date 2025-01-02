##########################################################
# Data Science - AS24
# DS-Assignment: Part2
# Part 1: Pre-trained Model Evaluation
# Magdalena Hardegger magdalena.hardegger@students.fhnw.ch
# Firat Turan firat.turan@students.fhnw.ch
# Sascha Frossard sascha.frossard@students.fhnw.ch
# Sebastian Fernandez sebastian.fernandez@students.fhnw.ch
##########################################################
# Assignment for DataScience Course at Olten, Switzerland
##########################################################
#install.packages(c("scales", "caret", "pROC", "ggplot2", "dplyr", "keras", "tidyr","fastDummies"))
library(scales)
library(caret)
library(pROC)
library(ggplot2)
library(keras)
library(dplyr)
library(tidyr)
library(fastDummies)

##############   Step 1: Environment   ##########################
rm(list = ls())

# Define file paths and parameters
model_file_path       <- 'best_model.h5'         # Path to the pre-trained model
test_data_file_path   <- './ressources/Dataset-part-2.csv' # Path to the test data
static_seed_value     <- 1                        # Set seed for reproducibility

# Check if the test data file path is provided, else take default test data
if (!exists("test_data_file_path") || is.null(test_data_file_path) || test_data_file_path == "") {
  test_data_file_path <- "./path/to/test_data.csv"
  cat("Using default test data file path:", test_data_file_path, "\n")
} else {
  cat("Using provided test data file path:", test_data_file_path, "\n")
}


##############   Step 2: Data Preprocessing   ##########################
# declaring cleaning funciton we wrote
clean_column <- function(data, 
                         column_name, 
                         k = 5, 
                         outlier_factor = 1.5, 
                         is_ordered_factor = FALSE,
                         order_levels = NULL,
                         remove_outliers=FALSE, 
                         onehot_encode = FALSE) {
  # Step 1: Remove rows with missing values in the specified column only
  data <- data[!is.na(data[[column_name]]), ]
  # Step 2: Impute missing values only in the specified column (if any remain after filtering)
  if (is.numeric(data[[column_name]])) {
    # Impute missing values with mean for numeric columns
    # data[[column_name]][is.na(data[[column_name]])] <- mean(data[[column_name]], na.rm = TRUE)
    # Apply kNN imputation to the specified column if it has missing values
    if (any(is.na(data[[column_name]]))) {
      data[[column_name]] <- kNN(data, variable = column_name, k = k, imp_var = FALSE)[[column_name]]
    }
  } else if (is.character(data[[column_name]])) {
    # Drop missing character values
    data <- data[!is.na(data[[column_name]]), ]
    
    if (onehot_encode) {
      data <- dummy_cols(data, select_columns = column_name, remove_selected_columns = TRUE)
    }
    else{
      # Convert the remaining column to factor
      data[[column_name]] <- as.factor(data[[column_name]])
    }
  }
  # Step 3: Convert to ordered factor if specified and if it is a factor
  if (is_ordered_factor && is.factor(data[[column_name]])) {
    data[[column_name]] <- as.integer(factor(data[[column_name]], ordered = TRUE, levels = order_levels))
  }
  # Step 4: Check if the column is a single-level factor and throw an error if so
  if (is.factor(data[[column_name]]) && nlevels(data[[column_name]]) <= 1) {
    data <- data[!is.na(data[[column_name]]), ]
    print("Specified column has only one level after cleaning, no information remaining in this column, we drop it.")
  }
  # Step 5: Remove outliers in the specified column only if it is numeric
  if (remove_outliers && is.numeric(data[[column_name]])) {
    Q1 <- quantile(data[[column_name]], 0.25, na.rm = TRUE)
    Q3 <- quantile(data[[column_name]], 0.75, na.rm = TRUE)
    IQR <- Q3 - Q1
    lower_bound <- Q1 - outlier_factor * IQR
    upper_bound <- Q3 + outlier_factor * IQR
    data <- data[data[[column_name]] >= lower_bound & data[[column_name]] <= upper_bound, ]
  }
  # Step 6: Normalize the specified column if it is numeric
  if (is.numeric(data[[column_name]])) {
    data[[column_name]] <- rescale(data[[column_name]], to = c(0, 1))
  }
  return(data)
}


# Load the test data
cat("Loading test data...\n")
data <- read.csv(test_data_file_path, sep = ",")

# Data preprocessing: Cleaning and transforming columns
cat("Running preprocessing on test data...\n")
# Clean and preprocess data using custom function
columns_to_remove <- c("ID", "CNT_CHILDREN")
data <- data[ , !(names(data) %in% columns_to_remove)]
data <- clean_column(data, "CODE_GENDER", is_ordered_factor = TRUE, order_levels=c("F", "M"))
data <- clean_column(data, "FLAG_OWN_CAR", is_ordered_factor = TRUE, order_levels=c("N", "Y"))
data <- clean_column(data, "FLAG_OWN_REALTY", is_ordered_factor = TRUE, order_levels=c("N", "Y"))
#data <- data %>% filter(CNT_CHILDREN < 12)
#data <- clean_column(data, "CNT_CHILDREN")
#max_income = quantile(data$AMT_INCOME_TOTAL,c(0.95))
#data <- filter(data, AMT_INCOME_TOTAL <= max_income)
data <- clean_column(data, "AMT_INCOME_TOTAL")
data <- clean_column(data, "NAME_INCOME_TYPE", onehot_encode = TRUE)
levels <- c("Lower secondary", "Secondary / secondary special", "Incomplete higher", "Higher education", "Academic degree")
data <- clean_column(data, "NAME_EDUCATION_TYPE", is_ordered_factor = TRUE, order_levels = levels)
data <- clean_column(data, "NAME_FAMILY_STATUS", onehot_encode = TRUE)
data <- clean_column(data, "NAME_HOUSING_TYPE", onehot_encode = TRUE)
data <- clean_column(data, "DAYS_BIRTH")
data[data$DAYS_EMPLOYED > 0, "DAYS_EMPLOYED"] <- 1
data <- clean_column(data, "DAYS_EMPLOYED")
data <- subset(data, select = -FLAG_MOBIL)
data$OCCUPATION_TYPE[is.na(data$OCCUPATION_TYPE)] <- "Missing"
data <- clean_column(data, "OCCUPATION_TYPE", onehot_encode = TRUE)
data <- clean_column(data, "CNT_FAM_MEMBERS")
summary(as.factor(data$status))
data$status <- case_when(data$status == "C" ~ 7,
                         data$status == "X" ~ 6,
                         data$status == "0" ~ 0,
                         data$status == "1" ~ 1,
                         data$status == "2" ~ 2,
                         data$status == "3" ~ 3,
                         data$status == "4" ~ 4,
                         data$status == "5" ~ 5)
#data_cleaned_cross_corr <- data %>% select(-"CNT_CHILDREN")



X <- as.matrix(subset(data, select = -status))
y <- keras::to_categorical(data$status)

##############   Step 3: Load Pre-trained Model and Predict   ##########################
cat("Loading pre-trained model...\n")
best_model <- load_model_hdf5(model_file_path)

cat("Making predictions...\n")
predictions <- best_model %>% predict(X)

##############   Step 4: Evaluation   ##########################

# Convert predictions (softmax) to class labels
# Predicted probabilities
pred_class <- apply(predictions, 1, which.max) - 1

# Actual class labels
y_class <- apply(y, 1, which.max) - 1  # Convert to 0-indexed class labels

#Confusion Matrix
conf_matrix <- confusionMatrix(factor(pred_class), factor(y_class))
print(conf_matrix)


#ROC Curve and AUC
roc_list <- list()
auc_values <- numeric(length = ncol(y))

# One-vs-rest ROC for each class
for (i in 1:ncol(y)) {
  roc_list[[i]] <- roc(y_class == (i-1), predictions[, i])
  auc_values[i] <- auc(roc_list[[i]])
}

# Plot ROC curves
roc_plot <- ggroc(roc_list)
print(roc_plot)

# AUC values for each class
cat("AUC Values for each class: \n")
print(auc_values)

# Average AUC across all classes
avg_auc <- mean(auc_values)
cat("Average AUC: ", avg_auc, "\n")
