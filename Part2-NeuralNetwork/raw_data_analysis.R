# Install the 'scales' package if not already installed
#install.packages("scales")
# Load the library
library(scales)
library(corrplot)
library(tidyverse)
library(fastDummies)


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
      
      # Check if the one-hot encoding resulted in only two columns for the variable
      new_columns <- grep(paste0("^", column_name, "_"), names(data), value = TRUE)
      
      if (length(new_columns) == 2) {
        # Drop one of the new columns to avoid redundancy
        data <- data[ , !(names(data) %in% new_columns[1])]
      }
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


# Load the CSV file into R
data <- read.csv("ressources/Dataset-part-2.csv", sep = ",")

summary(data)

str(data)
# Raw data analysis

# TARGET int_rate: Interest Rate on the loan. Predicting is task.
summary(data$status)
str(data$status)

# Removing -> Not relevant
# 
# id: A unique LC assigned ID for the loan listing.

# ------------------------------------------------------------------------------
# Removing columns

columns_to_remove <- c("ID")

data <- data[ , !(names(data) %in% columns_to_remove)]

# CODE_GENDER - Gender
# data seems fine. No missing values and all data is binary. Therefore onehot encoding.
str(data$CODE_GENDER)
summary(data$CODE_GENDER)

data <- clean_column(data, "CODE_GENDER", onehot_encode = TRUE)

# FLAG_OWN_CAR - Is there a car
# data seems fine. No missing values and all data is binary. Therefore onehot encoding.
str(data$FLAG_OWN_CAR)
summary(data$FLAG_OWN_CAR)

data <- clean_column(data, "FLAG_OWN_CAR", onehot_encode = TRUE)

# FLAG_OWN_REALTY - Is there a property
# data seems fine. No missing values and all data is binary. Therefore onehot encoding.
str(data$FLAG_OWN_REALTY)
summary(data$FLAG_OWN_REALTY)

data <- clean_column(data, "FLAG_OWN_REALTY", onehot_encode = TRUE)

# CNT_CHILDREN - Number of children
# Ordered values and Numeric. Third quantile is 1 so we do not use our function to remove outlieres here. We do it manually.
str(data$CNT_CHILDREN)
summary(data$CNT_CHILDREN)
table(data$CNT_CHILDREN)
# We see suspecting values that people have 6, 7, 12, 19 Children
poeple_with_lot_children <- filter(data, CNT_CHILDREN >=6 )
poeple_with_lot_children$AGE <- poeple_with_lot_children$DAYS_BIRTH / (-365)

# Scatterplot
plot(x = poeple_with_lot_children$AGE, 
     y = poeple_with_lot_children$CNT_CHILDREN, 
     main = "Scatterplot of column_x vs column_y", 
     xlab = "Age", 
     ylab = "amount of children", 
     col = "blue", 
     pch = 19)  # Customize point style
# we see here that the persons with 19 and 12 children above the age of 30 and 36 seems suspicious and we drop them.
data <- data %>% filter(CNT_CHILDREN < 12)

# We use our cleaning function only for normalization 
data <- clean_column(data, "CNT_CHILDREN")

# AMT_INCOME_TOTAL - Annual income
str(data$AMT_INCOME_TOTAL)
summary(data$AMT_INCOME_TOTAL)
# we have ouliers here but in praxis it is possible to have an anual income of over 6750000. We do not drop them

data <- clean_column(data, "AMT_INCOME_TOTAL")

# NAME_INCOME_TYPE - Income category 
str(data$NAME_INCOME_TYPE)
summary(data$NAME_INCOME_TYPE)

# data seems fine. No missing values and all data is categorical (nominal). Therefore onehot encoding.
data <- clean_column(data, "NAME_INCOME_TYPE", onehot_encode = TRUE)

# NAME_EDUCATION_TYPE - Education level 
str(data$NAME_EDUCATION_TYPE)
summary(data$NAME_EDUCATION_TYPE)
table(data$NAME_EDUCATION_TYPE)
levels <- c("Lower secondary", "Secondary / secondary special", "Incomplete higher", "Higher education", "Academic degree")
# data seems fine. No missing values and all data is categorical (nominal). Therefore using ordered factor.
data <- clean_column(data, "NAME_EDUCATION_TYPE", is_ordered_factor = TRUE, order_levels = levels)


# NAME_FAMILY_STATUS - Marital status
str(data$NAME_FAMILY_STATUS)
summary(data$NAME_FAMILY_STATUS)
table(data$NAME_FAMILY_STATUS)
# data seems fine. No missing values and all data is categorical (nominal). Therefore onehot encoding.
data <- clean_column(data, "NAME_FAMILY_STATUS", onehot_encode = TRUE)


# NAME_HOUSING_TYPE - Way of living
str(data$NAME_HOUSING_TYPE)
summary(data$NAME_HOUSING_TYPE)
table(data$NAME_HOUSING_TYPE)
# data seems fine. No missing values and all data is categorical (nominal). Therefore onehot encoding.
data <- clean_column(data, "NAME_HOUSING_TYPE", onehot_encode = TRUE)


# DAYS_BIRTH
# Count backwards from current day (0), -1 means yesterday 
str(data$DAYS_BIRTH)
summary(data$DAYS_BIRTH)
hist(data$DAYS_BIRTH)
# data seems fine. Distribution between 20 and 69 years of age. No missing values and all data is numerical.
data <- clean_column(data, "DAYS_BIRTH")


# DAYS_EMPLOYED
# Count backwards from current day(0). If positive, it means the person unemployed. 
str(data$DAYS_EMPLOYED)
summary(data$DAYS_EMPLOYED)
hist(data$DAYS_EMPLOYED)
# data for unemployed needs to be adjusted. Default value is set to 365243. This creates a big gap if normalized 0-1.
# Adjust unemployed to 1.
data[data$DAYS_EMPLOYED > 0, "DAYS_EMPLOYED"] <- 1
hist(data$DAYS_EMPLOYED)
# Now distribution looks better
data <- clean_column(data, "DAYS_EMPLOYED")


# FLAG_MOBIL - Is there a mobile phone
summary(data$FLAG_MOBIL)
# All entries have value 1. No variation in data. Therefore remove column.
data <- subset(data, select = -FLAG_MOBIL)


# FLAG_WORK_PHONE - Is there a work phone
summary(data$FLAG_WORK_PHONE)
# No NAs. Data is already between 0 and 1. No changes required.


# FLAG_PHONE - Is there a phone
summary(data$FLAG_PHONE)
# No NAs. Data is already between 0 and 1. No changes required.


# FLAG_EMAIL - Is there an email
summary(data$FLAG_EMAIL)
# No NAs. Data is already between 0 and 1. No changes required.


# OCCUPATION_TYPE - Occupation
summary(as.factor(data$OCCUPATION_TYPE))
# There are 20699 NA's which is about a third of the total rows. Create new category missing.
data$OCCUPATION_TYPE[is.na(data$OCCUPATION_TYPE)] <- "Missing"
data <- clean_column(data, "OCCUPATION_TYPE", onehot_encode = TRUE)


# CNT_FAM_MEMBERS - Family size
summary(data$CNT_FAM_MEMBERS)
hist(data$CNT_FAM_MEMBERS)
# Numeric value between 1 and 9, seems realistic.
data <- clean_column(data, "CNT_FAM_MEMBERS")


# status - “pay-back behavior” this is the classification classes
# 0: 1-29 days past due  
# 1: 30-59 days past due  
# 2: 60-89 days overdue  
# 3: 90-119 days overdue  
# 4: 120-149 days overdue  
# 5: Overdue or bad debts, write-offs for more than 150 days  
# C: paid off that month  
# X: No loan for the month 
summary(as.factor(data$status))
# Reorder: C is the best no overdue then 0 - 5
# Remark: Here X was chosen even better as C but it remains to be seen if this makes sense.
data$status <- as.integer(factor(data$status, ordered = TRUE, levels = c("X", "C", "0", "1", "2", "3", "4", "5")))


y <- data$status  # Target variable
X <- data[, -which(names(data) == "status")]  # Features

# Plot the correlation matrix and save because of many rows it is easier to read.
correlation_matrix <- cor(X)
png(filename = "raw_data_corrplot.png", width = 2000, height = 2000)
corrplot(correlation_matrix, method = "color", type = "upper", tl.col = "black", addCoef.col = "black",number.cex = 1, tl.cex = 1)
dev.off()

# we remove CNT_CHILDREN due to redondancy. Corr. is quite high. The rest which correlates it regarding one_hot -> we keep them.
# we could also leave this feature since it will disapear during training but for completeness we drop it for now
data_cleaned_cross_corr <- data %>% select(-"CNT_CHILDREN")

# Plot the correlation matrix and save because of many rows it is easier to read.
correlation_matrix <- cor(data_cleaned_cross_corr)
png(filename = "cleanded_data_corrplot.png", width = 2000, height = 2000)
corrplot(correlation_matrix, method = "color", type = "upper", tl.col = "black", addCoef.col = "black",number.cex = 1, tl.cex = 1)
dev.off()


#----------------------------Train model --------------------------- transfer in other file 
# Load necessary libraries
# Install necessary libraries if not already installed
library(keras)
library(tensorflow)
install_tensorflow(version = "2.15.0")


tf$constant("Hello TensorFlow!")


# Ensure reproducibility
set.seed(1)

# Split data into training and testing sets
train_indices <- sample(1:nrow(X), size = 0.8 * nrow(X))
x_train <- as.matrix(X[train_indices, ])
y_train <- as.numeric(as.factor(y[train_indices])) - 1  # Ensure zero-based index
x_test <- as.matrix(X[-train_indices, ])
y_test <- as.numeric(as.factor(y[-train_indices])) - 1

# Manually one-hot encode the targets
one_hot_encode <- function(y, num_classes) {
  matrix <- matrix(0, nrow = length(y), ncol = num_classes)
  for (i in seq_along(y)) {
    matrix[i, y[i] + 1] <- 1
  }
  return(matrix)
}

# Define number of classes
num_classes <- length(unique(y))
y_train <- one_hot_encode(y_train, num_classes)
y_test <- one_hot_encode(y_test, num_classes)

# Verify dimensions
cat("x_train dimensions:", dim(x_train), "\n")
cat("y_train dimensions:", dim(y_train), "\n")

# Get the number of input features
input_dim <- ncol(x_train)

# Define the model
model <- keras_model_sequential() %>% 
  layer_dense(units = 64, activation = "relu", input_shape = c(input_dim)) %>% 
  layer_dense(units = 64, activation = 'relu') %>%
  layer_dense(units = num_classes, activation = 'softmax')

# Compile the model
model %>% compile(
  optimizer = optimizer_adam(),
  loss = 'categorical_crossentropy',
  metrics = c('accuracy')
)

# Train the model
model %>% fit(
  x_train,
  y_train,
  epochs = 100,
  batch_size = 128,
  validation_data = list(x_test, y_test)
)

#--------------------cv-------------------------
library(caret)  
# Define parameters for cross-validation
# Use `data_cleaned_cross_corr` to define X and y


# One-hot encode the target variable
y_onehot <- keras::to_categorical(y, num_classes = length(unique(y)))

# Define parameters
k_folds <- 5  # Number of cross-validation folds
epochs <- 50
batch_size <- 32
learning_rate <- 0.001

# Create k-fold cross-validation splits
set.seed(1)  # Ensure reproducibility
folds <- createFolds(1:nrow(X), k = k_folds, list = TRUE)

# Define a function to build the model
build_model <- function(input_shape, num_classes, learning_rate) {
  model <- keras_model_sequential() %>%
    layer_dense(units = 512, activation = "relu", input_shape = input_shape) %>%
    layer_batch_normalization() %>%
    layer_dropout(rate = 0.4) %>%
    layer_dense(units = 256, activation = "relu") %>%
    layer_batch_normalization() %>%
    layer_dropout(rate = 0.4) %>%
    layer_dense(units = 128, activation = "relu", input_shape = input_shape) %>%
    layer_batch_normalization() %>%
    layer_dropout(rate = 0.4) %>%
    layer_dense(units = 64, activation = "relu") %>%
    layer_batch_normalization() %>%
    layer_dropout(rate = 0.4) %>%
    layer_dense(units = num_classes, activation = "softmax")
  
  model %>% compile(
    optimizer = optimizer_adam(learning_rate = learning_rate),
    loss = "categorical_crossentropy",
    metrics = c("accuracy")
  )
  
  return(model)
}

# Cross-validation loop
cv_results <- data.frame(fold = 1:k_folds, train_accuracy = 0, val_accuracy = 0)

for (i in 1:k_folds) {
  cat("Running fold", i, "/", k_folds, "\n")
  
  # Split data into training and validation for this fold
  val_indices <- folds[[i]]
  x_val <- X[val_indices, ]
  y_val <- y_onehot[val_indices, ]
  x_train <- X[-val_indices, ]
  y_train <- y_onehot[-val_indices, ]
  
  # Build and train the model
  model <- build_model(input_shape = ncol(X), num_classes = ncol(y_onehot), learning_rate = learning_rate)
  
  history <- model %>% fit(
    x_train, y_train,
    epochs = epochs,
    batch_size = batch_size,
    validation_data = list(x_val, y_val),
    verbose = 1
  )
  
  # Evaluate on validation set
  val_scores <- model %>% evaluate(x_val, y_val, verbose = 0)
  
  # Save results
  cv_results$train_accuracy[i] <- max(history$metrics$accuracy)
  cv_results$val_accuracy[i] <- val_scores["accuracy"]
}

# Summarize cross-validation results
cat("Cross-validation results:\n")
print(cv_results)
cat("Average validation accuracy:", mean(cv_results$val_accuracy), "\n")

# Train final model on the entire dataset
cat("Training final model on the full dataset\n")
final_model <- build_model(input_shape = ncol(X), num_classes = ncol(y_onehot), learning_rate = learning_rate)

# Use early stopping for final training
early_stopping <- callback_early_stopping(monitor = "val_loss", patience = 5)

history_final <- final_model %>% fit(
  X, y_onehot,
  epochs = epochs,
  batch_size = batch_size,
  validation_split = 0.2,
  callbacks = list(early_stopping),
  verbose = 1
)

# Save the final model
final_model %>% save_model_hdf5("final_model.h5")

# Evaluate on test data if provided
cat("Final model trained successfully. Save and ready for deployment!\n")
