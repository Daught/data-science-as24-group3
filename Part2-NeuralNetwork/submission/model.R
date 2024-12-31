#----------------------------Train model --------------------------- 
# Load necessary libraries
# Install necessary libraries if not already installed
library(keras)
library(tensorflow)
library(caret)
library(scales)
library(corrplot)
library(tidyverse)
library(fastDummies)

# install_tensorflow(version = "2.15.0")


#tf$constant("Hello TensorFlow!")


# Ensure reproducibility
set.seed(1)


# Load the cleaned data from a CSV file
data_cleaned_cross_corr <- read.csv("cleaned_data.csv")

#--------------------cv-------------------------
# Make sure that each class gets split individually
# Take a bit more training from classes 2-5 with low representation

# The following code ensures that each class (status variable) is split properly
# into training and testing sets. The goal is to handle class imbalance by
# adjusting the training size for different classes.
data_0 <- filter(data_cleaned_cross_corr, status==0)
train_0_indices <- sample(1:nrow(data_0), size = 0.8 * nrow(data_0))
train_0 <- data_0[train_0_indices, ]
test_0 <- data_0[-train_0_indices, ]


data_1 <- filter(data_cleaned_cross_corr, status==1)
train_1_indices <- sample(1:nrow(data_1), size = 0.8 * nrow(data_1))
train_1 <- data_1[train_1_indices, ]
test_1 <- data_1[-train_1_indices, ]


data_2 <- filter(data_cleaned_cross_corr, status==2)
train_2_indices <- sample(1:nrow(data_2), size = 0.9 * nrow(data_2))
train_2 <- data_2[train_2_indices, ]
test_2 <- data_2[-train_2_indices, ]


data_3 <- filter(data_cleaned_cross_corr, status==3)
train_3_indices <- sample(1:nrow(data_3), size = 0.9 * nrow(data_3))
train_3 <- data_3[train_3_indices, ]
test_3 <- data_3[-train_3_indices, ]


data_4 <- filter(data_cleaned_cross_corr, status==4)
train_4_indices <- sample(1:nrow(data_4), size = 0.9 * nrow(data_4))
train_4 <- data_4[train_4_indices, ]
test_4 <- data_4[-train_4_indices, ]


data_5 <- filter(data_cleaned_cross_corr, status==5)
train_5_indices <- sample(1:nrow(data_5), size = 0.9 * nrow(data_5))
train_5 <- data_5[train_5_indices, ]
test_5 <- data_5[-train_5_indices, ]


data_6 <- filter(data_cleaned_cross_corr, status==6)
train_6_indices <- sample(1:nrow(data_6), size = 0.8 * nrow(data_6))
train_6 <- data_6[train_6_indices, ]
test_6 <- data_6[-train_6_indices, ]


data_7 <- filter(data_cleaned_cross_corr, status==7)
train_7_indices <- sample(1:nrow(data_7), size = 0.8 * nrow(data_7))
train_7 <- data_7[train_7_indices, ]
test_7 <- data_7[-train_7_indices, ]

train <- rbind(train_0, train_1, train_2, train_3, train_4, train_5, train_6, train_7)
test <- rbind(test_0, test_1, test_2, test_3, test_4, test_5, test_6, test_7)

table(train$status)

# library(scutr)
# ov_0 = undersample_kmeans(train, 0, "status", 5000)
# ov_1 = oversample_smote(train, 1, "status", 5000)
# ov_2 = oversample_smote(train, 2, "status", 1000)
# ov_3 = oversample_smote(train, 3, "status", 500)
# ov_4 = oversample_smote(train, 4, "status", 500)
# ov_5 = oversample_smote(train, 5, "status", 1000)
# ov_6 = oversample_smote(train, 6, "status", 5000)
# ov_7 = oversample_smote(train, 7, "status", 5000)
# train <- rbind(ov_0, ov_1, ov_2, ov_3, ov_4, ov_5, ov_6, ov_7)



hist(train$status)
hist(test$status)

# -------------------------- Prepare Data ---------------------------
# Prepare the data for training. We separate the features (x_train) and the target variable (y_train),
# and do the same for the validation set (x_val and y_val).

x_train <- as.matrix(subset(train, select=-status))
y_train <- keras::to_categorical(train$status)
x_val <- as.matrix(subset(test, select=-status))
y_val <- keras::to_categorical(test$status)

# ----------------------- Compute Class Weights --------------------
# In case the classes are imbalanced, we compute dynamic class weights inversely proportional
# to the frequency of each class. This helps the model treat underrepresented classes with more importance.
y <- data_cleaned_cross_corr$status
class_counts <- table(y)
total_samples <- sum(class_counts)
num_classes <- length(class_counts)

# Compute class weights inversely proportional to class frequencies
dynamic_class_weights <- total_samples / (num_classes * class_counts)

# Normalize weights for better balance (optional)
dynamic_class_weights <- dynamic_class_weights / max(dynamic_class_weights)

# Convert to a named list for Keras
class_weights <- as.list(as.numeric(dynamic_class_weights))

# Learning params for model before grid search
epochs <- 1000
batch_size <- 128
learning_rate <- 0.001
dropout_rate <- 0.3

# Feed-Forward Network (FFN) is chosen because the data is tabular, and relationships between features are not spatial or sequential.
# CNNs and RNNs are not suitable for this task as there are no image or sequential dependencies in the data.


model <- keras_model_sequential() %>%
  layer_dense(units = 512, activation = "relu", input_shape = ncol(x_train)) %>%
  layer_batch_normalization() %>%
  layer_dropout(rate = dropout_rate) %>%
  layer_dense(units = 256, activation = "relu", kernel_regularizer = regularizer_l2(0.001)) %>%
  layer_batch_normalization() %>%
  layer_dropout(rate = dropout_rate) %>%
  layer_dense(units = 128, activation = "relu", kernel_regularizer = regularizer_l2(0.001)) %>%
  layer_batch_normalization() %>%
  layer_dropout(rate = dropout_rate) %>%
  layer_dense(units = 64, activation = "relu", kernel_regularizer = regularizer_l2(0.001)) %>%
  layer_batch_normalization() %>%
  layer_dropout(rate = dropout_rate) %>%
  layer_dense(units = 32, activation = "relu", kernel_regularizer = regularizer_l2(0.001)) %>%
  layer_batch_normalization() %>%
  layer_dropout(rate = dropout_rate) %>%
  layer_dense(units = 16, activation = "relu", kernel_regularizer = regularizer_l2(0.001)) %>%
  layer_batch_normalization() %>%
  layer_dropout(rate = dropout_rate) %>%
  layer_dense(units = num_classes, activation = "softmax")


model %>% compile(
  optimizer = optimizer_adam(learning_rate = learning_rate),
  loss = "categorical_crossentropy",
  metrics = c("accuracy")
)

# Use early stopping
early_stopping <- callback_early_stopping(monitor = "val_loss", patience = 100)

lr_schedule <- callback_learning_rate_scheduler(
  schedule = function(epoch, lr) {
    if (epoch %% 50 == 0 && epoch != 0 && epoch >= 100) {
      return(lr * 0.75)
    }
    return(lr)
  }
)

history <- tryCatch({
  model %>% fit(
    x_train, y_train,
    epochs = epochs,
    batch_size = batch_size,
    validation_data = list(x_val, y_val),
    callbacks = list(early_stopping, lr_schedule),
    verbose = 1,
    class_weight = class_weights
  )
}, error = function(e) {
  stop(e)  # Stop execution for debugging
})


#------------------------- grid search  for better hyperparameter ------------------ 
# Function to create the model
create_model <- function(input_dim, num_classes, learning_rate, dropout_rate, regularization_value) {
  model <- keras_model_sequential() %>%
    layer_dense(units = 512, activation = "relu", input_shape = c(input_dim)) %>%
    layer_batch_normalization() %>%
    layer_dropout(rate = dropout_rate) %>%
    layer_dense(units = 256, activation = "relu", kernel_regularizer = regularizer_l2(regularization_value)) %>%
    layer_batch_normalization() %>%
    layer_dropout(rate = dropout_rate) %>%
    layer_dense(units = 128, activation = "relu", kernel_regularizer = regularizer_l2(regularization_value)) %>%
    layer_batch_normalization() %>%
    layer_dropout(rate = dropout_rate) %>%
    layer_dense(units = 64, activation = "relu", kernel_regularizer = regularizer_l2(regularization_value)) %>%
    layer_batch_normalization() %>%
    layer_dropout(rate = dropout_rate) %>%
    layer_dense(units = 32, activation = "relu", kernel_regularizer = regularizer_l2(regularization_value)) %>%
    layer_batch_normalization() %>%
    layer_dropout(rate = dropout_rate) %>%
    layer_dense(units = 16, activation = "relu", kernel_regularizer = regularizer_l2(regularization_value)) %>%
    layer_batch_normalization() %>%
    layer_dropout(rate = dropout_rate) %>%
    layer_dense(units = num_classes, activation = "softmax")
  
  model %>% compile(
    optimizer = optimizer_adam(learning_rate = learning_rate),
    loss = "categorical_crossentropy",
    metrics = c("accuracy")
  )
  
  return(model)
}

# Learning rate scheduler callback we will not use it here we have already 100 epochs for one run
lr_schedule <- callback_learning_rate_scheduler(
  schedule = function(epoch, lr) {
    if (epoch %% 50 == 0 && epoch != 0 && epoch >= 100) {
      return(lr * 0.75)
    }
    return(lr)
  }
)

# Callback to log learning rate and print parameters
learning_rate_tracker <- R6::R6Class(
  "LearningRateTracker",
  public = list(
    lr_values = numeric(0),
    on_epoch_end = function(epoch, logs = NULL) {
      lr <- model$optimizer$learning_rate %>% keras::k_eval()
      self$lr_values <- c(self$lr_values, lr)
      cat(sprintf("Epoch %d: Learning rate = %.6f\n", epoch + 1, lr))
    }
  )
)

# Hyperparameter grid (this hyperparameter are going to be used to find the best combination)
dropout_rates <- c(0.2, 0.3)
learning_rates <- c(0.001, 0.0005)
batch_sizes <- c(64, 128)
regularization_values <- c(0.001, 0.01)

# Prepare the data for cross-validation
x <- as.matrix(subset(data_cleaned_cross_corr, select = -status))
num_classes <- length(unique(data_cleaned_cross_corr$status))
data_cleaned_cross_corr$status <- as.integer(factor(data_cleaned_cross_corr$status)) - 1  # Ensure status starts from 0
y <- keras::to_categorical(data_cleaned_cross_corr$status, num_classes = num_classes)

# Results storage
results <- data.frame()

# Loop over hyperparameter combinations
for (dropout_rate in dropout_rates) {
  for (learning_rate in learning_rates) {
    for (batch_size in batch_sizes) {
      for (regularization_value in regularization_values) {
        
        # Print current combination of hyperparameters
        cat(sprintf(
          "Testing combination: dropout_rate = %.2f, learning_rate = %.5f, batch_size = %d, regularization_value = %.4f\n", 
          dropout_rate, learning_rate, batch_size, regularization_value
        ))
        
        # Train-validation split
        indices <- sample(1:nrow(x), size = 0.8 * nrow(x))
        x_train <- x[indices, ]
        y_train <- y[indices, ]
        x_val <- x[-indices, ]
        y_val <- y[-indices, ]
        
        # Create model with the current hyperparameters
        model <- create_model(
          input_dim = ncol(x),
          num_classes = ncol(y),
          learning_rate = learning_rate,
          dropout_rate = dropout_rate,
          regularization_value = regularization_value
        )
        
        # Create learning rate tracker
        lr_tracker <- learning_rate_tracker$new()
        
        # Train the model
        history <- model %>% fit(
          x_train, y_train,
          epochs = 100,
          batch_size = batch_size,
          validation_data = list(x_val, y_val),
          verbose = 1,
          callbacks = list(
            lr_schedule,
            callback_lambda(on_epoch_end = lr_tracker$on_epoch_end)
          )
        )
        
        # Plot learning rate
        plot(1:length(lr_tracker$lr_values), lr_tracker$lr_values, type = "l", 
             xlab = "Epoch", ylab = "Learning Rate", main = "Learning Rate Schedule")
        
        # Evaluate the model
        scores <- model %>% evaluate(x_val, y_val, verbose = 0)
        
        # Store results
        results <- rbind(results, data.frame(
          dropout_rate = dropout_rate,
          learning_rate = learning_rate,
          batch_size = batch_size,
          regularization_value = regularization_value,
          val_loss = scores["loss"],
          val_accuracy = scores["accuracy"]
        ))
      }
    }
  }
}

# Display the best parameters
best_params <- results[which.min(results$val_loss), ]
print("Best Hyperparameters:")
print(best_params)

#----------------------- learn best model and save -------------------
# print(best_params)
#dropout_rate learning_rate batch_size regularization_value val_loss val_accuracy
# 0.2         5e-04        128                0.001         0.605094    0.8486267


class_counts <- table(y)
total_samples <- sum(class_counts)
num_classes <- length(class_counts)

# Compute class weights inversely proportional to class frequencies
dynamic_class_weights <- total_samples / (num_classes * class_counts)

# Normalize weights for better balance (optional)
dynamic_class_weights <- dynamic_class_weights / max(dynamic_class_weights)

# Convert to a named list for Keras
class_weights <- as.list(as.numeric(dynamic_class_weights))

# Learning params
epochs <- 1000
batch_size <- 128
learning_rate <- 0.001
dropout_rate <- 0.2

# Feed-Forward Network (FFN) is chosen because the data is tabular, and relationships between features are not spatial or sequential.
# CNNs and RNNs are not suitable for this task as there are no image or sequential dependencies in the data.


model <- keras_model_sequential() %>%
  layer_dense(units = 512, activation = "relu", input_shape = ncol(x_train)) %>%
  layer_batch_normalization() %>%
  layer_dropout(rate = dropout_rate) %>%
  layer_dense(units = 256, activation = "relu", kernel_regularizer = regularizer_l2(0.001)) %>%
  layer_batch_normalization() %>%
  layer_dropout(rate = dropout_rate) %>%
  layer_dense(units = 128, activation = "relu", kernel_regularizer = regularizer_l2(0.001)) %>%
  layer_batch_normalization() %>%
  layer_dropout(rate = dropout_rate) %>%
  layer_dense(units = 64, activation = "relu", kernel_regularizer = regularizer_l2(0.001)) %>%
  layer_batch_normalization() %>%
  layer_dropout(rate = dropout_rate) %>%
  layer_dense(units = 32, activation = "relu", kernel_regularizer = regularizer_l2(0.001)) %>%
  layer_batch_normalization() %>%
  layer_dropout(rate = dropout_rate) %>%
  layer_dense(units = 16, activation = "relu", kernel_regularizer = regularizer_l2(0.001)) %>%
  layer_batch_normalization() %>%
  layer_dropout(rate = dropout_rate) %>%
  layer_dense(units = num_classes, activation = "softmax")


model %>% compile(
  optimizer = optimizer_adam(learning_rate = learning_rate),
  loss = "categorical_crossentropy",
  metrics = c("accuracy")
)

# Use early stopping
early_stopping <- callback_early_stopping(monitor = "val_loss", patience = 100)

lr_schedule <- callback_learning_rate_scheduler(
  schedule = function(epoch, lr) {
    if (epoch %% 50 == 0 && epoch != 0 && epoch >= 100) {
      return(lr * 0.75)
    }
    return(lr)
  }
)

history <- tryCatch({
  model %>% fit(
    x_train, y_train,
    epochs = epochs,
    batch_size = batch_size,
    validation_data = list(x_val, y_val),
    callbacks = list(early_stopping, lr_schedule),
    verbose = 1,
    class_weight = class_weights
  )
}, error = function(e) {
  stop(e)  # Stop execution for debugging
})

# Save the trained model after fitting
save_model_hdf5(model, "best_model.h5")
cat("Model saved successfully at 'best_model.h5'!\n")
# Training result
#370/370 [==============================] - 4s 10ms/step - loss: 0.1680 - accuracy: 0.9553 - val_loss: 0.3751 - val_accuracy: 0.9203 - lr: 1.7798e-04
