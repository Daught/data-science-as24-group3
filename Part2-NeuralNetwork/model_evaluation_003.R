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
# data seems fine. No missing values and all data is binary. Therefore factor encoding.
str(data$CODE_GENDER)
summary(data$CODE_GENDER)

data <- clean_column(data, "CODE_GENDER", is_ordered_factor = TRUE, order_levels=c("F", "M"))


# FLAG_OWN_CAR - Is there a car
# data seems fine. No missing values and all data is binary. Therefore factor encoding.
str(data$FLAG_OWN_CAR)
summary(data$FLAG_OWN_CAR)

data <- clean_column(data, "FLAG_OWN_CAR", is_ordered_factor = TRUE, order_levels=c("N", "Y"))

# FLAG_OWN_REALTY - Is there a property
# data seems fine. No missing values and all data is binary. Therefore factor encoding.
str(data$FLAG_OWN_REALTY)
summary(data$FLAG_OWN_REALTY)

data <- clean_column(data, "FLAG_OWN_REALTY", is_ordered_factor = TRUE, order_levels=c("N", "Y"))

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

max_income = quantile(data$AMT_INCOME_TOTAL,c(0.95))
data <- filter(data, AMT_INCOME_TOTAL <= max_income)

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

table(data$NAME_EDUCATION_TYPE)


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
# data <- subset(data, select = -FLAG_WORK_PHONE)


# FLAG_PHONE - Is there a phone
summary(data$FLAG_PHONE)
# No NAs. Data is already between 0 and 1. No changes required.
# data <- subset(data, select = -FLAG_PHONE)


# FLAG_EMAIL - Is there an email
summary(data$FLAG_EMAIL)
# No NAs. Data is already between 0 and 1. No changes required.
# data <- subset(data, select = -FLAG_EMAIL)


# OCCUPATION_TYPE - Occupation
summary(as.factor(data$OCCUPATION_TYPE))
# There are 20699 NA's which is about a third of the total rows. Create new category missing.
data$OCCUPATION_TYPE[is.na(data$OCCUPATION_TYPE)] <- "Missing"
data <- clean_column(data, "OCCUPATION_TYPE", onehot_encode = TRUE)
# data <- subset(data, select = -OCCUPATION_TYPE)

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
data$status <- case_when(data$status == "C" ~ 7,
                         data$status == "X" ~ 6,
                         data$status == "0" ~ 0,
                         data$status == "1" ~ 1,
                         data$status == "2" ~ 2,
                         data$status == "3" ~ 3,
                         data$status == "4" ~ 4,
                         data$status == "5" ~ 5)


# Plot the correlation matrix and save because of many rows it is easier to read.
correlation_matrix <- cor(data)
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

y <- data_cleaned_cross_corr$status  # Target variable
X <- data_cleaned_cross_corr[, -which(names(data_cleaned_cross_corr) == "status")]  # Features

table(data_cleaned_cross_corr$status)


#----------------------------Train model --------------------------- transfer in other file 
# Load necessary libraries
# Install necessary libraries if not already installed
library(keras)
library(tensorflow)
# install_tensorflow(version = "2.15.0")


#tf$constant("Hello TensorFlow!")


# Ensure reproducibility
set.seed(1)


#--------------------cv-------------------------
library(caret)

# Make sure that each class gets split individually
# Take a bit more training from classes 2-5 with low representation

data_0 <- filter(data_cleaned_cross_corr, status==0)
train_0_indices <- sample(1:nrow(data_0), size = 0.8 * nrow(data_0))
train_0 <- data_0[train_0_indices, ]
test_0 <- data_0[-train_0_indices, ]


data_1 <- filter(data_cleaned_cross_corr, status==1)
train_1_indices <- sample(1:nrow(data_1), size = 0.0 * nrow(data_1))
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

x_train <- as.matrix(subset(train, select=-status))
y_train <- keras::to_categorical(train$status)
x_val <- as.matrix(subset(test, select=-status))
y_val <- keras::to_categorical(test$status)


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
dropout_rate <- 0.3

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

