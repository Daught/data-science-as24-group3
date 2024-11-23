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
    
    if (onehot_encode){
      data <- dummy_cols(data, select_columns=column_name, remove_selected_columns = TRUE)
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


# Plot the correlation matrix and save because of many rows it is easier to read.
correlation_matrix <- cor(data)
png(filename = "raw_data_corrplot.png", width = 2000, height = 2000)
corrplot(correlation_matrix, method = "color", type = "upper", tl.col = "black", addCoef.col = "black",number.cex = 1, tl.cex = 1)
dev.off()
