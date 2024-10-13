#####################################################
# Data Science - AS24
# DS-Assignment: Part1
# Part 1: Logistic Regression
# Magdalena Hardegger magdalena.hardegger@students.fhnw.ch
# Firat Turan firat.turan@students.fhnw.ch
# Sascha Frossard sascha.frossard@students.fhnw.ch
# Sebastian Fernandez sebastian.fernandez@students.fhnw.ch
#####################################################
# Assignment for DataScience Course at Olten, Switzerland
#####################################################


##############   PRELIMINARIES   ##########################
# Load the CSV file into R
data <- read.csv("ressources/LCdata.csv", sep = ";")


##############   Step 2 - Data Preprocessing   ##########################
#### Define the columns to be removed
columns_to_remove <- c("collection_recovery_fee", "installment", "funded_amnt", 
                       "funded_amnt_inv", "issue_d", "last_pymnt_amnt", 
                       "last_pymnt_d", "loan_status", "next_pymnt_d", 
                       "out_prncp", "out_prncp_inv", "pymnt_plan", 
                       "recoveries", "total_pymnt", "total_pymnt_inv", 
                       "total_rec_int", "total_rec_late_fee", "total_rec_prncp")

#### Manual removal


# Remove the specified columns from the dataset
data <- data[ , !(names(data) %in% columns_to_remove)]

# View first few rows of the dataset
head(data)
# Or view it as a table
View(data)

# View the structure of the dataset
str(data) 
# "name" is categorical (factor). 
# "origin" is numerical, but should be categorical (see data dictionary).

# Set threshold (e.g., remove columns with more than 50% NAs)
threshold <- 0.1

# Calculate the proportion of NAs in each column
na_percentage <- colMeans(is.na(data))

# Keep columns where the proportion of NAs is below the threshold
data_clean <- data[, na_percentage < threshold]

# Summary Statistics
summary(data_clean) 


# Missing data
sum(is.na(data_clean)) # no missing values

# Outliers

# Loop through each column in the data frame
for (colname in names(data_clean)) {
  # Check if the column is numeric
  if (is.numeric(data_clean[[colname]])) {
    # Create a histogram for the numeric column
    hist(data_clean[[colname]], 
         main = paste("Histogram of", colname), 
         xlab = colname, 
         col = "lightblue", 
         border = "black")
  }
}


############# 1st iteration: Fit a model to all variables #############

# For a start, we fit a regression model on ALL variables, to see what happens :-)
# ~. --> .(all the others attributes in the data), ~ things are linear related to 
lm.fit1 <- lm(mpg ~., data=data_clean)