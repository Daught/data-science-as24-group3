#####################################################
# Data Science - AS24
# DS-Assignment: Part1
# Part 1: Logistic Regression
# Magdalena Hardegger magdalena.hardegger@students.fhnw.ch
# Firat Turan firat.turan@students.fhnw.ch
# Sascha Frossard sascha.frossard@students.fhnw.ch
# Sebastian Fernandez sebastian.fernandez@students.fhnw.ch
#####################################################
# Assignment for DataScience Course at Olten
#####################################################

##############   PRELIMINARIES   ##########################


# Load the CSV file into R
data <- read.csv("ressources/LCdata.csv", sep = ";")


##############   Step 2 - Data Preprocessing   ##########################
# Define the columns to be removed
columns_to_remove <- c("collection_recovery_fee", "installment", "funded_amnt", 
                       "funded_amnt_inv", "issue_d", "last_pymnt_amnt", 
                       "last_pymnt_d", "loan_status", "next_pymnt_d", 
                       "out_prncp", "out_prncp_inv", "pymnt_plan", 
                       "recoveries", "total_pymnt", "total_pymnt_inv", 
                       "total_rec_int", "total_rec_late_fee", "total_rec_prncp")

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




