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


##############   1. Preliminaries   ##########################

# Install
if(!require('corrplot')) install.packages('corrplot', dependencies = TRUE)
if(!require('ggplot2')) install.packages('ggplot2', dependencies = TRUE)
if(!require('tidyverse')) install.packages('tidyverse', dependencies = TRUE)
if(!require('fastDummies')) install.packages('fastDummies', dependencies = TRUE)
if(!require('GGally')) install.packages('GGally', dependencies = TRUE)
if(!require('dplyr')) install.packages('dplyr', dependencies = TRUE)
if(!require("caret")) install.packages("caret", dependencies = TRUE)
if(!require("VIM")) install.packages("VIM", dependencies = TRUE)

# Load packages
library('dplyr')
library('caret')
library('GGally')
library('fastDummies')
library('tidyverse')
library('ggplot2')
library('corrplot')
library('VIM')  # For kNN imputation

# load data set
#data <- read.csv2("ressources/LCdata.csv", header = TRUE, row.names=NULL, sep=";")
dataset_file_path = "./ressources/LCdata.csv"
data <- read.csv2(dataset_file_path, header = TRUE, row.names=NULL, sep=";")

LC <- data # make a copy of the original data set, so that we dont mess with it

##########################   2. Data Exploration   ##########################
head(LC)

str(LC)
  # 322 observations and 20 variables.
  # Data types seem to correspond to scale levels.

summary(LC)
  # Missing values
  #   - Salary: 59 NAs (18% of the data set)
  #   - Complete otherwise


##############   Step 2 - Data Preprocessing   ##########################

clean_column <- function(data, column_name, k = 5, outlier_factor = 1.5, is_ordered_factor = FALSE, remove_outliers=TRUE) {
  
 # Step 1: Remove rows with missing values in the specified column only
  data <- data[!is.na(data[[column_name]]), ]
  
  # Step 2: Impute missing values only in the specified column (if any remain after filtering)
  if (is.numeric(data[[column_name]])) {
    # Impute missing values with mean for numeric columns
    # data[[column_name]][is.na(data[[column_name]])] <- mean(data[[column_name]], na.rm = TRUE)
    
    # Apply kNN imputation to the specified column if it has missing values
    if (any(is.na(data[[column_name]]))) {
      data[[column_name]] <- kNN(data[, column_name, drop = FALSE], variable = column_name, k = k, imp_var = FALSE)[[column_name]]
    }
  } else if (is.character(data[[column_name]])) {
    # Replace missing values with "Unknown" for character columns
    data[[column_name]][is.na(data[[column_name]])] <- "Unknown"
    # Convert to factor
    data[[column_name]] <- as.factor(data[[column_name]])
  }
  
  # Step 3: Convert to ordered factor if specified and if it is a factor
  if (is_ordered_factor && is.factor(data[[column_name]])) {
    data[[column_name]] <- factor(data[[column_name]], ordered = TRUE)
  }
  
  # Step 4: Check if the column is a single-level factor and throw an error if so
  if (is.factor(data[[column_name]]) && nlevels(data[[column_name]]) <= 1) {
    stop("Specified column has only one level after cleaning, which may be insufficient for regression.")
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
    data[[column_name]] <- scale(data[[column_name]]) %>% as.numeric()
  }
  
  return(data)
}

# Feature Descriptions:
# 1. id: This is a unique identifier for each loan listing. Although it's essential for tracking loans, it holds no predictive value 
#    for analysis. We'll check for any anomalies (missing, zero, or negative values) and remove it as it's irrelevant for our analysis.

# Dropping 'id' column since it's not useful for the decision-making process
LC_Cleaned <- subset(LC, select = -id)


# 2. member_id: This represents the unique identifier for each borrower (member). It could potentially be useful in tracking multiple 
#    loans for the same borrower, but in this dataset, there's no evidence of multiple loans per member. We'll remove it after verifying for any anomalies.

# Since it’s not relevant, we'll remove 'member_id'
LC_Cleaned <- subset(LC_Cleaned, select = -member_id)


# 3. loan_amnt: This column records the amount of money requested by the borrower. It's an essential feature for analysis and 
#    should be non-negative, non-zero, and without missing values. We’ll explore its distribution and check for any irregularities.

# DO NOTHING


# 4. funded_amnt: This column shows the amount of money that has been funded for the loan at the current point. It's essential to analyze 
#    the progress of loan funding. We will check for anomalies and compare it with the loan amount requested.\
#    not available in the Test data!!!
LC_Cleaned <- subset(LC_Cleaned, select = -funded_amnt)


# 5. funded_amnt_inv: This represents the amount funded by investors for the loan. It's currently stored as a character but 
#    should be converted to an integer for analysis. We’ll also explore how this compares to the total loan amount requested.
#    not available in the Test data!!!

LC_Cleaned <- subset(LC_Cleaned, select = -funded_amnt_inv)


# 6. term: This column specifies the length of the loan in months (either 36 or 60 months). It is essentially a categorical variable, 
#    so we’ll trim any unnecessary spaces and explore how the term affects loan amounts and funding.

LC_Cleaned$term <- str_trim(LC_Cleaned$term)  # Trimming unnecessary spaces


# 7. int_rate: The interest rate assigned to the loan. This is a key feature for analysis.
#    It's originally stored as a character, but should be converted to a float (double).
#    We'll also check for missing values and explore its distribution.

# Convert 'int_rate' to a numeric format
LC_Cleaned$int_rate <- as.double(LC_Cleaned$int_rate)


# 8. installment: The monthly installment the borrower needs to pay.
# Might have a direct correlation with interest rate and loan amount
#     We'll convert it to double and remove the feature.
#    not available in the Test data!!!
LC_Cleaned <- subset(LC_Cleaned, select = -installment)


# 9. emp_title: The borrower’s job title, provided during the application.
#    This is a free-text field that is not useful for predictive analysis, so it will be dropped.
#   * Employer Title replaces Employer Name for all loans listed after 9/23/2013
# Drop 'emp_title' as it's not useful for analysis
LC_Cleaned <- subset(LC_Cleaned, select = -emp_title)


# 10. emp_length: The number of years the borrower has been employed.
#    Convert this from string to integer and check for missing values.
# Convert 'emp_length' to integer
LC_Cleaned$emp_length <- as.integer(ordered(LC_Cleaned$emp_length, levels = c("< 1 year", "1 year", "2 years", "3 years", "4 years", "5 years", "6 years", "7 years", "8 years", "9 years", "10+ years"))) - 1
# Drop 'emp_title' as it's not useful for analysis
LC_Cleaned <- subset(LC_Cleaned, select = -emp_length)


# 11. home_ownership: Indicates whether the borrower owns, rents, or has a mortgage on their home.
#     Convert this to an ordered set of integers and create dummy columns for categorical analysis.
#     We have 47 missing values.. to OTHER? Or shall we drop them? only

# these values are assigned to Other (Other is unclear as well.)
# Replace "ANY" and "NONE" with "OTHER" in the home_ownership column
LC_Cleaned$home_ownership[LC_Cleaned$home_ownership %in% c("ANY", "NONE")] <- "OTHER"

# Convert 'home_ownership' to ordered integer
LC_Cleaned$home_ownership <- as.integer(ordered(LC_Cleaned$home_ownership, levels = c("OTHER", "RENT", "MORTGAGE", "OWN")))

# Create dummy columns for 'home_ownership'
LC_Cleaned <- dummy_columns(LC_Cleaned, select_columns = "home_ownership", remove_selected_columns = TRUE)


# 12. annual_inc: The annual income reported by the borrower during registration.
#     Convert from string to integer, check for missing, zero, or extreme values, and handle any income outliers.

# TODO-Firat: might be impactfull feature, might need some adjustments
# Remove NA values
LC_Cleaned <- filter(LC_Cleaned, !is.na(annual_inc))
# Remove rows with negative values in 'annual_inc'
LC_Cleaned <- filter(LC_Cleaned, annual_inc >= 0)
# Convert 'annual_inc' to integer
LC_Cleaned$annual_inc <- as.integer(LC_Cleaned$annual_inc)


# 13. verification_status: Indicates whether the borrower’s income has been verified.
#     Convert this to an integer and analyze its relationship to the interest rate.

# TODO-FiratXsebastian: merge both cells joint_verification_status & verification_status
# TODO: check whether high loan_amount are always verified

# Convert 'verification_status' to integer
LC_Cleaned$verification_status <- as.integer(factor(LC_Cleaned$verification_status)) - 1


# 14. issue_d: The date when the loan was funded.
#     Not part of Test data!!!
# Drop 'issue_d' as it won't be relevant for future data
LC_Cleaned <- subset(LC_Cleaned, select = -issue_d)


# 15. loan_status: The current status of the loan (e.g., fully paid, defaulted). Not part of test data!!!!
#     This is only applicable post-loan issuance, so it’s not useful for predicting interest rates. We'll drop it.
# Drop 'loan_status' as it’s not relevant for the analysis
LC_Cleaned <- subset(LC_Cleaned, select = -loan_status)


# 16. pymnt_plan: Indicates if a payment plan is in place for the loan.
#     This is not part of the test data!!, so it will be dropped.
# Drop 'pymnt_plan' as it's irrelevant for the analysis
LC_Cleaned <- subset(LC_Cleaned, select = -pymnt_plan)


# 17. url: The URL for the loan listing on the LC platform.
#     This feature has no analytical value, so it will be dropped.
# Drop 'url' as it’s irrelevant for analysis
LC_Cleaned <- subset(LC_Cleaned, select = -url)


# 18. desc: A text field describing the loan provided by the borrower.
#     This feature is difficult to analyze, so we will drop it.
# Drop 'desc' as it is not suitable for analysis
LC_Cleaned <- subset(LC_Cleaned, select = -desc)


# 19. 'purpose': A category provided by the borrower for the loan request.
# Investigate $purpose, (character)
str(LC_Cleaned$purpose)
unique(LC_Cleaned$purpose) # "other", "debt_consolidation", "moving", "credit_card", "home_improvement", "major_purchase", "wedding", "small_business", "medical", "car", "educational", "vacation", "house", "renewable_energy"
length(unique(LC_Cleaned$purpose)) # 14 unique values
LC_Cleaned$purpose <- tolower(LC_Cleaned$purpose) # Convert all characters in the column to lowercase
# Make sure that there are no empty strings
sum(LC_Cleaned$purpose == "") # 0
sum(LC_Cleaned$purpose == " ") # 0
LC_Cleaned$purpose <- factor(LC_Cleaned$purpose) # Change to unordered factor
sum(is.na(LC_Cleaned$purpose)) # no missing values

# 20. 'title': The loan title provided by the borrower.
# Investigate $title, (character)
str(LC_Cleaned$title) # character
unique(LC_Cleaned$title)
length(unique(LC_Cleaned$title)) # 57719
# To lower case
LC_Cleaned$title <- tolower(LC_Cleaned$title)
# Remove all whitespace
LC_Cleaned$title <- gsub(" ", "", LC_Cleaned$title)
length(unique(LC_Cleaned$title)) # from 57719 to 48722 unique values, but still!!!
# As it contains similar information to 'purpose' in free text and thus harder to analyze format we will drop this.
LC_Cleaned <- subset(LC_Cleaned, select = -title)

# 21. 'zip_code': The first 3 numbers of the zip code provided by the borrower in the loan application.

# TODO-MHA: gsub only the (first, second) number --> make categorical
str(LC_Cleaned$zip_code)
unique(LC_Cleaned$zip_code) # e.g.: "806xx" "100xx" "665xx" "068xx" "300xx"
LC_Cleaned$zip_code <- gsub("xx", "", LC_Cleaned$zip_code) # Remove 'xx'
# Extract the first two characters of each zip code
LC_Cleaned$zip_code <- substr(LC_Cleaned$zip_code, 1, 2)
length(unique(LC_Cleaned$zip_code)) # 100
sum(LC_Cleaned$zip_code == "00") # 0
sum(is.na(LC_Cleaned$zip_code)) # no missing values

# As more reducing does not make sense (too big and not necessarily logical connected areas with only 2 digits) we will drop it and use 'addr_state' instead
LC_Cleaned <- subset(LC_Cleaned, select = -zip_code)

# 22. 'addr_state': The state provided by the borrower in the loan application.

unique(LC_Cleaned$addr_state) # e.g.: ""CO" "NY" "KS" "CT" "GA" "MO" "SD" "FL" "OK" "IN" "PA" "OR" "OH" "TX" "WV" "NM""
length(unique(LC_Cleaned$addr_state)) # 51 but only 50 states??? DC is included as an extra state
sum(is.na(LC_Cleaned$addr_state)) # no missing values
table(LC_Cleaned$addr_state)
# Count and sort the values
counts <- table(LC_Cleaned$addr_state)
sorted_value_counts <- sort(counts, decreasing = TRUE)
print(sorted_value_counts)
# Look for empty strings
sum(LC_Cleaned$addr_state == "") # 0
sum(LC_Cleaned$addr_state == " ") # 0

# As 51 states handling as a factor is no option. We will map it per region in the US according to this map 
# (https://stock.adobe.com/fr/images/united-states-geographic-regions-colored-political-map-five-regions-according-to-their-geographic-position-on-the-continent-common-but-unofficial-way-of-referring-to-regions-of-the-united-states/514824675)
# Define the mapping of state abbreviations to regions as a named vector
state_to_region <- c(
  'AL' = 'Southeast', 'AK' = 'West', 'AZ' = 'Southwest', 'AR' = 'Southeast',
  'CA' = 'West', 'CO' = 'West', 'CT' = 'Northeast', 'DE' = 'Northeast',
  'DC' = 'Northeast', 'FL' = 'Southeast', 'GA' = 'Southeast', 'HI' = 'West',
  'ID' = 'West', 'IL' = 'Midwest', 'IN' = 'Midwest', 'IA' = 'Midwest',
  'KS' = 'Midwest', 'KY' = 'Southeast', 'LA' = 'Southeast', 'ME' = 'Northeast',
  'MD' = 'Northeast', 'MA' = 'Northeast', 'MI' = 'Midwest', 'MN' = 'Midwest',
  'MS' = 'Southeast', 'MO' = 'Midwest', 'MT' = 'West', 'NE' = 'Midwest',
  'NV' = 'West', 'NH' = 'Northeast', 'NJ' = 'Northeast', 'NM' = 'Southwest',
  'NY' = 'Northeast', 'NC' = 'Southeast', 'ND' = 'Midwest', 'OH' = 'Midwest',
  'OK' = 'Southwest', 'OR' = 'West', 'PA' = 'Northeast', 'RI' = 'Northeast',
  'SC' = 'Southeast', 'SD' = 'Midwest', 'TN' = 'Southeast', 'TX' = 'Southwest',
  'UT' = 'West', 'VT' = 'Northeast', 'VA' = 'Southeast', 'WA' = 'West',
  'WV' = 'Southeast', 'WI' = 'Midwest', 'WY' = 'West'
)

# Remove any leading/trailing whitespace from addr_state
LC_Cleaned$addr_state <- trimws(LC_Cleaned$addr_state)

# Create a new column 'region' by mapping each state to its region
LC_Cleaned$region <- state_to_region[LC_Cleaned$addr_state]

# Verify the result
table(LC_Cleaned$region)
LC_Cleaned$region <- factor(LC_Cleaned$region) # Change to factor
# Be sure there are no NAs
sum(is.na(LC_Cleaned$addr_state)) # no missing values

# Drop addr_state column as we will 'region' instead
LC_Cleaned <- subset(LC_Cleaned, select = -addr_state)


# 23. 'dti': A ratio calculated using the borrower’s total monthly debt payments on the total debt obligations, 
# excluding mortgage and the requested LC loan, divided by the borrower’s self-reported monthly income.
# A lower DTI generally indicates a more manageable debt load relative to income, suggesting the borrower is less risky.
# A higher DTI suggests a higher burden of debt relative to income, which may indicate greater financial strain or risk.
## This needs to be differentiated with application type ("INDIVIDUAL" --> "dti" and "JOINT" --> "dti_joint")
str(LC_Cleaned$dti)
unique(LC_Cleaned$dti)
# Handle empty strings, if there are some
LC_Cleaned$dti[LC_Cleaned$annual_inc == ""] <- NA
sum(is.na(LC_Cleaned$dti))
# Change to number
LC_Cleaned$dti <- as.numeric(LC_Cleaned$dti)
summary(LC_Cleaned$dti)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 0.00   11.91   17.66   18.16   23.95 9999.00

# Find the maximum value
max_value <- max(LC_Cleaned$dti, na.rm = TRUE)

# Check how many times the maximum value occurs
max_count <- sum(LC_Cleaned$dti == max_value)
print(max_count)  # 2

# There are 2 values with 9999.00 --> look for there rows (duplicates??)

# Define the value you are looking for
max_value_to_find <- 150
# Find the row indices where dti is greater than the value
row_indices_max <- which(LC_Cleaned$dti > max_value_to_find)
# Display the rows with this value
data_with_max_value <- LC_Cleaned[row_indices_max, ]
print(data_with_max_value) # no duplicates, but annual-inc 0, both are application_type "JOINT" and have annual_inc_joint 185000 resp. 40988
# Display the rows with this dti value and retrieve only the application_type column
application_types_with_high_dti <- LC_Cleaned[row_indices_max, "application_type"]
# Print the application_type values for these rows
print(application_types_with_high_dti)

# Same for very low values
min_value_to_find <- 0
# Find the row indices where column_num equals the value
row_indices_min <- which(LC_Cleaned$dti == min_value_to_find)
# Display the rows with this value
data_with_min_value <- LC_Cleaned[row_indices_min, ]

# Display the rows with this dti value and retrieve only the application_type column
application_types_with_low_dti <- LC_Cleaned[row_indices_min, "application_type"]
# Print the application_type values for these rows
print(application_types_with_low_dti)


# 24. 'delinq_2yrs': The number of 30+ days past-due incidences of delinquency in the borrower's credit file for the past 2 years.
# TODO: MHA: please check again regarding NA's (odd that it only has 25...) CHECK: NAs: 21 --> imputed 0 for them.
# Investigate $delinq_2yrs (integer)
summary(LC_Cleaned$delinq_2yrs)
unique(LC_Cleaned$delinq_2yrs)
sum(is.na(LC_Cleaned$delinq_2yrs)) # 21 NAs
# Calculate the frequency of each unique value in delinq_2yrs and sort by frequency
delinq_counts <- sort(table(LC_Cleaned$delinq_2yrs), decreasing = TRUE)
# Display the result
print(delinq_counts)

# Find the row indices where NAs are
NA_indices_delinq_2yrs <- which(is.na(LC_Cleaned$delinq_2yrs))
# Display the rows with NAs
delinq_2yrs_with_NAvalue <- LC_Cleaned[NA_indices_delinq_2yrs, ]
print(delinq_2yrs_with_NAvalue) # multiple NA-columns
# Display the rows with 'delinq_2yrs'NAs and retrieve only the application_type column
application_types_delinq_2yrs_with_NAvalue <- LC_Cleaned[NA_indices_delinq_2yrs, "application_type"]
print(application_types_delinq_2yrs_with_NAvalue) # Only 'INDIVIDUAL' applications
  
# Find application types of values over 0
delinq_2yrs_present <- 0
# Find the row indices where 'delinq_2yrs' is over 0
row_delinq_2yrs_present <- which(LC_Cleaned$delinq_2yrs > delinq_2yrs_present)
# Display the rows with this value
data_with_delinq_2yrs_present <- LC_Cleaned[row_delinq_2yrs_present, ]
print(data_with_delinq_2yrs_present) #
# Display the rows with this 'delinq_2yrs' value and retrieve only the application_type column
application_types_delinq_2yrs_present <- LC_Cleaned[row_delinq_2yrs_present, "application_type"]
# Print the application_type values for these rows
print(application_types_delinq_2yrs_present)
unique(application_types_delinq_2yrs_present)
table(application_types_delinq_2yrs_present)
  # INDIVIDUAL      JOINT 
  #    153351        103
  # A separation of 'INDIVIDUAL' and 'JOINT' and merging makes sense here as well
  
# Impute NA values in delinq_2yrs with 0
LC_Cleaned$delinq_2yrs[is.na(LC_Cleaned$delinq_2yrs)] <- 0

# 25. 'earliest_cr_line': The month the borrower's earliest reported credit line was opened.

# Remove any leading or trailing whitespace and any non-printing characters
LC_Cleaned$earliest_cr_line <- trimws(LC_Cleaned$earliest_cr_line)
LC_Cleaned$earliest_cr_line <- gsub("[^[:print:]]", "", LC_Cleaned$earliest_cr_line)

# Set locale to English for date conversion to be sure date-conversion is working
Sys.setlocale("LC_TIME", "C")  # "C" is used for the default English locale

# Replace any empty strings with NA
LC_Cleaned$earliest_cr_line[LC_Cleaned$earliest_cr_line == ""] <- NA
sum(is.na(LC_Cleaned$earliest_cr_line)) # 25 NAs
str(LC_Cleaned$earliest_cr_line)
# Convert earliest_cr_line from "Mon-YYYY" to Date format, assuming the first day of the month
LC_Cleaned$earliest_cr_line_date <- as.Date(paste0("01-", LC_Cleaned$earliest_cr_line), format = "%d-%b-%Y")

# Display the rows to be sure this worked correctly
head(LC_Cleaned[, c("earliest_cr_line", "earliest_cr_line_date")], 10)

# Conversion to character and then numerical for analysis
LC_Cleaned$earliest_cr_line_char <- format(LC_Cleaned$earliest_cr_line_date, "%Y%m")
LC_Cleaned$earliest_cr_line_numeric <- as.numeric(LC_Cleaned$earliest_cr_line_char)

# Display the first few rows to verify
head(LC_Cleaned[, c("earliest_cr_line_date", "earliest_cr_line_char", "earliest_cr_line_numeric")], 10)

# Remove the last two digits by dividing by 100 and taking the integer part to get only the year
LC_Cleaned$earliest_cr_line_year <- LC_Cleaned$earliest_cr_line_numeric %/% 100

summary(LC_Cleaned$earliest_cr_line_numeric)
summary(LC_Cleaned$earliest_cr_line_year)

# Drop rows with NA in the 'earliest_cr_line_numeric' column
LC_Cleaned <- LC_Cleaned[!is.na(LC_Cleaned$earliest_cr_line_numeric), ]


# 26. 'inq_last_6mths': The number of inquiries in past 6 months (excluding auto and mortgage inquiries)data.frame(..., row.names = NULL, check.rows = FALSE, check.names = TRUE, stringsAsFactors = default.stringsAsFactors())
summary(LC_Cleaned$inq_last_6mths) #    Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
#                                     0.0000  0.0000  0.0000  0.6947  1.0000 33.0000      25 
unique(LC_Cleaned$inq_last_6mths) # e.g.: NA  0  1  4  2  3  5 
sum(is.na(LC_Cleaned$inq_last_6mths)) #  25 NAs on original dataset, same rows as NAs already dropped from earliest_cr_line


# 27. mths_since_last_delinq ... The number of months since the borrower's last delinquency.
# A value of 12 would indicate that the borrower’s last delinquency was 12 months ago (one year).
# A value of 0 would indicate that a delinquency occurred within the current month.

summary(LC_Cleaned$mths_since_last_delinq)
unique(LC_Cleaned$mths_since_last_delinq) # e.g.: NA   0  66  62  28  37  78   8  23  44 
sum(is.na(LC_Cleaned$mths_since_last_delinq)) #  408818 NA
  
# We will impute NAs with a very high number as low numbers indicate more risk for the lender.
# Replace NA values with 999
LC_Cleaned$mths_since_last_delinq[is.na(LC_Cleaned$mths_since_last_delinq)] <- 999
sum(is.na(LC_Cleaned$mths_since_last_delinq)) # 0


# 28. mths_since_last_record ... The number of months since the last public record.
# A specific number (e.g., 12) means the last public record was 12 months ago.
# A 0 indicates a public record within the current month

str(LC_Cleaned$mths_since_last_record)
unique(LC_Cleaned$mths_since_last_record) # e.g.: NA   0  95 111  57  76 102 115  54 106  86  19  61   
sum(is.na(LC_Cleaned$mths_since_last_record)) #  675190 NA
summary(LC_Cleaned$mths_since_last_record)
  
# As NA normally indicates no public record history we impute with a very high number
LC_Cleaned$mths_since_last_record[is.na(LC_Cleaned$mths_since_last_record)] <- 999
sum(is.na(LC_Cleaned$mths_since_last_record)) # 0


# 29. open_acc ... The number of open credit lines in the borrower's credit file.
# A value of 5 means the borrower has five open lines of credit. --> higher risk
# A value of 0 indicates that the borrower currently has no active lines of credit. --> lower risk
# Is it possible that there are people with 90 lines of credit open?

# Investigate $open_acc (int)
summary(LC_Cleaned$open_acc)
unique(LC_Cleaned$open_acc) # e.g.: NA  7  9  8  5 12  4 17 10 13 11   
sum(is.na(LC_Cleaned$open_acc)) #  25 NA, disappeared as NAs were dropped before


# 30. pub_rec ... Number of derogatory public records. Derogatory public records are negative financial events that are publicly accessible and can significantly impact a borrower’s creditworthiness.
# A value of 0 means the borrower has no derogatory public records in their credit file.
# A value of 1 or higher indicates the number of derogatory public records recorded against the borrower.
# Higher values suggest a history of multiple financial issues

summary(LC_Cleaned$pub_rec)
unique(LC_Cleaned$pub_rec) # e.g.: NA  0  1  2  3  5  9    
sum(is.na(LC_Cleaned$pub_rec)) #  25 NA, disappeared after deleting NA rows before


# 31. revol_bal ... Total credit revolving balance.
# A high revol_bal could indicate a borrower is utilizing a lot of available revolving credit, potentially signaling a higher risk if they are close to maxing out their credit limits.
# A low or zero revol_bal shows minimal use of revolving credit, which can be favorable, though some utilization is typically seen positively as it shows credit activity.
## This needs NA handling. Drop rows? Impute with 0 or mean or knn. By now the NAs will be filled with 0

summary(LC_Cleaned$revol_bal)
unique(LC_Cleaned$revol_bal) # e.g.: 0 150786  11608   9204   5046   6360  16842   4429 (>70445)    
sum(is.na(LC_Cleaned$revol_bal)) #  2 NA
# Find the row indices NAs
column_to_search <- LC_Cleaned$revol_bal
NA_indices <- which(is.na(column_to_search))
# Display the rows with NAs
data_with_NAvalue <- LC_Cleaned[NA_indices, ]
print(data_with_NAvalue)
# Replace NA values with 0
LC_Cleaned$revol_bal[is.na(LC_Cleaned$revol_bal)] <- 0
sum(is.na(LC_Cleaned$revol_bal))

# 32. revol_util ... Revolving line utilization rate, or the amount of credit the borrower is using relative to all available revolving credit.
# A low revol_util (e.g., 0-30%) indicates that the borrower is using a small portion of their available credit, which is generally seen positively in credit scoring.
# A high revol_util (e.g., > 50%) suggests that the borrower is utilizing a large amount of their credit limit, which can indicate higher risk.
# 100% or higher means the borrower has maxed out or exceeded their credit limit, which can be a significant risk factor.
## This needs NA handling. Drop rows? Impute with 0 or mean or knn. By now the NAs will be filled with 0

summary(LC_Cleaned$revol_util)
# Handle empty strings
LC_Cleaned$revol_util[LC_Cleaned$revol_util == ""] <- NA
sum(is.na(LC_Cleaned$revol_util))
# Transform to integer
LC_Cleaned$revol_util <- as.integer(LC_Cleaned$revol_util)
summary(LC_Cleaned$revol_util)
unique(LC_Cleaned$revol_util) # e.g.: NA   2.20  56.90  29.30  19.90  25.00    
sum(is.na(LC_Cleaned$revol_util)) #  454 NA, changed to 429 as rows already were dropped
# Find the row indices NAs
column_to_search <- LC_Cleaned$revol_util
NA_indices <- which(is.na(column_to_search))
# Display the rows with NAs
data_with_NAvalue <- LC_Cleaned[NA_indices, ]
print(data_with_NAvalue)
  
# Replace NA values with 0
LC_Cleaned$revol_util[is.na(LC_Cleaned$revol_util)] <- 0
sum(is.na(LC_Cleaned$revol_util))


# 33. total_acc ... The total number of credit lines currently in the borrower's credit file.
# A specific number in total_acc indicates the total number of accounts that have ever been opened in the borrower’s name.
# For example, a value of 10 means the borrower has opened ten credit accounts over time, regardless of whether they are currently open or closed.
# This could indicate a higher experience with credit, which could be positive.

# Investigate $total_acc (int)
summary(LC_Cleaned$total_acc)
unique(LC_Cleaned$total_acc) # e.g.: NA  16  19  43  28  20  35  33  26     
sum(is.na(LC_Cleaned$total_acc)) #  25 NA, disappeared by deletion before


# 34. initial_list_status ... The initial listing status of the loan. Possible values are – W, F
# TODO-MHA: Transform capital W to lower-w
# Investigate $initial_list_status, (character)
unique(LC_Cleaned$initial_list_status) # e.g.: ""f" "w""
# Convert to lowercase and remove any whitespace for possible mistakes in the test-data
LC_Cleaned$initial_list_status <- tolower(gsub("\\s+", "", LC_Cleaned$initial_list_status))
# Verify no maltransformation
unique(LC_Cleaned$initial_list_status)
sum(is.na(LC_Cleaned$initial_list_status)) # no missing values
summary(LC_Cleaned$initial_list_status)
table(LC_Cleaned$initial_list_status) # f: 411119  w: 387497, seems to be balanced
# Change to factor
LC_Cleaned$initial_list_status <- factor(LC_Cleaned$initial_list_status)
# Make sure it worked
str(LC_Cleaned$initial_list_status)
summary(LC_Cleaned$initial_list_status)


# 35. out_prncp ... Remaining outstanding principal for total amount funded.
LC_Cleaned <- subset(LC_Cleaned, select = -out_prncp)


# 36. out_prncp_inv ... Remaining outstanding principal for portion of total amount funded by investors.
LC_Cleaned <- subset(LC_Cleaned, select = -out_prncp_inv)


# 37. total_pymnt: Payments received to date for total amount funded.
# This column tracks the repayment behaviour of the borrower, it is likely not a factor
# that would directly determine the interest rate at the time othe loan is issued. The interest rate is usually set at the beginning of the loan based on the
# borrowers credit risk and financial history, not on how much the borrower has paid over time.
# Conclusion:
# The interest rate is generally determined based on risk factors before loan issuance, such as creditworthiness, income, loan amount, and loan term.
# total_pymnt reflects payment history, which happens after the loan has been issued, so it wouldn’t be a factor used to set the original interest rate.
# Not available in the Test data!!!
LC_Cleaned <- subset(LC_Cleaned, select = -total_pymnt)


# 38. total_pymnt_inv: 
# reflects post-load payment behaviour, whereas the interest rate is determined before any payments are made.
# Not available in the Test data!!!
LC_Cleaned <- subset(LC_Cleaned, select = -total_pymnt_inv)


# 39. total_rec_prncp: Principal received to date
# total_rec_prncp tracks the amount of principal repaid after the loan has been issued, whereas the interest rate is determined before any payments are made.
# Not available in the Test data!!!
LC_Cleaned <- subset(LC_Cleaned, select = -total_rec_prncp)


#  40. total_rec_int: Interest received to date
# total_rec_int reflects post-loan repayment behavior and simply tracks the interest that has been paid so far. 
# Since the interest rate is already set when the loan is originated, total_rec_int is an outcome of the interest rate, not a factor influencing its determination.
# Not available in the Test data!!!
LC_Cleaned <- subset(LC_Cleaned, select = -total_rec_int)


# 41. total_rec_late_fee: Late fees received to date
# total_rec_late_fee tracks borrower behavior during the loan repayment process but does not provide information relevant to determining the interest rate at loan origination
# Not available in the Test data!!!
LC_Cleaned <- subset(LC_Cleaned, select = -total_rec_late_fee)


# 42. recoveries: post charge off gross recovery
# recoveries is a post-loan feature that captures the amount recovered after the loan has defaulted,
# Not available in the Test data!!!
LC_Cleaned <- subset(LC_Cleaned, select = -recoveries)


# 43. collection_recovery_fee: post charge off collection fee
# collection_recovery_fee represents a post-loan event related to recovering funds from a borrower after they have defaulted or missed payments
# Not available in the Test data!!!
LC_Cleaned <- subset(LC_Cleaned, select = -collection_recovery_fee)


# 44. last_pymnt_d: Last month payment was received
# last_pymnt_d records the date of the most recent payment, which occurs after the loan has been issued 
# Not available in the Test data!!!
LC_Cleaned <- subset(LC_Cleaned, select = -last_pymnt_d)


# 45. last_pymnt_amnt: Last total payment amount received
# last_pymnt_amnt reflects post-loan payment behavior and tracks the most recent payment made by the borrower.
# Not available in the Test data!!!
LC_Cleaned <- subset(LC_Cleaned, select = -last_pymnt_amnt)


# 46. next_pymnt_d: Next scheduled payment date
# next_pymnt_d is not eseential as it is a post-loan feature that indicates the next scheduled payment date, which occurs after the loan has been issued.
# Not available in the Test data!!!
LC_Cleaned <- subset(LC_Cleaned, select = -next_pymnt_d)


# 47. Last_credit_pull_d: The most recent month LC pulled credit for this loan
# tracks when the lender last accessed the borrower’s credit information, which happens after the loan is issued. 
LC_Cleaned <- subset(LC_Cleaned, select = -last_credit_pull_d)


# 48. collections_12_mths_ex_med: Number of collections in 12 months excluding medical collections
# collections_12_mths_ex_med reflects the borrower’s history of debt mismanagement over the past 12 months (excluding medical-related collections), 
# which is a significant indicator of credit risk. Lenders use this information to assess the borrower’s likelihood of default and adjust the interest rate accordingly.
# Consclusion:
# collections_12_mths_ex_med is an important feature for predicting int_rate, as it directly impacts the lender’s risk assessment during the loan origination process.
#LC_Cleaned$collections_12_mths_ex_med <- factor(LC_Cleaned$collections_12_mths_ex_med)
LC_Cleaned$collections_12_mths_ex_med[is.na(LC_Cleaned$collections_12_mths_ex_med)] <- 0
#LC_Cleaned <- clean_column(LC_Cleaned,"collections_12_mths_ex_med")


# 49. mths_since_last_major_derog: Months since most recent 90-day or worse rating
# mths_since_last_major_derog refers to the number of months since the borrower's last major derogatory mark on their credit file.
# This feature is highly relevant, as it directly reflects the borrower's credit history. It indicates how recently the borrower experienced a significant credit problem.
# Conclusion:
# A borrower with a recent major derogatory event is seen as a higher risk and will likely face a higher interest rate. On the other hand,
# borrowers with no recent derogatory marks are seen as less risky, potentially resulting in a lower interest rate.
# Replace NA with 0 for 'mths_since_last_major_derog' where missing indicates no derogatory event
LC_Cleaned$mths_since_last_major_derog[is.na(LC_Cleaned$mths_since_last_major_derog)] <- 0 

# Specify the actual target variable in your regression (for example, 'target_variable_name')
#LC_Cleaned <- clean_column(LC_Cleaned, "mths_since_last_major_derog")



# 50. policy_code: publicly available policy_code=1, new products not publicly available policy_code=2
# policy_code might have a moderate influence on int_rate, particularly if different policies or products have different interest rate models
# Conclusion:
# the policy code has it's min and max at 1, the 126 data-point which have NA might be relevant.
#LC_Cleaned <- clean_column(LC_Cleaned,"policy_code") # 126 NA's might need to be imputed.
LC_Cleaned <- subset(LC_Cleaned, select = -policy_code)


# 51. application_type: Indicates whether the loan is an individual application or a joint application with two co-borrowers
# application_type differentiates between individual and joint applications. 
# Joint applications tend to reduce risk by incorporating the financial profiles and incomes of two borrowers, which may result in a lower interest rate.
# Conclusion:
# Individual applications are considered higher risk due to reliance on a single borrower’s financial situation.
# transforming 'application_type' as numeric factors where:
# 0 is 'individual' a single borrower applies for the loan.
# 1 is 'joint' the loan application is made by two co-borrowers, and both incomes, credit profiles, and financial situations are considered.

# Approach: Option 2 (Exclude Joint Attributes with Business Rules for Joint Applications)
# Reasoning:
# - Given the limited presence of joint applications (only 13 cases), excluding joint attributes simplifies the model without significantly impacting its generalization capability.
# - Business rules offer a practical way to account for joint applications in a manner consistent with lending policies, which may improve real-world applicability.
# - This approach avoids potential distortion from imputation and the complexity of training separate models, which isn’t feasible given the data volume.

# Train the Model on Individual Applications Only
# Remove joint-specific features for training
LC_Cleaned <- LC_Cleaned %>% 
  filter(LC_Cleaned$application_type == "INDIVIDUAL") %>%
  select(-c('annual_inc_joint', 'dti_joint', 'verification_status_joint'))
LC_Cleaned <- subset(LC_Cleaned, select = -application_type)


# 55. acc_now_delinq: The number of accounts on which the borrower is now delinquent.
LC_Cleaned <- clean_column(LC_Cleaned,"acc_now_delinq", remove_outliers=FALSE)


# 56. tot_coll_amt: Total collection amounts ever owed.
LC_Cleaned <- subset(LC_Cleaned, select = -tot_coll_amt)


# 57. tot_cur_bal: Total current balance of all accounts.
LC_Cleaned <- subset(LC_Cleaned, select = -tot_cur_bal)


# 58. total_rev_hi_lim: Total revolving high credit/credit limit.
LC_Cleaned <- subset(LC_Cleaned, select = -total_rev_hi_lim)


# 59. open_acc_6m: Number of open trades in last 6 months.
LC_Cleaned <- subset(LC_Cleaned, select = -open_acc_6m)


# 60. open_il_6m: Number of currently active installment trades
LC_Cleaned <- subset(LC_Cleaned, select = -open_il_6m)


# 61. open_il_12m: Number of installment accounts opened in past 12 months
LC_Cleaned <- subset(LC_Cleaned, select = -open_il_12m)


# 62. open_il_24m: Number of installment accounts opened in past 24 months
LC_Cleaned <- subset(LC_Cleaned, select = -open_il_24m)


# 63. mths_since_rcnt_il: Months since most recent installment accounts opened
LC_Cleaned <- subset(LC_Cleaned, select = -mths_since_rcnt_il)


# 64. total_bal_il: Total current balance of all installment accounts
LC_Cleaned <- subset(LC_Cleaned, select = -total_bal_il)


# 65. il_util: Ratio of total current balance to high credit/credit limit on all install acct
LC_Cleaned <- subset(LC_Cleaned, select = -il_util)


# 66. open_rv_12m: Number of revolving trades opened in past 12 months.
LC_Cleaned <- subset(LC_Cleaned, select = -open_rv_12m)


# 67. open_rv_24m: Number of revolving trades opened in past 24 months.
LC_Cleaned <- subset(LC_Cleaned, select = -open_rv_24m)


# 68. max_bal_bc: Maximum current balance owed on all revolving accounts.
LC_Cleaned <- subset(LC_Cleaned, select = -max_bal_bc)


# 69. all_util: Balance to credit limit on all trades.
LC_Cleaned <- subset(LC_Cleaned, select = -all_util)


# 70. inq_fi: Number of personal finance inquiries.
LC_Cleaned <- subset(LC_Cleaned, select = -inq_fi)


# 71. total_cu_tl: Number of finance trades.
LC_Cleaned <- subset(LC_Cleaned, select = -total_cu_tl)


# 72. inq_last_12m: Number of credit inquiries in past 12 months.
LC_Cleaned <- subset(LC_Cleaned, select = -inq_last_12m)

##############   Step 3 - Corr   ##########################
# Load required package
library(corrplot)

# Select numeric columns
numeric_data <- LC_Cleaned %>% select_if(is.numeric)

# Calculate correlation matrix
correlation_matrix <- cor(numeric_data)

# Plot the correlation matrix
corrplot(correlation_matrix, method = "color", type = "upper", tl.col = "black", tl.srt = 45)


##############   Step 4 - Prediction Task   ##########################
set.seed(1)


# Train a regression model (e.g., linear regression) using individual data
model <- train(int_rate ~ ., data = LC_Cleaned, method = "lm")

# Generate predictions for all applications (including joint)
LC_Cleaned$predicted_int_rate <- predict(model, newdata = LC_Cleaned)

# XGBTree
set.seed(1)
xgbGrid <-  expand.grid(nrounds = c(50, 75, 100, 150, 200, 250, 500, 1000, 1500, 2000, 5000),
                        max_depth = c(6, 9, 12, 15, 21),
                        eta = c(0.3),
                        gamma = c(0),
                        colsample_bytree = c(1.0),
                        min_child_weight = c(5),
                        subsample = c(0.6))

model_xgb <- train(int_rate ~ .,
                   data = LC_Cleaned,
                   method = "xgbTree",
                   trControl = trainControl(method = "repeatedcv", 
                                            number = 5, 
                                            repeats = 1, 
                                            verboseIter = TRUE),
                   tuneGrid = xgbGrid,
                   verbose = 1)
model_xgb

model_xgb$bestTune

head(predicted_xgb)
summary(predicted_xgb)

##############   Step 5 - Post-Processing (apply business rules)   ##########################

# Apply business rules for joint applications
LC_Cleaned <- LC_Cleaned %>%
  mutate(final_int_rate = case_when(
    application_type == "JOINT" & verification_status_joint == "Source Verified" ~ predicted_int_rate,
    application_type == "JOINT" & verification_status_joint == "Verified" ~ predicted_int_rate * 1.02,  # Increase by 2%
    application_type == "JOINT" & verification_status_joint == "Not Verified" ~ NA_real_,  # Flag or set to NA
    TRUE ~ predicted_int_rate  # Keep the original prediction for individual applications
  ))

# Display the final interest rates with adjustments
head(LC_Cleaned %>% select(application_type, verification_status_joint, predicted_int_rate, final_int_rate))
























#columns_to_inspect <- c("total_pymnt", "total_pymnt_inv", "total_rec_prncp", "total_rec_int", "total_rec_late_fee", "recoveries",
#                        "collection_recovery_fee", "last_pymnt_d", "last_pymnt_amnt", "next_pymnt_d", "last_credit_pull_d", "collections_12_mths_ex_med",
#                        "mths_since_last_major_derog", "policy_code", "application_type", "annual_inc_joint", "dti_joint", 
#                        "verification_status_joint", "int_rate")
#
#categorical_columns <- c("collections_12_mths_ex_med", "policy_code", "application_type", "last_pymnt_d", 
#                        "collections_12_mths_ex_med", "mths_since_last_major_derog",
#                        "annual_inc_joint", "verification_status_joint", "dti_joint", "last_credit_pull_d",
#                        "next_pymnt_d")
#
#columns_remove_na <- c("Mths_since_last_major_derog", "Annual_inc_joint", "Dti_joint")
#
#columns_after_investigation <- c("total_pymnt", "total_pymnt_inv", "total_rec_prncp", "total_rec_int", "total_rec_late_fee", "recoveries",
#                                 "collection_recovery_fee", "last_pymnt_amnt", "int_rate")
#
#data_to_inspect <- data[, columns_to_inspect]
#non_categorical_data <- data_to_inspect %>% select(-one_of(categorical_columns))
#data_to_inspect <- data[, columns_after_investigation]
#data_after_investigation <- non_categorical_data %>% select(-one_of())
#str(data_to_inspect)
#summary(data_to_inspect)
#
#
## Outliers
## Assuming your filtered dataset is in new_data
## Loop through each column to create a boxplot for each numeric column
#
## Set the plot layout to show multiple boxplots
#par(mfrow = c(1, 1))  # Adjust the layout as necessary (3x3 grid in this case)
#
## Loop through each column and create a boxplot
#for(column_name in colnames(data_to_inspect)) {
#  # Check if the column is numeric
#  if(is.numeric(data_to_inspect[[column_name]])) {
#    boxplot(data_to_inspect[[column_name]], main=column_name, outline=TRUE, 
#            col="lightblue", border="black")
#  }
#}
#
## Loop through each column in the data frame
#for (colname in names(data_to_inspect)) {
#  # Check if the column is numeric
#  if (is.numeric(data_to_inspect[[colname]])) {
#    # Create a histogram for the numeric column
#    hist(data_to_inspect[[colname]], 
#         main = paste("Histogram of", colname), 
#         xlab = colname, 
#         col = "lightblue", 
#         border = "black")
#  }
#}
#
#
#data_to_inspect_without_chr <- data_to_inspect %>% select_if(~ !is.character(.))
#plot(data_to_inspect_without_chr[,c(1,2)])
#
#library(corrplot)
##summary(non_categorical_data)
##cor(non_categorical_data[,-c(3,4)]) # We remove the categorical variables before applying cor(). cor() needs library corrplot. 
##corrplot(cor(non_categorical_data[,-c(3,4)]), order = "hclust", tl.col = "black", tl.srt = 45)
#
## Correlation Plot
##plot(non_categorical_data)
#
## Scatterplot matrix
#corrplot(cor(data_to_inspect[,-c(3,4)]), order = "hclust", tl.col = "black", tl.srt = 45)
#plot(data_to_inspect) # scatterplot matrix
#cor(data_to_inspect[,-c(1,2)])
#
############## 1st iteration: Fit a model to all variables #############
#
## For a start, we fit a regression model on ALL variables, to see what happens :-)
## ~. --> .(all the others attributes in the data), ~ things are linear related to 
#lm.fit1 <- lm(inc_rate ~., data=data_to_inspect)