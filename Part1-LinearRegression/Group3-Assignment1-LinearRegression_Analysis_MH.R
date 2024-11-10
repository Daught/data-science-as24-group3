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

# Install / load packages

if(!require('corrplot')) {              # Contains our data set
  install.packages('corrplot')
  library('corrplot')
}
if(!require('ggplot2')) {               # Contains our data set
  install.packages('ggplot2')
  library('ggplot2')
}
if(!require('tidyverse')) {             # Contains our data set
  install.packages('tidyverse')
  library('tidyverse')
}
if(!require('fastDummies')) {           # Contains our data set
  install.packages('fastDummies')
  library('fastDummies')
}
if(!require('GGally')) {                # Contains our data set
  install.packages('GGally')
  library('GGally')
}
if(!require('dplyr')) {                # Contains our data set
  install.packages('dplyr')
  library('dplyr')
}

# load data set
#data <- read.csv2("ressources/LCdata.csv", header = TRUE, row.names=NULL, sep=";")
dataset_file_path = "./ressources/LCdata.csv"
data <- read.csv2(dataset_file_path, header = TRUE, row.names=NULL, sep=";")

LC <- data # make a copy of the original data set, so that we dont mess with it

##########################   2. Data Exploration   ##########################

#### Examine Data Set
show_plots_flag <- FALSE

head(LC)

str(LC)
  # 322 observations and 20 variables.
  # Data types seem to correspond to scale levels.

summary(LC)
  # Missing values
  #   - Salary: 59 NAs (18% of the data set)
  #   - Complete otherwise


##############   Step 2 - Data Preprocessing   ##########################

# Feature Descriptions:
# 1. id: This is a unique identifier for each loan listing. Although it's essential for tracking loans, it holds no predictive value 
#    for analysis. We'll check for any anomalies (missing, zero, or negative values) and remove it as it's irrelevant for our analysis.

# Clean the 'id' column
if(show_plots_flag){
  sum(is.na(LC$id))  # Checking for missing values in 'id'
  sum(LC$id == 0)    # Checking if any 'id' is zero
  sum(LC$id < 0)     # Checking for negative values in 'id'
}
# Dropping 'id' column since it's not useful for the decision-making process
LC_Cleaned <- subset(LC, select = -id)

# 2. member_id: This represents the unique identifier for each borrower (member). It could potentially be useful in tracking multiple 
#    loans for the same borrower, but in this dataset, there's no evidence of multiple loans per member. We'll remove it after verifying for any anomalies.

# Clean the 'member_id' 
if(show_plots_flag){
  sum(is.na(LC_Cleaned$member_id))  # Checking for missing values
  sum(LC_Cleaned$member_id == 0)    # Checking for zero values
  sum(LC_Cleaned$member_id < 0)     # Checking for negative values
  sum(duplicated(LC_Cleaned$member_id))  # Checking for duplicate 'member_id'
}
# Since it’s not relevant, we'll remove 'member_id'
LC_Cleaned <- subset(LC_Cleaned, select = -member_id)

# 3. loan_amnt: This column records the amount of money requested by the borrower. It's an essential feature for analysis and 
#    should be non-negative, non-zero, and without missing values. We’ll explore its distribution and check for any irregularities.

# Explore the 'loan_amnt' column
if(show_plots_flag){
  summary(LC_Cleaned$loan_amnt)  # Summary statistics for the loan amount
  # Visualizing the distribution of loan amounts with different binwidths
  ggplot(data = LC_Cleaned) +
    geom_histogram(aes(x = loan_amnt), binwidth = 100) +
    labs(title = "Loan Amount Distribution (binwidth = 100)")
  ggplot(data = LC_Cleaned) +
    geom_histogram(aes(x = loan_amnt), binwidth = 250) +
    labs(title = "Loan Amount Distribution (binwidth = 250)")
  ggplot(data = LC_Cleaned) +
    geom_histogram(aes(x = loan_amnt), binwidth = 500) +
    labs(title = "Loan Amount Distribution (binwidth = 500)")
  ggplot(data = LC_Cleaned) +
    geom_histogram(aes(x = loan_amnt), binwidth = 1000) +
    labs(title = "Loan Amount Distribution (binwidth = 1000)")

  # Checking for missing, zero, or negative values in 'loan_amnt'
  sum(is.na(LC_Cleaned$loan_amnt))  # Looking for missing values
  sum(LC_Cleaned$loan_amnt == 0)    # Checking if any amounts are zero
  sum(LC_Cleaned$loan_amnt < 0)     # Ensuring no negative values
  # Identify potential outliers in 'loan_amnt' using boxplot statistics
  boxplot.stats(LC_Cleaned$loan_amnt)$out
}



# 4. funded_amnt: This column shows the amount of money that has been funded for the loan at the current point. It's essential to analyze 
#    the progress of loan funding. We will check for anomalies and compare it with the loan amount requested.\
#    not available in the Test data!!!

# Analyze 'funded_amnt' column
if(show_plots_flag){
  summary(LC_Cleaned$funded_amnt)  # Summary statistics for funded amount
  # Visualizing the relationship between 'loan_amnt' and 'funded_amnt'
  ggplot(data = LC_Cleaned) +
    geom_point(aes(x = loan_amnt, y = funded_amnt), alpha = 0.1) +
    labs(title = "Loan Amount vs Funded Amount")

  # Checking for missing, zero, or negative values in 'funded_amnt'
  sum(is.na(LC_Cleaned$funded_amnt))  # Looking for missing values
  sum(LC_Cleaned$funded_amnt == 0)    # Checking for zero amounts
  sum(LC_Cleaned$funded_amnt < 0)     # Ensuring no negative values
  # Identify potential outliers in 'funded_amnt' using boxplot statistics
  boxplot.stats(LC_Cleaned$funded_amnt)$out
}

LC_Cleaned <- subset(LC_Cleaned, select = -funded_amnt)

# 5. funded_amnt_inv: This represents the amount funded by investors for the loan. It's currently stored as a character but 
#    should be converted to an integer for analysis. We’ll also explore how this compares to the total loan amount requested.
#    not available in the Test data!!!


# Clean and analyze 'funded_amnt_inv'
LC_Cleaned$funded_amnt_inv <- as.integer(LC_Cleaned$funded_amnt_inv)  # Converting to integer
if(show_plots_flag){
  summary(LC_Cleaned$funded_amnt_inv)  # Summary statistics of funded amount by investors
  # Visualizing the relationship between 'loan_amnt' and 'funded_amnt_inv'
  ggplot(data = LC_Cleaned) +
    geom_point(aes(x = loan_amnt, y = funded_amnt_inv), alpha = 0.1) +
    labs(title = "Loan Amount vs Investor Funded Amount")

  # Visualizing the relationship between 'funded_amnt' and 'funded_amnt_inv'
  ggplot(data = LC_Cleaned) +
    geom_point(aes(x = funded_amnt_inv, y = funded_amnt), alpha = 0.1) +
    labs(title = "Investor Funded Amount vs Funded Amount")

  # Checking for missing, zero, or negative values in 'funded_amnt_inv'
  sum(is.na(LC_Cleaned$funded_amnt_inv))  # Looking for missing values
  sum(LC_Cleaned$funded_amnt_inv == 0)    # Checking for zero amounts which is possible in this data
  sum(LC_Cleaned$funded_amnt_inv < 0)     # Ensuring no negative values
  # Identify potential outliers in 'funded_amnt_inv' using boxplot statistics
  boxplot.stats(LC_Cleaned$funded_amnt_inv)$out
}

LC_Cleaned <- subset(LC_Cleaned, select = -funded_amnt_inv)

# 6. term: This column specifies the length of the loan in months (either 36 or 60 months). It is essentially a categorical variable, 
#    so we’ll trim any unnecessary spaces and explore how the term affects loan amounts and funding.

# Clean and analyze the 'term' column
LC_Cleaned$term <- str_trim(LC_Cleaned$term)  # Trimming unnecessary spaces
if(show_plots_flag){
  summary(LC_Cleaned$term)  # Summary of loan term distribution
  # Visualizing the distribution of loan terms (more than twice of 36M compared to 60)
  ggplot(LC_Cleaned, aes(x = term)) + 
    geom_bar() +
    labs(title = "Distribution of Loan Terms")

  # Analyze 'funded_amnt_inv' for loans with 36 months and 60 months terms (Similar.. lower therm more for lower amounts)
  # Scatter plot for loans with 36 months term
  ggplot(data = filter(LC_Cleaned, term == "36 months"), aes(x = loan_amnt, y = funded_amnt_inv)) +
    geom_point(alpha = 0.05) +
    labs(title = "Loan Amount vs Investor Funded Amount (36 months)")

  # Scatter plot for loans with 60 months term
  ggplot(data = filter(LC_Cleaned, term == "60 months"), aes(x = loan_amnt, y = funded_amnt_inv)) +
    geom_point(alpha = 0.05) +
    labs(title = "Loan Amount vs Investor Funded Amount (60 months)")

  # Visualizing loan amount by term using a box plot
  # We see a difference in loan amount. 
  ggplot() + 
    geom_boxplot(mapping = aes(x = term, y = loan_amnt), data = LC_Cleaned) +
    labs(title = "Loan Amount Distribution by Term")

  # Visualizing funded amount by term using a box plot
  ggplot() + 
    geom_boxplot(mapping = aes(x = term, y = funded_amnt), data = LC_Cleaned) +
    labs(title = "Funded Amount by Term")

  # Visualizing the ratio of funded amount to loan amount for each term (Ratio for 60 month higher. Credibility higher I would keept this.)
  ggplot() + 
    geom_boxplot(mapping = aes(x = term, y = funded_amnt / loan_amnt), data = LC_Cleaned) +
    labs(title = "Funded Amount Ratio by Term")
}


# 7. int_rate: The interest rate assigned to the loan. This is a key feature for analysis.
#    It's originally stored as a character, but should be converted to a float (double).
#    We'll also check for missing values and explore its distribution.

# Convert 'int_rate' to a numeric format
LC_Cleaned$int_rate <- as.double(LC_Cleaned$int_rate)

if(show_plots_flag){
  # Visualize the distribution of interest rates with different binwidths
  ggplot(LC_Cleaned) +
    geom_histogram(aes(x = int_rate), binwidth = 0.1) +
    labs(title = "Distribution of Interest Rate (binwidth = 0.1)")
  ggplot(LC_Cleaned) +
    geom_histogram(aes(x = int_rate), binwidth = 0.25) +
    labs(title = "Distribution of Interest Rate (binwidth = 0.25)")
  ggplot(LC_Cleaned) +
    geom_histogram(aes(x = int_rate), binwidth = 0.5) +
    labs(title = "Distribution of Interest Rate (binwidth = 0.5)")
  ggplot(LC_Cleaned) +
    geom_histogram(aes(x = int_rate), binwidth = 1) +
    labs(title = "Distribution of Interest Rate (binwidth = 1)")

  # Interest rate vs loan amount and term
  ggplot(data = LC_Cleaned, mapping = aes(x = loan_amnt, y = int_rate, color = term)) +
    geom_point(alpha = 0.05) +
    labs(title = "Loan Amount vs Interest Rate, colored by Term")

  # Checking for missing, zero, or negative values in 'int_rate'
  sum(is.na(LC_Cleaned$int_rate))  # Checking for missing values
  sum(LC_Cleaned$int_rate == 0)    # Check for zero values
  sum(LC_Cleaned$int_rate < 0)     # Ensure there are no negative values

  # we have around 1K ouliers here.. need to be discussed 
  length(boxplot.stats(LC_Cleaned$int_rate)$out)
}

# 8. installment: The monthly installment the borrower needs to pay.
# Might have a direct correlation with interest rate and loan amount
#     We'll convert it to double and remove the feature.
#    not available in the Test data!!!


# Convert 'installment' to double and remove it afterward
#LC_Cleaned$installment <- as.double(LC_Cleaned$installment)
LC_Cleaned <- subset(LC_Cleaned, select = -installment)


# 9. emp_title: The borrower’s job title, provided during the application.
#    This is a free-text field that is not useful for predictive analysis, so it will be dropped.
#   * Employer Title replaces Employer Name for all loans listed after 9/23/2013

# Check for dublicates
# Calculate the proportion of duplicate 'emp_title' values
if(show_plots_flag){
  num_duplicates <- sum(duplicated(LC_Cleaned$emp_title))  # Number of duplicate titles
  total_titles <- nrow(LC_Cleaned)  # Total number of rows

  # Proportion of duplicates
  duplicate_proportion <- num_duplicates / total_titles
  duplicate_proportion
}


# Drop 'emp_title' as it's not useful for analysis
LC_Cleaned <- subset(LC_Cleaned, select = -emp_title)

# 10. emp_length: The number of years the borrower has been employed.
#    Convert this from string to integer and check for missing values.

# Convert 'emp_length' to integer
LC_Cleaned$emp_length <- as.integer(ordered(LC_Cleaned$emp_length, levels = c("< 1 year", "1 year", "2 years", "3 years", "4 years", "5 years", "6 years", "7 years", "8 years", "9 years", "10+ years"))) - 1

if(show_plots_flag){
  # Checking for missing, zero, or negative values in 'emp_length'
  # over 40000 missing values.. Might be removed
  sum(is.na(LC_Cleaned$emp_length))  # Looking for missing values
  sum(LC_Cleaned$emp_length == 0)    # Checking for zero amounts which is possible in this data
  sum(LC_Cleaned$emp_length < 0)     # Ensuring no negative values
}
# I'll remove it for now 
LC_Cleaned <- subset(LC_Cleaned, select = -emp_length)


# 11. home_ownership: Indicates whether the borrower owns, rents, or has a mortgage on their home.
#     Convert this to an ordered set of integers and create dummy columns for categorical analysis.
#     We have 47 missing values.. we assign those to Other. "Other" is also unknown.

sum(is.na(LC_Cleaned$home_ownership))  # Looking for missing values

table(LC_Cleaned$home_ownership) # We see that there are 2 "Any" 45 "None"values

# these values are assigned to Other (Other is unclear as well.)
# Replace "ANY" and "NONE" with "OTHER" in the home_ownership column
LC_Cleaned$home_ownership[LC_Cleaned$home_ownership %in% c("ANY", "NONE")] <- "OTHER"

table(LC_Cleaned$home_ownership) # We see that there are 2 "Any" 45 "None"values

# Convert 'home_ownership' to ordered integer
LC_Cleaned$home_ownership <- as.integer(ordered(LC_Cleaned$home_ownership, levels = c("OTHER", "RENT", "MORTGAGE", "OWN")))

# check unique values
unique(LC_Cleaned$home_ownership)

# Create dummy columns for 'home_ownership'
LC_Cleaned <- dummy_columns(LC_Cleaned, select_columns = "home_ownership", remove_selected_columns = TRUE)

# Check the names of the new dummy columns generated for 'home_ownership'
colnames(LC_Cleaned)

# View the first few rows of the new dummy columns to confirm
head(LC_Cleaned[, grepl("home_ownership_", colnames(LC_Cleaned))])

# Calculate the count of each dummy column
(dummy_counts <- colSums(LC_Cleaned[, grepl("home_ownership_", colnames(LC_Cleaned))]))


# 12. annual_inc: The annual income reported by the borrower during registration.
#     Convert from string to integer, check for missing, zero, or extreme values, and handle any income outliers.


if(show_plots_flag){
  summary(LC_Cleaned$annual_inc)  # Summary of loan term distribution
  sum(is.na(LC_Cleaned$annual_inc))  # Looking for missing values
  sum(LC_Cleaned$annual_inc == 0)    # Checking for zero amounts. There are 2
  sum(LC_Cleaned$annual_inc < 0)     # Ensuring no negative values. There are 4
}

# Convert 'annual_inc' to integer
LC_Cleaned <- filter(LC_Cleaned, !is.na(annual_inc))
# Remove rows with negative values in 'annual_inc'
LC_Cleaned <- filter(LC_Cleaned, annual_inc >= 0)
# Convert 'annual_inc' to integer
LC_Cleaned$annual_inc <- as.integer(LC_Cleaned$annual_inc)

if(show_plots_flag){
  ggplot(data = LC_Cleaned, aes(x = annual_inc)) +
    geom_histogram(binwidth = 50000, fill = "blue", color = "black") +
    labs(title = "Histogram of Annual Income", x = "Annual Income", y = "Count")

  ggplot(data = LC_Cleaned, aes(y = annual_inc)) +
    geom_boxplot(fill = "lightblue", color = "black") +
    labs(title = "Boxplot of Annual Income", y = "Annual Income")
  # We have a lot of outliers here
  boxplot.stats(LC_Cleaned$annual_inc)$out

  #lets see how it looks like from one million upwards
  ggplot(data = filter(LC_Cleaned, annual_inc > 1000000), aes(x = annual_inc)) +
    geom_histogram(binwidth = 100000, fill = "blue", color = "black") +
    labs(title = "Histogram of Annual Income (Before Adjustment)", x = "Annual Income", y = "Count")

  # Needs to be discussed.. I suppose that pople entered the income incorrectely. Seems like they entered the "cents"as well.
# Suggestion: If this is the case the we need a threshold (here 1M).. could also be real (Sucessfull people take out loans!)
# Le's continue with these values and see how the regression behaves.. We do not have proof that the annual income has a mistake.  

#LC_Cleaned$annual_inc <- ifelse(LC_Cleaned$annual_inc >= 1000000, LC_Cleaned$annual_inc / 100, LC_Cleaned$annual_inc)
#filter(LC_Cleaned, annual_inc > 1000000)

# Second way.. impute the outlier by dividing by 100
#outliers <- boxplot.stats(LC_Cleaned$annual_inc)$out
#LC_Cleaned$annual_inc <- ifelse(LC_Cleaned$annual_inc %in% outliers, LC_Cleaned$annual_inc / 100, LC_Cleaned$annual_inc)

# Summary statistics of 'annual_inc'
summary(LC_Cleaned$annual_inc)
}


# 13. verification_status: Indicates whether the borrower’s income has been verified.
#     Convert this to an integer and analyze its relationship to the interest rate.

# TODO-FiratXsebastian: merge both cells joint_verification_status & verification_status
# TODO: check whether high loan_amount are always verified

# Convert 'verification_status' to integer
LC_Cleaned$verification_status <- as.integer(factor(LC_Cleaned$verification_status)) - 1

if(show_plots_flag){
  # Check for missing values in 'verification_status'
  sum(is.na(LC_Cleaned$verification_status))  # Looking for missing values

  # Analyze the relationship between 'verification_status' and 'int_rate'
  # Create boxplots for all verification_status categories in one plot (interest rate higher for people with more income.. maybe becaus they loan more money)
  ggplot(data = LC_Cleaned, aes(x = as.factor(verification_status), y = int_rate)) +
    geom_boxplot() +
    labs(title = "Boxplot of Interest Rate by Verification Status", x = "Verification Status", y = "Interest Rate") +
    scale_x_discrete(labels = c("0" = "Not Verified", "1" = "Source Verified", "2" = "Verified"))
}

# 14. issue_d: The date when the loan was funded.
#     Not part of Test data!!!

# Drop 'issue_d' as it won't be relevant for future data
LC_Cleaned <- subset(LC_Cleaned, select = -issue_d)

# 15. loan_status: The current status of the loan (e.g., fully paid, defaulted). Not part of test data!!!!
#     This is only applicable post-loan issuance, so it’s not useful for predicting interest rates. We'll drop it.

levels(factor(LC_Cleaned$loan_status))

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
if(show_plots_flag){
  # Investigate $purpose, (character)
  str(LC_Cleaned$purpose)
  unique(LC_Cleaned$purpose) # "other", "debt_consolidation", "moving", "credit_card", "home_improvement", "major_purchase", "wedding", "small_business", "medical", "car", "educational", "vacation", "house", "renewable_energy"
  length(unique(LC_Cleaned$purpose)) # 14 unique values
  sum(LC_Cleaned$purpose == "") # 0
  sum(LC_Cleaned$purpose == " ") # 0
  table(LC_Cleaned$purpose)
#  car        credit_card debt_consolidation        educational   home_improvement              house     major_purchase            medical             moving              other   renewable_energy 
#  7923             185389             471636                380              46517               3320              15578               7658               4869              38670                505 
#  small_business     vacation            wedding 
#  9372               4241               2098 
}

LC_Cleaned$purpose <- tolower(LC_Cleaned$purpose) # Convert all characters in the column to lowercase
LC_Cleaned$purpose <- factor(LC_Cleaned$purpose) # Change to unordered factor
if(show_plots_flag){
  length(unique(LC_Cleaned$purpose)) # 14 unique values
  sum(is.na(LC_Cleaned$purpose)) # no missing values
}

# 20. 'title': The loan title provided by the borrower.
# Investigate $title, (character)
str(LC_Cleaned$title) # character
unique(LC_Cleaned$title)
length(unique(LC_Cleaned$title)) # 57719

top_three_values <- names(sorted_value_counts)[1:3] # Identify the three most frequent values
# To lower case
LC_Cleaned$title <- tolower(LC_Cleaned$title)
# Remove all whitespace
LC_Cleaned$title <- gsub(" ", "", LC_Cleaned$title)
length(unique(LC_Cleaned$title)) # from 57719 to 48722 unique values, but still!!!
# As it contains similar information to 'purpose' in free text and thus harder to analyze format we will drop this.
LC_Cleaned <- subset(LC_Cleaned, select = -title) # remove the original column

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

if(show_plots_flag){
  unique(LC_Cleaned$zip_code) # e.g.: "806xx" "100xx" "665xx" "068xx" "300xx"
}
# As more reducing does not make sense (too big and not necessarily logical connected areas with only 2 digits) we will drop it and use 'addr_state' instead
LC_Cleaned <- subset(LC_Cleaned, select = -zip_code)


if(show_plots_flag){
  length(unique(data$zip_code)) # inital 931 unique values
  length(unique(LC_Cleaned$zip_code)) # no change to original
  sum(is.na(LC_Cleaned$zip_code)) # no missing values
  summary(LC_Cleaned$zip_code)
  table(LC_Cleaned$zip_code)
  # Count and sort the values
  counts <- table(LC_Cleaned$zip_code)
  sorted_value_counts <- sort(counts, decreasing = TRUE)
  print(sorted_value_counts)
  sort(LC_Cleaned$zip_code)

  # Change to factor
  LC_Cleaned$zip_code <- factor(LC_Cleaned$zip_code)
  summary(LC_Cleaned$zip_code)
  str(LC_Cleaned$zip_code)
  summary(LC_Cleaned$zip_code)

  # Barplot for zip_code
  counts <- table(LC_Cleaned$zip_code)
  barplot(counts,main = "Distribution of Character Column",xlab = "Categories",ylab = "Count",col = "lightblue",border = "black",las = 2,cex.names = 0.7)
  # Plot target to zip_code
  boxplot(int_rate ~ zip_code, data = LC_Cleaned,main = "Box Plot of Interest Rate by Purpose",xlab = "Investigated Column",ylab = "Interest Rate",col = "lightblue",border = "black",las = 2,cex.axis = 0.7)
}

# dropped zip_code as redundant information with the feature addr_state
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

# Histogramm without very high values, Filter out the rows where column_num is 9999
filtered_data <- LC_Cleaned$dti[LC_Cleaned$dti < 150]

if(show_plots_flag){
  # Create the histogram with the filtered data
  hist(filtered_data,
     main = "Histogram of Numeric Column (Excluding 9999)",
     xlab = "Values",
     ylab = "Frequency",
     col = "lightblue",
     border = "black")

  boxplot(LC_Cleaned$dti)
  boxplot(filtered_data)
}


# 24. 'delinq_2yrs': The number of 30+ days past-due incidences of delinquency in the borrower's credit file for the past 2 years.

# TODO: MHA: please check again regarding NA's (odd that it only has 25...)
if(show_plots_flag){
  # Investigate $delinq_2yrs (integer)
  summary(LC_Cleaned$delinq_2yrs)
  unique(LC_Cleaned$delinq_2yrs)
  sum(is.na(LC_Cleaned$delinq_2yrs)) # 21 NAs

  # Calculate the frequency of each unique value in delinq_2yrs and sort by frequency
  # A higher delinq_2yrs value may indicate a greater risk of future payment delinquencies.
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

  # Plots and Histogramms
  hist(LC_Cleaned$delinq_2yrs)
  boxplot(LC_Cleaned$delinq_2yrs)

  # Plot target to delinq_2yrs
  boxplot(int_rate ~ delinq_2yrs, 
          data = LC_Cleaned,
          main = "Box Plot of Interest Rate by Purpose",
          xlab = "Investigated Column",
          ylab = "Interest Rate",
          col = "lightblue",
          border = "black",
          las = 2,  # Rotate x-axis labels for readability
          cex.axis = 0.7) # Adjust label size if needed

  # Find the row indices where NAs are
  column_to_search <- LC_Cleaned$delinq_2yrs
  NA_indices <- which(is.na(column_to_search))
  # Display the rows with NAs
  data_with_NAvalue <- data[NA_indices, ]
  print(data_with_NAvalue) # multiple NA-columns
}


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

# Creates $earliest_cr_line_year, $earliest_cr_line_numeric, $earliest_cr_line_char, $earliest_cr_line_date

if(show_plots_flag){
  # Make sure it worked
  summary(LC_Cleaned$earliest_cr_line)
  str(LC_Cleaned$earliest_cr_line)

  # Plot target to earliest_cr_line
  boxplot(int_rate ~ earliest_cr_line, 
          data = LC_Cleaned,
          main = "Box Plot of Interest Rate by Purpose",
          xlab = "Investigated Column",
          ylab = "Interest Rate",
          col = "lightblue",
          border = "black",
          las = 2,  # Rotate x-axis labels for readability
          cex.axis = 0.7) # Adjust label size if needed

  # Find the row indices where NAs are
  column_to_search <- LC_Cleaned$earliest_cr_line
  NA_indices <- which(is.na(column_to_search))
  # Display the rows with NAs
  data_with_NAvalue <- data[NA_indices, ]
  print(data_with_NAvalue) # multiple NA-columns
}


# 26. 'inq_last_6mths': The number of inquiries in past 6 months (excluding auto and mortgage inquiries)data.frame(..., row.names = NULL, check.rows = FALSE, check.names = TRUE, stringsAsFactors = default.stringsAsFactors())

if(show_plots_flag){
  summary(LC_Cleaned$inq_last_6mths) #    Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  #                                 0.0000  0.0000  0.0000  0.6947  1.0000 33.0000      25 
  unique(LC_Cleaned$inq_last_6mths) # e.g.: NA  0  1  4  2  3  5 
  sum(is.na(LC_Cleaned$inq_last_6mths)) #  25 NAs on original dataset, same rows as NAs already dropped from earliest_cr_line

  # Plots and Histogramms
  hist(LC_Cleaned$inq_last_6mths)
  boxplot(LC_Cleaned$inq_last_6mths)

  # Plot target to inq_last_6mths
  boxplot(int_rate ~ inq_last_6mths, 
          data = LC_Cleaned,
          main = "Box Plot of Interest Rate by Purpose",
          xlab = "Investigated Column",
          ylab = "Interest Rate",
          col = "lightblue",
          border = "black",
          las = 2,  # Rotate x-axis labels for readability
          cex.axis = 0.7) # Adjust label size if needed

  # Find the row indices where NAs are
  column_to_search <- LC_Cleaned$inq_last_6mths
  NA_indices <- which(is.na(column_to_search))
  # Display the rows with NAs
  data_with_NAvalue <- data[NA_indices, ]
  print(data_with_NAvalue) # multiple NA-columns
}


# 27. mths_since_last_delinq ... The number of months since the borrower's last delinquency.
# A value of 12 would indicate that the borrower’s last delinquency was 12 months ago (one year).
# A value of 0 would indicate that a delinquency occurred within the current month.
# We will impute NAs with a very high number as low numbers indicate more risk for the lender.
# Replace NA values with 999


if(show_plots_flag){
  summary(LC_Cleaned$mths_since_last_delinq)
  unique(LC_Cleaned$mths_since_last_delinq) # e.g.: NA   0  66  62  28  37  78   8  23  44 
  sum(is.na(LC_Cleaned$mths_since_last_delinq)) #  408818 NA
  LC_Cleaned$mths_since_last_delinq[is.na(LC_Cleaned$mths_since_last_delinq)] <- 999
  sum(is.na(LC_Cleaned$mths_since_last_delinq)) # 0

  # Plots and Histogramms
  hist(LC_Cleaned$mths_since_last_delinq)
  boxplot(LC_Cleaned$mths_since_last_delinq)

  # Plot target to mths_since_last_delinq
  boxplot(int_rate ~ mths_since_last_delinq, 
        data = LC_Cleaned,
        main = "Box Plot of Interest Rate by Purpose",
        xlab = "Investigated Column",
        ylab = "Interest Rate",
        col = "lightblue",
        border = "black",
        las = 2,  # Rotate x-axis labels for readability
        cex.axis = 0.7) # Adjust label size if needed
}


# 28. mths_since_last_record ... The number of months since the last public record.

if(show_plots_flag){
  str(LC_Cleaned$mths_since_last_record)
  summary(LC_Cleaned$mths_since_last_record)
  unique(LC_Cleaned$mths_since_last_record) # e.g.: NA   0  95 111  57  76 102 115  54 106  86  19  61   
  sum(is.na(LC_Cleaned$mths_since_last_record)) #  675190 NA

  # Plots and Histogramms
  hist(LC_Cleaned$mths_since_last_record)
  boxplot(LC_Cleaned$mths_since_last_record)

  # Plot target to mths_since_last_record
  boxplot(int_rate ~ mths_since_last_record, 
          data = LC_Cleaned,
          main = "Box Plot of Interest Rate by Purpose",
          xlab = "Investigated Column",
          ylab = "Interest Rate",
          col = "lightblue",
          border = "black",
          las = 2,  # Rotate x-axis labels for readability
          cex.axis = 0.7) # Adjust label size if needed

  # Find the row indices not NAs
  column_to_search <- LC_Cleaned$mths_since_last_record
  NA_indices <- which(!is.na(column_to_search))
  # Display the rows with not NAs
  data_with_NAvalue <- data[NA_indices, ]
  print(data_with_NAvalue)
}

# As NA normally indicates no public record history we impute with a very high number
LC_Cleaned$mths_since_last_record[is.na(LC_Cleaned$mths_since_last_record)] <- 999
sum(is.na(LC_Cleaned$mths_since_last_record)) # 0

# 29. open_acc ... The number of open credit lines in the borrower's credit file.
# A value of 5 means the borrower has five open lines of credit. --> higher risk
# A value of 0 indicates that the borrower currently has no active lines of credit. --> lower risk
# Is it possible that there are people with 90 lines of credit open?


if(show_plots_flag){
  # Investigate $open_acc (int)
  summary(LC_Cleaned$open_acc)
  unique(LC_Cleaned$open_acc) # e.g.: NA  7  9  8  5 12  4 17 10 13 11   
  sum(is.na(LC_Cleaned$open_acc)) #  25 NA, disappeared as NAs were dropped before

  # Plots and Histogramms
  hist(LC_Cleaned$open_acc)
  boxplot(LC_Cleaned$open_acc)

  # Plot target to open_acc
  boxplot(int_rate ~ open_acc, 
          data = LC_Cleaned,
          main = "Box Plot of Interest Rate by Purpose",
          xlab = "Investigated Column",
          ylab = "Interest Rate",
          col = "lightblue",
          border = "black",
          las = 2,  # Rotate x-axis labels for readability
          cex.axis = 0.7) # Adjust label size if needed

  # Find the row indices NAs
  column_to_search <- LC_Cleaned$open_acc
  NA_indices <- which(is.na(column_to_search))
  # Display the rows with NAs
  data_with_NAvalue <- data[NA_indices, ]
  print(data_with_NAvalue)
}




# 30. pub_rec ... Number of derogatory public records. Derogatory public records are negative financial events that are publicly accessible and can significantly impact a borrower’s creditworthiness.
# A value of 0 means the borrower has no derogatory public records in their credit file.
# A value of 1 or higher indicates the number of derogatory public records recorded against the borrower.
# Higher values suggest a history of multiple financial issues
if(show_plots_flag){
  summary(LC_Cleaned$pub_rec)
  unique(LC_Cleaned$pub_rec) # e.g.: NA  0  1  2  3  5  9    
  sum(is.na(LC_Cleaned$pub_rec)) #  25 NA, disappeared after deleting NA rows before

  # Plots and Histogramms
  hist(LC_Cleaned$pub_rec)
  boxplot(LC_Cleaned$pub_rec)

  # Plot target to pub_rec
  boxplot(int_rate ~ pub_rec, 
          data = LC_Cleaned,
          main = "Box Plot of Interest Rate by Purpose",
          xlab = "Investigated Column",
          ylab = "Interest Rate",
          col = "lightblue",
          border = "black",
          las = 2,  # Rotate x-axis labels for readability
          cex.axis = 0.7) # Adjust label size if needed

  # Find the row indices NAs
  column_to_search <- LC_Cleaned$pub_rec
  NA_indices <- which(is.na(column_to_search))
  # Display the rows with NAs
  data_with_NAvalue <- data[NA_indices, ]
  print(data_with_NAvalue)
}



# 31. revol_bal ... Total credit revolving balance.
# A high revol_bal could indicate a borrower is utilizing a lot of available revolving credit, potentially signaling a higher risk if they are close to maxing out their credit limits.
# A low or zero revol_bal shows minimal use of revolving credit, which can be favorable, though some utilization is typically seen positively as it shows credit activity.
## This needs NA handling. Drop rows? Impute with 0 or mean or knn. By now the NAs will be filled with 0

if(show_plots_flag){
  summary(LC_Cleaned$revol_bal)
  unique(LC_Cleaned$revol_bal) # e.g.: 0 150786  11608   9204   5046   6360  16842   4429 (>70445)    
  sum(is.na(LC_Cleaned$revol_bal)) #  2 NA

  # Plots and Histogramms
  hist(LC_Cleaned$revol_bal)
  boxplot(LC_Cleaned$revol_bal)

  # Plot target to revol_bal
  boxplot(int_rate ~ revol_bal, 
          data = LC_Cleaned,
          main = "Box Plot of Interest Rate by Purpose",
          xlab = "Investigated Column",
          ylab = "Interest Rate",
          col = "lightblue",
          border = "black",
          las = 2,  # Rotate x-axis labels for readability
          cex.axis = 0.7) # Adjust label size if needed

  # Find the row indices NAs
  column_to_search <- LC_Cleaned$revol_bal
  NA_indices <- which(is.na(column_to_search))
  # Display the rows with NAs
  data_with_NAvalue <- data[NA_indices, ]
  print(data_with_NAvalue)
  # Replace NA values with 0
  LC_Cleaned$revol_bal[is.na(LC_Cleaned$revol_bal)] <- 0
  sum(is.na(LC_Cleaned$revol_bal))
}

# 32. revol_util ... Revolving line utilization rate, or the amount of credit the borrower is using relative to all available revolving credit.
# A low revol_util (e.g., 0-30%) indicates that the borrower is using a small portion of their available credit, which is generally seen positively in credit scoring.
# A high revol_util (e.g., > 50%) suggests that the borrower is utilizing a large amount of their credit limit, which can indicate higher risk.
# 100% or higher means the borrower has maxed out or exceeded their credit limit, which can be a significant risk factor.
## This needs NA handling. Drop rows? Impute with 0 or mean or knn. By now the NAs will be filled with 0

if(show_plots_flag){
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
    


  # Plots and Histogramms
  hist(LC_Cleaned$revol_util)
  boxplot(LC_Cleaned$revol_util)

  # Plot target to revol_util
  boxplot(int_rate ~ revol_util, 
          data = LC_Cleaned,
          main = "Box Plot of Interest Rate by Purpose",
          xlab = "Investigated Column",
          ylab = "Interest Rate",
          col = "lightblue",
          border = "black",
          las = 2,  # Rotate x-axis labels for readability
          cex.axis = 0.7) # Adjust label size if needed
  
  # Replace NA values with 0
  LC_Cleaned$revol_util[is.na(LC_Cleaned$revol_util)] <- 0
  sum(is.na(LC_Cleaned$revol_util))
}

# 33. total_acc ... The total number of credit lines currently in the borrower's credit file.
# A specific number in total_acc indicates the total number of accounts that have ever been opened in the borrower’s name.
# For example, a value of 10 means the borrower has opened ten credit accounts over time, regardless of whether they are currently open or closed.
# This could indicate a higher experience with credit, which could be positive.

if(show_plots_flag){
  # Investigate $total_acc (int)
  summary(LC_Cleaned$total_acc)
  unique(LC_Cleaned$total_acc) # e.g.: NA  16  19  43  28  20  35  33  26     
  sum(is.na(LC_Cleaned$total_acc)) #  25 NA, disappeared by deletion before

  # Plots and Histogramms
  hist(LC_Cleaned$total_acc)
  boxplot(LC_Cleaned$total_acc)

  # Plot target to total_acc
  boxplot(int_rate ~ total_acc, 
          data = LC_Cleaned,
          main = "Box Plot of Interest Rate by Purpose",
          xlab = "Investigated Column",
          ylab = "Interest Rate",
          col = "lightblue",
          border = "black",
          las = 2,  # Rotate x-axis labels for readability
          cex.axis = 0.7) # Adjust label size if needed

  # Find the row indices NAs
  column_to_search <- LC_Cleaned$total_acc
  NA_indices <- which(is.na(column_to_search))
  # Display the rows with NAs
  data_with_NAvalue <- data[NA_indices, ]
  print(data_with_NAvalue)
}

# 34. initial_list_status ... The initial listing status of the loan. Possible values are – W, F


LC_Cleaned$initial_list_status <- factor(LC_Cleaned$initial_list_status)
# TODO-MHA: Transform capital W to lower-w

if(show_plots_flag){
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

  # Barplot for initial_list_status
  counts <- table(LC_Cleaned$initial_list_status)
  barplot(counts,
          main = "Distribution of Character Column",
          xlab = "Categories",
          ylab = "Count",
          col = "lightblue",
          border = "black",
          las = 2,   # Rotate x-axis labels for readability
          cex.names = 0.7)  # Adjust label size if needed

  # Plot target to initial_list_status
  boxplot(int_rate ~ initial_list_status, 
          data = LC_Cleaned,
          main = "Box Plot of Interest Rate by Purpose",
          xlab = "Investigated Column",
          ylab = "Interest Rate",
          col = "lightblue",
          border = "black",
          las = 2,  # Rotate x-axis labels for readability
          cex.axis = 0.7) # Adjust label size if needed
}


# 35. out_prncp ... Remaining outstanding principal for total amount funded.
# Not for analysis
LC_Cleaned <- subset(LC_Cleaned, select = -out_prncp)

# 36. out_prncp_inv ... Remaining outstanding principal for portion of total amount funded by investors.
# Not for analysis
LC_Cleaned <- subset(LC_Cleaned, select = -out_prncp_inv)


# 55. acc_now_delinq: The number of accounts on which the borrower is now delinquent.

if(show_plots_flag){
  occurence = table(LC_Cleaned$acc_now_delinq)
  row_amount = as.integer(count(LC_Cleaned))
  ratio = table(LC_Cleaned$acc_now_delinq)/row_amount
  cbind(occurence, ratio)
  # >99% have 0
  # Keep column, 25 NA's
}


# In the following there is a subset with in exclusively data instances with NA's in tot_cur_bal, tot_coll_amt, total_rev_hi_lim simultaniously.
# Total 63276 instances.
# 
# Following appears:
# 
# initial_list_status: f -> with total f of 411144 -> ~15% of it
# mths_since_last_major_derog: "NA" -> total 599107 NA's -> ~10.5 % of it
# application_type: INDIVIDUAL -> total 798181 -> 8% of it
# annual_inc_joint: "NA"
# dti_joint: "NA"
# verification_status_joint: empty
# 
# The subset does not seem suspicious in other areas. Keep it in mind.


# 56. tot_coll_amt: Total collection amounts ever owed.
# Dropping columns because, it represents the total dollar amount of collections the borrower has accumulated over time.
# This value indicates the borrower's historical creditworthiness and risk level but is not related to setting an interest rate recommendation.

LC_Cleaned <- subset(LC_Cleaned, select = -tot_coll_amt)

# 57. tot_cur_bal: Total current balance of all accounts.
# tot_cur_bal represents the total balance currently owed by the borrower across all credit accounts.
# It’s a snapshot of the borrower’s current debt load and could be useful as a predictor variable in assessing the borrower’s creditworthiness or debt burden.

LC_Cleaned <- subset(LC_Cleaned, select = -tot_cur_bal)


# 58. total_rev_hi_lim: Total revolving high credit/credit limit.
# total_rev_hi_lim represents the maximum available credit limit on revolving accounts (like credit cards) across all of the borrower’s credit lines. 
# It’s a useful indicator of the borrower’s access to credit but not directly related to the interest rate on a new loan.
# Remove column

if(show_plots_flag){
  total_row_amount = as.integer(count(data))
  summary(LC_Cleaned$total_rev_hi_lim)
  boxplot(LC_Cleaned$total_rev_hi_lim)
}

LC_Cleaned <- subset(LC_Cleaned, select = -total_rev_hi_lim)


# 59. open_acc_6m: Number of open trades in last 6 months.

if(show_plots_flag){
  total_row_amount = as.integer(count(data))
  summary(LC_Cleaned$open_acc_6m)
  summary(LC_Cleaned$open_acc_6m)["NA's"]/total_row_amount
  # ~98% N/A -> Sparse data
  # All not N/A have the same issue_d Dec-2015
  # Remove column
}

LC_Cleaned <- subset(LC_Cleaned, select = -open_acc_6m)


# 60. open_il_6m: Number of currently active installment trades

if(show_plots_flag){
  total_row_amount = as.integer(count(data))
  summary(LC_Cleaned$open_il_6m)
  summary(LC_Cleaned$open_il_6m)["NA's"]/total_row_amount
  # ~98% N/A -> Sparse data
  # All not N/A have the same issue_d Dec-2015
  # Remove column
}

LC_Cleaned <- subset(LC_Cleaned, select = -open_il_6m)


# 61. open_il_12m: Number of installment accounts opened in past 12 months

if(show_plots_flag){
  total_row_amount = as.integer(count(data))
  summary(LC_Cleaned$open_il_12m)
  summary(LC_Cleaned$open_il_12m)["NA's"]/total_row_amount
  # ~98% N/A -> Sparse data
  # All not N/A have the same issue_d Dec-2015
  # Remove column
}

LC_Cleaned <- subset(LC_Cleaned, select = -open_il_12m)


# 62. open_il_24m: Number of installment accounts opened in past 24 months

if(show_plots_flag){
  total_row_amount = as.integer(count(data))
  summary(LC_Cleaned$open_il_24m)
  summary(LC_Cleaned$open_il_24m)["NA's"]/total_row_amount
  # ~98% N/A -> Sparse data
  # All not N/A have the same issue_d Dec-2015
  # Remove column
}

LC_Cleaned <- subset(LC_Cleaned, select = -open_il_24m)


# 63. mths_since_rcnt_il: Months since most recent installment accounts opened

if(show_plots_flag){
  total_row_amount = as.integer(count(data))
  summary(LC_Cleaned$mths_since_rcnt_il)
  summary(LC_Cleaned$mths_since_rcnt_il)["NA's"]/total_row_amount
  # ~98% N/A -> Sparse data
  # All not N/A have the same issue_d Dec-2015
  # Remove column
}

LC_Cleaned <- subset(LC_Cleaned, select = -mths_since_rcnt_il)


# 64. total_bal_il: Total current balance of all installment accounts

if(show_plots_flag){
  total_row_amount = as.integer(count(data))
  summary(LC_Cleaned$total_bal_il)
  summary(LC_Cleaned$total_bal_il)["NA's"]/total_row_amount
  # ~98% N/A -> Sparse data
  # All not N/A have the same issue_d Dec-2015
  # Remove column
}

LC_Cleaned <- subset(LC_Cleaned, select = -total_bal_il)


# 65. il_util: Ratio of total current balance to high credit/credit limit on all install acct

if(show_plots_flag){
  total_row_amount = as.integer(count(data))
  summary(LC_Cleaned$il_util)
  summary(LC_Cleaned$il_util)["NA's"]/total_row_amount
  # ~98% N/A -> Sparse data
  # All not N/A have the same issue_d Dec-2015
  # Remove column
}

LC_Cleaned <- subset(LC_Cleaned, select = -il_util)


# 66. open_rv_12m: Number of revolving trades opened in past 12 months.

if(show_plots_flag){
  total_row_amount = as.integer(count(data))
  summary(LC_Cleaned$open_rv_12m)
  summary(LC_Cleaned$open_rv_12m)["NA's"]/total_row_amount
  # ~98% N/A -> Sparse data
  # All not N/A have the same issue_d Dec-2015
  # Remove column
}

LC_Cleaned <- subset(LC_Cleaned, select = -open_rv_12m)


# 67. open_rv_24m: Number of revolving trades opened in past 24 months.

if(show_plots_flag){
  total_row_amount = as.integer(count(data))
  summary(LC_Cleaned$open_rv_24m)
  summary(LC_Cleaned$open_rv_24m)["NA's"]/total_row_amount
  # ~98% N/A -> Sparse data
  # All not N/A have the same issue_d Dec-2015
  # Remove column
}

LC_Cleaned <- subset(LC_Cleaned, select = -open_rv_24m)


# 68. max_bal_bc: Maximum current balance owed on all revolving accounts.

if(show_plots_flag){
  total_row_amount = as.integer(count(data))
  summary(LC_Cleaned$max_bal_bc)
  summary(LC_Cleaned$max_bal_bc)["NA's"]/total_row_amount
  # ~98% N/A -> Sparse data
  # All not N/A have the same issue_d Dec-2015
  # Remove column
}

LC_Cleaned <- subset(LC_Cleaned, select = -max_bal_bc)


# 69. all_util: Balance to credit limit on all trades.

if(show_plots_flag){
  total_row_amount = as.integer(count(data))
  summary(LC_Cleaned$all_util)
  summary(LC_Cleaned$all_util)["NA's"]/total_row_amount
  # ~98% N/A -> Sparse data
  # All not N/A have the same issue_d Dec-2015
  # Remove column
}

LC_Cleaned <- subset(LC_Cleaned, select = -all_util)


# 70. inq_fi: Number of personal finance inquiries.

if(show_plots_flag){
  total_row_amount = as.integer(count(data))
  summary(LC_Cleaned$inq_fi)
  summary(LC_Cleaned$inq_fi)["NA's"]/total_row_amount
  # ~98% N/A -> Sparse data
  # All not N/A have the same issue_d Dec-2015
  # Remove column
}

LC_Cleaned <- subset(LC_Cleaned, select = -inq_fi)


# 71. total_cu_tl: Number of finance trades.

if(show_plots_flag){
  total_row_amount = as.integer(count(data))
  summary(LC_Cleaned$total_cu_tl)
  summary(LC_Cleaned$total_cu_tl)["NA's"]/total_row_amount
  # ~98% N/A -> Sparse data
  # All not N/A have the same issue_d Dec-2015
  # Remove column
}

LC_Cleaned <- subset(LC_Cleaned, select = -total_cu_tl)


# 72. inq_last_12m: Number of credit inquiries in past 12 months.

if(show_plots_flag){
  total_row_amount = as.integer(count(data))
  summary(LC_Cleaned$inq_last_12m)
  summary(LC_Cleaned$inq_last_12m)["NA's"]/total_row_amount
  # ~98% N/A -> Sparse data
  # All not N/A have the same issue_d Dec-2015
  # Remove column
}

LC_Cleaned <- subset(LC_Cleaned, select = -inq_last_12m)































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