library(corrplot)
library(ggplot2)
library(tidyverse)
library(fastDummies)
library(GGally)

# Load the dataset into R
raw_data <- read.csv2("ressources/LCdata.csv", header = TRUE, row.names=NULL, sep=";")

# Preview the dataset structure and some basic statistics
head(raw_data)
summary(raw_data)

# Store the raw data into a new variable for cleaning and manipulation
my_data <- raw_data

# Feature Descriptions:
# 1. id: This is a unique identifier for each loan listing. Although it's essential for tracking loans, it holds no predictive value 
#    for analysis. We'll check for any anomalies (missing, zero, or negative values) and remove it as it's irrelevant for our analysis.

# Clean the 'id' column
sum(is.na(my_data$id))  # Checking for missing values in 'id'
sum(my_data$id == 0)    # Checking if any 'id' is zero
sum(my_data$id < 0)     # Checking for negative values in 'id'
# Dropping 'id' column since it's not useful for the decision-making process
df_cleaned <- subset(my_data, select = -id)

# 2. member_id: This represents the unique identifier for each borrower (member). It could potentially be useful in tracking multiple 
#    loans for the same borrower, but in this dataset, there's no evidence of multiple loans per member. We'll remove it after verifying for any anomalies.

# Clean the 'member_id' column
sum(is.na(df_cleaned$member_id))  # Checking for missing values
sum(df_cleaned$member_id == 0)    # Checking for zero values
sum(df_cleaned$member_id < 0)     # Checking for negative values
sum(duplicated(df_cleaned$member_id))  # Checking for duplicate 'member_id'
# Since it’s not relevant, we'll remove 'member_id'
df_cleaned <- subset(df_cleaned, select = -member_id)

# 3. loan_amnt: This column records the amount of money requested by the borrower. It's an essential feature for analysis and 
#    should be non-negative, non-zero, and without missing values. We’ll explore its distribution and check for any irregularities.

# Explore the 'loan_amnt' column
summary(df_cleaned$loan_amnt)  # Summary statistics for the loan amount
# Visualizing the distribution of loan amounts with different binwidths
ggplot(data = df_cleaned) +
  geom_histogram(aes(x = loan_amnt), binwidth = 100) +
  labs(title = "Loan Amount Distribution (binwidth = 100)")
ggplot(data = df_cleaned) +
  geom_histogram(aes(x = loan_amnt), binwidth = 250) +
  labs(title = "Loan Amount Distribution (binwidth = 250)")
ggplot(data = df_cleaned) +
  geom_histogram(aes(x = loan_amnt), binwidth = 500) +
  labs(title = "Loan Amount Distribution (binwidth = 500)")
ggplot(data = df_cleaned) +
  geom_histogram(aes(x = loan_amnt), binwidth = 1000) +
  labs(title = "Loan Amount Distribution (binwidth = 1000)")

# Checking for missing, zero, or negative values in 'loan_amnt'
sum(is.na(df_cleaned$loan_amnt))  # Looking for missing values
sum(df_cleaned$loan_amnt == 0)    # Checking if any amounts are zero
sum(df_cleaned$loan_amnt < 0)     # Ensuring no negative values
# Identify potential outliers in 'loan_amnt' using boxplot statistics
boxplot.stats(df_cleaned$loan_amnt)$out

# 4. funded_amnt: This column shows the amount of money that has been funded for the loan at the current point. It's essential to analyze 
#    the progress of loan funding. We will check for anomalies and compare it with the loan amount requested.\
#    not available in the Test data!!!

# Analyze 'funded_amnt' column
summary(df_cleaned$funded_amnt)  # Summary statistics for funded amount
# Visualizing the relationship between 'loan_amnt' and 'funded_amnt'
ggplot(data = df_cleaned) +
  geom_point(aes(x = loan_amnt, y = funded_amnt), alpha = 0.1) +
  labs(title = "Loan Amount vs Funded Amount")

# Checking for missing, zero, or negative values in 'funded_amnt'
sum(is.na(df_cleaned$funded_amnt))  # Looking for missing values
sum(df_cleaned$funded_amnt == 0)    # Checking for zero amounts
sum(df_cleaned$funded_amnt < 0)     # Ensuring no negative values
# Identify potential outliers in 'funded_amnt' using boxplot statistics
boxplot.stats(df_cleaned$funded_amnt)$out

# 5. funded_amnt_inv: This represents the amount funded by investors for the loan. It's currently stored as a character but 
#    should be converted to an integer for analysis. We’ll also explore how this compares to the total loan amount requested.
#    not available in the Test data!!!


# Clean and analyze 'funded_amnt_inv'
df_cleaned$funded_amnt_inv <- as.integer(df_cleaned$funded_amnt_inv)  # Converting to integer
summary(df_cleaned$funded_amnt_inv)  # Summary statistics of funded amount by investors
# Visualizing the relationship between 'loan_amnt' and 'funded_amnt_inv'
ggplot(data = df_cleaned) +
  geom_point(aes(x = loan_amnt, y = funded_amnt_inv), alpha = 0.1) +
  labs(title = "Loan Amount vs Investor Funded Amount")

# Visualizing the relationship between 'funded_amnt' and 'funded_amnt_inv'
ggplot(data = df_cleaned) +
  geom_point(aes(x = funded_amnt_inv, y = funded_amnt), alpha = 0.1) +
  labs(title = "Investor Funded Amount vs Funded Amount")

# Checking for missing, zero, or negative values in 'funded_amnt_inv'
sum(is.na(df_cleaned$funded_amnt_inv))  # Looking for missing values
sum(df_cleaned$funded_amnt_inv == 0)    # Checking for zero amounts which is possible in this data
sum(df_cleaned$funded_amnt_inv < 0)     # Ensuring no negative values
# Identify potential outliers in 'funded_amnt_inv' using boxplot statistics
boxplot.stats(df_cleaned$funded_amnt_inv)$out

# 6. term: This column specifies the length of the loan in months (either 36 or 60 months). It is essentially a categorical variable, 
#    so we’ll trim any unnecessary spaces and explore how the term affects loan amounts and funding.

# Clean and analyze the 'term' column
df_cleaned$term <- str_trim(df_cleaned$term)  # Trimming unnecessary spaces
summary(df_cleaned$term)  # Summary of loan term distribution
# Visualizing the distribution of loan terms (more than twice of 36M compared to 60)
ggplot(df_cleaned, aes(x = term)) + 
  geom_bar() +
  labs(title = "Distribution of Loan Terms")

# Analyze 'funded_amnt_inv' for loans with 36 months and 60 months terms (Similar.. lower therm more for lower amounts)
# Scatter plot for loans with 36 months term
ggplot(data = filter(df_cleaned, term == "36 months"), aes(x = loan_amnt, y = funded_amnt_inv)) +
  geom_point(alpha = 0.05) +
  labs(title = "Loan Amount vs Investor Funded Amount (36 months)")

# Scatter plot for loans with 60 months term
ggplot(data = filter(df_cleaned, term == "60 months"), aes(x = loan_amnt, y = funded_amnt_inv)) +
  geom_point(alpha = 0.05) +
  labs(title = "Loan Amount vs Investor Funded Amount (60 months)")

# Visualizing loan amount by term using a box plot
# We see a difference in loan amount. 
ggplot() + 
  geom_boxplot(mapping = aes(x = term, y = loan_amnt), data = df_cleaned) +
  labs(title = "Loan Amount Distribution by Term")

# Visualizing funded amount by term using a box plot
ggplot() + 
  geom_boxplot(mapping = aes(x = term, y = funded_amnt), data = df_cleaned) +
  labs(title = "Funded Amount by Term")

# Visualizing the ratio of funded amount to loan amount for each term (Ratio for 60 month higher. Credibility higher I would keept this.)
ggplot() + 
  geom_boxplot(mapping = aes(x = term, y = funded_amnt / loan_amnt), data = df_cleaned) +
  labs(title = "Funded Amount Ratio by Term")

# 7. int_rate: The interest rate assigned to the loan. This is a key feature for analysis.
#    It's originally stored as a character, but should be converted to a float (double).
#    We'll also check for missing values and explore its distribution.

# Convert 'int_rate' to a numeric format
head(df_cleaned$int_rate)
df_cleaned$int_rate <- as.double(df_cleaned$int_rate)
head(df_cleaned$int_rate)

# Visualize the distribution of interest rates with different binwidths
ggplot(df_cleaned) +
  geom_histogram(aes(x = int_rate), binwidth = 0.1) +
  labs(title = "Distribution of Interest Rate (binwidth = 0.1)")
ggplot(df_cleaned) +
  geom_histogram(aes(x = int_rate), binwidth = 0.25) +
  labs(title = "Distribution of Interest Rate (binwidth = 0.25)")
ggplot(df_cleaned) +
  geom_histogram(aes(x = int_rate), binwidth = 0.5) +
  labs(title = "Distribution of Interest Rate (binwidth = 0.5)")
ggplot(df_cleaned) +
  geom_histogram(aes(x = int_rate), binwidth = 1) +
  labs(title = "Distribution of Interest Rate (binwidth = 1)")

# Interest rate vs loan amount and term
ggplot(data = df_cleaned, mapping = aes(x = loan_amnt, y = int_rate, color = term)) +
  geom_point(alpha = 0.05) +
  labs(title = "Loan Amount vs Interest Rate, colored by Term")

# Checking for missing, zero, or negative values in 'int_rate'
sum(is.na(df_cleaned$int_rate))  # Checking for missing values
sum(df_cleaned$int_rate == 0)    # Check for zero values
sum(df_cleaned$int_rate < 0)     # Ensure there are no negative values

# we have around 1K ouliers here.. need to be discussed 
length(boxplot.stats(df_cleaned$int_rate)$out)

# 8. installment: The monthly installment the borrower needs to pay.
# Might have a direct correlation with interest rate and loan amount
#     We'll convert it to double and remove the feature.
#    not available in the Test data!!!


# Convert 'installment' to double and remove it afterward
head(df_cleaned$installment)
df_cleaned$installment <- as.double(df_cleaned$installment)
head(df_cleaned$installment)

# 9. emp_title: The borrower’s job title, provided during the application.
#    This is a free-text field that is not useful for predictive analysis, so it will be dropped.
#   * Employer Title replaces Employer Name for all loans listed after 9/23/2013

# Check for dublicates
# Calculate the proportion of duplicate 'emp_title' values
num_duplicates <- sum(duplicated(df_cleaned$emp_title))  # Number of duplicate titles
total_titles <- nrow(df_cleaned)  # Total number of rows

# Proportion of duplicates
duplicate_proportion <- num_duplicates / total_titles
duplicate_proportion


# Drop 'emp_title' as it's not useful for analysis
df_cleaned <- subset(df_cleaned, select = -emp_title)

# 10. emp_length: The number of years the borrower has been employed.
#    Convert this from string to integer and check for missing values.

# Convert 'emp_length' to integer
head(df_cleaned$emp_length)
df_cleaned$emp_length <- as.integer(ordered(df_cleaned$emp_length, levels = c("< 1 year", "1 year", "2 years", "3 years", "4 years", "5 years", "6 years", "7 years", "8 years", "9 years", "10+ years"))) - 1
head(df_cleaned$emp_length)

# Checking for missing, zero, or negative values in 'emp_length'
# over 40000 missing values.. Might be removed
sum(is.na(df_cleaned$emp_length))  # Looking for missing values
sum(df_cleaned$emp_length == 0)    # Checking for zero amounts which is possible in this data
sum(df_cleaned$emp_length < 0)     # Ensuring no negative values

# I'll remove it for now 
df_cleaned <- subset(df_cleaned, select = -emp_length)


# 11. home_ownership: Indicates whether the borrower owns, rents, or has a mortgage on their home.
#     Convert this to an ordered set of integers and create dummy columns for categorical analysis.
#     We have 47 missing values.. to OTHER? Or shall we drop them? only


# Convert 'home_ownership' to ordered integer
head(df_cleaned$home_ownership)
df_cleaned$home_ownership <- as.integer(ordered(df_cleaned$home_ownership, levels = c("OTHER", "RENT", "MORTGAGE", "OWN")))
head(df_cleaned$home_ownership)

sum(is.na(df_cleaned$home_ownership))  # Looking for missing values
# Drop rows with missing values in 'home_ownership'
filter(df_cleaned, is.na(df_cleaned$home_ownership))
df_cleaned <- filter(df_cleaned, !is.na(df_cleaned$home_ownership))
filter(df_cleaned, is.na(df_cleaned$home_ownership))

# Create dummy columns for 'home_ownership'
df_cleaned <- dummy_columns(df_cleaned, select_columns = "home_ownership", remove_selected_columns = TRUE)



# 12. annual_inc: The annual income reported by the borrower during registration.
#     Convert from string to integer, check for missing, zero, or extreme values, and handle any income outliers.
summary(df_cleaned$annual_inc)  # Summary of loan term distribution

sum(is.na(df_cleaned$annual_inc))  # Looking for missing values
sum(df_cleaned$annual_inc == 0)    # Checking for zero amounts. There are 2
sum(df_cleaned$annual_inc < 0)     # Ensuring no negative values

# Convert 'annual_inc' to integer
df_cleaned <- filter(df_cleaned, !is.na(annual_inc))
df_cleaned$annual_inc <- as.integer(df_cleaned$annual_inc)
head(df_cleaned$annual_inc)

ggplot(data = df_cleaned, aes(x = annual_inc)) +
  geom_histogram(binwidth = 50000, fill = "blue", color = "black") +
  labs(title = "Histogram of Annual Income", x = "Annual Income", y = "Count")

ggplot(data = df_cleaned, aes(y = annual_inc)) +
  geom_boxplot(fill = "lightblue", color = "black") +
  labs(title = "Boxplot of Annual Income", y = "Annual Income")
# We have a lot of outliers here
boxplot.stats(df_cleaned$annual_inc)$out

#lets see how it looks like from one million upwards
ggplot(data = filter(df_cleaned, annual_inc > 1000000), aes(x = annual_inc)) +
  geom_histogram(binwidth = 100000, fill = "blue", color = "black") +
  labs(title = "Histogram of Annual Income (Before Adjustment)", x = "Annual Income", y = "Count")

# Needs to be discussed.. I suppose that pople entered the income incorrectely. Seems like they entered the "cents"as well.
# Suggestion: If this is the case the we need a threshold (here 1M).. could also be real (Sucessfull people take out loans!)

#df_cleaned$annual_inc <- ifelse(df_cleaned$annual_inc >= 1000000, df_cleaned$annual_inc / 100, df_cleaned$annual_inc)
#filter(df_cleaned, annual_inc > 1000000)

# Second way.. impute the outlier by dividing by 100
#outliers <- boxplot.stats(df_cleaned$annual_inc)$out
#df_cleaned$annual_inc <- ifelse(df_cleaned$annual_inc %in% outliers, df_cleaned$annual_inc / 100, df_cleaned$annual_inc)

# Summary statistics of 'annual_inc'
summary(df_cleaned$annual_inc)

# 13. verification_status: Indicates whether the borrower’s income has been verified.
#     Convert this to an integer and analyze its relationship to the interest rate.

# Convert 'verification_status' to integer
head(df_cleaned$verification_status)
df_cleaned$verification_status <- as.integer(factor(df_cleaned$verification_status)) - 1
head(df_cleaned$verification_status)

# Check for missing values in 'verification_status'
sum(is.na(df_cleaned$verification_status))  # Looking for missing values

# Analyze the relationship between 'verification_status' and 'int_rate'
# Create boxplots for all verification_status categories in one plot (interest rate higher for people with more income.. maybe becaus they loan more money)
ggplot(data = df_cleaned, aes(x = as.factor(verification_status), y = int_rate)) +
  geom_boxplot() +
  labs(title = "Boxplot of Interest Rate by Verification Status", x = "Verification Status", y = "Interest Rate") +
  scale_x_discrete(labels = c("0" = "Not Verified", "1" = "Source Verified", "2" = "Verified"))


# 14. issue_d: The date when the loan was funded.
#     Not part of Test data!!!

# Drop 'issue_d' as it won't be relevant for future data
df_cleaned <- subset(df_cleaned, select = -issue_d)

# 15. loan_status: The current status of the loan (e.g., fully paid, defaulted). Not part of test data!!!!
#     This is only applicable post-loan issuance, so it’s not useful for predicting interest rates. We'll drop it.

levels(factor(df_cleaned$loan_status))

# Drop 'loan_status' as it’s not relevant for the analysis
df_cleaned <- subset(df_cleaned, select = -loan_status)

# 16. pymnt_plan: Indicates if a payment plan is in place for the loan.
#     This is not part of the test data!!, so it will be dropped.


# Drop 'pymnt_plan' as it's irrelevant for the analysis
df_cleaned <- subset(df_cleaned, select = -pymnt_plan)

# 17. url: The URL for the loan listing on the LC platform.
#     This feature has no analytical value, so it will be dropped.

# Drop 'url' as it’s irrelevant for analysis
df_cleaned <- subset(df_cleaned, select = -url)

# 18. desc: A text field describing the loan provided by the borrower.
#     This feature is difficult to analyze, so we will drop it.

# Drop 'desc' as it is not suitable for analysis
df_cleaned <- subset(df_cleaned, select = -desc)
