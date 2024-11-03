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
df_cleaned <- my_data

#Feature Descriptions:
# 1. total_pymnt: Payments received to date for total amount funded.
# The column has the type character and should be casted as a numerical value.
# Not available in the Test data!!!

# This column tracks the repayment behaviour of the borrower, it is likely not a factor
# that would directly determine the interest rate at the time othe loan is issued. The interest rate is usually set at the beginning of the loan based on the
# borrowers credit risk and financial history, not on how much the borrower has paid over time.

# Conclusion:
# The interest rate is generally determined based on risk factors before loan issuance, such as creditworthiness, income, loan amount, and loan term.
# total_pymnt reflects payment history, which happens after the loan has been issued, so it wouldn’t be a factor used to set the original interest rate.

# Explore the 'total_pymnt' column
df_cleaned$total_pymnt <- as.numeric(df_cleaned$total_pymnt) # cast chr to numeric
summary(df_cleaned$total_pymnt)  # Summary statistics for the total_pymnt
str(df_cleaned$total_pymnt)

# Checking for missing, zero, or negative values in 'total_pymnt'
sum(is.na(df_cleaned$total_pymnt))  # Looking for missing values
sum(df_cleaned$total_pymnt == 0)    # Checking if any amounts are zero
sum(df_cleaned$total_pymnt < 0)     # Ensuring no negative values
# Identify potential outliers in 'total_pymnt' using boxplot statistics
boxplot.stats(df_cleaned$total_pymnt)$out

# Not relevant removing 'total_pymnt'
df_cleaned <- subset(df_cleaned, select = -total_pymnt)



# 2. total_pymnt_inv: 
# reflects post-load payment behaviour, whereas the interest rate is determined before any payments are made.
# Not available in the Test data!!!
df_cleaned <- subset(df_cleaned, select = -total_pymnt_inv)



# 3. total_rec_prncp: Principal received to date
# total_rec_prncp tracks the amount of principal repaid after the loan has been issued, whereas the interest rate is determined before any payments are made.
# Not available in the Test data!!!
df_cleaned$total_rec_prncp <- as.numeric(df_cleaned$total_rec_prncp) # cast chr to numeric
summary(df_cleaned$total_rec_prncp)  # Summary statistics for the total_rec_prncp
str(df_cleaned$total_rec_prncp)
df_cleaned <- subset(df_cleaned, select = -total_rec_prncp)



#  4. total_rec_int: Interest received to date
# total_rec_int reflects post-loan repayment behavior and simply tracks the interest that has been paid so far. 
# Since the interest rate is already set when the loan is originated, total_rec_int is an outcome of the interest rate, not a factor influencing its determination.
# Not available in the Test data!!!
df_cleaned <- subset(df_cleaned, select = -total_rec_int)


# 5. total_rec_late_fee: Late fees received to date
# total_rec_late_fee tracks borrower behavior during the loan repayment process but does not provide information relevant to determining the interest rate at loan origination
# Not available in the Test data!!!
df_cleaned <- subset(df_cleaned, select = -total_rec_late_fee)



# 6. recoveries: post charge off gross recovery
# recoveries is a post-loan feature that captures the amount recovered after the loan has defaulted,
# Not available in the Test data!!!
df_cleaned <- subset(df_cleaned, select = -recoveries)



#7. collection_recovery_fee: post charge off collection fee
# collection_recovery_fee represents a post-loan event related to recovering funds from a borrower after they have defaulted or missed payments
# Not available in the Test data!!!
df_cleaned <- subset(df_cleaned, select = -collection_recovery_fee)



# 8. last_pymnt_d: Last month payment was received
# last_pymnt_d records the date of the most recent payment, which occurs after the loan has been issued 
# Not available in the Test data!!!
df_cleaned <- subset(df_cleaned, select = -last_pymnt_d)



#9. last_pymnt_amnt: Last total payment amount received
# last_pymnt_amnt reflects post-loan payment behavior and tracks the most recent payment made by the borrower.
# Not available in the Test data!!!
df_cleaned <- subset(df_cleaned, select = -last_pymnt_amnt)



# 10. next_pymnt_d: Next scheduled payment date
# next_pymnt_d is not eseential as it is a post-loan feature that indicates the next scheduled payment date, which occurs after the loan has been issued.
# Not available in the Test data!!!
df_cleaned <- subset(df_cleaned, select = -next_pymnt_d)




# 11. Last_credit_pull_d: The most recent month LC pulled credit for this loan
# tracks when the lender last accessed the borrower’s credit information, which happens after the loan is issued. 
df_cleaned <- subset(df_cleaned, select = -last_credit_pull_d)




# 12. collections_12_mths_ex_med: Number of collections in 12 months excluding medical collections
# collections_12_mths_ex_med reflects the borrower’s history of debt mismanagement over the past 12 months (excluding medical-related collections), 
# which is a significant indicator of credit risk. Lenders use this information to assess the borrower’s likelihood of default and adjust the interest rate accordingly.

# Explore collections_12_mths_ex_med
summary(df_cleaned$collections_12_mths_ex_med)  # Summary statistics for the collections_12_mths_ex_med
str(df_cleaned$collections_12_mths_ex_med)

# Remove values greater than 12
df_cleaned <- df_cleaned[df_cleaned$collections_12_mths_ex_med <= 12, ]

# Checking for missing, zero, or negative values in 'collections_12_mths_ex_med'
sum(is.na(df_cleaned$collections_12_mths_ex_med))  # Looking for missing values
sum(df_cleaned$collections_12_mths_ex_med == 0)    # Checking if any amounts are zero
sum(df_cleaned$collections_12_mths_ex_med < 0)     # Ensuring no negative values
# 126 NA's might need to be imputed.

# Identify potential outliers in 'collections_12_mths_ex_med' using boxplot statistics
boxplot.stats(df_cleaned$collections_12_mths_ex_med)
boxplot(df_cleaned$collections_12_mths_ex_med, outline=TRUE, col="lightblue", border="black")
hist(df_cleaned$collections_12_mths_ex_med,, col = "lightblue", border = "black")
# Interest rate vs collections 12 months excluding medical
ggplot(data = df_cleaned, mapping = aes(x = collections_12_mths_ex_med, y = int_rate)) +
  geom_point(alpha = 0.05) +
  labs(title = "collections 12 months excluding medical vs Interest Rate")

# Consclusion:
# collections_12_mths_ex_med is an important feature for predicting int_rate, as it directly impacts the lender’s risk assessment during the loan origination process.





# 13. mths_since_last_major_derog: Months since most recent 90-day or worse rating
# mths_since_last_major_derog refers to the number of months since the borrower's last major derogatory mark on their credit file.
# This feature is highly relevant, as it directly reflects the borrower's credit history. It indicates how recently the borrower experienced a significant credit problem.

summary(df_cleaned$mths_since_last_major_derog)

# Checking for missing, zero, or negative values in 'mths_since_last_major_derog'
sum(is.na(df_cleaned$mths_since_last_major_derog))  # Looking for missing values
sum(df_cleaned$mths_since_last_major_derog == 0)    # Checking if any amounts are zero
sum(df_cleaned$mths_since_last_major_derog < 0)     # Ensuring no negative values
# mths_since_last_major_derog has a lot of missing values NA's (599106), if the borrower has no major deroagatory event the value is NA.

# Conclusion: A borrower with a recent major derogatory event is seen as a higher risk and will likely face a higher interest rate. On the other hand,
# borrowers with no recent derogatory marks are seen as less risky, potentially resulting in a lower interest rate.

# set all NA to the highest possible value: 0
df_cleaned$mths_since_last_major_derog[is.na(df_cleaned$mths_since_last_major_derog)] <- 0
summary(df_cleaned$mths_since_last_major_derog)

# Interest rate vs mths_since_last_major_derog
ggplot(data = df_cleaned, mapping = aes(x = mths_since_last_major_derog, y = int_rate)) +
  geom_point(alpha = 0.05) +
  labs(title = "mths_since_last_major_derog vs Interest Rate")





# 14. policy_code: publicly available policy_code=1, new products not publicly available policy_code=2
# policy_code might have a moderate influence on int_rate, particularly if different policies or products have different interest rate models

summary(df_cleaned$policy_code)

# Checking for missing, zero, or negative values in 'mths_since_last_major_derog'
sum(is.na(df_cleaned$policy_code))  # Looking for missing values
sum(df_cleaned$policy_code == 0)    # Checking if any amounts are zero
sum(df_cleaned$policy_code < 0)     # Ensuring no negative values
# 126 NA's might need to be imputed.

# Identify potential outliers in 'collections_12_mths_ex_med' using boxplot statistics
boxplot.stats(df_cleaned$policy_code)
boxplot(df_cleaned$policy_code, outline=TRUE, col="lightblue", border="black")
hist(df_cleaned$policy_code,, col = "lightblue", border = "black")

# Conclusion:
# the policy code has it's min and max at 1, the 126 data-point which have NA might be relevant. --> to be discussed
# removing it for now
df_cleaned <- subset(df_cleaned, select = -policy_code)




# 15. application_type: Indicates whether the loan is an individual application or a joint application with two co-borrowers
# application_type differentiates between individual and joint applications. 
# Joint applications tend to reduce risk by incorporating the financial profiles and incomes of two borrowers, which may result in a lower interest rate.

summary(df_cleaned$application_type)

# Checking for missing, zero, or negative values in 'mths_since_last_major_derog'
sum(is.na(df_cleaned$application_type))  # Looking for missing values
sum(df_cleaned$application_type == 0)    # Checking if any amounts are zero
sum(df_cleaned$application_type < 0)     # Ensuring no negative values
# 126 NA's might need to be imputed.

# Conclusion:
# Individual applications are considered higher risk due to reliance on a single borrower’s financial situation.

# transforming 'application_type' as numeric factors where:
# 0 is 'individual' a single borrower applies for the loan.
# 1 is 'joint' the loan application is made by two co-borrowers, and both incomes, credit profiles, and financial situations are considered.
df_cleaned$application_type <- as.numeric(factor(df_cleaned$application_type))

# remove application_type for now, due to highly unbalanced dataset
df_cleaned <- subset(df_cleaned, select = -application_type)




# 16. annual_inc_joint: The combined self-reported annual income provided by the co-borrowers during registration
# annual_inc_joint reflects the combined financial capacity of two borrowers in a joint loan application, 
# and higher combined income typically reduces the lender’s risk, leading to a lower interest rate.

summary(df_cleaned$annual_inc_joint)

# Conclusion:
# cast 'annual_inc_joint' as numeric
# we might need to merge 'annual_inc' and 'annual_inc_joint'
df_cleaned$annual_inc_joint <- as.numeric(df_cleaned$annual_inc_joint)


# Checking for missing, zero, or negative values in 'annual_inc_joint'
sum(is.na(df_cleaned$annual_inc_joint))  # Looking for missing values
sum(!is.na(df_cleaned$annual_inc_joint))  # Looking for values
sum(df_cleaned$annual_inc_joint == 0)    # Checking if any amounts are zero
sum(df_cleaned$annual_inc_joint < 0)     # Ensuring no negative values
# annual_inc_joint has a lot of missing values NA's (798638), if the application_type is joint, we have 416 values.

# Checking for missing, zero, or negative values in 'annual_inc'
sum(is.na(df_cleaned$annual_inc))  # Looking for missing values
sum(!is.na(df_cleaned$annual_inc))  # Looking for values
sum(df_cleaned$annual_inc == 0)    # Checking if any amounts are zero
sum(df_cleaned$annual_inc < 0)     # Ensuring no negative values

# remove 'annual_inc_joint' for now, due to highly unbalanced dataset
df_cleaned <- subset(df_cleaned, select = -annual_inc_joint)




# 17. dti_joint: A ratio calculated using the co-borrowers' total monthly payments on the total debt obligations, excluding mortgages and the requested LC loan, divided by the co-borrowers' combined self-reported monthly income
# dti_joint refers to the debt-to-income (DTI) ratio for joint loan applications. 
# It is calculated as the total monthly debt obligations of both co-borrowers (excluding mortgage payments) divided by their combined monthly income. 
# This ratio provides a picture of how much of the co-borrowers' income is committed to repaying debt, helping lenders assess their ability to take on additional loans.
 

# Conclusion:
# cast 'dti_joint' as numeric
# A higher DTI ratio signifies higher risk to the lender, resulting in a higher interest rate. 
# Conversely, a lower DTI ratio implies a greater ability to handle new debt, leading to a lower interest rate.
df_cleaned$dti_joint <- as.numeric(df_cleaned$dti_joint)

summary(df_cleaned$dti_joint)
# Checking for missing, zero, or negative values in 'dti_joint'
sum(df_cleaned$dti_joint == "NA")  # Looking for missing values
sum(is.na(df_cleaned$dti_joint))  # Looking for missing values
sum(!is.na(df_cleaned$dti_joint))  # Looking for values
sum(df_cleaned$dti_joint == 0)    # Checking if any amounts are zero
sum(df_cleaned$dti_joint < 0)     # Ensuring no negative values
# dti_joint has a lot of missing values NA's (798180), if the application_type is joint, we have 458 values.

# remove 'dti_joint' for now, due to highly unbalanced dataset
df_cleaned <- subset(df_cleaned, select = -dti_joint)



# 18. verification_status_joint: Indicates if the co-borrowers' joint income was verified by LC, not verified, or if the income source was verified

summary(unique(df_cleaned$verification_status_joint))

# transforming 'verification_status_joint' as numeric factors where:
# 1 is 'Not Verified' the loan application is made by two co-borrowers, and both incomes, credit profiles, and financial situations are considered.
# 2 is 'Source Verified'' 
# 3 is 'Verified' 
df_cleaned$verification_status_joint[is.na(df_cleaned$verification_status_joint)] <- "Not Verified"
df_cleaned$verification_status_joint <- as.numeric(ordered(df_cleaned$verification_status_joint, levels = c("Not Verified", "Source Verified", "Verified")))

sum(df_cleaned$verification_status_joint == 1)    # Checking if any amounts are zero
sum(df_cleaned$verification_status_joint == 2)    # Checking if any amounts are zero
sum(df_cleaned$verification_status_joint == 3)    # Checking if any amounts are zero
sum(is.na(df_cleaned$verification_status_joint))  

# Conclusion: Fully verified income provides assurance to the lender that the borrowers can afford to repay the loan, leading to a lower interest rate. 
# In contrast, unverified income increases uncertainty and risk, which may result in a higher interest rate.
# we might want to merge 'verification_status_joint' and 'verification_status'

df_cleaned$verification_status <- as.numeric(ordered(df_cleaned$verification_status, levels = c("Not Verified", "Source Verified", "Verified")))
# 1 is 'Not Verified'
# 3 is 'Verified'' 
# 4 is 'Source Verified' 
sum(df_cleaned$verification_status == 1)
sum(df_cleaned$verification_status == 
sum(df_cleaned$verification_status == 
sum(is.na(df_cleaned$verification_status))