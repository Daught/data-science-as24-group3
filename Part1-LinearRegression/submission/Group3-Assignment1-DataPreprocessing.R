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


##############   1. Preliminaries                                               ##########################

# Install
if(!require('corrplot')) install.packages('corrplot', dependencies = TRUE)
if(!require('ggplot2')) install.packages('ggplot2', dependencies = TRUE)
if(!require('tidyverse')) install.packages('tidyverse', dependencies = TRUE)
if(!require('fastDummies')) install.packages('fastDummies', dependencies = TRUE)
if(!require('GGally')) install.packages('GGally', dependencies = TRUE)
if(!require('dplyr')) install.packages('dplyr', dependencies = TRUE)
if(!require("caret")) install.packages("caret", dependencies = TRUE)
if(!require("VIM")) install.packages("VIM", dependencies = TRUE)
if(!require("xgboost")) install.packages("xgboost", dependencies = TRUE)
if(!require("glmnet")) install.packages("glmnet", dependencies = TRUE)
if(!require("earth")) install.packages("earth", dependencies = TRUE)
if(!require("gbm")) install.packages("gbm", dependencies = TRUE)
if(!require("doParallel")) install.packages("doParallel", dependencies = TRUE)

# Load packages
library('dplyr')
library('caret')
library('GGally')
library('fastDummies')
library('tidyverse')
library('ggplot2')
library('corrplot')
library('VIM')  # For kNN imputation
library('xgboost')
library('glmnet')
library('earth')
library('gbm')
library('xgboost')
library('doParallel')


# load data set
#data <- read.csv2("ressources/LCdata.csv", header = TRUE, row.names=NULL, sep=";")
dataset_file_path = "./ressources/LCdata.csv"
data <- read.csv2(dataset_file_path, header = TRUE, row.names=NULL, sep=";")

LC <- data # make a copy of the original data set, so that we dont mess with it

##############   Step 2 - Data Preprocessing                                    ##########################

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

LC_Cleaned$term <- gsub("\\s+", "", LC_Cleaned$term) # Remove every white space to ensure consistency
# Convert 'term' to ordered factor
LC_Cleaned$term <- factor(LC_Cleaned$term, levels = c("36months", "60months"), ordered = TRUE)


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
# Drop 'emp_length' as it's not useful for analysis
LC_Cleaned <- subset(LC_Cleaned, select = -emp_length)


# 11. home_ownership: Indicates whether the borrower owns, rents, or has a mortgage on their home.
#     Convert this to an ordered set of integers and create dummy columns for categorical analysis.
#     We have 47 missing values.. to OTHER? Or shall we drop them? only

# these values are assigned to Other (Other is unclear as well.)
# Replace "ANY" and "NONE" with "OTHER" in the home_ownership column
LC_Cleaned$home_ownership[LC_Cleaned$home_ownership %in% c("ANY", "NONE")] <- "OTHER"

# Convert 'home_ownership' to ordered factor instead of integer
LC_Cleaned$home_ownership <- factor(LC_Cleaned$home_ownership, levels = c("OTHER", "RENT", "MORTGAGE", "OWN"), ordered = TRUE)

# Create dummy columns for 'home_ownership'
# LC_Cleaned <- dummy_columns(LC_Cleaned, select_columns = "home_ownership", remove_selected_columns = TRUE)



# 12. annual_inc: The annual income reported by the borrower during registration.
#     Convert from string to integer, check for missing, zero, or extreme values, and handle any income outliers.
# Remove NA values
LC_Cleaned <- filter(LC_Cleaned, !is.na(annual_inc))
# Remove rows with negative values in 'annual_inc'
LC_Cleaned <- filter(LC_Cleaned, annual_inc >= 0)
# Convert 'annual_inc' to integer
LC_Cleaned$annual_inc <- as.integer(LC_Cleaned$annual_inc)


# 13. verification_status: Indicates whether the borrower’s income has been verified.
#     Convert this to an integer and analyze its relationship to the interest rate.
LC_Cleaned$verification_status <- as.integer(factor(LC_Cleaned$verification_status)) - 1 # Convert 'verification_status' to integer


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
LC_Cleaned$purpose <- tolower(LC_Cleaned$purpose) # Convert all characters in the column to lowercase
LC_Cleaned$purpose <- factor(LC_Cleaned$purpose) # Change to unordered factor


# 20. 'title': The loan title provided by the borrower.
LC_Cleaned <- subset(LC_Cleaned, select = -title)

# 21. 'zip_code': The first 3 numbers of the zip code provided by the borrower in the loan application.
# As more reducing does not make sense (too big and not necessarily logical connected areas with only 2 digits) we will drop it and use 'addr_state' instead
LC_Cleaned <- subset(LC_Cleaned, select = -zip_code)

# 22. 'addr_state': The state provided by the borrower in the loan application.
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
LC_Cleaned$region <- factor(LC_Cleaned$region) # Change to factor
# Drop addr_state column as we will 'region' instead
LC_Cleaned <- subset(LC_Cleaned, select = -addr_state)


# 23. 'dti': A ratio calculated using the borrower’s total monthly debt payments on the total debt obligations, 
# excluding mortgage and the requested LC loan, divided by the borrower’s self-reported monthly income.
# A lower DTI generally indicates a more manageable debt load relative to income, suggesting the borrower is less risky.
# A higher DTI suggests a higher burden of debt relative to income, which may indicate greater financial strain or risk.
## This needs to be differentiated with application type ("INDIVIDUAL" --> "dti" and "JOINT" --> "dti_joint")
LC_Cleaned$dti[LC_Cleaned$annual_inc == ""] <- NA # Handle empty strings, if there are some
LC_Cleaned$dti <- as.numeric(LC_Cleaned$dti) # Change to number


# 24. 'delinq_2yrs': The number of 30+ days past-due incidences of delinquency in the borrower's credit file for the past 2 years.
LC_Cleaned$delinq_2yrs[is.na(LC_Cleaned$delinq_2yrs)] <- 0 # Impute NA values in delinq_2yrs with 0


# 25. 'earliest_cr_line': The month the borrower's earliest reported credit line was opened.
# Remove any leading or trailing whitespace and any non-printing characters
LC_Cleaned$earliest_cr_line <- trimws(LC_Cleaned$earliest_cr_line)
LC_Cleaned$earliest_cr_line <- gsub("[^[:print:]]", "", LC_Cleaned$earliest_cr_line)

# Set locale to English for date conversion to be sure date-conversion is working
Sys.setlocale("LC_TIME", "C")  # "C" is used for the default English locale 
LC_Cleaned$earliest_cr_line[LC_Cleaned$earliest_cr_line == ""] <- NA # Replace any empty strings with NA
# Convert earliest_cr_line from "Mon-YYYY" to Date format, assuming the first day of the month
LC_Cleaned$earliest_cr_line_date <- as.Date(paste0("01-", LC_Cleaned$earliest_cr_line), format = "%d-%b-%Y")

# Conversion to character and then numerical for analysis
LC_Cleaned$earliest_cr_line_char <- format(LC_Cleaned$earliest_cr_line_date, "%Y%m")
LC_Cleaned$earliest_cr_line_numeric <- as.numeric(LC_Cleaned$earliest_cr_line_char)

# Remove the last two digits by dividing by 100 and taking the integer part to get only the year
LC_Cleaned$earliest_cr_line_year <- LC_Cleaned$earliest_cr_line_numeric %/% 100

# Drop rows with NA in the 'earliest_cr_line_numeric' column
LC_Cleaned <- LC_Cleaned[!is.na(LC_Cleaned$earliest_cr_line_numeric), ]


# 26. 'inq_last_6mths': The number of inquiries in past 6 months (excluding auto and mortgage inquiries)data.frame(..., row.names = NULL, check.rows = FALSE, check.names = TRUE, stringsAsFactors = default.stringsAsFactors())
# No adjustments for the analysis needed

# 27. mths_since_last_delinq ... The number of months since the borrower's last delinquency.
# A value of 12 would indicate that the borrower’s last delinquency was 12 months ago (one year).
# A value of 0 would indicate that a delinquency occurred within the current month.
# We will impute NAs with a very high number as low numbers indicate more risk for the lender.
# Replace NA values with 999
LC_Cleaned$mths_since_last_delinq[is.na(LC_Cleaned$mths_since_last_delinq)] <- 999

# 28. mths_since_last_record ... The number of months since the last public record.
# A specific number (e.g., 12) means the last public record was 12 months ago.
# A 0 indicates a public record within the current month
# As NA normally indicates no public record history we impute with a very high number
LC_Cleaned$mths_since_last_record[is.na(LC_Cleaned$mths_since_last_record)] <- 999
sum(is.na(LC_Cleaned$mths_since_last_record)) # 0


# 29. open_acc ... The number of open credit lines in the borrower's credit file.
# A value of 5 means the borrower has five open lines of credit. --> higher risk
# A value of 0 indicates that the borrower currently has no active lines of credit. --> lower risk
# Is it possible that there are people with 90 lines of credit open?
# No adjustments for the analysis needed

# 30. pub_rec ... Number of derogatory public records. Derogatory public records are negative financial events that are publicly accessible and can significantly impact a borrower’s creditworthiness.
# A value of 0 means the borrower has no derogatory public records in their credit file.
# A value of 1 or higher indicates the number of derogatory public records recorded against the borrower.
# Higher values suggest a history of multiple financial issues
# No adjustments for the analysis needed

# 31. revol_bal ... Total credit revolving balance.
# A high revol_bal could indicate a borrower is utilizing a lot of available revolving credit, potentially signaling a higher risk if they are close to maxing out their credit limits.
# A low or zero revol_bal shows minimal use of revolving credit, which can be favorable, though some utilization is typically seen positively as it shows credit activity.
## This needs NA handling. Drop rows? Impute with 0 or mean or knn. By now the NAs will be filled with 0

LC_Cleaned$revol_bal[is.na(LC_Cleaned$revol_bal)] <- 0

# 32. revol_util ... Revolving line utilization rate, or the amount of credit the borrower is using relative to all available revolving credit.
# A low revol_util (e.g., 0-30%) indicates that the borrower is using a small portion of their available credit, which is generally seen positively in credit scoring.
# A high revol_util (e.g., > 50%) suggests that the borrower is utilizing a large amount of their credit limit, which can indicate higher risk.
# 100% or higher means the borrower has maxed out or exceeded their credit limit, which can be a significant risk factor.
## This needs NA handling. Drop rows? Impute with 0 or mean or knn. By now the NAs will be filled with 0

LC_Cleaned$revol_util[LC_Cleaned$revol_util == ""] <- NA # Handle empty strings
LC_Cleaned$revol_util <- as.integer(LC_Cleaned$revol_util) # Transform to integer
LC_Cleaned$revol_util[is.na(LC_Cleaned$revol_util)] <- 0 # Replace NA values with 0


# 33. total_acc ... The total number of credit lines currently in the borrower's credit file.
# A specific number in total_acc indicates the total number of accounts that have ever been opened in the borrower’s name.
# For example, a value of 10 means the borrower has opened ten credit accounts over time, regardless of whether they are currently open or closed.
# This could indicate a higher experience with credit, which could be positive.



# 34. initial_list_status ... The initial listing status of the loan. Possible values are – W, F
# Investigate $initial_list_status, (character)
LC_Cleaned$initial_list_status <- tolower(gsub("\\s+", "", LC_Cleaned$initial_list_status)) # Convert to lowercase and remove any whitespace for possible mistakes in the test-data
LC_Cleaned$initial_list_status <- factor(LC_Cleaned$initial_list_status) # Change to factor


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
# Create a new data frame with only the "JOINT" application type rows
LC_Cleaned_application_type_joint <- LC_Cleaned %>% 
  filter(application_type == "JOINT")

# Remove "JOINT" application type rows from the original LC_Cleaned
LC_Cleaned <- LC_Cleaned %>% 
  filter(application_type == "INDIVIDUAL") %>%
  select(-c('annual_inc_joint', 'dti_joint', 'verification_status_joint', 'application_type'))

# 52. annual_inc_joint
# This feature is dropped, as we implement business logic (at the end of this cleanup section)

# 53. dti_joint
# This feature is dropped, as we implement business logic (at the end of this cleanup section)

# 54. verification_status_joint
# This feature is dropped, as we implement business logic (at the end of this cleanup section)

# 55. acc_now_delinq: The number of accounts on which the borrower is now delinquent.
#LC_Cleaned <- clean_column(LC_Cleaned,"acc_now_delinq", remove_outliers=FALSE)


# 56. tot_coll_amt: Total collection amounts ever owed.
#LC_Cleaned <- subset(LC_Cleaned, select = -tot_coll_amt)
LC_Cleaned$tot_coll_amt[is.na(LC_Cleaned$tot_coll_amt)] <- 0

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
#LC_Cleaned <- subset(LC_Cleaned, select = -open_il_24m)
LC_Cleaned$open_il_24m[is.na(LC_Cleaned$open_il_24m)] <- 0


# 63. mths_since_rcnt_il: Months since most recent installment accounts opened
LC_Cleaned <- subset(LC_Cleaned, select = -mths_since_rcnt_il)


# 64. total_bal_il: Total current balance of all installment accounts
LC_Cleaned <- subset(LC_Cleaned, select = -total_bal_il)


# 65. il_util: Ratio of total current balance to high credit/credit limit on all install acct
#LC_Cleaned <- subset(LC_Cleaned, select = -il_util)
LC_Cleaned <- subset(LC_Cleaned, select = -il_util)


# 66. open_rv_12m: Number of revolving trades opened in past 12 months.
LC_Cleaned <- subset(LC_Cleaned, select = -open_rv_12m)


# 67. open_rv_24m: Number of revolving trades opened in past 24 months.
#LC_Cleaned <- subset(LC_Cleaned, select = -open_rv_24m)
LC_Cleaned$open_rv_24m[is.na(LC_Cleaned$open_rv_24m)] <- 0


# 68. max_bal_bc: Maximum current balance owed on all revolving accounts.
#LC_Cleaned <- subset(LC_Cleaned, select = -max_bal_bc)
LC_Cleaned <- subset(LC_Cleaned, select = -max_bal_bc)


# 69. all_util: Balance to credit limit on all trades.
LC_Cleaned <- subset(LC_Cleaned, select = -all_util)
#LC_Cleaned$all_util <- as.numeric(LC_Cleaned$all_util)
#LC_Cleaned$all_util[is.na(LC_Cleaned$all_util)] <- 0


# 70. inq_fi: Number of personal finance inquiries.
LC_Cleaned <- subset(LC_Cleaned, select = -inq_fi)


# 71. total_cu_tl: Number of finance trades.
#LC_Cleaned <- subset(LC_Cleaned, select = -total_cu_tl)
LC_Cleaned$total_cu_tl[is.na(LC_Cleaned$total_cu_tl)] <- 0

# 72. inq_last_12m: Number of credit inquiries in past 12 months.
#LC_Cleaned <- subset(LC_Cleaned, select = -inq_last_12m)
LC_Cleaned$inq_last_12m[is.na(LC_Cleaned$inq_last_12m)] <- 0



##############   Step 2 - Preprocessed Data Export                                    ##########################

# Export the data frame to a CSV file
write.csv(data, file = "./submission/data/LCdata.csv", row.names = FALSE)