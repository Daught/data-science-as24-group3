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

# CODE_GENDER
# data seems fine. No missing values and all data is binary. Therefore onehot encoding.
str(data$CODE_GENDER)
summary(data$CODE_GENDER)

data <- clean_column(data, "CODE_GENDER", onehot_encode = TRUE)

# FLAG_OWN_CAR
# data seems fine. No missing values and all data is binary. Therefore onehot encoding.
str(data$FLAG_OWN_CAR)
summary(data$FLAG_OWN_CAR)

data <- clean_column(data, "FLAG_OWN_CAR", onehot_encode = TRUE)

# FLAG_OWN_REALTY
# data seems fine. No missing values and all data is binary. Therefore onehot encoding.
str(data$FLAG_OWN_REALTY)
summary(data$FLAG_OWN_REALTY)

data <- clean_column(data, "FLAG_OWN_REALTY", onehot_encode = TRUE)

# CNT_CHILDREN
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

# AMT_INCOME_TOTAL
str(data$AMT_INCOME_TOTAL)
summary(data$AMT_INCOME_TOTAL)
# we have ouliers here but in praxis it is possible to have an anual income of over 6750000. We do not drop them

data <- clean_column(data, "AMT_INCOME_TOTAL")

# NAME_INCOME_TYPE
str(data$NAME_INCOME_TYPE)
summary(data$NAME_INCOME_TYPE)

# data seems fine. No missing values and all data is categorical (nominal). Therefore onehot encoding.
data <- clean_column(data, "NAME_INCOME_TYPE", onehot_encode = TRUE)

# NAME_EDUCATION_TYPE
str(data$NAME_EDUCATION_TYPE)
summary(data$NAME_EDUCATION_TYPE)
table(data$NAME_EDUCATION_TYPE)
levels <- c("Lower secondary", "Secondary / secondary special", "Incomplete higher", "Higher education", "Academic degree")
# data seems fine. No missing values and all data is categorical (nominal). Therefore onehot encoding.
data <- clean_column(data, "NAME_EDUCATION_TYPE", is_ordered_factor = TRUE, order_levels = levels)


















# ------------------------------------------------------------------------------
# Conversion of labels to numeric values


# term: The number of payments on the loan.
# "36 months", "60 months" -> levels
unique(data$term)

data$term <- as.integer(ordered(str_trim(data$term), levels=c("36 months", "60 months")))

# emp_length: Employment length in years. Possible values are between 0 and 10 where 0 means less than one year and 10 means ten or more years.
# Convert to ordered levels because of duration context.
unique(data$emp_length)

data$emp_length <- as.integer(ordered(str_trim(data$emp_length), levels=c("< 1 year", 
                                                                          "1 year", 
                                                                          "2 years", 
                                                                          "3 years", 
                                                                          "4 years", 
                                                                          "5 years",
                                                                          "6 years",
                                                                          "7 years",
                                                                          "8 years",
                                                                          "9 years",
                                                                          "10+ years")))


# home_ownership: The home ownership status provided by the borrower during registration.
# Convert one-hot because type of home ownership does not seem to be ordered, especially with ANY and OTHER.
unique(data$home_ownership)

data <- dummy_cols(data, select_columns="home_ownership", remove_selected_columns = TRUE)

# verification_status: "Not Verified", "Verified", "Source Verified"
# Ordered because of trustworthiness of information increases.
unique(data$verification_status)

data$verification_status <- as.integer(ordered(str_trim(data$verification_status), levels=c("Not Verified", "Verified", "Source Verified")))


# verification_status_joint
# Ordered because of trustworthiness of information increases.
unique(data$verification_status_joint)
  
data$verification_status_joint <- as.integer(ordered(str_trim(data$verification_status_joint), levels=c("Not Verified", "Verified", "Source Verified")))


# pymnt_plan: Indicates if a payment plan has been put in place for the loan. Values: "y", "n"
# Apply order n -> y because it might be better to have a plan than not.
unique(data$pymnt_plan)

data$pymnt_plan <- as.integer(ordered(str_trim(data$pymnt_plan), levels=c("n", "y")))


# purpose: A category provided by the borrower for the loan request.
# Apply one-hot because no order makes sense for purpose.

unique(data$purpose)

data <- dummy_cols(data, select_columns="purpose", remove_selected_columns = TRUE)


# application_type: Indicates whether the loan is an individual application or a joint application with two co-borrowers. INDIVIUDAL, JOINT
# Apply ordered probably joint is lower risk than individual
unique(data$application_type)

data$application_type <- as.integer(ordered(str_trim(data$application_type), levels=c("INDIVIUDAL", "JOINT")))

# initial_list_status: The initial listing status of the loan. Possible values are – W, F -> No idea what W, F means.
summary(data$initial_list_status)
unique(data$initial_list_status)

data$initial_list_status <- as.integer(ordered(str_trim(data$initial_list_status), levels=c("f", "w")))


# ------------------------------------------------------------------------------
# Numeric

# loan_amnt: The listed amount of the loan applied for by the borrower. If at some point in time, the credit department reduces the loan amount, then it will be reflected in this value.
summary(data$loan_amnt)

# funded_amnt: The total amount committed to that loan at that point in time.
summary(data$funded_amnt)

# funded_amnt_inv: The total amount committed by investors for that loan at that point in time.
summary(data$funded_amnt_inv)

# installment: The monthly payment owed by the borrower if the loan originates.
summary(data$installment)

# annual_inc: The self-reported annual income provided by the borrower during registration.
summary(data$annual_inc)

# dti: A ratio calculated using the borrower’s total monthly debt payments on the total debt obligations, excluding mortgage and the requested LC loan, divided by the borrower’s self-reported monthly income.
summary(data$dti)

# delinq_2yrs: The number of 30+ days past-due incidences of delinquency in the borrower's credit file for the past 2 years.
summary(data$delinq_2yrs)

# inq_last_6mths: number - The number of inquiries in past 6 months (excluding auto and mortgage inquiries).
summary(data$inq_last_6mths)

# mths_since_last_delinq: The number of 30+ days past-due incidences of delinquency in the borrower's credit file for the past 2 years. Delinquency: https://www.investopedia.com/terms/d/delinquent.asp
summary(data$mths_since_last_delinq)

# mths_since_last_record:	The number of months since the last public record. Public record: https://www.thebalancemoney.com/public-records-and-your-credit-report-960740
summary(data$mths_since_last_record)

# open_acc: The number of open credit lines in the borrower's credit file.
summary(data$open_acc)

# pub_rec: Number of derogatory public records. Derogatory public record: https://www.experian.com/blogs/ask-experian/meaning-of-derogatory-public-record/?msockid=101f7315cdd36ee71e996797cca36faf
summary(data$pub_rec)

# revol_bal: Total credit revolving balance: https://www.investopedia.com/terms/r/revolvingcredit.asp
summary(data$revol_bal)

# revol_util: Revolving line utilization rate, or the amount of credit the borrower is using relative to all available revolving credit.
summary(data$revol_util)

# total_acc: The total number of credit lines currently in the borrower's credit file.
summary(data$total_acc)

# out_prncp: Remaining outstanding principal for total amount funded. https://www.investopedia.com/terms/p/principal.asp
summary(data$out_prncp)

# out_prncp_inv: Remaining outstanding principal for portion of total amount funded by investors.
summary(data$out_prncp_inv)

# total_pymnt: Payments received to date for total amount funded.
summary(data$total_pymnt)

# total_pymnt_inv:	Payments received to date for portion of total amount funded by investors.
summary(data$total_pymnt_inv)

# total_rec_prncp: Principal received to date.
summary(data$total_rec_prncp)

# total_rec_int: Interest received to date.
summary(data$total_rec_int)

# total_rec_late_fee: Late fees received to date.
summary(data$total_rec_late_fee)



# last_pymnt_amnt: Last total payment amount received.
summary(data$last_pymnt_amnt)

# collections_12_mths_ex_med: Number of collections in 12 months excluding medical collections.
summary(data$collections_12_mths_ex_med)

# mths_since_last_major_derog: Months since most recent 90-day or worse rating
summary(data$mths_since_last_major_derog)

# policy_code: publicly available policy_code=1, new products not publicly available policy_code=2
summary(data$policy_code)

# annual_inc_joint: The combined self-reported annual income provided by the co-borrowers during registration.
summary(data$annual_inc_joint)

# dti_joint: A ratio calculated using the co-borrowers' total monthly payments on the total debt obligations, excluding mortgages and the requested LC loan, divided by the co-borrowers' combined self-reported monthly income.
summary(data$dti_joint)

# verification_status_joint: Indicates if the co-borrowers' joint income was verified by LC, not verified, or if the income source was verified.
summary(data$verification_status_joint)

# acc_now_delinq: The number of accounts on which the borrower is now delinquent.
summary(data$acc_now_delinq)

# tot_coll_amt: Total collection amounts ever owed.
summary(data$tot_coll_amt)

# tot_cur_bal: Total current balance of all accounts.
summary(data$tot_cur_bal)

# open_acc_6m: Number of open trades in last 6 months.
summary(data$open_acc_6m)

# open_il_6m: Number of currently active installment trades.
summary(data$open_il_6m)

# open_il_12m: Number of installment accounts opened in past 12 months
summary(data$open_il_12m)

# open_il_24m: Number of installment accounts opened in past 24 months
summary(data$open_il_24m)

# mths_since_rcnt_il: Months since most recent installment accounts opened
summary(data$mths_since_rcnt_il)

# total_bal_il: Total current balance of all installment accounts
summary(data$total_bal_il)

# il_util: Ratio of total current balance to high credit/credit limit on all install acct
summary(data$il_util)

# open_rv_12m: Number of revolving trades opened in past 12 months.
summary(data$open_rv_12m)

# open_rv_24m: Number of revolving trades opened in past 24 months.
summary(data$open_rv_24m)

# max_bal_bc: Maximum current balance owed on all revolving accounts.
summary(data$max_bal_bc)

# all_util: Balance to credit limit on all trades.
summary(data$all_util)

# total_rev_hi_lim: Total revolving high credit/credit limit.
summary(data$total_rev_hi_lim)

# inq_fi: Number of personal finance inquiries.
summary(data$inq_fi)

# total_cu_tl: Number of finance trades.
summary(data$total_cu_tl)

# inq_last_12m: Number of credit inquiries in past 12 months.
summary(data$inq_last_12m)



# Print out to csv, easier to look at in Excel

corr = cor(data)

write.csv(corr, 'raw_data_corr.csv')



# Test cleaned data on linear regression

lm.fit1 <- lm(int_rate~., data=data)

summary(lm.fit1)
