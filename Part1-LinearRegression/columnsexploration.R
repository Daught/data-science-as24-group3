# Load necessary libraries
library(ggplot2)

# Set locale to English for date conversion 
Sys.setlocale("LC_TIME", "C") # or use "en_US.UTF-8" for Unix systems


data <- read.csv("ressources/LCdata.csv", sep = ";")

df <- data

columns_to_keep <- c(7, 19:36)
mh_df <- data[, columns_to_keep]

# Investigate the new dataframe
str(mh_df)

## Columns kept incl. former indices and description
# 7  $ int_rate ... Interest Rate on the loan
# 19 $ purpose ... A category provided by the borrower for the loan request.
# 20 $ title ... The loan title provided by the borrower.
# 21 $ zip_code ... The first 3 numbers of the zip code provided by the borrower in the loan application.
# 22 $ addr_state ... The state provided by the borrower in the loan application.
# 23 $ dti ... A ratio calculated using the borrower’s total monthly debt payments on the total debt obligations, excluding mortgage and the requested LC loan, divided by the borrower’s self-reported monthly income.
# 24 $ delinq_2yrs ... The number of 30+ days past-due incidences of delinquency in the borrower's credit file for the past 2 years.
# 25 $ earliest_cr_line ... The month the borrower's earliest reported credit line was opened.
# 26 $ inq_last_6mths ... The number of inquiries in past 6 months (excluding auto and mortgage inquiries).
# 27 $ mths_since_last_delinq ... The number of months since the borrower's last delinquency.
# 28 $ mths_since_last_record ... The number of months since the last public record.
# 29 $ open_acc ... The number of open credit lines in the borrower's credit file.
# 30 $ pub_rec ... Number of derogatory public records.
# 31 $ revol_bal ... Total credit revolving balance.
# 32 $ revol_util ... Revolving line utilization rate, or the amount of credit the borrower is using relative to all available revolving credit.
# 33 $ total_acc ... The total number of credit lines currently in the borrower's credit file.
# 34 $ initial_list_status ... The initial listing status of the loan. Possible values are – W, F
# 35 $ out_prncp ... Remaining outstanding principal for total amount funded.
# 36 $ out_prncp_inv ... Remaining outstanding principal for portion of total amount funded by investors.

# Investigate $purpose, (character)
unique(mh_df$purpose) # "other", "debt_consolidation", "moving", "credit_card", "home_improvement", "major_purchase", "wedding", "small_business", "medical", "car", "educational", "vacation", "house", "renewable_energy"

# Convert all characters in the column to lowercase
mh_df$purpose <- tolower(mh_df$purpose)
length(unique(mh_df$purpose)) # 14 unique values
sum(is.na(mh_df$purpose)) # no missing values
# Change to unordered factor
mh_df$purpose <- factor(mh_df$purpose)
summary(mh_df$purpose)
# car        credit_card     debt_consolidation        educational   home_improvement           house     major_purchase 
# 7924             185491             471937                380              46543               3320              15579 
# medical           moving              other   renewable_energy     small_business           vacation            wedding 
# 7660               4870              38720                505               9372               4242               2098

# Barplot for purpose
counts <- table(mh_df$purpose)
barplot(counts,
        main = "Distribution of Character Column",
        xlab = "Categories",
        ylab = "Count",
        col = "lightblue",
        border = "black",
        las = 2,   # Rotate x-axis labels for readability
        cex.names = 0.7)  # Adjust label size if needed

# Plot target to purpose
boxplot(int_rate ~ purpose, 
        data = mh_df,
        main = "Box Plot of Interest Rate by Purpose",
        xlab = "Investing Column",
        ylab = "Interest Rate",
        col = "lightblue",
        border = "black",
        las = 2,  # Rotate x-axis labels for readability
        cex.axis = 0.7) # Adjust label size if needed

################################## keep it

# Investigate $title, (character)
unique(mh_df$title)
str(mh_df$title) # character
# To lower case
mh_df$title <- tolower(mh_df$title)
# Remove all whitespace
mh_df$title <- gsub(" ", "", mh_df$title)
# Now again
length(unique(data$title)) # inital 57719 unique values
length(unique(mh_df$title)) # from 57719 to 48722 unique values, but still!!!
sum(is.na(mh_df$title)) # no missing values
summary(mh_df$title)
table(mh_df$title)

# Barplot for title
counts <- table(mh_df$title)
barplot(counts,
        main = "Distribution of Character Column",
        xlab = "Categories",
        ylab = "Count",
        col = "lightblue",
        border = "black",
        las = 2,   # Rotate x-axis labels for readability
        cex.names = 0.7)  # Adjust label size if needed

# Count and sort the values
sorted_value_counts <- sort(counts, decreasing = TRUE)
print(sorted_value_counts)

# debtconsolidation               creditcardrefinancing                   homeimprovement                       other 
# 393096                              148278                               38538                               28833 
# majorpurchase                       consolidation                       business                     medicalexpenses 
# 11171                                7391                                6171                                6079 


# Identify the two most frequent values
top_three_values <- names(sorted_value_counts)[1:3]
# Create a new column with grouped categories
mh_df$grouped_title <- ifelse(mh_df$title %in% top_three_values, 
                              mh_df$title, "Other")
# Check the resulting categories
table(mh_df$grouped_title)

# Change to factor
mh_df$grouped_title <- factor(mh_df$grouped_title)

# Check again
summary(mh_df$grouped_title)

# creditcardrefinancing     debtconsolidation       homeimprovement       Other 
# 148278                    393096                  38538                 218729


# Plot target to title
boxplot(int_rate ~ grouped_title, 
        data = mh_df,
        main = "Box Plot of Interest Rate by Purpose",
        xlab = "Investing Column",
        ylab = "Interest Rate",
        col = "lightblue",
        border = "black",
        las = 2,  # Rotate x-axis labels for readability
        cex.axis = 0.7) # Adjust label size if needed

################################## I would merge them as stated above and keep $grouped_title, else or drop it.


# Investigate $zip_code, (character)
unique(mh_df$zip_code) # e.g.: "806xx" "100xx" "665xx" "068xx" "300xx"
# Remove 'xx'
mh_df$zip_code <- gsub("xx", "", mh_df$zip_code)
# Now again
length(unique(data$zip_code)) # inital 931 unique values
length(unique(mh_df$zip_code)) # no change to original
sum(is.na(mh_df$zip_code)) # no missing values
summary(mh_df$zip_code)
table(mh_df$zip_code)

# Count and sort the values
counts <- table(mh_df$zip_code)
sorted_value_counts <- sort(counts, decreasing = TRUE)
print(sorted_value_counts)
sort(mh_df$zip_code)

# Change to factor
mh_df$zip_code <- factor(mh_df$zip_code)
summary(mh_df$zip_code)
str(mh_df$zip_code)
summary(mh_df$zip_code)

# Barplot for zip_code
counts <- table(mh_df$zip_code)
barplot(counts,
        main = "Distribution of Character Column",
        xlab = "Categories",
        ylab = "Count",
        col = "lightblue",
        border = "black",
        las = 2,   # Rotate x-axis labels for readability
        cex.names = 0.7)  # Adjust label size if needed

# Plot target to zip_code
boxplot(int_rate ~ zip_code, 
        data = mh_df,
        main = "Box Plot of Interest Rate by Purpose",
        xlab = "Investigated Column",
        ylab = "Interest Rate",
        col = "lightblue",
        border = "black",
        las = 2,  # Rotate x-axis labels for readability
        cex.axis = 0.7) # Adjust label size if needed


################################## At the moment keep it for further investigation.

# Investigate $addr_state, (character)
unique(mh_df$addr_state) # e.g.: ""CO" "NY" "KS" "CT" "GA" "MO" "SD" "FL" "OK" "IN" "PA" "OR" "OH" "TX" "WV" "NM""
length(unique(mh_df$addr_state)) # 51 but only 50 states??? DC is included as an extra state
sum(is.na(mh_df$addr_state)) # no missing values
summary(mh_df$addr_state)
table(mh_df$addr_state)
# Count and sort the values
counts <- table(mh_df$addr_state)
sorted_value_counts <- sort(counts, decreasing = TRUE)
print(sorted_value_counts)
sort(mh_df$addr_state)

# Barplot for zip_code
counts <- table(mh_df$addr_state)
barplot(counts,
        main = "Distribution of Character Column",
        xlab = "Categories",
        ylab = "Count",
        col = "lightblue",
        border = "black",
        las = 2,   # Rotate x-axis labels for readability
        cex.names = 0.7)  # Adjust label size if needed
# Change to factor
mh_df$addr_state <- factor(mh_df$addr_state)

# Make sure it worked
summary(mh_df$addr_state)
str(mh_df$addr_state)

# Plot target to addr_state
boxplot(int_rate ~ addr_state, 
        data = mh_df,
        main = "Box Plot of Interest Rate by Purpose",
        xlab = "Investigated Column",
        ylab = "Interest Rate",
        col = "lightblue",
        border = "black",
        las = 2,  # Rotate x-axis labels for readability
        cex.axis = 0.7) # Adjust label size if needed

################################## At the moment keep it for further investigation.

# Investigate $dti, (number): A ratio calculated using the borrower’s total monthly debt payments on the total debt obligations, excluding mortgage and the requested LC loan, divided by the borrower’s self-reported monthly income
summary(mh_df$dti)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 0.00   11.91   17.66   18.16   23.95 9999.00
unique(mh_df$dti) # e.g.: "6.46  1.00 10.00  5.60 24.55"
length(unique(mh_df$dti)) # 4079
sum(is.na(mh_df$dti)) # no missing values

# Sort by counts
sorted_value_counts <- sort(mh_df$dti, decreasing = TRUE)
print(sorted_value_counts)

# Histogramm
hist(mh_df$dti, 
     main = "Histogram of dti", 
     xlab = "dti",
     ylab = "Frequency",
     col = "lightblue", 
     border = "black")

# Find the maximum value
max_value <- max(mh_df$dti, na.rm = TRUE)

# Check how many times the maximum value occurs
max_count <- sum(mh_df$dti == max_value)
print(max_count)  # This will show how many times the max value appears

# There are 2 values with 9999.00 --> look for there rows (duplicates??)

# Define the value you are looking for
value_to_find <- 9999.00
# Find the row indices where column_num equals the value
row_indices <- which(mh_df$dti == value_to_find)
# Display the rows with this value
data_with_value <- data[row_indices, ]
print(data_with_value) # no duplicates, but annual-inc 0, both are application_type "JOINT" and have annual_inc_joint 185000 resp. 40988

# Histogramm without very high values
# Filter out the rows where column_num is 9999
filtered_data <- mh_df$dti[mh_df$dti < 150]

# Create the histogram with the filtered data
hist(filtered_data,
     main = "Histogram of Numeric Column (Excluding 9999)",
     xlab = "Values",
     ylab = "Frequency",
     col = "lightblue",
     border = "black")

# Boxplot of dti
boxplot(mh_df$dti)
boxplot(filtered_data)

# Plot target to addr_state
boxplot(int_rate ~ dti, 
        data = mh_df,
        main = "Box Plot of Interest Rate by Purpose",
        xlab = "Investigated Column",
        ylab = "Interest Rate",
        col = "lightblue",
        border = "black",
        las = 2,  # Rotate x-axis labels for readability
        cex.axis = 0.7) # Adjust label size if needed

################################## At the moment keep it for further investigation. Maybe adapt according to application_type column

# Investigate $delinq_2yrs (integer)
summary(mh_df$delinq_2yrs)
unique(mh_df$delinq_2yrs)
sum(is.na(mh_df$delinq_2yrs)) # 25 NAs

# Plots and Histogramms
hist(mh_df$delinq_2yrs)
boxplot(mh_df$delinq_2yrs)

# Plot target to delinq_2yrs
boxplot(int_rate ~ delinq_2yrs, 
        data = mh_df,
        main = "Box Plot of Interest Rate by Purpose",
        xlab = "Investigated Column",
        ylab = "Interest Rate",
        col = "lightblue",
        border = "black",
        las = 2,  # Rotate x-axis labels for readability
        cex.axis = 0.7) # Adjust label size if needed

# Find the row indices where NAs are
column_to_search <- mh_df$delinq_2yrs
NA_indices <- which(is.na(column_to_search))
# Display the rows with NAs
data_with_NAvalue <- data[NA_indices, ]
print(data_with_NAvalue) # multiple NA-columns




################################## At the moment keep it for further investigation. As multiple NAs in other columns maybe drop the NA-rows

# Investigate $earliest_cr_line (character)

unique(mh_df$earliest_cr_line) # e.g.: ""         "Mar-1984" "Dec-1994" "Jul-1993" "Jan-1983"
sum(is.na(mh_df$earliest_cr_line)) # 0 NA

# Remove all blank spaces from the values in the column so that NAs are clearly seen
mh_df$earliest_cr_line <- gsub(" ", "", mh_df$earliest_cr_line)
# Replace any empty strings (values with no content) with NA
mh_df$earliest_cr_line[mh_df$earliest_cr_line == ""] <- NA

# Check again
unique(mh_df$earliest_cr_line)
sum(is.na(mh_df$earliest_cr_line)) # now clearly stated 25 NA

# Now further exploration
length(unique(mh_df$earliest_cr_line)) # 695
length(unique(data$earliest_cr_line))
# no change to original

summary(mh_df$earliest_cr_line)
table(mh_df$earliest_cr_line)

# Barplot for earliest_cr_line
counts <- table(mh_df$earliest_cr_line)
barplot(counts,
        main = "Distribution of Character Column",
        xlab = "Categories",
        ylab = "Count",
        col = "lightblue",
        border = "black",
        las = 2,   # Rotate x-axis labels for readability
        cex.names = 0.7)  # Adjust label size if needed
# Change to factor
mh_df$earliest_cr_line <- factor(mh_df$earliest_cr_line)

# Make sure it worked
summary(mh_df$earliest_cr_line)
str(mh_df$earliest_cr_line)

# Plot target to earliest_cr_line
boxplot(int_rate ~ earliest_cr_line, 
        data = mh_df,
        main = "Box Plot of Interest Rate by Purpose",
        xlab = "Investigated Column",
        ylab = "Interest Rate",
        col = "lightblue",
        border = "black",
        las = 2,  # Rotate x-axis labels for readability
        cex.axis = 0.7) # Adjust label size if needed

# Find the row indices where NAs are
column_to_search <- mh_df$earliest_cr_line
NA_indices <- which(is.na(column_to_search))
# Display the rows with NAs
data_with_NAvalue <- data[NA_indices, ]
print(data_with_NAvalue) # multiple NA-columns

# Not sure if that is working better, let's try. Format the column to Date and then to two separate numeric columns with month and year.

# Convert the character column to Date type, assuming the first day of the month
mh_df$earliest_cr_line_date <- as.Date(paste0("01-", as.character(mh_df$earliest_cr_line)), format = "%d-%b-%Y")

# Check the result
print(mh_df$earliest_cr_line_date)

# Convert the date to the number of days since the earliest date in the column
mh_df$earliest_cr_line_date <- as.numeric(mh_df$earliest_cr_line_date - min(mh_df$earliest_cr_line_date, na.rm = TRUE))

# Initialize the new column with NA values
mh_df$earliest_cr_line_date_year <- NA

# Only assign the year to rows where 'earliest_cr_line_date' is not NA
mh_df$earliest_cr_line_date_year[!is.na(mh_df$earliest_cr_line_date)] <- 
  as.numeric(format(mh_df$earliest_cr_line_date[!is.na(mh_df$earliest_cr_line_date)], "%Y"))

# Check the result
head(mh_df)

# Extract year and month
mh_df$earliest_cr_line_date_year <- as.numeric(format(mh_df$earliest_cr_line_date, "%Y"))
mh_df$earliest_cr_line_date_month <- as.numeric(format(mh_df$earliest_cr_line_date, "%m"))
sum(is.na(mh_df$earliest_cr_line_date))

################################## At the moment keep it for further investigation. As multiple NAs in other columns maybe drop the NA-rows


# Investigate $inq_last_6mths (int)
summary(mh_df$inq_last_6mths) #    Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
#                                 0.0000  0.0000  0.0000  0.6947  1.0000 33.0000      25 
unique(mh_df$inq_last_6mths) # e.g.: NA  0  1  4  2  3  5 
sum(is.na(mh_df$inq_last_6mths)) #  25 NA

# Plots and Histogramms
hist(mh_df$inq_last_6mths)
boxplot(mh_df$inq_last_6mths)

# Plot target to inq_last_6mths
boxplot(int_rate ~ inq_last_6mths, 
        data = mh_df,
        main = "Box Plot of Interest Rate by Purpose",
        xlab = "Investigated Column",
        ylab = "Interest Rate",
        col = "lightblue",
        border = "black",
        las = 2,  # Rotate x-axis labels for readability
        cex.axis = 0.7) # Adjust label size if needed

# Find the row indices where NAs are
column_to_search <- mh_df$inq_last_6mths
NA_indices <- which(is.na(column_to_search))
# Display the rows with NAs
data_with_NAvalue <- data[NA_indices, ]
print(data_with_NAvalue) # multiple NA-columns

################################## At the moment keep it for further investigation. As multiple NAs in other columns maybe drop the NA-rows. Maybe drop the outliers as well (scewed data).

# Investigate $mths_since_last_delinq (int)
summary(mh_df$mths_since_last_delinq)
unique(mh_df$mths_since_last_delinq) # e.g.: NA   0  66  62  28  37  78   8  23  44 
sum(is.na(mh_df$mths_since_last_delinq)) #  408818 NA

# Plots and Histogramms
hist(mh_df$mths_since_last_delinq)
boxplot(mh_df$mths_since_last_delinq)

# Plot target to mths_since_last_delinq
boxplot(int_rate ~ mths_since_last_delinq, 
        data = mh_df,
        main = "Box Plot of Interest Rate by Purpose",
        xlab = "Investigated Column",
        ylab = "Interest Rate",
        col = "lightblue",
        border = "black",
        las = 2,  # Rotate x-axis labels for readability
        cex.axis = 0.7) # Adjust label size if needed

################################## Probably not important, half of the rows NAs, plot agains target seems unspectacular.

# Investigate $mths_since_last_record (int)
summary(mh_df$mths_since_last_record)
unique(mh_df$mths_since_last_record) # e.g.: NA   0  95 111  57  76 102 115  54 106  86  19  61   
sum(is.na(mh_df$mths_since_last_record)) #  675190 NA

# Plots and Histogramms
hist(mh_df$mths_since_last_record)
boxplot(mh_df$mths_since_last_record)

# Plot target to mths_since_last_record
boxplot(int_rate ~ mths_since_last_record, 
        data = mh_df,
        main = "Box Plot of Interest Rate by Purpose",
        xlab = "Investigated Column",
        ylab = "Interest Rate",
        col = "lightblue",
        border = "black",
        las = 2,  # Rotate x-axis labels for readability
        cex.axis = 0.7) # Adjust label size if needed

# Find the row indices not NAs
column_to_search <- mh_df$mths_since_last_record
NA_indices <- which(!is.na(column_to_search))
# Display the rows with not NAs
data_with_NAvalue <- data[NA_indices, ]
print(data_with_NAvalue)

################################## Probably not important, 675190 of the rows NAs, plot agains target seems unspectacular, however, maybe rows with content important for other column.

# Investigate $open_acc (int)
summary(mh_df$open_acc)
unique(mh_df$open_acc) # e.g.: NA  7  9  8  5 12  4 17 10 13 11   
sum(is.na(mh_df$open_acc)) #  25 NA

# Plots and Histogramms
hist(mh_df$open_acc)
boxplot(mh_df$open_acc)

# Plot target to open_acc
boxplot(int_rate ~ open_acc, 
        data = mh_df,
        main = "Box Plot of Interest Rate by Purpose",
        xlab = "Investigated Column",
        ylab = "Interest Rate",
        col = "lightblue",
        border = "black",
        las = 2,  # Rotate x-axis labels for readability
        cex.axis = 0.7) # Adjust label size if needed

# Find the row indices NAs
column_to_search <- mh_df$open_acc
NA_indices <- which(is.na(column_to_search))
# Display the rows with NAs
data_with_NAvalue <- data[NA_indices, ]
print(data_with_NAvalue)

################################## Keep, also 25 NAs, outliers. More preparation needed if kept.

# Investigate $pub_rec (int)
summary(mh_df$pub_rec)
unique(mh_df$pub_rec) # e.g.: NA  0  1  2  3  5  9    
sum(is.na(mh_df$pub_rec)) #  25 NA

# Plots and Histogramms
hist(mh_df$pub_rec)
boxplot(mh_df$pub_rec)

# Plot target to pub_rec
boxplot(int_rate ~ pub_rec, 
        data = mh_df,
        main = "Box Plot of Interest Rate by Purpose",
        xlab = "Investigated Column",
        ylab = "Interest Rate",
        col = "lightblue",
        border = "black",
        las = 2,  # Rotate x-axis labels for readability
        cex.axis = 0.7) # Adjust label size if needed

# Find the row indices NAs
column_to_search <- mh_df$pub_rec
NA_indices <- which(is.na(column_to_search))
# Display the rows with NAs
data_with_NAvalue <- data[NA_indices, ]
print(data_with_NAvalue)
################################## Keep, also 25 NAs, outliers. More preparation needed if kept.

# Investigate $revol_bal (int)
summary(mh_df$revol_bal)
unique(mh_df$revol_bal) # e.g.: 0 150786  11608   9204   5046   6360  16842   4429 (>70445)    
sum(is.na(mh_df$revol_bal)) #  2 NA

# Plots and Histogramms
hist(mh_df$revol_bal)
boxplot(mh_df$revol_bal)

# Plot target to revol_bal
boxplot(int_rate ~ revol_bal, 
        data = mh_df,
        main = "Box Plot of Interest Rate by Purpose",
        xlab = "Investigated Column",
        ylab = "Interest Rate",
        col = "lightblue",
        border = "black",
        las = 2,  # Rotate x-axis labels for readability
        cex.axis = 0.7) # Adjust label size if needed

# Find the row indices NAs
column_to_search <- mh_df$revol_bal
NA_indices <- which(is.na(column_to_search))
# Display the rows with NAs
data_with_NAvalue <- data[NA_indices, ]
print(data_with_NAvalue)
################################## Keep, min 0, max 2904836! 2 NAs. Further examination.

# Investigate $revol_util (num)
summary(mh_df$revol_util)
unique(mh_df$revol_util) # e.g.: NA   2.20  56.90  29.30  19.90  25.00    
sum(is.na(mh_df$revol_util)) #  454 NA

# Plots and Histogramms
hist(mh_df$revol_util)
boxplot(mh_df$revol_util)

# Plot target to revol_util
boxplot(int_rate ~ revol_util, 
        data = mh_df,
        main = "Box Plot of Interest Rate by Purpose",
        xlab = "Investigated Column",
        ylab = "Interest Rate",
        col = "lightblue",
        border = "black",
        las = 2,  # Rotate x-axis labels for readability
        cex.axis = 0.7) # Adjust label size if needed

# Find the row indices NAs
column_to_search <- mh_df$revol_util
NA_indices <- which(is.na(column_to_search))
# Display the rows with NAs
data_with_NAvalue <- data[NA_indices, ]
print(data_with_NAvalue)
################################## Keep, seems to have some linearity. Further examination.

# Investigate $total_acc (int)
summary(mh_df$total_acc)
unique(mh_df$total_acc) # e.g.: NA  16  19  43  28  20  35  33  26     
sum(is.na(mh_df$total_acc)) #  25 NA

# Plots and Histogramms
hist(mh_df$total_acc)
boxplot(mh_df$total_acc)

# Plot target to total_acc
boxplot(int_rate ~ total_acc, 
        data = mh_df,
        main = "Box Plot of Interest Rate by Purpose",
        xlab = "Investigated Column",
        ylab = "Interest Rate",
        col = "lightblue",
        border = "black",
        las = 2,  # Rotate x-axis labels for readability
        cex.axis = 0.7) # Adjust label size if needed

# Find the row indices NAs
column_to_search <- mh_df$total_acc
NA_indices <- which(is.na(column_to_search))
# Display the rows with NAs
data_with_NAvalue <- data[NA_indices, ]
print(data_with_NAvalue)

################################## Keep, seems to have some linearity to target. Further examination.

# Investigate $initial_list_status, (character)
unique(mh_df$initial_list_status) # e.g.: ""f" "w""
sum(is.na(mh_df$initial_list_status)) # no missing values
summary(mh_df$initial_list_status)
table(mh_df$initial_list_status)

# Barplot for initial_list_status
counts <- table(mh_df$initial_list_status)
barplot(counts,
        main = "Distribution of Character Column",
        xlab = "Categories",
        ylab = "Count",
        col = "lightblue",
        border = "black",
        las = 2,   # Rotate x-axis labels for readability
        cex.names = 0.7)  # Adjust label size if needed
# Change to factor
mh_df$initial_list_status <- factor(mh_df$initial_list_status)

# Make sure it worked
summary(mh_df$initial_list_status)
str(mh_df$initial_list_status)

# Plot target to initial_list_status
boxplot(int_rate ~ initial_list_status, 
        data = mh_df,
        main = "Box Plot of Interest Rate by Purpose",
        xlab = "Investigated Column",
        ylab = "Interest Rate",
        col = "lightblue",
        border = "black",
        las = 2,  # Rotate x-axis labels for readability
        cex.axis = 0.7) # Adjust label size if needed

################################## Keep, balanced data. What is f and what is w?

# Investigate $out_prncp (num)
summary(mh_df$out_prncp)
unique(mh_df$out_prncp) # e.g.: 0.00  9000.00 27297.61  4873.34 26000.00 18000.00  7311.85  9746.69 19200.00  6000.00     
sum(is.na(mh_df$out_prncp)) #  no missing values

# Plots and Histogramms
hist(mh_df$out_prncp)
boxplot(mh_df$out_prncp)

# Plot target to out_prncp
boxplot(int_rate ~ out_prncp, 
        data = mh_df,
        main = "Box Plot of Interest Rate by Purpose",
        xlab = "Investigated Column",
        ylab = "Interest Rate",
        col = "lightblue",
        border = "black",
        las = 2,  # Rotate x-axis labels for readability
        cex.axis = 0.7) # Adjust label size if needed

################################## Keep, many 0 values. Not available for prediction

# Investigate $out_prncp_inv (num)
summary(mh_df$out_prncp_inv)
unique(mh_df$out_prncp_inv) # e.g.: 0.00  9000.00 27297.61  4873.34 26000.00 18000.00  7311.85  9746.69 19200.00  6000.00     
sum(is.na(mh_df$out_prncp_inv)) #  no missing values

# Plots and Histogramms
hist(mh_df$out_prncp_inv)
boxplot(mh_df$out_prncp_inv)

# Plot target to out_prncp_inv
boxplot(int_rate ~ out_prncp_inv, 
        data = mh_df,
        main = "Box Plot of Interest Rate by Purpose",
        xlab = "Investigated Column",
        ylab = "Interest Rate",
        col = "lightblue",
        border = "black",
        las = 2,  # Rotate x-axis labels for readability
        cex.axis = 0.7) # Adjust label size if needed

################################## Not available for prediction.