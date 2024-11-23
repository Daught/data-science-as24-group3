##########################################################
# Data Science - AS24
# DS-Assignment: Part1
# Part 1: Logistic Regression
# Magdalena Hardegger magdalena.hardegger@students.fhnw.ch
# Firat Turan firat.turan@students.fhnw.ch
# Sascha Frossard sascha.frossard@students.fhnw.ch
# Sebastian Fernandez sebastian.fernandez@students.fhnw.ch
##########################################################
# Assignment for DataScience Course at Olten, Switzerland
##########################################################


##############   1. Preliminaries                                               ##########################

# Install



# Load packages



##############   Step 2: Environment                                                   ##########################
model_file_path = '.RData'
test_data_file_path = ''
static_seed_value = 1

# Check if the variable 'dataset_file_path' already exists
dataset_file_path <- test_data_file_path
if (!exists("dataset_file_path") || is.null(dataset_file_path) || dataset_file_path == "") {
  dataset_file_path <- "./submission/data/LCdata_preprocessed.csv"
  cat("Using default dataset file path:", dataset_file_path, "\n")
} else {
  cat("Using existing dataset file path:", dataset_file_path, "\n")
}

##############   Step 3: Data Preprocessing                                             ##########################

# Load functions from another R script
cat("Sourcing helper functions...\n")
source("./submission/Group3-Assignment1-DataPreprocessing.R", echo = TRUE, print.eval = TRUE)

cat("Running analysis...\n")
analysis_type <- "return"   # Example parameter for analysis type 
                            #       export = writes csv to ./submission/data/LCdata_preprocessed.csv
                            #       return = returns the preprocessed dataframe
filter_value <- 100         # Example parameter for filtering


cat("Running preprocessing...\n")
result <- perform_data_preprocessing(dataset_file_path, analysis_type, filter_value)

##############   Step 4: Setup                                                          ##########################

# Load the model
load(model_file_path)

# load data set
data_final <- read.csv2(test_data_file_path, header = TRUE, row.names=NULL, sep=";")
LC_Final <- data_final # make a copy of the original data set, so that we dont mess with it

perform_data_preprocessing()

