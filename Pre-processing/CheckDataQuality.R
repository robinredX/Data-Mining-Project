# Data Mining Project - M1 - Robin Khatri
# Detection of Attacks on a Water Treatment Unit
# General dataset information: README.md, to request dataset: https://itrust.sutd.edu.sg/testbeds/secure-water-treatment-swat/
# Dataset is also available in the repository.

############################## 
Part I: Check quality of data
##############################

# Load required libraries
# install.packages(c("openxlsx", "ggplot2", "lubridate", "dplyr","DMwR", "randomForest", "keras")) # Unquote if you need to install the packages

require(openxlsx)
require(dplyr)
require(lubridate)
require(ggplot2)

# Set directory

setwd("C:/users/robin/Desktop/DataMiningProject")
dir() # Load files in directory

# import dataset

dataset <- read.xlsx("SWaT_Dataset_Attack_v0.xlsx")
rescue <- dataset # A clone of the dataset (Since dataset is large and takes some time to be loaded, it's good to have a copy ready if needed)
glimpse(dataset) # Dependent variable is character, others are numeric and timestamp is character as well.

n_features = ncol(dataset)-1 # Last column is dependent variable (class)
n_obs = nrow(dataset) # Number of observations
n_features
n_obs

# Response/Dependent Variable

# type of data in response variable

dataset$`Normal/Attack` <- as.factor(dataset$`Normal/Attack`) # Convert to factor
levels(dataset$`Normal/Attack`)
dataset <- within(dataset, `Normal/Attack` <- factor(`Normal/Attack`, labels = c(1, 1, 0))) 
levels(dataset$`Normal/Attack`)
# 1: Attack, 0: Normal
# Visualize target variable

# Check for missing data

which(is.na(dataset)) # No missing datapoint


# Timestamps: Convert timestamps into date into POSIXlt format and Sort dataset by timestamps in increasing order

head(dataset$Timestamp)
dataset$Timestamp <- strptime(dataset$Timestamp, format="%d/%m/%Y %I:%M:%S %p")  # format timestamps into 24 hrs from AM/PM
dataset <- dataset[order(data$Timestamp),] 
head(dataset$Timestamp)
tail(dataset$Timestamp)

# Note: Since attacks were carried out at scheduled times, there will be no enquiry into relationship of time in detection of attacks

# TODO Missing timestamps

# Number of attacks by date
# Extract dates from timestamps
dataset$date <- date(dataset$Timestamp)

# Get count of attack instances by date
dataset$new <- as.character(dataset$`Normal/Attack`)
dataset$new <- as.numeric(dataset$new)
date.attacks <- dataset %>%
  group_by(date) %>%
  summarise(n_attacks = sum(new)) %>%
  arrange(desc(n_attacks))
date.attacks
# Plot count of attack instances by date
ggplot(date.attacks, aes(x = date, y = n_attacks)) + geom_col() + labs(x = 'Date', y = 'Attacks', title = 'Attacks by date')
table(dataset$`Normal/Attack`)

#########################
Part I: Feature Selection
#########################

# TODO Get variances of all features

# Remove features with 0 variances as they do not provide any distinction between two categories of response, if any
var_zero <- rep(0, n_features)

for (i in 2:n_features){
  if (var(as.numeric(dataset[,i])) == 0){
    var_zero[i] <- 1
  }
}

var_zero
indices <- which(var_zero==1) 
print(colnames(dataset)[indices]) # print names of features with zero variance, if any
dataset_new <- dataset[,-indices] 
colnames(dataset_new)
# Normalize features
dataset_normalized <- data.frame(scale(dataset_new[,2:45]))
dataset_normalized$label <- dataset_new$`Normal/Attack`
glimpse(dataset_normalized)

# Importance of features

# Principal component analysis

# Sampling

# SMOTE

# Training

# Testing

# Accuracy

# Robustness for different categories of attacks





