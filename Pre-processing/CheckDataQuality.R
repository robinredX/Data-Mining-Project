# Data Mining Project - M1 - Robin Khatri
# Detection of Attacks on a Water Treatment Unit
# General dataset information: README.md, to request dataset: https://itrust.sutd.edu.sg/testbeds/secure-water-treatment-swat/
# Dataset is also available in the repository.

############################## 
Part I: Check quality of data
##############################

# Load required libraries
# install.packages(c("openxlsx", "dplyr","DMwR", "randomForest", "keras")) # Unquote if you need to install the packages

require(openxlsx)
require(dplyr)

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

# Plot normal and attack instances over the duration
plot(dataset$Timestamp, dataset$`Normal/Attack`)

# Visualize target variable


