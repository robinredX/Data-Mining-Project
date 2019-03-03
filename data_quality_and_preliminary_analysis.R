# Data Mining Project - M1 - Robin Khatri
# Detection of Attacks on a Water Treatment Unit
# General dataset information is in README.md. 
# To request dataset: https://itrust.sutd.edu.sg/testbeds/secure-water-treatment-swat/ 

# Set directory
getwd()
setwd("usr/local/Dropbox/Data_Mining_Project")
dir() 

# Load required libraries
# install.packages(c("openxlsx", "dplyr","DMwR")) # Unquote if you need to install the package
library(openxlsx)
library(dplyr)
library(DMwR)

# ---------------------------------
# 1. Data import and Pre-processing
# ---------------------------------

data <- read.xlsx("SWaT_Dataset_Attack_v0.xlsx") # dataset contains version 0 of normal instances. 
# Read README.md or the project report for details on it.
rescue <- data # A clone of the dataset (Since dataset is large and takes time to be loaded, it's good to have a copy ready if needed)
glimpse(data)

# -----------------
# 1(a) Data quality
# -----------------

# Response variable (Dependent variable): Replace labels "At tack", "Attack" and "Normal" with 1,1 and 0 respectively
data$`Normal/Attack` <- as.factor(data$`Normal/Attack`)
levels(data$`Normal/Attack`)
data <- within(data, `Normal/Attack` <- factor(`Normal/Attack`, labels = c(1, 1, 0))) 
levels(data$`Normal/Attack`)

# Timestamps: Convert timestamps into date into POSIXlt format and Sort dataset by timestamps in increasing order

data$Timestamp <- strptime(data$Timestamp, format="%d/%m/%Y %I:%M:%S %p") 
data <- data[order(data$Timestamp),] 

# Labelling attacks into further categories. For information about timelines of different attacks, read README.md
# Get excel file for list of attacks
list_attacks <- read.xlsx("List_of_attacks_Final.xlsx")
glimpse(list_attacks)
list_attacks$Start.time <- as.Date(list_attacks$Start.time)

# -------------
# 1(b) Sampling
# -------------

# Get frequency of normal instances and of each kind of attack


# Perform undersampling and oversampling to get a less unbalanced dataset: We'll use SMOTE
newData <- SMOTE(`Normal/Attack` ~ ., data = data[,2:46], perc.over = 150,perc.under=50)
# Read about SMOTE here: https://arxiv.org/pdf/1106.1813.pdf

# Sample 50000 observations while maintaining prportions of and normal instances and individual attacks

# ----------------------------
# 3. Feature Selection and PCA 
# ----------------------------

# # Remove features with 0 variances as they do not provide any distinction between two categories of response, if any
n = ncol(data)
var_zero <- rep(0, n)

for (i in 2:(n-1)){
  if (var(as.numeric(data[,i])) == 0){
    var_zero[i] <- 1
  }
}

var_zero
indices <- which(var_zero==1) 
print(colnames(data)[indices]) # print names of features with zero variance, if any
data <- data[,-indices] 


# Normalization and feature selection


# Principal component Analysis

# -------------------------------------------
# 4. Stage 1: Classify into normal and attack 
# -------------------------------------------

# Naive Bayes

# Logistic regression

# Support Vector Machine

# Random Forest

# Neural Net

# -------------------------------------------
# 5. Stage 2: multi-category classification 
# -------------------------------------------

# Naive Bayes

# Logistic regression

# Support Vector Machine

# Random Forest

# Neural Net

# -------------------------------------------
# 6. Stage 3: Detecing unknown attacks 
# -------------------------------------------

# Data aggregation
