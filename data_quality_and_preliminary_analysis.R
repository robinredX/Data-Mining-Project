# Data Mining Project - M1 - Robin Khatri
# Detection of Attacks on a Water Treatment Unit
# General dataset information: README.md, to request dataset: https://itrust.sutd.edu.sg/testbeds/secure-water-treatment-swat/

# Set directory
getwd()
setwd("~/home/Downloads")
dir() 

# Load required libraries
# install.packages(c("openxlsx", "dplyr","DMwR", "randomForest", "keras")) # Unquote if you need to install the package

library(openxlsx)
library(dplyr)
library(DMwR)
library(randomForest)
library(nnet)

# ---------------------------------
# 1. Data import and Pre-processing
# ---------------------------------

data <- read.xlsx("SWaT_Dataset_Attack_v0.xlsx") # dataset contains version 0 of normal instances. 
# Read README.md or the project report for details on it.
rescue <- data # A clone of the dataset (Since dataset is large and takes some time to be loaded, it's good to have a copy ready if needed)
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

# Perform undersampling and oversampling to get a less unbalanced dataset: We'll use SMOTE
#newData <- SMOTE(`Normal/Attack` ~ ., data = data[,2:46], perc.over = 150,perc.under=50)
# Read about SMOTE here: https://arxiv.org/pdf/1106.1813.pdf

# SMOTE Took a very long time due to size of the dataset. Taking a small random sample without replacemnet to produce 20000 instances,
# while Maintaining prportions of and normal instances and individual attacks
# Get frequency of normal instances and of each kind of attack

m = nrow(data)
p = 50000 # Sample size
l = p/m
normal_data <- data[data$`Normal/Attack`==0,]
head(data)
normal <- normal_data[sample(nrow(normal_data),round(l*nrow(normal_data))),]
nrow(normal)
attack_data <- data[data$`Normal/Attack`==1,]
attack <- attack_data[sample(nrow(attack_data), round(l*nrow(attack_data))),]
sample_data <- rbind(normal,attack)
sample_data <- sample_data[order(sample_data$Timestamp),]
nrow(sample_data)

# Perform undersampling and oversampling to get a less unbalanced dataset: We'll use SMOTE
# Parameters for SMOTE:
# Ratio of normal to attack is almost 90 percent and if we want to reduce the ratio to say 70%, we should oversample minority class
# by 200 and undersample by 100 approximately. Read project report in the repository for calculations
newdata <- SMOTE(`Normal/Attack` ~ ., data = sample_data[,2:46], perc.over = 200,perc.under=100)
# Read about SMOTE here: https://arxiv.org/pdf/1106.1813.pdf
nrow(newdata)
nrow(newdata[newdata$`Normal/Attack`==1,])
head(newdata)

# ----------------------------
# 3. Feature Selection and PCA 
# ----------------------------

# NOTE: Time won't be taken into account because the attacks were done at pre-determined times and so response (dependent) variable cannot be
# thought of changing with respect to time according to observations from data.

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

normal_newdata <- data.frame(scale(newdata[,2:(ncol(newdata)-1)]))
normal_newdata$label <- newdata[[ncol(newdata)]]


# Principal component Analysis

normal_data_pca <- prcomp(newdata[,1:(ncol(normal_newdata)-1)], center=TRUE)
summary(normal_data_pca)

list_of_pca <- normal_data_pca$x
list_of_pca <- data.frame(list_of_pca[,1:5])

newdata_pca <- data.frame(list_of_pca, normal_newdata[[ncol(normal_newdata)]])
colnames(newdata_pca) <- c("V1","V2","V3","V4","V5", "label")

# -------------------------------------------
# 4. Stage 1: Classify into normal and attack 
# -------------------------------------------
newdata_pca <- newdata_pca[sample(nrow(newdata_pca), nrow(newdata_pca)),]
n_train <- 0.80*nrow(newdata_pca)
n_test <- 0.20*nrow(newdata_pca)
train <- newdata_pca[1:round(n_train),]
test <- newdata_pca[(nrow(train)+1):nrow(newdata_pca),]

# Logistic regression

model_logit <- glm(label~., data = train, family = binomial(link = "logit"))
predict_logit <- predict(model_logit, test, type="response")
predict_logit <- ifelse(predict_logit > 0.5,0,1)
table(predict_logit, test$label)

# Random Forest

rf_output=randomForest(x=train[1:(ncol(train)-1)], y=train$label, importance = TRUE, ntree = 100, proximity=TRUE, keep.forest=TRUE)
rf_output
predict_forest <- predict(rf_output, test, type="response")
table(predict_forest, test$label)
# Random forest performed better over neural network
# Neural Net

model_nn <- nnet(label~.,data=train, size=40, maxit=500, softmax=FALSE)
predicted <- "predict"(model_nn, test, type = "class")
table(predicted, test$label)

# -------------------------------------------
# 4. Stage 2: multi-category classification 
# -------------------------------------------

# We'll test model's power to classify individual attacks
# In case model's power is poor, we'll build best performing model for individual attacks and will test them on general attacks

# Naive Bayes

# Logistic regression

# Support Vector Machine

# Random Forest

# Neural Net

# -------------------------------------------
# 6. Stage 3: Detecing unknown attacks 
# -------------------------------------------

# In this stage, we'll train model using two or more attacks and will predict previously unknown attacks.
# Strategy for training and testing: 
# Training: 80% of normal data and 20% of known attack data. Testing: 80% of normal data, 20% of unknown attacks
# I. Known attacks: SSSP, SSMP, MSSP Unknown attacks: MSMP
# Data aggregation
