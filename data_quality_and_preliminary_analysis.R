# Data Mining Project: Robin Khatri
# Detection of attacks on Secure Water Treatment (SWaT) Testbed
# Information on SWaT: https://itrust.sutd.edu.sg/testbeds/secure-water-treatment-swat/

# Directory and loading data
setwd("~/Documents/Data Mining Project")
dir()
data <- read.csv("dataset-labelled-attacks.csv")

# Libraries
#library(e1071)
library(randomForest)
library(e1071) 

# Data Quality and Pre-processing
nrow(data)
newvector <- data$Normal.Attack
head(newvector)

newvector <- as.character(newvector)

# Dependent Variable
newvector[newvector=="Attack"]<- 1
newvector[newvector=="Normal"]<- 0
data$Normal.Attack <-  newvector
colnames(data) # 44 features (First column consists of timestamps)
nrow(data) # 449919 observations
data1 <- data[,-1]
data2 <- data1[,-44]
colnames(data2)[43] <- "Label"
data2$'Label' <- as.numeric(data2$'Label')

# Step 1: Detecting attacks (all kinds)
# Fitting Support Vector Machine
svmfit = svm(Label ~ ., data = data2, kernel = "linear", cost = 10, scale = FALSE)
print(svmfit)

forestfit <- randomForest(Label~., data=data2, importance = TRUE)
print(forestfit)

# Principal component analysis (PCA)

