# Data Mining Project - M1 - Robin Khatri
# Detection of Attacks on a Water Treatment Unit
# General dataset information: README.md, to request dataset: https://itrust.sutd.edu.sg/testbeds/secure-water-treatment-swat/
# Dataset is also available in the repository.

############################## 
#Part I: Check quality of data
##############################

# Load required libraries
# install.packages(c("openxlsx", "ggplot2", "dplyr","DMwR", "randomForest", "keras")) # Unquote if you need to install the packages

require(openxlsx)
require(dplyr)
require(lubridate)
require(ggplot2)
require(corrplot)
require(randomForest)
require(h2o)

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
dataset_normalized <- data.frame(scale(dataset_new[,2:45])) # Remove Timestamps
dataset_normalized$label <- dataset_new$`Normal/Attack`
glimpse(dataset_normalized)
ncol(dataset_normalized)
# Importance of features

# Correlation
# source("http://www.sthda.com/upload/rquery_cormat.r")
new <- cor(dataset_normalized[,-45])

# Principal component analysis

# Sampling (SRS with keeping the original label proportions)

m = nrow(dataset_normalized)
p = 50000 # Sample size
l = p/m

normal_data <- dataset_normalized[dataset_normalized$label==0,]
n_normal_sample = nrow(normal_data)*p/m
sample_normal <- normal_data[sample(nrow(normal_data),round(n_normal_sample)),]
train <- sample_normal[1:80*p/100,] # Only normal data - 80 percent
test_normal <- sample_normal[(nrow(train)+1):nrow(sample_normal),]

attack_data <- dataset_normalized[dataset_normalized$label==1,]
sample_attack <- attack_data[sample(nrow(attack_data), round(p-round(n_normal_sample))),]
test <- rbind(test_normal, sample_attack) # Remaining 20 percent - both normal and attack instances
table(sample_attack$label)

sample_d <- rbind(sample_normal, sample_attack)
sample_d <- sample_d[sample(nrow(sample_d),nrow(sample_d)),]
train <- sample_d[1:40000,]
test <- sample_d[(nrow(train)+1):nrow(sample_d),]

# convert datasets to H2OFrame
h2o.init() # Initiailize h2o
train_h2o <- as.h2o(train)
test_h2o <- as.h2o(test)
response <- "label"
features <- setdiff(colnames(train_h2o), response)
model_nn <- h2o.deeplearning(x = features,
                             training_frame = train_h2o,
                             model_id = "model_nn",
                             autoencoder = TRUE,
                             reproducible = TRUE, #slow - turn off for real problems
                             ignore_const_cols = FALSE,
                             seed = 42,
                             hidden = c(10, 2, 10), 
                             epochs = 100,
                             activation = "Tanh")

h2o.saveModel(model_nn, path="model_nn", force = TRUE)

model_nn <- h2o.loadModel("C:\\Users\\robin\\Desktop\\DataMiningProject\\model_nn\\model_nn")
model_nn
test_autoenc <- h2o.predict(model_nn, test_h2o)


train_features <- h2o.deepfeatures(model_nn, train_h2o, layer = 2) %>%
  as.data.frame() %>%
  mutate(Class = as.vector(train_h2o$label))

ggplot(train_features, aes(x = DF.L2.C1, y = DF.L2.C2, color = Class)) +
  geom_point(alpha = 0.1)

train_features <- h2o.deepfeatures(model_nn, train_h2o, layer = 3) %>%
  as.data.frame() %>%
  mutate(Class = as.factor(as.vector(train_h2o$label))) %>%
  as.h2o()

features_dim <- setdiff(colnames(train_features), response)

model_nn_dim <- h2o.deeplearning(y = "Class",
                                 x = features_dim,
                                 training_frame = train_features,
                                 reproducible = TRUE, #slow - turn off for real problems
                                 balance_classes = TRUE,
                                 ignore_const_cols = FALSE,
                                 seed = 42,
                                 hidden = c(10, 2, 10), 
                                 epochs = 100,
                                 activation = "Tanh")

h2o.saveModel(model_nn_dim, path="model_nn_dim", force = TRUE)
model_nn_dim <- h2o.loadModel("C:\\Users\\robin\\Desktop\\DataMiningProject\\model_nn_dim\\DeepLearning_model_R_1552686872729_1")
model_nn_dim
# Training
head(train_h2o$label)
sum(train_h2o$label)


# Testing

# Accuracy

# Robustness for different categories of attacks


