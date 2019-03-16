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
require(reshape2)
# require(h2o)

# Set directory

setwd("C:/users/robin/Desktop/DataMiningProject")
dir() # Load files in directory

# Preparing dataset

# import datasets

start = Sys.time()
normal <- read.xlsx("SWaT_Dataset_Normal_v0.xlsx") # Normal behavior
attack <- read.xlsx("SWaT_Dataset_Attack_v0.xlsx") # Both normal and attack instances
end = Sys.time()

end-start # It took around 7 minutes to read the full data

glimpse(attack)
glimpse(normal)

dataset <- rbind(normal, attack)

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


# Timestamps: Convert timestamps into date into POSIXlt format and Sort dataset by timestamps in increasing order

head(dataset$Timestamp)
dataset$Timestamp <- strptime(dataset$Timestamp, format="%d/%m/%Y %I:%M:%S %p")
dataset <- dataset[order(dataset$Timestamp),] 
# Convert timestamps into 24 hrs from AM/PM
dataset$Timestamp <- ymd_hms(dataset$Timestamp)
starttime = min(dataset$Timestamp)
endtime = max(dataset$Timestamp)
starttime
endtime


# Check for missing data

which(is.na(dataset)) # No missing datapoint

# Missing timestamps

# Generate a sequence of all timestamps between starttime and endtime
time_vector <- seq(ymd_hms(starttime),ymd_hms(endtime), by = '1 sec')
head(time_vector)

missing_times <- time_vector[!time_vector %in% dataset$Timestamp]
range(missing_times) # There is one missing period
range = max(missing_times) - min(missing_times)
start_missing <- min(missing_times)-1 # Last observation in dataset before data is missed
end_missing <- max(missing_times)+1 # First observation in dataset after data is continued

# Checking if timestamps are missing at random

# For all features, we'll plot graphs to see if there is any change in the instances since we have discontinuity of
# Timestamps

index1 = which(dataset$Timestamp==start_missing-60*60) # 1 hour before the timestamps went missing
index2 = which(dataset$Timestamp==start_missing-1) # Last timestamp before discontinuity
index3 = which(dataset$Timestamp==end_missing+1) # First timestamp after discontinuity
index4 = which(dataset$Timestamp==end_missing-60*60) # 1 hour after the resume in continuity of timestamps
dataset_before <- dataset[index1:index2,]
dataset_after <- dataset[index3:index4,]

dir.create(" before_time_discontinuity")
dir.create("after_time_discontinuity")
original_path = getwd()

path_before = paste(original_path,"/before_time_discontinuity",sep="")
path_after = paste(original_path,"/after_time_discontinuity",sep="")
  
# Plot features before the discontinuity
setwd(path_before)
for(i in 2:(n_features)){
  #   Plots and saves variation in each feature as png images
  yname = colnames(dataset_new)[i]
  ggplot(dataset_before, aes(dataset_before$Timestamp, dataset_before[[i]])) + geom_line() +
  xlab("1 hour before the first missing timestamp") + ylab(yname)
  ggsave(paste(yname, "before.png"), device = "png") 
}

# Plot features after the discontinuity
setwd(path_after)
for(i in 2:(n_features)){
  #   Plots and saves variation in each feature as png images
  yname = colnames(dataset_after)[i]
  ggplot(dataset_after, aes(dataset_after$Timestamp, dataset_after[[i]])) + geom_line() +
    xlab("1 hour after the first missing timestamp") + ylab(yname)
  ggsave(paste(yname, "after.png"), device = "png") # Saves all the plots as png images
}
setwd(original_path)

# Note: Since attacks were carried out at scheduled times, there will be no enquiry into relationship of time in detection of attacks

# Visualize target variable

ggplot(dataset, aes(x=`Normal/Attack`))+
  geom_bar(stat="count", width=0.7, fill="lightblue")+ ggtitle("Number of attacks and normal instances")+
  labs(x = "Normal/Attack", y = "Count")+
  theme_minimal()

# Target variable distribution by date

# Identify variables - categorical
summary(dataset)
# it is clear from summary that some variables are categorical - taking only two values, and so we can first see
# there relationships with the target variable

# Binary variables
find_cat <- function(dataset){
  categorical = rep(0, n_features)
  for(i in 2:n_features){
    if (length(unique(dataset[[i]])) <=2){categorical[i]=1}
  }
  return(which(categorical==1))
}
 
cat_indices <- find_cat(dataset) # Returns indices of features that are binary

# Visualize binary variables with respect to the normal and attack instances

dir.create("categorical_variable_plots")
path = paste(original_path, "/categorical_variable_plots",sep="")
setwd(path)

for(i in cat_indices){
  # Plots and saves to directory - path
  ggplot(dataset, aes(dataset[[i]], ..count..)) + geom_bar(aes(fill = dataset[[53]]), position = "dodge") +
    labs(x = colnames(dataset)[i], y = 'Count', title = 'Attacks by date') +
    scale_fill_discrete(name="Instance",
                        breaks=c(1,0),
                        labels=c("Attack", "Normal"))
  ggsave(paste(colnames(dataset)[i], ".png"), device = "png") # Saves all the plots as png images
}

setwd(original_path)  

# we found from plots that
# Variables P202, P401, P404, P4502, P601, P603 have only one value i.e. they have zero variance
# we can also check it from following function

findvarzero <- function(dataset){
  varindices <- rep(0,n_features+1)
  for (i in 2:n_features){
    if(isTRUE(var(dataset[[i]])==0)){
      varindices[i] = 1
      }
  }
  varindices = which(varindices==1)
  return(varindices)
}

zero_var_indices <- findvarzero(dataset)
zero_var_indices # The function returns the same features that we found from plot

for(i in zero_var_indices){
print(colnames(dataset)[i])}

# Variables P402, P501, UV401 have one category that is significant during attacks
# We'll use this information during feature selection

# Understanding continuous variables
# Distribution of continuous variables during normal and attack instances
# We'll use violin plots for all continuous variables for this 

# Get indices of continuous variables
cat_indices
feature_indices = c(1:n_features) # Not taking timestamp
cont_indices <- which(!feature_indices %in% cat_indices)
cont_indices <- cont_indices[-1]

# Plot and save the plots
dir.create("Continuous_feature_distributions")
path = paste(original_path,"/continuous_feature_distributions",sep="")
setwd(path)
for (i in cont_indices){
  ggplot(dataset, aes(x=dataset$`Normal/Attack`, y=dataset[[i]], fill=dataset$`Normal/Attack`)) +
    geom_violin(trim=FALSE)+
    scale_fill_manual(values = c("red", "lightblue"), name = "Instance",
                      breaks = c(1,0),
                      labels = c("Attack", "Normal")) +
    labs(x = 'Instance - Normal/Attack', y = colnames(dataset)[i], title = paste('Distribution of',colnames(dataset)[i],sep=" ")) +
    theme_minimal()
  ggsave(paste(colnames(dataset)[i], ".png"), device = "png") # Saves all the plots as png images
}
setwd(original_path)

# After analyzing plots, it is clear that during instances labelled as attack, 
# the distribution of continuous features is different that those of normal distributions. 
# A key observation is also that not all continuous variables represent that much difference
# in distributions during instances for normal and attack.
# We'll use this information during feature selection

# Relationship between processes of the system



# Number of attacks by date
# Extract dates from timestamps
dataset$date <- date(dataset$Timestamp)

# Get count of attack instances by date
# dataset$new <- as.character(dataset$`Normal/Attack`)
# dataset$new <- as.numeric(dataset$new)
# date.attacks <- dataset %>%
#  group_by(date) %>%
#  summarise(n_attacks = sum(new)) %>%
#  arrange(desc(n_attacks))
# date.attacks
# Plot count of attack instances by date
# ggplot(date.attacks, aes(x = date, y = n_attacks)) + geom_col() + labs(x = 'Date', y = 'Attacks', title = 'Attacks by date')
# table(dataset$`Normal/Attack`)

###########################
# Part I: Feature Selection
###########################

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


# Testing

# Accuracy

# Robustness for different categories of attacks


