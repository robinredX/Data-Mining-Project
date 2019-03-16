# Data Mining Project - M1 - Robin Khatri
# Detection of Attacks on a Water Treatment Unit
# General dataset information: README.md, to request dataset: https://itrust.sutd.edu.sg/testbeds/secure-water-treatment-swat/
# Dataset is also available in the repository.

############################## 
#Part I: Check quality of data
##############################

# Load required libraries
# install.packages(c("openxlsx", "corrplot", "lubridate", "FSelector", "ggplot2", "dplyr","DMwR", "randomForest", "keras")) # Unquote if you need to install the packages

require(openxlsx)
require(dplyr)
require(lubridate)
require(ggplot2)
require(corrplot)
require(randomForest)
require(DMwR)
require(FSelector)
require(randomForest)
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
setwd(original_path)   # Set path to the path we originally used

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

setwd(original_path)   # Set path to the path we originally used

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

# After analyzing plots, it is clear that during instances labelled as attack, 
# the distribution of continuous features is different that those of normal distributions. 
# A key observation is also that not all continuous variables represent that much difference
# in distributions during instances for normal and attack.
# We'll use this information during feature selection

# Correlation between variables

M = cor(as.matrix(dataset[,cont_indices]))
png(filename="correlations.png")
corrplot(M, method = "color")
dev.off()
setwd(original_path) # Set path to the path we originally used

# Here many variables are not correlated and as we have seen, some variables gets more affected
# by the attacks, and therefore we shall not do PCA, instead we shall select best feature for
# classification of an instance into an attack or a normal instance


# We want to get an idea of number of attack instances per day
# We'll create a column with day number i.e. Dec 22 = day 1, Feb 02 = last 11
temp = date(dataset$Timestamp) 
temp1 = date(dataset$Timestamp[1])
dataset$day = as.numeric(temp - temp1)+1 # Starts at day 0
head(dataset$day)

# Visualize number of attacks daywise (Since the attacks dataset starts from Dec 28,
# we'll start from day 7)

dataset_day_attack <- dataset[dataset$day>=6,]
dataset_day_attack <- dataset[dataset$`Normal/Attack`==1,]

dataset_day_attack %>% 
  count(dataset_day_attack$day) %>% 
  mutate(perc = n*100 / nrow(dataset_day_attack)) -> temp_data

x = c("28 Dec", "29 Dec", "30 Dec", "31 Dec", "1 Feb", "2 Feb")
colnames(temp_data)[1] = "day"
temp_data$day = c("28 Dec", "29 Dec", "30 Dec", "31 Dec", "1 Feb", "2 Feb")
temp_data$day <- factor(temp_data$day,levels = x)
temp_data
ggplot(data = temp_data, aes(x=day, y=perc, fill=day))+
  geom_bar(stat="identity")+ ggtitle("Number of attacks instances")+
  labs(x = "Day", y = "Percent of Attack instances")+
  theme_minimal()
path = paste(original_path, "/distribution_of_attacks",sep="")
dir.create(path)
setwd(path)
ggsave(paste("day_wise_distribution", ".png"), device = "png")

# Visualize attacks hour wise
dataset$hour <- hour(dataset$Timestamp)
dataset_hour_attack <- dataset[dataset$day>=6,]
dataset_hour_attack <- dataset[dataset$`Normal/Attack`==1,]

dataset_hour_attack %>% 
  count(dataset_hour_attack$hour) %>% 
  mutate(perc = n*100 / nrow(dataset_hour_attack)) -> temp_data

colnames(temp_data)[1] = "hour"

ggplot(data = temp_data, aes(x=hour, y=perc, fill=hour))+
  geom_bar(stat="identity")+ ggtitle("Number of attacks instances")+
  labs(x = "Hour", y = "Percent of Attack instances")+
  theme_minimal()

ggsave(paste("hour_wise_distribution", ".png"), device = "png")
setwd(original_path)


# Relationship between processes of the system

# We'll idenify the behavior of level of the water tank, valves, pump, ORP Fluctuations,  

# Relationship between ORP fluctuation (AIT402) and attacks 

sn = c(1:n_obs)  # Series number to indicate sample number
dataset$sn = sn


ggplot(data=dataset, aes(x=sn, y=AIT402, group=dataset$`Normal/Attack`)) +
  geom_line(aes(linetype=dataset$`Normal/Attack`))+
  geom_point()
ggsave(paste("orp", ".png"), device = "png")

# Feature selection

# ensure the results are repeatable





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

# Normalize data
temp2 <- dataset[dataset$day>=6,]
# Remove features from previous analysis - categorical variables for which variance was 0
temp2 <- temp2[,-(zero_var_indices)]
colnames(temp2)
temp2 <- temp2[,2:(ncol(temp2)-3)] # Removing label, day, hour, sn
n = ncol(temp2)
colnames(temp2)[n] <- "label" # Label is the new target variable name
temp3 <- data.frame(scale(temp2[,1:(n-1)]))
label <- temp2$label
data <- cbind(temp3,label)
head(data)

# # Sampling (SRS with keeping the original label proportions)

p = 100000
m = nrow(data)
data_normal <- data[data$label==0,]
data_attack <- data[data$label==1,]
n_normal_sample = round(nrow(data_normal)*p/m)
n_attack_sample = p - n_normal_sample

sample_normal <- data_normal[sample(nrow(data_normal),round(n_normal_sample)),] # Only normal data - 80 percent
sample_attack <- data_attack[sample(nrow(data_attack),n_attack_sample),]
sample_data <- rbind(sample_normal, sample_attack)
sample_data <- sample_data[sample(p,p),]
  
n_train <- round(80*p/100)
  
train <- sample_data[1:n_train,]
test <- sample_data[(n_train+1):p,]
head(train)
head(test)

# Since data is unbalanced, we can do undersampling for the majority class (normal instances) and oversampling for the
# minority class (attack instances)
# One way to do it is SMOTE. Read about SMOTE at https://arxiv.org/pdf/1106.1813.pdf
# To test the model accurately, the SMOTE will be done only on the training set

newsample <- SMOTE(label ~ ., data = train, perc.over = 200,perc.under=100)
newsample <- data.frame(newsample)
t <- table(newsample$label) # Number of observations in our training data after SMOTE = 19095 (7638: normal, 11457: attack)
prop_0 <- t[1]/sum(t); prop_1 <- t[2]/sum(t)
prop_0 # 40 % Normal instances
prop_1 # 60 % Attack instances  
t
# Write files for training and testing
write.csv(newsample,"training.csv")
write.csv(test, "testing.csv")

which(colnames(newsample)=="P301")
newsample1 = newsample[,-23]
test1 = test[,-23]

# Feature selection
# We'll use information gain and see if the results match our observations 
# We'll use only training set for the feature selection so that we can test accurately and there is no influence of test
# set on training our model

newsample1[,1:(ncol(newsample1)-1)] <- newsample1[,1:(ncol(newsample1)-1)] *1000 # Multiplying by 1000 to increase the bin
ig <- information.gain(label~., newsample1)

# size. The package FSelector calls Weka, Rweka:Discretize and when various values are found similar, it gives bin error
head(newsample1)
min(ig)
max(ig)
mean_ig <- sum(ig)/ncol(newsample1)
mean_ig
ig$attr_importance
Feature <- rownames(ig)
importance <- ig$attr_importance
feature_importance <- data.frame(Feature, importance)
feature_importance <- feature_importance[order(feature_importance$importance, decreasing = TRUE),] # Order in decreasing order by feature importance
sorted_feature <- feature_importance$Feature

feature_importance$Feature <- factor(feature_importance$Feature, levels = sorted_feature)

# Visualize feature importance
plot <- ggplot(data = feature_importance, aes(x=Feature, y=round(importance,4), fill=importance))+
  geom_bar(stat="identity")+ ggtitle("Feature Importance")+
  geom_text(aes(label=round(importance,4), hjust=-0.5, vjust=0.5)) +
  labs(x = "Feature", y = "Importance")+
  theme_minimal() + 
  coord_flip()
plot

path = paste(original_path, "/feature_selection", sep="")
dir.create(path)
setwd(path)
ggsave("information_gain.png", device="png")
setwd(original_path)


text(x = plot, y = newsample1$importance, label = newsample1$importance, pos = 3, cex = 0.8, col = "black")

# It is evident that many features are redudandant. 
# We'll see if the number of features with importance greater than mean importance seems enough

temp4 = rep(0,nrow(ig))
for(i in 1:nrow(ig)){
  if(ig$attr_importance[i]>mean_ig){
    temp4[i] = rownames(ig)[i]  
  }
}
sld_features = temp4[temp4!=0] # Selected features

print("Selected features are:"); sld_features
reduction = ncol(newsample1)- length(sld_features) # 
sld_features

paste(24, "features remaining from original 53")

index = rep(0,ncol(train1))

for(i in 1:(ncol(newsample1)-1)){
  for(j in 1:length(sld_features)){
    if(colnames(newsample1)[i]==sld_features[j]){index[i] = 1}  
  }
}

index <- which(index==1)
index # Provides column numbers of feature selected

new_train <- newsample1[,c(index,ncol(newsample1))]
glimpse(new_train)

# Select same indices in testing set

new_test <- test1[,c(index,ncol(test1))]
colnames(new_test)
colnames(new_train)

# So now our training and testing sets are ready

# Training

# Testing

# Evaluation Metrics

# Accuracy

# Robustness for different categories of attacks

# Random Forest




