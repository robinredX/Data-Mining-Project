# Data-Mining-Project - SWaT

# Required R Packages
openxlsx, dplyr, DMwR, randomForest, nnet, ROCR
library(dplyr)
library(DMwR)
library(randomForest)
library(nnet)

# Timeline

Dataset 1 - 22/12/2015 04:00 PM to 28/12/2015 09:59 AM - No attack
Dataset 2 - 28/12/2015 09:59 AM to 2/1/2016 2:59:59 PM - Both attack and no attacks

In total, there are 395298 normal observations (87.86 %) and 54621 attacks (12.41 %).

# Attacks

Four categories of attacks:
1. SSSP: Single Stage Multi Point
2. SSMP: Single Stage Multi Point
3. MSSP: Multi Stage Single Point
4. MSMP: Multi Stage Multi Point

Description for each attack: 

# Objective

1. To detect attacks with accuracy and less false positives (Accuracy measure to be used F-1 Score, Recall %)
2. To identify the category of the attacks 
