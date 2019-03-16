# Data-Mining-Project - SWaT

# Required R Packages
openxlsx, dplyr, DMwR, randomForest, lubridate, corrplot, FSelector, reshape2, ggplot2, ROCR

# Dataset

Dataset 1 - 22/12/2015 04:00 PM to 28/12/2015 09:59 AM - No attack </br>
Dataset 2 - 28/12/2015 09:59 AM to 2/1/2016 2:59:59 PM - Both attack and no attacks </br>

In total, there are 946719 observations: Approx. 95% Normal and 5 % attack instances. </br>
There are total 53 variables (Either continuous or categorical, first variable consists of dates (per second))

## Dataset

Dataset comes in two parts - first part is for first 7 days, second part is for 5 parts. </br>

## File sizes 

First seven days (normalv0.xlsx) - 120 MB </br>
second part (attackv0.xlsx) - 110 MB </br>

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
