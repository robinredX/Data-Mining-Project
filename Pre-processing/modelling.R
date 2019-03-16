############################
# Part II: Modelling
############################
traind = new_train[sample(nrow(new_train), 10000),]
# Training
iris.rf <- randomForest(label ~ ., data=traind, importance=FALSE,
                        proximity=TRUE)

rf_output=randomForest(x=traind[1:(ncol(traind)-1)], y=traind$label, importance = TRUE, ntree = 100, proximity=TRUE, keep.forest=TRUE)
rf_output
predict_forest <- predict(rf_output, test, type="response")
table(predict_forest, test$label)

table(new_train$label)

# We'll use Random Forest and will do supervised learning

########################
# Testing on testing set
########################

# Load required libraries

require(randomForest)
setwd("C:/users/robin/Desktop/DataMiningProject")

# First, we'll test on our testing set and evaluate performance of our model
# Evaluation Metrics

#############################################################################
# Testing on data outside on which the model was trained or tested previously
#############################################################################

# Since dataset is large -  Around half a million for days when attacks took place, we'll test in batches of 20000
# We'll evaluate AUC and precision for every batch
train <- read.csv("training_set.csv")

train$label <- as.factor(train$label)

new <- train[,1:(ncol(train)-1)]/1000
train <- cbind(new, train$label)

head(train)
colnames(train)[ncol(train)] <- "label"
train <- train[sample(nrow(train), 15000),]

head(train$label)
rf_output=randomForest(x=train[,1:(ncol(train)-1)], y=train$label, importance = TRUE, ntree = 100, proximity=TRUE, keep.forest=TRUE)
rf_output

test <- read.csv("testing_set.csv")
test <- test[sample(nrow(test), 5000),]
test$label <- as.factor(test$label) # Convert label in our testing set into a factor
table(test$label)
predict_forest <- predict(rf_output, test[,1:(ncol(test)-1)], type="prob")[,2]

CM = table(predict_forest, test$label)
accuracy = (sum(diag(CM)))/sum(CM)
accuracy


library(ROCR)

perf <- prediction(predict_forest, test$label)
perd <- performance(perf, "tpr", "fpr")
precision_recall <- performance(perf, "prec", "rec")
plot(precision_recall)

spec <- performance(perf, "spec", "sens")
plot(spec)
plot(perd,main="ROC Curve for Random Forest",col=2,lwd=2)
abline(a=0,b=1,lwd=2,lty=2,col="gray")
plot(perd, avg='threshold', spread.estimate='stddev',
     colorize=T)

names(perf)

plot(perd, avg= "vertical",
     spread.estimate="boxplot",
     show.spread.at= seq(0.1, 0.9, by=0.1))

auc.tmp <- performance(perf,"auc");
auc <- as.numeric(auc.tmp@y.values)
auc

pred = perf
perf = perd

acc.perf = performance(pred, measure = "acc")
plot(acc.perf)

ind = which.max( slot(acc.perf, "y.values")[[1]] )
acc = slot(acc.perf, "y.values")[[1]][ind]
cutoff = slot(acc.perf, "x.values")[[1]][ind]
print(c(accuracy= acc, cutoff = cutoff))

batch_test <- function(batch_data){
  return(AUC, Precision)
}

n_batches = 5*10^5/20000
n_batches



# Testing for different categories of attacks







