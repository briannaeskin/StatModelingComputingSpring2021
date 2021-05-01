library("caret")
library("class")
library("faraway")
library("InformationValue")
library("leaps")
library("randomForest")
library("tidyverse")

set.seed(9999)

#Data initialization and split to train/test
heart_disease <- read.csv("https://raw.githubusercontent.com/briannaeskin/StatModelingComputingSpring2021/main/heart_failure_clinical_records_dataset.csv", 
                          header=TRUE)

trainIndex <- createDataPartition(heart_disease$DEATH_EVENT, p=0.70, list=FALSE, times=1)

heart_disease_train <- heart_disease[trainIndex,]
heart_disease_test <- heart_disease[-trainIndex,]

#BIC Model Selection
heart_disease_train_BIC <- regsubsets(DEATH_EVENT ~ ., data=heart_disease_train)
heart_disease_train_BIC_sum <- summary(heart_disease_train_BIC)
heart_disease_train_BIC_sum$which

plot(heart_disease_train_BIC_sum$bic, ylab="BIC", xlab="Number of Predictors")

#Save File
jpeg(file="BIC_ModelSelection.jpeg")
plot(heart_disease_train_BIC_sum$bic, ylab="BIC", xlab="Number of Predictors")
dev.off()

which.min(heart_disease_train_BIC_sum$bic)