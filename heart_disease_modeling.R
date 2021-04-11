install.packages("caret")
library("caret")
library("leaps")
library("tidyverse")

set.seed(1234)

#Data initialization and split to train/test
heart_disease <- read.csv("https://raw.githubusercontent.com/briannaeskin/StatModelingComputingSpring2021/main/heart_failure_clinical_records_dataset.csv", 
                          header=TRUE)

trainIndex <- createDataPartition(heart_disease$DEATH_EVENT, p=0.7, list=FALSE, times=1)

heart_disease_train <- heart_disease[trainIndex,]
heart_disease_test <- heart_disease[-trainIndex,]

#First check, logistic regression with all variables
lmod_all <- glm(DEATH_EVENT ~ ., family=binomial, heart_disease_train)
summary(lmod_all)

lmod_all_prob <- predict(lmod_all, heart_disease_test, type="response")
heart_disease_test_pred <- heart_disease_test %>%
  mutate(predict=1*(lmod_all_prob > 0.5)) %>%
  mutate(accurate=1*(predict==DEATH_EVENT))
sum(heart_disease_test_pred$accurate)/nrow(heart_disease_test_pred)

#Second check, logistic regression with AIC
heart_disease_train_AIC <- regsubsets(DEATH_EVENT ~ ., data=heart_disease_train)
heart_disease_train_AIC_sum <- summary(heart_disease_train_AIC)
heart_disease_train_AIC_sum$which
plot(heart_disease_train_AIC_sum$bic, main='BIC')

lmod_filtered <- glm(DEATH_EVENT ~ ejection_fraction + serum_creatinine + time,
                     family=binomial, heart_disease_train)
summary(lmod_filtered)

lmod_filtered_prob <- predict(lmod_filtered, heart_disease_test, type="response")
heart_disease_test_pred <- heart_disease_test %>%
  mutate(predict=1*(lmod_filtered_prob > 0.5)) %>%
  mutate(accurate=1*(predict==DEATH_EVENT))
sum(heart_disease_test_pred$accurate)/nrow(heart_disease_test_pred)
