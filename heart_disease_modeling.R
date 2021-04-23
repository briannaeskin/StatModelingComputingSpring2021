install.packages("caret")
install.packages("InformationValue")
library("caret")
library("InformationValue")
library("leaps")
library("tidyverse")

set.seed(9999)

#Data initialization and split to train/test
heart_disease <- read.csv("https://raw.githubusercontent.com/briannaeskin/StatModelingComputingSpring2021/main/heart_failure_clinical_records_dataset.csv", 
                          header=TRUE)

trainIndex <- createDataPartition(heart_disease$DEATH_EVENT, p=0.70, list=FALSE, times=1)

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

#Second check, logistic regression with BIC
heart_disease_train_BIC <- regsubsets(DEATH_EVENT ~ ., data=heart_disease_train)
heart_disease_train_BIC_sum <- summary(heart_disease_train_BIC)
heart_disease_train_BIC_sum$which
plot(heart_disease_train_BIC_sum$bic, ylab="BIC", xlab="Number of Predictors")
which.min(heart_disease_train_BIC_sum$bic)

lmod_filtered <- glm(DEATH_EVENT ~ ejection_fraction + serum_creatinine + time,
                     family=binomial, heart_disease_train)
summary(lmod_filtered)

lmod_filtered_prob <- predict(lmod_filtered, heart_disease_test, type="response")
optCutoff <- optimalCutoff(heart_disease_test$DEATH_EVENT,lmod_filtered_prob)
heart_disease_test_pred <- heart_disease_test %>%
  mutate(predict=1*(lmod_filtered_prob > optCutoff)) %>%
  mutate(accurate=1*(predict==DEATH_EVENT))
sum(heart_disease_test_pred$accurate)/nrow(heart_disease_test_pred)

confusion_matrix_filtered <- as.data.frame(table(heart_disease_test_pred$DEATH_EVENT,heart_disease_test_pred$predict))
ggplot(data=confusion_matrix_filtered, mapping=aes(x=Var1,y=Var2)) +
  geom_tile(aes(fill=Freq), color = "white") +
  geom_text(aes(label=sprintf("%1.0f", Freq)), vjust=1) +
  scale_fill_gradient(low="blue", high="red") +
  theme_bw() + theme(legend.position="none") +
  xlab("Predicted") + ylab("Actual") + ggtitle("Predicted versus Actual")
  
