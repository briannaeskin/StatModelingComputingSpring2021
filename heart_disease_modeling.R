install.packages("caret")
install.packages("class")
install.packages("InformationValue")
library("caret")
library("class")
library("faraway")
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

#Logistic regression with BIC model selection
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
  scale_fill_gradient(low="steelblue", high="red") +
  theme_bw() + theme(legend.position="none") +
  xlab("Predicted") + ylab("Actual") + ggtitle("Predicted versus Actual")



#KNN with filtered model
heart_disease_train_filtered_x <- heart_disease_train %>%
  select(ejection_fraction,serum_creatinine,time)
heart_disease_train_filtered_y <- heart_disease_train_y
heart_disease_test_filtered_x <- heart_disease_test %>%
  select(ejection_fraction,serum_creatinine,time)
heart_disease_test_filtered_y <- heart_disease_test_y

#Calculate error
calc_error <- function(actual, predicted){
  error <- mean(actual != predicted)
  return(error)
}

#Determine k
ks = 1:20
errors <- rep(x=0, times=length(ks))

for (i in seq_along(ks)) {
  prediction <- knn(train=heart_disease_train_filtered_x,
                    test=heart_disease_test_filtered_x,
                    cl=heart_disease_train_filtered_y,
                    k=ks[i])
  errors[i] <- calc_error(heart_disease_test_filtered_y, prediction)
}

plot(errors, type = "b", col = "dodgerblue", cex = 1, pch = 20, 
     xlab = "k, number of neighbors", ylab = "Classification Error",
     main = "(Test) Error Rate vs Neighbors")
# add line for min error seen
abline(h = min(errors), col = "darkorange", lty = 3)
# add line for minority prevalence in test set
abline(h = mean(heart_disease_test_filtered_y == 1), col = "grey", lty = 2)

min(errors)
which(errors == min(errors))


heart_disease_knn <- knn(train=heart_disease_train_filtered_x,
                         test=heart_disease_test_filtered_x,
                         cl=heart_disease_train_filtered_y,
                         k=10)

1-calc_error(heart_disease_test_filtered_y,heart_disease_knn)
#87.64%

confusion_matrix_filtered_knn <- as.data.frame(table(heart_disease_test_filtered_y,heart_disease_knn))
ggplot(data=confusion_matrix_filtered_knn, mapping=aes(x=heart_disease_knn,y=heart_disease_test_filtered_y)) +
  geom_tile(aes(fill=Freq), color = "white") +
  geom_text(aes(label=sprintf("%1.0f", Freq)), vjust=1) +
  scale_fill_gradient(low="steelblue", high="red") +
  theme_bw() + theme(legend.position="none") +
  xlab("Predicted") + ylab("Actual") + ggtitle("Predicted versus Actual - KNN")
