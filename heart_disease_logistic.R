set.seed(9999)

#Logistic Regression
lmod_heart_disease <- glm(DEATH_EVENT ~ ejection_fraction + serum_creatinine + time,
                     family=binomial, heart_disease_train)
summary(lmod_heart_disease)

#Prediction
lmod_heart_disease_prob <- predict(lmod_heart_disease, heart_disease_test, type="response")
optCutoff <- optimalCutoff(heart_disease_test$DEATH_EVENT,lmod_heart_disease_prob)
heart_disease_test_pred_lmod <- heart_disease_test %>%
  mutate(predict=1*(lmod_heart_disease_prob > optCutoff)) %>%
  mutate(accurate=1*(predict==DEATH_EVENT))
lmod_acc <- sum(heart_disease_test_pred_lmod$accurate)/nrow(heart_disease_test_pred_lmod)

#Analysis
confusion_matrix_lmod <- as.data.frame(table(heart_disease_test_pred_lmod$DEATH_EVENT,heart_disease_test_pred_lmod$predict))
confusion_matrix_lmod$Var1 <- as.character(confusion_matrix_lmod$Var1)
confusion_matrix_lmod$Var2 <- as.character(confusion_matrix_lmod$Var2)
confusion_matrix_lmod$Var1[confusion_matrix_lmod$Var1 == 0] <- "Survived"
confusion_matrix_lmod$Var1[confusion_matrix_lmod$Var1 == 1] <- "Died"
confusion_matrix_lmod$Var2[confusion_matrix_lmod$Var2 == 0] <- "Survived"
confusion_matrix_lmod$Var2[confusion_matrix_lmod$Var2 == 1] <- "Died"

ggplot(data=confusion_matrix_lmod, mapping=aes(x=Var1,y=Var2)) +
  geom_tile(aes(fill=Freq), color = "white") +
  geom_text(aes(label=sprintf("%1.0f", Freq)), vjust=1) +
  scale_fill_gradient(low="steelblue", high="red") +
  theme_bw() + theme(legend.position="none") +
  xlab("Predicted") + ylab("Actual") + ggtitle("Predicted versus Actual - Logistic Regression")

#Save File
jpeg(file="confusionMatrix_logisticRegression.jpeg")
ggplot(data=confusion_matrix_lmod, mapping=aes(x=Var1,y=Var2)) +
  geom_tile(aes(fill=Freq), color = "white") +
  geom_text(aes(label=sprintf("%1.0f", Freq)), vjust=1) +
  scale_fill_gradient(low="steelblue", high="red") +
  theme_bw() + theme(legend.position="none") +
  xlab("Predicted") + ylab("Actual") + ggtitle("Predicted versus Actual - Logistic Regression")
dev.off()
