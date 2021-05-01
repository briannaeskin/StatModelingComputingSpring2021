set.seed(9999)

#Random Forest Modeling
random_forest_model <- randomForest(formula=as.factor(DEATH_EVENT) ~ ejection_fraction + serum_creatinine + time, data=heart_disease_train)

#Prediction
heart_disease_test_pred_rf <- heart_disease_test %>%
  mutate(pred = predict(random_forest_model, heart_disease_test))%>%
  mutate(accurate=1*(pred==DEATH_EVENT))
rf_acc <- sum(heart_disease_test_pred_rf$accurate)/nrow(heart_disease_test_pred_rf)

#Analysis
confusion_matrix_rf <- as.data.frame(table(heart_disease_test_pred_rf$DEATH_EVENT,heart_disease_test_pred_rf$pred))
confusion_matrix_rf$Var1 <- as.character(confusion_matrix_rf$Var1)
confusion_matrix_rf$Var2 <- as.character(confusion_matrix_rf$Var2)
confusion_matrix_rf$Var1[confusion_matrix_rf$Var1 == 0] <- "Survived"
confusion_matrix_rf$Var1[confusion_matrix_rf$Var1 == 1] <- "Died"
confusion_matrix_rf$Var2[confusion_matrix_rf$Var2 == 0] <- "Survived"
confusion_matrix_rf$Var2[confusion_matrix_rf$Var2 == 1] <- "Died"

ggplot(data=confusion_matrix_rf, mapping=aes(x=Var1,y=Var2)) +
  geom_tile(aes(fill=Freq), color = "white") +
  geom_text(aes(label=sprintf("%1.0f", Freq)), vjust=1) +
  scale_fill_gradient(low="steelblue", high="red") +
  theme_bw() + theme(legend.position="none") +
  xlab("Predicted") + ylab("Actual") + ggtitle("Predicted versus Actual - Random Forest")

#Save File
jpeg("confusionMatrix_RandomForest.jpeg")
ggplot(data=confusion_matrix_rf, mapping=aes(x=Var1,y=Var2)) +
  geom_tile(aes(fill=Freq), color = "white") +
  geom_text(aes(label=sprintf("%1.0f", Freq)), vjust=1) +
  scale_fill_gradient(low="steelblue", high="red") +
  theme_bw() + theme(legend.position="none") +
  xlab("Predicted") + ylab("Actual") + ggtitle("Predicted versus Actual - Random Forest")
dev.off()
