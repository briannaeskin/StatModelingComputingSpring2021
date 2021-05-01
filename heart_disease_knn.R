set.seed(9999)

#Filter table as required for KNN
heart_disease_train_filtered_x <- heart_disease_train %>%
  select(ejection_fraction,serum_creatinine,time)
heart_disease_train_filtered_y <- heart_disease_train$DEATH_EVENT
heart_disease_test_filtered_x <- heart_disease_test %>%
  select(ejection_fraction,serum_creatinine,time)
heart_disease_test_filtered_y <- heart_disease_test$DEATH_EVENT

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
     xlab = "k, Number of Neighbors", ylab = "Classification Error",
     main = "(Test) Error Rate vs Neighbors")
abline(h = min(errors), col = "darkorange", lty = 3)

jpeg(file="KNN_ErrorRate.jpeg")
plot(errors, type = "b", col = "dodgerblue", cex = 1, pch = 20, 
     xlab = "k, Number of Neighbors", ylab = "Classification Error",
     main = "(Test) Error Rate vs Neighbors")
# add line for min error seen
abline(h = min(errors), col = "darkorange", lty = 3)
dev.off()

min(errors)
k <- which(errors == min(errors))
k

#Final KNN
heart_disease_knn <- knn(train=heart_disease_train_filtered_x,
                         test=heart_disease_test_filtered_x,
                         cl=heart_disease_train_filtered_y,
                         k=k)
knn_acc <- 1-calc_error(heart_disease_test_filtered_y,heart_disease_knn)

#Analysis
confusion_matrix_knn <- as.data.frame(table(heart_disease_test_filtered_y,heart_disease_knn))
confusion_matrix_knn$heart_disease_test_filtered_y <- as.character(confusion_matrix_knn$heart_disease_test_filtered_y)
confusion_matrix_knn$heart_disease_knn <- as.character(confusion_matrix_knn$heart_disease_knn)
confusion_matrix_knn$heart_disease_test_filtered_y[confusion_matrix_knn$heart_disease_test_filtered_y == 0] <- "Survived"
confusion_matrix_knn$heart_disease_test_filtered_y[confusion_matrix_knn$heart_disease_test_filtered_y == 1] <- "Died"
confusion_matrix_knn$heart_disease_knn[confusion_matrix_knn$heart_disease_knn == 0] <- "Survived"
confusion_matrix_knn$heart_disease_knn[confusion_matrix_knn$heart_disease_knn == 1] <- "Died"

ggplot(data=confusion_matrix_knn, mapping=aes(x=heart_disease_knn,y=heart_disease_test_filtered_y)) +
  geom_tile(aes(fill=Freq), color = "white") +
  geom_text(aes(label=sprintf("%1.0f", Freq)), vjust=1) +
  scale_fill_gradient(low="steelblue", high="red") +
  theme_bw() + theme(legend.position="none") +
  xlab("Predicted") + ylab("Actual") + ggtitle("Predicted versus Actual - KNN")

#Save File
jpeg("confusionMatrix_KNN.jpeg")
ggplot(data=confusion_matrix_knn, mapping=aes(x=heart_disease_knn,y=heart_disease_test_filtered_y)) +
  geom_tile(aes(fill=Freq), color = "white") +
  geom_text(aes(label=sprintf("%1.0f", Freq)), vjust=1) +
  scale_fill_gradient(low="steelblue", high="red") +
  theme_bw() + theme(legend.position="none") +
  xlab("Predicted") + ylab("Actual") + ggtitle("Predicted versus Actual - KNN")
dev.off()

