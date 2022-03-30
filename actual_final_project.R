library (dplyr)
library (tidyverse)
library (car)
library(pROC)
library(broom)
library(ggpubr)
library(MASS)

# Load data and assign value labels for categorical variables 
setwd("~/Downloads/RTraining/RWork/BIOSTATS_203A")
heart_data <- read.csv("heart.csv", sep = ",", header = T, stringsAsFactors = F)
heart_data$sex <- factor (heart_data$sex, levels = c(0,1), labels = c("Female", "Male"))

heart_data$target <- factor (heart_data$target, levels = c(0,1), labels = c("No Heart Disease", "Heart Disease"))

heart_data$exang <- factor (heart_data$exang, levels = c(0,1), 
                                      labels = c("No", "Yes"))

heart_data$thal <- factor (heart_data$thal, levels = c(0,1,2), 
                                               labels = c("Normal", "Fixed Defect", "Reversible Defect"))
heart_data$fbs <- factor (heart_data$fbs, levels = c(0,1), labels = c("False", "True"))
heart_data

# Drop Thal because inconsistent coding leads to missing values
dplyr::select(heart_data,-c(thal))
table_1 <- heart_data %>% group_by(target) %>%summarize (n_patients= n(), m_age = mean (age), 
  sd_age = sd(age), m_restbp = mean(trestbps), 
  sd_restbp = sd(trestbps), m_max_heart_rate = mean(thalach), sd_max_heart_rate = sd(thalach), 
  m_cholesterol = mean(chol), sd_cholesterol = sd(chol))
t(table_1)
categorical <- heart_data %>% dplyr::select(sex,  exang,  fbs, target) 

# Figure 1
one<-ggplot (categorical, aes(x = sex, fill = target)) + ylab ("Patient Count") + geom_bar()
two<-ggplot (categorical, aes(x = exang, fill = target)) + xlab("exercised induced angina") +
                ylab("Patient Count") + geom_bar()
three<-ggplot (categorical, aes(x = fbs, fill = target)) + xlab("fasting blood sugar > 120 mg/dl") +
                ylab("Patient Count") + geom_bar ()
figure1 <- ggarrange(one, two, three, labels = c("a)", "b)", "c)"), ncol = 1, nrow = 3)
figure1

# Table 1
as.data.frame(table_1)
table_1$m_age <- format (table_1$m_age, digits = 2, nsmall = 2)
table_1$sd_age <- format (table_1$sd_age, digits = 2, nsmall = 2)
table_1$m_restbp<- format (table_1$m_restbp, digits = 2, nsmall = 2)
table_1$sd_restbp <- format (table_1$sd_restbp, digits = 2, nsmall = 2)
table_1$m_max_heart_rate <- format (table_1$m_max_heart_rate, digits = 2, nsmall = 2)
table_1$sd_max_heart_rate <- format (table_1$sd_max_heart_rate, digits = 2, nsmall = 2)
table_1$m_cholesterol <- format (table_1$m_cholesterol, digits = 2, nsmall = 2)
table_1$sd_cholesterol <- format (table_1$sd_cholesterol, digits = 2, nsmall = 2)
table_1 = t(table_1)
rownames (table_1)[1] <- "Statistic"
colnames(table_1) = c("","")
write.csv(table_1, file = "table1.csv", quote = FALSE)

# Change Column Names
colnames (heart_data)[8] <- "max_heart_rate" 
colnames (heart_data)[9] <- "exercise_angina"
colnames (heart_data)[4] <- "resting_bp"
colnames (heart_data)[5] <- "cholesterol"

# Final Dataset
heart_data <-heart_data %>% dplyr::select(age, sex, resting_bp, cholesterol, fbs, 
                                        max_heart_rate, exercise_angina, target)
write.csv(heart_data, file = "final_dataset.csv", quote = FALSE)

# Split Dataset Into 80% Training 20% Testing
set.seed(123)
train_index <- sample (1:820)
train <- heart_data[train_index,]
test <- heart_data [-train_index,]

# Create binary logistic model
regression <-glm(target ~age+resting_bp+max_heart_rate+cholesterol+sex+exercise_angina+fbs,
                 data = train, family = "binomial")
# Improve model
regression1 <-stepAIC(regression)
summary (regression1)
results_df <- summary.glm(regression1)$coefficients
write.csv(results_df ,file = "table2.csv", quote = FALSE)

# Assumptions
vif (regression1)
regression1.data <- augment(regression1) %>% mutate(index = 1:n())
ggplot (regression1.data, aes (index, .std.resid)) + geom_point (aes(color = target),
         alpha = .5) + theme_classic()

# Apply predictions to test
test$predicted_value <- predict (regression1,test,type = "response")

# Plot distribution of predicted values
ggplot(test, aes(x = predicted_value)) + geom_histogram(fill = "#F8766D", binwidth = 0.10, boundary = 0) + 
   labs(x = " Predicted Probability of Heart Disease", y = "Patient Count") 
# Set Prediction Threshold to 0.5
test$predict <- ifelse (test$predicted_value > 0.5, "pos", "neg")

# Confusion matrix and associated metrics
confusion_matrix <- table (test$target, test$predict )
colnames(confusion_matrix) <- c ("Predicted No", "Predicted Yes")
write.csv(confusion_matrix, file = "Confusion Matrix.csv", quote = FALSE)
confusion_matrix
sensitivity <- confusion_matrix[2,2]/sum(confusion_matrix[2,])
sensitivity
specificity <- confusion_matrix[1,1]/sum(confusion_matrix[1,])
specificity
accuracy <- sum(diag(confusion_matrix))/sum(confusion_matrix)
accuracy

# ROC Curve
roc(target~predicted_value, data = test, plot = TRUE, col = "Red", print.auc = TRUE)