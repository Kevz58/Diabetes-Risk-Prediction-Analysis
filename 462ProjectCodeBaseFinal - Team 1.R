#preamble
cat("\014")  # Clear Console
rm(list = ls(all.names = TRUE))# clear all
gc()
set.seed(33) # Set a seed to ensure repeatable random samples

library(caret)
library(rpart)
library(rpart.plot)
library(reshape2)
library(ggplot2)   
require(data.table)
require(pastecs)
require(PerformanceAnalytics)
require(corrplot)
require(tidyverse)
require(stargazer)
library(glmnet)
require(dplyr)
require(broom)
require(randomForest)


dt = fread("D:\\diabetesdataset\\diabetes_012_health_indicators_BRFSS2021.csv")


#=========correlation 
Diabetes_numeric <- dt %>%
  mutate_all(as.numeric)

# Create correlation matrix for all variables (converted to numeric)
cor_matrix_all <- cor(Diabetes_numeric, use = "complete.obs")

print("Correlation matrix for all variables (converted to numeric):")
print(round(cor_matrix_all, 3))

# Visualize full correlation matrix
corrplot(cor_matrix_all, 
         method = "color", 
         type = "upper", 
         order = "hclust",
         tl.cex = 0.6, 
         tl.col = "black",
)

cor_dt <- as.data.frame(cor_matrix_all) %>%
  mutate(Var1 = rownames(cor_matrix_all)) %>%
  pivot_longer(-Var1, names_to = "Var2", values_to = "Correlation") %>%
  filter(Var1 != Var2) %>%
  arrange(desc(abs(Correlation)))


#============data cleaning============
dt_clean <- dt %>%
  mutate(
    Age = factor(Age),      # Treat as categorical
    Income = factor(Income), # Treat as categorical  
    Education = factor(Education)  # Treat as categorical
  ) %>%
  filter(!is.na(BMI))  # Remove missing BMI rows

dt_clean <- dt %>%
  mutate(
    # Binary (0/1) variables -> factor
    HighBP = factor(HighBP),
    HighChol = factor(HighChol),
    CholCheck = factor(CholCheck),
    Smoker = factor(Smoker),
    Stroke = factor(Stroke),
    HeartDiseaseorAttack = factor(HeartDiseaseorAttack),
    PhysActivity = factor(PhysActivity),
    Fruits = factor(Fruits),
    Veggies = factor(Veggies),
    HvyAlcoholConsump = factor(HvyAlcoholConsump),
    AnyHealthcare = factor(AnyHealthcare),
    NoDocbcCost = factor(NoDocbcCost),
    DiffWalk = factor(DiffWalk),
    Sex = factor(Sex),
    
    # Ordinal variables -> ordered factor
    GenHlth = factor(GenHlth),
    Education = factor(Education),
    
    # Nominal variables -> unordered factor
    Age = factor(Age),
    Income = factor(Income),
    
    # Optional: Convert target if doing classification
    Diabetes_012 = factor(Diabetes_012)
  )

# Check the structure
#str(dt_clean)

healthy_diabetic <- dt_clean |>
  filter(Diabetes_012 %in% c(0,2)) |>
  mutate(Diabetes_012 = factor(ifelse(Diabetes_012 == 2, 1, 0)))

healthy_prediabetic = dt_clean |>
  filter(Diabetes_012 %in% c(0,1))


#==========Class imbalance chart========================
#Plot bar chart with count labels
ggplot(dt, aes(x = factor(Diabetes_012))) +
  geom_bar(fill = "skyblue") +
  geom_text(stat = "count", aes(label = ..count..), vjust = -0.5, size = 5) +
  scale_x_discrete(labels = c("0" = "Healthy", "1" = "Pre-diabetic", "2" = "Diabetic")) +
  labs(
    title = "dt Status Count",
    x = "dt Category",
    y = "Count"
  ) +
  theme_minimal()



#=======Data undersampling
# Balance using undersampling (real data only)
balance_data = function(data) {
  minority_count = sum(data$Diabetes_012 == 1)
  majority_indices = which(data$Diabetes_012 == 0)
  sampled_majority = sample(majority_indices, minority_count)
  all_minority = which(data$Diabetes_012 == 1)
  balanced_indices = c(sampled_majority, all_minority)
  return(data[balanced_indices, ])
}

healthy_diabetic_balanced = balance_data(healthy_diabetic)
healthy_prediabetic_balanced = balance_data(healthy_prediabetic)
healthy_prediabetic_balanced$Diabetes_012 <- factor(healthy_prediabetic_balanced$Diabetes_012, levels = c(0, 1))

#Check balance
print("Healthy vs Diabetic balance:")
table(healthy_diabetic_balanced$Diabetes_012)
print("Healthy vs Prediabetic balance:")
table(healthy_prediabetic_balanced$Diabetes_012)


#======Kitchensink balanced datasets - GLM models
model1 = glm(Diabetes_012 ~ ., data = healthy_diabetic_balanced, family = binomial)
model2 = glm(Diabetes_012 ~ ., data = healthy_prediabetic_balanced, family = binomial)
summary(model1)
summary(model2)

stargazer(model1, model2, type = "text")


#===Hypothesis 1
#====================================================================
#Hypothesis 1: Lifestyle Risk Factors GLM models

#==healthy, prediabetic
modelhypothesis_1pd = glm(Diabetes_012 ~Smoker + HvyAlcoholConsump + PhysActivity + Fruits + Veggies, data = healthy_prediabetic_balanced, family = binomial)
summary(modelhypothesis_1pd)

#interaction between smoking and phys activity
modelhypothesis_1pdinteraction = glm(Diabetes_012 ~Smoker + HvyAlcoholConsump + PhysActivity + Fruits + Veggies + Smoker*PhysActivity, data = healthy_prediabetic_balanced, family = binomial)
summary(modelhypothesis_1pd)

# Prediabetic model with alcohol × physical activity
modelhypothesis_1pd_alc_phys = glm(Diabetes_012 ~ Smoker + HvyAlcoholConsump + PhysActivity + Fruits + Veggies + HvyAlcoholConsump*PhysActivity, data = healthy_prediabetic_balanced, family = binomial)
summary(modelhypothesis_1pd_alc_phys)

# Prediabetic model with fruits × vegetables
modelhypothesis_1pd_fruit_veg = glm(Diabetes_012 ~ Smoker + HvyAlcoholConsump + PhysActivity + Fruits + Veggies + Fruits*Veggies, data = healthy_prediabetic_balanced, family = binomial)
summary(modelhypothesis_1pd_fruit_veg)

# Prediabetic model with alcohol × smoking
modelhypothesis_1pd_alc_smoke = glm(Diabetes_012 ~ Smoker + HvyAlcoholConsump + PhysActivity + Fruits + Veggies + HvyAlcoholConsump*Smoker, data = healthy_prediabetic_balanced, family = binomial)
summary(modelhypothesis_1pd_alc_smoke)

#===healthy, diabetic
modelhypothesis_1d = glm(Diabetes_012 ~Smoker + HvyAlcoholConsump + PhysActivity + Fruits + Veggies, data = healthy_diabetic_balanced, family = binomial)
summary(modelhypothesis_1d)

#interaction between smoking and phys activity
modelhypothesis_1dinteraction = glm(Diabetes_012 ~Smoker + HvyAlcoholConsump + PhysActivity + Fruits + Veggies + Smoker*PhysActivity, data = healthy_diabetic_balanced, family = binomial)
summary(modelhypothesis_1dinteraction )

# Prediabetic model with alcohol × physical activity
modelhypothesis_1d_alc_phys = glm(Diabetes_012 ~ Smoker + HvyAlcoholConsump + PhysActivity + Fruits + Veggies + HvyAlcoholConsump*PhysActivity, data = healthy_diabetic_balanced, family = binomial)
summary(modelhypothesis_1d_alc_phys)

# Prediabetic model with fruits × vegetables
modelhypothesis_1d_fruit_veg = glm(Diabetes_012 ~ Smoker + HvyAlcoholConsump + PhysActivity + Fruits + Veggies + Fruits*Veggies, data = healthy_diabetic_balanced, family = binomial)
summary(modelhypothesis_1d_fruit_veg)

# Prediabetic model with alcohol × smoking
modelhypothesis_1d_alc_smoke = glm(Diabetes_012 ~ Smoker + HvyAlcoholConsump + PhysActivity + Fruits + Veggies + HvyAlcoholConsump*Smoker, data = healthy_diabetic_balanced, family = binomial)
summary(modelhypothesis_1d_alc_smoke)

#=========recall hyp 1
# Simple Recall Test Function
simple_recall <- function(model, data) {
  # Get predictions
  pred_prob <- predict(model, data, type = "response")
  pred_class <- ifelse(pred_prob > 0.5, 1, 0)
  
  # Create confusion matrix
  cm <- table(Predicted = pred_class, Actual = data$Diabetes_012)
  
  # Calculate recall
  recall <- cm[2,2] / (cm[1,2] + cm[2,2])
  accuracy <- (cm[1,1] + cm[2,2]) / sum(cm)
  
  cat("Confusion Matrix:\n")
  print(cm)
  cat(sprintf("\nRecall: %.4f\n", recall))
  cat(sprintf("Accuracy: %.4f\n", accuracy))
  
  return(list(recall = recall, accuracy = accuracy, confusion_matrix = cm))
}

# Test Model 1: Prediabetic
cat("=== MODEL 1: PREDIABETIC ===\n")
result1 <- simple_recall(modelhypothesis_1pd, healthy_prediabetic_balanced)

# Test Model 2: Diabetic with interaction
cat("\n=== MODEL 2: DIABETIC ===\n")
result2 <- simple_recall(modelhypothesis_1d_alc_phys, healthy_diabetic_balanced)

#CART 
# Function to perform CART analysis with cross-validation and detailed metrics
perform_cart_analysis_lifestyle = function(data, dataset_name) {
  
  data_copy = copy(data)
  data_copy$Diabetes_012 = as.factor(ifelse(data_copy$Diabetes_012 == 1, "Yes", "No"))
  
  lifestyle_vars = c("Smoker", "HvyAlcoholConsump", "PhysActivity", "Fruits", "Veggies", "Diabetes_012")
  data_subset = data_copy[, lifestyle_vars, with = FALSE]
  
  control = trainControl(method = "cv", number = 10, classProbs = TRUE, savePredictions = "final")
  results = data.frame()
  
  print(paste("=== CART Analysis for", dataset_name, "==="))
  print(paste("Dataset size:", nrow(data_subset)))
  print("Class distribution:")
  print(table(data_subset$Diabetes_012))
  
  for (maxdepth in 1:7) {
    set.seed(33)
    
    fit <- train(Diabetes_012 ~ ., 
                 data = data_subset, 
                 method = "rpart",
                 trControl = control, 
                 tuneGrid = expand.grid(cp = 0.01),
                 control = rpart.control(maxdepth = maxdepth, cp = 0.01))
    
    cv_accuracy <- max(fit$results$Accuracy)
    cv_kappa <- max(fit$results$Kappa)
    
    cv_predictions <- fit$pred
    cv_predictions$pred <- factor(cv_predictions$pred, levels = c("No", "Yes"))
    cv_predictions$obs <- factor(cv_predictions$obs, levels = c("No", "Yes"))
    
    cm <- confusionMatrix(cv_predictions$pred, cv_predictions$obs, positive = "Yes")
    
    precision <- as.numeric(cm$byClass['Pos Pred Value'])
    recall <- as.numeric(cm$byClass['Sensitivity'])
    f1_score <- 2 * ((precision * recall) / (precision + recall))
    
    if(is.na(f1_score)) f1_score <- 0
    if(is.na(precision)) precision <- 0
    if(is.na(recall)) recall <- 0
    
    results <- rbind(results, 
                     data.frame(maxdepth = maxdepth, 
                                CV_Accuracy = cv_accuracy,
                                CV_Kappa = cv_kappa,
                                Precision = precision,
                                Recall = recall,
                                F1_Score = f1_score))
  }
  
  print("Max-Depth vs. Cross-Validation Performance:")
  print(results)
  
  data_long <- reshape2::melt(results, 
                              id.vars = "maxdepth", 
                              variable.name = "Metric", 
                              value.name = "Value")
  
  p <- ggplot(data_long, aes(x = maxdepth, y = Value, color = Metric)) +
    geom_line() + geom_point() +
    scale_color_manual(values = c("CV_Accuracy" = "blue", "CV_Kappa" = "orange", 
                                  "Precision" = "green", "Recall" = "red", "F1_Score" = "purple")) +
    ggtitle(paste("Cross-Validation Performance by Max Depth -", dataset_name)) +
    xlab("Max Depth") +
    ylab("Score") +
    theme_minimal() +
    theme(legend.position = "bottom")
  
  print(p)
  
  return(results)
}

# Function to find best performing model and generate CART trees (INCLUDES BEST RECALL)
find_best_model_lifestyle = function(results, data, dataset_name) {
  
  data_copy = copy(data)
  data_copy$Diabetes_012 = as.factor(ifelse(data_copy$Diabetes_012 == 1, "Yes", "No"))
  
  lifestyle_vars = c("Smoker", "HvyAlcoholConsump", "PhysActivity", "Fruits", "Veggies", "Diabetes_012")
  data_subset = data_copy[, lifestyle_vars, with = FALSE]
  
  best_maxdepth_f1 <- results$maxdepth[which.max(results$F1_Score)]
  best_maxdepth_accuracy <- results$maxdepth[which.max(results$CV_Accuracy)]
  best_maxdepth_recall <- results$maxdepth[which.max(results$Recall)]
  
  print(paste("Best maxdepth for F1_Score:", best_maxdepth_f1, 
              "with F1:", round(max(results$F1_Score), 4)))
  print(paste("Best maxdepth for CV_Accuracy:", best_maxdepth_accuracy, 
              "with accuracy:", round(max(results$CV_Accuracy), 4)))
  print(paste("Best maxdepth for Recall:", best_maxdepth_recall, 
              "with Recall:", round(max(results$Recall), 4)))
  
  set.seed(33)
  control = trainControl(method = "none")
  
  # Final model trained on F1-selected depth (main tree)
  final_model_f1 <- train(Diabetes_012 ~ ., 
                          data = data_subset, 
                          method = "rpart",
                          trControl = control, 
                          tuneGrid = expand.grid(cp = 0.01),
                          control = rpart.control(maxdepth = best_maxdepth_f1, cp = 0.01))
  
  # Optional: also build tree using best Recall depth (for visualization)
  final_model_recall <- train(Diabetes_012 ~ ., 
                              data = data_subset, 
                              method = "rpart",
                              trControl = control, 
                              tuneGrid = expand.grid(cp = 0.01),
                              control = rpart.control(maxdepth = best_maxdepth_recall, cp = 0.01))
  
  # Plot tree from best F1 depth
  rpart.plot(final_model_f1$finalModel, 
             main = paste("CART Tree (F1) -", dataset_name, "(Depth =", best_maxdepth_f1, ")"),
             extra = 104, fallen.leaves = TRUE, cex = 0.8, branch = 0.3)
  
  # Plot tree from best Recall depth
  rpart.plot(final_model_recall$finalModel, 
             main = paste("CART Tree (Recall) -", dataset_name, "(Depth =", best_maxdepth_recall, ")"),
             extra = 104, fallen.leaves = TRUE, cex = 0.8, branch = 0.3)
  
  # Print variable importance (from F1 model)
  if(length(final_model_f1$finalModel$variable.importance) > 0) {
    print(paste("=== Variable Importance for", dataset_name, "(F1 model) ==="))
    importance_df <- data.frame(
      Variable = names(final_model_f1$finalModel$variable.importance),
      Importance = round(final_model_f1$finalModel$variable.importance, 2)
    )
    importance_df <- importance_df[order(importance_df$Importance, decreasing = TRUE), ]
    print(importance_df)
  }
  
  return(list(best_f1_depth = best_maxdepth_f1,
              best_accuracy_depth = best_maxdepth_accuracy,
              best_recall_depth = best_maxdepth_recall,
              final_model_f1 = final_model_f1,
              final_model_recall = final_model_recall,
              results = results))
}

# Updated function calls using the corrected functions:
print("Starting CART Analysis for Hypothesis 1: Lifestyle Risk Factors (FIXED)")

# Analysis for Healthy vs Prediabetic (using balanced dataset)
cart_results_prediabetic = perform_cart_analysis_lifestyle(healthy_prediabetic_balanced, "Healthy vs Prediabetic")
best_model_prediabetic = find_best_model_lifestyle(cart_results_prediabetic, healthy_prediabetic_balanced, "Healthy vs Prediabetic")

# Analysis for Healthy vs Diabetic (using balanced dataset)
cart_results_diabetic = perform_cart_analysis_lifestyle(healthy_diabetic_balanced, "Healthy vs Diabetic")
best_model_diabetic = find_best_model_lifestyle(cart_results_diabetic, healthy_diabetic_balanced, "Healthy vs Diabetic")

print("CART Analysis for Lifestyle Factors Complete!")


#Hypothesis 2: Socioeconomic Factors (LOGIT)=======================================================================================
#====================================================================
#healthy/prediabetic
modelhypothesis_2pd = glm(Diabetes_012 ~Income + Education + Age + Sex, data = healthy_prediabetic_balanced, family = binomial)
summary(modelhypothesis_2pd)

# Healthy, Prediabetic with Age * Sex Interaction
modelhypothesis_2pdinteraction1 = glm(Diabetes_012 ~Income + Education + Age + Sex + Age*Sex, data = healthy_prediabetic_balanced, family = binomial)
summary(modelhypothesis_2pdinteraction1) #significant interaction


#====Healthy/Diabetic====#
#healthy/iabetic Model
modelhypothesis_2d = glm(Diabetes_012 ~Income + Education + Age + Sex, data = healthy_diabetic_balanced, family = binomial)
summary(modelhypothesis_2d)

#Healthy/Diabetic with Age * Sex interaction Model
modelhypothesis_2dinteraction1 = glm(Diabetes_012 ~Income + Education + Age + Sex + Age*Sex, data = healthy_diabetic_balanced, family = binomial)
summary(modelhypothesis_2dinteraction1) #significant interaction

# Hypothesis 2 Heatlhy/Diabetic Recall
Hyp2ResultHD <- simple_recall(modelhypothesis_2dinteraction1,healthy_diabetic_balanced)

# Convert log odds to odds ratio
Hyp2HDOdds <- tidy(modelhypothesis_2dinteraction1, exponentiate = TRUE, conf.int = TRUE)
print(Hyp2HDOdds, n = 41, width = 120)

#====Compare====#

stargazer(modelhypothesis_2pd, modelhypothesis_2pdinteraction1, type = "text")
stargazer(modelhypothesis_2d, modelhypothesis_2dinteraction1, type = "text")

# Hypothesis 2 Heatlhy/Prediabetic Recall
Hyp2ResultHP <- simple_recall(modelhypothesis_2pdinteraction1, healthy_prediabetic_balanced)

# Convert log odds to odds ratio
Hyp2ResultHP <- tidy(modelhypothesis_2dinteraction1, exponentiate = TRUE, conf.int = TRUE)
print(Hyp2ResultHP, n = 41, width = 120)


#====Extra Plot====#
# Fixed version of the plotting code
data.frame(
  Dataset = rep(c("HP", "HD"), each = 2),
  Metric = rep(c("Recall", "Accuracy"), 2),
  Value = c(Hyp2ResultHP$recall, Hyp2ResultHP$accuracy, 
            Hyp2ResultHD$recall, Hyp2ResultHD$accuracy)
) %>%
  ggplot(aes(x = Metric, y = Value, fill = Dataset)) +
  geom_col(position = "dodge", width = 0.7) +
  geom_text(aes(label = round(Value, 3)), 
            position = position_dodge(width = 0.7), 
            vjust = -0.3, size = 3.5) +
  scale_fill_manual(values = c("HP" = "#2E86AB", "HD" = "#A23B72")) +
  theme_minimal() +
  labs(title = "Model Metrics Comparison: Healthy/Diabetic vs Healthy/Prediabetic",
       y = "Score",
       x = "Metrics") +
  theme(legend.position = "top")  # Added the missing = sign here

#==LASSO HYP 2
#=============lasso for hypothesis 2
# Healthy vs Prediabetic
x_pd <- model.matrix(Diabetes_012 ~ Income + Education + Age + Sex, 
                     data = healthy_prediabetic_balanced)[,-1]
y_pd <- healthy_prediabetic_balanced$Diabetes_012

cv_lasso_pd <- cv.glmnet(x_pd, y_pd, family = "binomial", alpha = 1, nfolds = 10)
plot(cv_lasso_pd, main = "LASSO CV - Healthy vs Prediabetic")
coef(cv_lasso_pd, s = "lambda.min")


# Healthy vs Diabetic  
x_d <- model.matrix(Diabetes_012 ~ Income + Education + Age + Sex, 
                    data = healthy_diabetic_balanced)[,-1]
y_d <- healthy_diabetic_balanced$Diabetes_012

cv_lasso_d <- cv.glmnet(x_d, y_d, family = "binomial", alpha = 1, nfolds = 10)
plot(cv_lasso_d, main = "LASSO CV - Healthy vs Diabetic")
coef(cv_lasso_d, s = "lambda.min")

#========= Recall for Lasso:
# Modified version for glmnet models
simple_recall_glmnet <- function(model, newx, actual_labels) {
  # Get predicted probabilities
  pred_prob <- predict(model, newx = newx, s = "lambda.min", type = "response")
  
  # Convert to class labels
  pred_class <- ifelse(pred_prob > 0.5, 1, 0)
  
  # Create confusion matrix
  cm <- table(Predicted = pred_class, Actual = actual_labels)
  
  # Calculate recall (handling edge case where denominator might be zero)
  recall <- ifelse(sum(cm[, "1"]) == 0, NA, cm["1", "1"] / sum(cm[, "1"]))
  accuracy <- sum(diag(cm)) / sum(cm)
  
  cat("Confusion Matrix:\n")
  print(cm)
  cat(sprintf("\nRecall: %.4f\n", recall))
  cat(sprintf("Accuracy: %.4f\n", accuracy))
  
  return(list(recall = recall, accuracy = accuracy, confusion_matrix = cm))
}

# Assuming you've already built your matrix x_pd and label y_pd
simple_recall_glmnet(cv_lasso_pd, newx = x_pd, actual_labels = y_pd)
simple_recall_glmnet(cv_lasso_d, newx = x_d, actual_labels = y_d)


#LASSO ODDS RATIO CONVERSION (TODO)----------------


#===== CART Analysis for Hypothesis 3: Health History Risk Factors (RECALL-PRIORITIZED)

perform_cart_analysis_health = function(data, dataset_name) {
  
  data_copy = copy(data)
  data_copy$Diabetes_012 = as.factor(ifelse(data_copy$Diabetes_012 == 1, "Yes", "No"))
  
  health_vars = c("HighBP", "HighChol", "BMI", "Stroke", "HeartDiseaseorAttack", "Diabetes_012")
  data_subset = data_copy[, health_vars, with = FALSE]
  
  control = trainControl(method = "cv", number = 10, classProbs = TRUE, savePredictions = "final")
  
  results = data.frame()
  
  print(paste("=== CART Analysis for", dataset_name, "==="))
  print(paste("Dataset size:", nrow(data_subset)))
  print("Class distribution:")
  print(table(data_subset$Diabetes_012))
  
  for (maxdepth in 1:7) {
    set.seed(33)
    
    fit <- train(Diabetes_012 ~ ., 
                 data = data_subset, 
                 method = "rpart",
                 trControl = control, 
                 tuneGrid = expand.grid(cp = 0.01),
                 control = rpart.control(maxdepth = maxdepth, cp = 0.01))
    
    cv_accuracy <- max(fit$results$Accuracy)
    cv_kappa <- max(fit$results$Kappa)
    
    cv_predictions <- fit$pred
    cv_predictions$pred <- factor(cv_predictions$pred, levels = c("No", "Yes"))
    cv_predictions$obs  <- factor(cv_predictions$obs,  levels = c("No", "Yes"))
    
    cm <- confusionMatrix(cv_predictions$pred, cv_predictions$obs, positive = "Yes")
    
    precision <- as.numeric(cm$byClass['Pos Pred Value'])
    recall <- as.numeric(cm$byClass['Sensitivity'])
    f1_score <- 2 * ((precision * recall) / (precision + recall))
    
    if(is.na(f1_score)) f1_score <- 0
    if(is.na(precision)) precision <- 0
    if(is.na(recall)) recall <- 0
    
    results <- rbind(results, 
                     data.frame(maxdepth = maxdepth, 
                                CV_Accuracy = cv_accuracy,
                                CV_Kappa = cv_kappa,
                                Precision = precision,
                                Recall = recall,
                                F1_Score = f1_score))
  }
  
  print("Max-Depth vs. Cross-Validation Performance:")
  print(results)
  
  data_long <- reshape2::melt(results, 
                              id.vars = "maxdepth", 
                              variable.name = "Metric", 
                              value.name = "Value")
  
  p <- ggplot(data_long, aes(x = maxdepth, y = Value, color = Metric)) +
    geom_line() + geom_point() +
    scale_color_manual(values = c("CV_Accuracy" = "blue", "CV_Kappa" = "orange", 
                                  "Precision" = "green", "Recall" = "red", "F1_Score" = "purple")) +
    ggtitle(paste("Cross-Validation Performance by Max Depth -", dataset_name)) +
    xlab("Max Depth") +
    ylab("Score") +
    theme_minimal() +
    theme(legend.position = "bottom")
  
  print(p)
  
  return(results)
}


# Function to find best performing model for health history
find_best_model_health = function(results, data, dataset_name) {
  
  data_copy = copy(data)
  data_copy$Diabetes_012 = as.factor(ifelse(data_copy$Diabetes_012 == 1, "Yes", "No"))
  
  health_vars = c("HighBP", "HighChol", "BMI", "Stroke", "HeartDiseaseorAttack", "Diabetes_012")
  data_subset = data_copy[, health_vars, with = FALSE]
  
  # Select best model by Recall instead of F1
  best_maxdepth_recall <- results$maxdepth[which.max(results$Recall)]
  best_maxdepth_accuracy <- results$maxdepth[which.max(results$CV_Accuracy)]
  
  print(paste("Best maxdepth for Recall:", best_maxdepth_recall, 
              "with Recall:", round(max(results$Recall), 4)))
  print(paste("Best maxdepth for Accuracy:", best_maxdepth_accuracy, 
              "with Accuracy:", round(max(results$CV_Accuracy), 4)))
  
  set.seed(33)
  control = trainControl(method = "none")
  
  final_model <- train(Diabetes_012 ~ ., 
                       data = data_subset, 
                       method = "rpart",
                       trControl = control, 
                       tuneGrid = expand.grid(cp = 0.01),
                       control = rpart.control(maxdepth = best_maxdepth_recall, cp = 0.01))
  
  rpart.plot(final_model$finalModel, 
             main = paste("CART Tree -", dataset_name, "(Depth =", best_maxdepth_recall, ", Recall =", round(max(results$Recall), 3), ")"),
             extra = 104, 
             fallen.leaves = TRUE, 
             cex = 0.8,
             branch = 0.3)
  
  if(length(final_model$finalModel$variable.importance) > 0) {
    print(paste("=== Variable Importance for", dataset_name, "==="))
    importance_df <- data.frame(
      Variable = names(final_model$finalModel$variable.importance),
      Importance = round(final_model$finalModel$variable.importance, 2)
    )
    importance_df <- importance_df[order(importance_df$Importance, decreasing = TRUE), ]
    print(importance_df)
  }
  
  return(list(best_recall_depth = best_maxdepth_recall,
              best_accuracy_depth = best_maxdepth_accuracy,
              final_model = final_model,
              results = results))
}


print("Starting CART Analysis for Hypothesis 3: Health History Risk Factors (FIXED)")

# Analysis for Healthy vs Prediabetic (using balanced dataset)
cart_results_health_prediabetic = perform_cart_analysis_health(healthy_prediabetic_balanced, "Healthy vs Prediabetic - Health History")
best_model_health_prediabetic = find_best_model_health(cart_results_health_prediabetic, healthy_prediabetic_balanced, "Healthy vs Prediabetic - Health History")

# Analysis for Healthy vs Diabetic (using balanced dataset)  
cart_results_health_diabetic = perform_cart_analysis_health(healthy_diabetic_balanced, "Healthy vs Diabetic - Health History")
best_model_health_diabetic = find_best_model_health(cart_results_health_diabetic, healthy_diabetic_balanced, "Healthy vs Diabetic - Health History")

print("CART Analysis for Health History Factors Complete!")

#=========RANDOM FOREST OVERALL MODEL
# Load required libraries
library(randomForest)
library(caret)

#=========RECALL-PRIORITIZED RANDOM FOREST MODELS=========

# Function to tune Random Forest for maximum recall (Fixed 500 trees)
tune_rf_for_recall <- function(data, dataset_name, ntree = 500) {
  
  set.seed(33)
  
  cat(paste("\n=== Tuning Random Forest for Maximum Recall:", dataset_name, "===\n"))
  cat(paste("Using fixed ntree =", ntree, "\n"))
  
  # Prepare parameter grid (only mtry now)
  mtry_options <- c(2, 3, 4, floor(sqrt(ncol(data)-1)), floor((ncol(data)-1)/3))
  mtry_options <- unique(mtry_options[mtry_options <= ncol(data)-1])
  
  best_recall <- 0
  best_params <- list()
  best_model <- NULL
  results_df <- data.frame()
  
  # Grid search for best recall (only mtry)
  for(mtry in mtry_options) {
    
    # Build model
    rf_model <- randomForest(
      Diabetes_012 ~ ., 
      data = data,
      ntree = ntree,
      mtry = mtry,
      importance = TRUE
    )
    
    # Calculate recall using OOB predictions
    oob_pred <- rf_model$predicted
    cm <- table(Predicted = oob_pred, Actual = data$Diabetes_012)
    
    if("1" %in% rownames(cm) && "1" %in% colnames(cm)) {
      recall <- cm["1", "1"] / sum(cm[, "1"])
      precision <- cm["1", "1"] / sum(cm["1", ])
      accuracy <- sum(diag(cm)) / sum(cm)
      f1_score <- 2 * ((precision * recall) / (precision + recall))
    } else {
      recall <- precision <- accuracy <- f1_score <- 0
    }
    
    # Store results
    results_df <- rbind(results_df, data.frame(
      ntree = ntree,
      mtry = mtry,
      recall = recall,
      precision = precision,
      accuracy = accuracy,
      f1_score = f1_score,
      oob_error = rf_model$err.rate[ntree, "OOB"]
    ))
    
    # Update best model if recall improved
    if(recall > best_recall) {
      best_recall <- recall
      best_params <- list(ntree = ntree, mtry = mtry)
      best_model <- rf_model
    }
    
    cat(sprintf("mtry=%d -> Recall=%.4f, Precision=%.4f, Accuracy=%.4f\n", 
                mtry, recall, precision, accuracy))
  }
  
  cat(sprintf("\n=== Best Parameters for %s ===\n", dataset_name))
  cat(sprintf("Best Recall: %.4f\n", best_recall))
  cat(sprintf("Fixed ntree: %d\n", best_params$ntree))
  cat(sprintf("Best mtry: %d\n", best_params$mtry))
  
  return(list(
    best_model = best_model,
    best_params = best_params,
    best_recall = best_recall,
    all_results = results_df
  ))
}

# Function to evaluate recall-optimized model with probability threshold tuning
evaluate_recall_model <- function(tuned_result, data, dataset_name) {
  
  set.seed(33)
  
  model <- tuned_result$best_model
  
  cat(paste("\n=== Evaluating Recall-Optimized Model:", dataset_name, "===\n"))
  
  # Get probability predictions
  prob_pred <- predict(model, data, type = "prob")[, "1"]
  
  # Test different probability thresholds to maximize recall
  thresholds <- seq(0.1, 0.9, 0.05)
  threshold_results <- data.frame()
  
  for(threshold in thresholds) {
    pred_class <- ifelse(prob_pred >= threshold, "1", "0")
    pred_class <- factor(pred_class, levels = c("0", "1"))
    actual <- factor(data$Diabetes_012, levels = c("0", "1"))
    
    cm <- table(Predicted = pred_class, Actual = actual)
    
    if("1" %in% rownames(cm) && "1" %in% colnames(cm)) {
      recall <- cm["1", "1"] / sum(cm[, "1"])
      precision <- cm["1", "1"] / sum(cm["1", ])
      accuracy <- sum(diag(cm)) / sum(cm)
      f1_score <- 2 * ((precision * recall) / (precision + recall))
    } else {
      recall <- precision <- accuracy <- f1_score <- 0
    }
    
    threshold_results <- rbind(threshold_results, data.frame(
      threshold = threshold,
      recall = recall,
      precision = precision,
      accuracy = accuracy,
      f1_score = f1_score
    ))
  }
  
  # Find threshold that maximizes recall
  best_threshold_idx <- which.max(threshold_results$recall)
  best_threshold <- threshold_results$threshold[best_threshold_idx]
  max_recall <- threshold_results$recall[best_threshold_idx]
  
  cat(sprintf("Best Threshold for Maximum Recall: %.2f\n", best_threshold))
  cat(sprintf("Maximum Achievable Recall: %.4f\n", max_recall))
  
  # Final predictions with best threshold
  final_pred <- ifelse(prob_pred >= best_threshold, "1", "0")
  final_pred <- factor(final_pred, levels = c("0", "1"))
  final_cm <- table(Predicted = final_pred, Actual = factor(data$Diabetes_012, levels = c("0", "1")))
  
  cat("\n=== Final Confusion Matrix (Recall-Optimized) ===\n")
  print(final_cm)
  
  # Plot threshold analysis
  library(ggplot2)
  threshold_long <- reshape2::melt(threshold_results, id.vars = "threshold")
  
  p1 <- ggplot(threshold_long, aes(x = threshold, y = value, color = variable)) +
    geom_line(size = 1) + geom_point() +
    geom_vline(xintercept = best_threshold, linetype = "dashed", color = "red") +
    labs(title = paste("Threshold Analysis -", dataset_name),
         subtitle = paste("Best Threshold for Recall:", best_threshold),
         x = "Probability Threshold", y = "Score") +
    theme_minimal() +
    scale_color_manual(values = c("recall" = "red", "precision" = "blue", 
                                  "accuracy" = "green", "f1_score" = "purple"))
  print(p1)
  
  # Variable importance plot
  varImpPlot(model, main = paste("Variable Importance (Recall-Optimized) -", dataset_name))
  
  # Simplified Random Forest summary
  cat(sprintf("\nRandom Forest Summary for %s:\n", dataset_name))
  cat(sprintf("Total trees: %d\n", model$ntree))
  cat(sprintf("Variables tried at each split: %d\n", model$mtry))
  cat(sprintf("Total variables: %d\n", ncol(data) - 1))
  cat("Note: Individual Random Forest trees are typically very complex.\n")
  cat("For interpretable tree visualization, consider using CART/rpart instead.\n")
  cat("Random Forest strength comes from combining many complex trees.\n")
  
  return(list(
    model = model,
    best_threshold = best_threshold,
    max_recall = max_recall,
    final_confusion_matrix = final_cm,
    threshold_results = threshold_results,
    tuning_results = tuned_result$all_results
  ))
}

# Execute recall-prioritized analysis
cat("Starting Recall-Prioritized Random Forest Analysis...\n")

# 1. Tune for Healthy vs Prediabetic
tuned_prediabetic <- tune_rf_for_recall(
  healthy_prediabetic_balanced, 
  "Healthy vs Prediabetic"
)

# 2. Tune for Healthy vs Diabetic
tuned_diabetic <- tune_rf_for_recall(
  healthy_diabetic_balanced, 
  "Healthy vs Diabetic"
)

# 3. Evaluate with threshold optimization
eval_prediabetic <- evaluate_recall_model(
  tuned_prediabetic, 
  healthy_prediabetic_balanced, 
  "Healthy vs Prediabetic"
)

eval_diabetic <- evaluate_recall_model(
  tuned_diabetic, 
  healthy_diabetic_balanced, 
  "Healthy vs Diabetic"
)

# Final comparison focusing on recall
cat("\n", paste(rep("=", 60), collapse=""), "\n")
cat("RECALL-PRIORITIZED MODEL COMPARISON\n")
cat(paste(rep("=", 60), collapse=""), "\n")

recall_comparison <- data.frame(
  Dataset = c("Healthy vs Prediabetic", "Healthy vs Diabetic"),
  Optimized_Recall = c(eval_prediabetic$max_recall, eval_diabetic$max_recall),
  Best_Threshold = c(eval_prediabetic$best_threshold, eval_diabetic$best_threshold),
  Best_ntree = c(tuned_prediabetic$best_params$ntree, tuned_diabetic$best_params$ntree),
  Best_mtry = c(tuned_prediabetic$best_params$mtry, tuned_diabetic$best_params$mtry)
)

print(recall_comparison)

cat("\nRecall-Prioritized Random Forest Analysis Complete!\n")
