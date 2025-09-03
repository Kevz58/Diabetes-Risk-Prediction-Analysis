# Diabetes-Risk-Prediction-Analysis

A comprehensive machine learning analysis of diabetes risk factors using the Behavioral Risk Factor Surveillance System (BRFSS) 2021 dataset. This project implements multiple predictive modeling approaches to identify key risk factors for diabetes and pre-diabetes.

**Project Overview**
This study analyzes diabetes risk prediction through three main hypotheses:

1. Lifestyle Risk Factors: Impact of smoking, alcohol consumption, physical activity, and diet
2. Socioeconomic Factors: Effects of income, education, age, and gender
3. Health History Factors: Influence of blood pressure, cholesterol, BMI, and cardiovascular conditions

**Key Features**

- Multiple ML Approaches: Logistic Regression, CART Decision Trees, LASSO Regression, Random Forest
- Recall-Optimized Models: Prioritizes detection of positive diabetes cases for medical screening
- Class Imbalance Handling: Implements undersampling techniques for balanced datasets
- Comprehensive Visualization: Correlation matrices, decision trees, performance metrics
- Rigorous Validation: 10-fold cross-validation and hyperparameter optimization

**Dataset Information**
- Source: BRFSS 2021 - Diabetes Health Indicators Dataset
- Size: 253,680 survey responses
- Features: 21 health-related variables
- Target: Diabetes status (0=Healthy, 1=Pre-diabetic, 2=Diabetic)
- 
**Key Variables**

**Demographic**: Age, Sex, Income, Education
**Lifestyle**: Smoking, Alcohol Consumption, Physical Activity, Diet
**Health Metric**s: BMI, Blood Pressure, Cholesterol
**Medical History**: Heart Disease, Stroke, Healthcare Access

**Methodology**

Data Preprocessing

- Factor conversion for categorical variables
- Missing value handling (BMI imputation)
- Class imbalance correction through undersampling
- Creation of binary classification datasets

**Model Development**
- Logistic Regression: Baseline models with interaction terms
- CART Decision Trees: Interpretable rule-based classification
- LASSO Regression: Regularized feature selection
- Random Forest: Ensemble learning with hyperparameter tuning

**Evaluation Metrics**

- Primary: Recall (sensitivity) - critical for medical screening
- Secondary: Precision, Accuracy, F1-Score
- Validation: 10-fold cross-validation

**Academic Context**
Course: BUS 462 Business Analytics
Institution: Simon Fraser University
Semester: Summer 2025
Team Members:

Muneer Akbani
Derek Lin
Kevin Zhou

**Important Notes**

Medical Disclaimer: This project is for educational purposes only and should not be used for actual medical diagnosis
Data Privacy: All data used is publicly available and de-identified
Reproducibility: Set seed values are used for consistent results

**Prerequisites**

- R (>= 4.0.0)
- RStudio (recommended)

Additionally, install the required R packages by running the following code in RStudio's Console:

```r
install.packages(c(
  "caret", "rpart", "rpart.plot", "reshape2", "ggplot2",
  "data.table", "pastecs", "PerformanceAnalytics", "corrplot",
  "tidyverse", "stargazer", "glmnet", "dplyr", "broom",
  "randomForest"
))

