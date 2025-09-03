# Diabetes Risk Prediction Analysis

A comprehensive machine learning analysis of diabetes risk factors using the Behavioral Risk Factor Surveillance System (BRFSS) 2021 dataset. This project implements multiple predictive modeling approaches to identify key risk factors for diabetes and pre-diabetes.

## Table of Contents
- [Project Overview](#project-overview)
- [Key Features](#key-features)
- [Dataset Information](#dataset-information)
- [Methodology](#methodology)
- [Key Results](#key-results)
- [Academic Context](#academic-context)
- [How to Run](#how-to-run)
- [Prerequisites](#prerequisites)
- [Important Notes & Acknowledgements](#important-notes--acknowledgements)

---

## Project Overview

This study analyzes diabetes risk prediction by testing three main hypotheses:

1.  **Lifestyle Risk Factors:** Impact of smoking, alcohol consumption, physical activity, and diet.
2.  **Socioeconomic Factors:** Effects of income, education, age, and gender.
3.  **Health History Factors:** Influence of blood pressure, cholesterol, BMI, and cardiovascular conditions.

## Key Features

- **Multiple ML Approaches:** Logistic Regression, CART Decision Trees, LASSO Regression, Random Forest.
- **Recall-Optimized Models:** Prioritizes detection of positive diabetes cases for medical screening.
- **Class Imbalance Handling:** Implements undersampling techniques for balanced datasets.
- **Comprehensive Visualization:** Correlation matrices, decision trees, and performance metrics.
- **Rigorous Validation:** 10-fold cross-validation and hyperparameter optimization.

## Dataset Information

- **Source:** [BRFSS 2021 - Diabetes Health Indicators Dataset on Kaggle](https://www.kaggle.com/datasets/julnazz/diabetes-health-indicators-dataset)
- **Size:** 253,680 survey responses
- **Features:** 21 health-related variables
- **Target:** Diabetes status (0=Healthy, 1=Pre-diabetic, 2=Diabetic)

### Key Variables
- **Lifestyle:** Smoking, Alcohol Consumption, Physical Activity, Fruit & Vegetable Intake
- **Demographics:** Age, Income Level, Education Level, Sex
- **Health History:** High BP, High Cholesterol, BMI, Stroke, Heart Disease or Attack

## Methodology

### Data Preprocessing
- Factor conversion for categorical variables
- Missing value handling (BMI imputation)
- Class imbalance correction through undersampling
- Creation of binary classification datasets

### Model Development & Evaluation
- **Logistic Regression:** Baseline models with interaction terms.
- **CART Decision Trees:** Interpretable rule-based classification.
- **LASSO Regression:** Regularized feature selection.
- **Random Forest:** Ensemble learning with hyperparameter tuning.
- **Primary Metric:** **Recall (sensitivity)** - critical for medical screening.
- **Secondary Metrics:** Precision, Accuracy, F1-Score.
- **Validation:** 10-fold cross-validation.

## Key Results

### Hypotheses Validation:
1.  **Lifestyle factors** on their own were weak predictors of prediabetes and diabetes.
2.  **Socioeconomic factors** were decent predictors, achieving a recall of **0.72 for diabetes** and **0.67 for prediabetes**.
3.  **Health history factors** were the strongest predictors, achieving a recall of **0.73 for diabetes** and **0.74 for prediabetes**.

### Top Risk Factors Identified (in order of predictive power):
1.  **Health History:** High BP, High Cholesterol, BMI, Stroke, Heart Disease or Attack
2.  **Demographics:** Age, Income Level, Education Level, Sex
3.  **Lifestyle:** Smoking, Alcohol Consumption, Physical Activity, Fruit & Vegetable Intake

## Academic Context
- **Course:** BUS 462 Business Analytics
- **Institution:** Simon Fraser University
- **Semester:** Summer 2025
- **Professor:** Chaitanya "CK"​ Kaligotla

### Team Members:
- Muneer Akbani
- Derek Lin
- Kevin Zhou

## How to Run

1.  Ensure you have the [prerequisites](#prerequisites) installed.
2.  Clone this repository or download the `462ProjectCodeBaseFinal - Team 1.R` file
3.  Open the file in RStudio.
4.  Install any missing packages using the code in the Prerequisites section.
5.  Run the entire file using Ctrl + Alt + E (Windows/Linux) or Cmd + Option + E (Mac)
## Prerequisites

- **R** (>= 4.0.0)
- **RStudio** (recommended)

Install the required R packages by running the following in your R console:

```r
install.packages(c(
  "caret", "rpart", "rpart.plot", "reshape2", "ggplot2",
  "data.table", "pastecs", "PerformanceAnalytics", "corrplot",
  "tidyverse", "stargazer", "glmnet", "dplyr", "broom",
  "randomForest"
))
