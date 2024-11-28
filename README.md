# NHANES_2023
NHANES 2023 Analysis of Depression using predictive factors 
NHANES Depression Analysis Project
This project involves analyzing the National Health and Nutrition Examination Survey (NHANES) data to explore predictors of depression, including smoking status, physical activity, alcohol consumption, and socio-demographic factors. The analysis includes data cleaning, visualization, hypothesis testing, and logistic regression modeling.

Table of Contents
Overview
Dataset Preparation
Analysis Workflow
Visualization
Statistical Tests
Modeling
Requirements
Usage
Contributing
License
Overview
This project aims to:

Clean and preprocess NHANES data.
Analyze depression's association with lifestyle and socio-demographic factors.
Develop predictive logistic regression models.
Key methods include:

Data cleaning and categorization.
Chi-square, Fisher's exact, and Z-tests for statistical relationships.
Logistic regression modeling with interaction terms and AIC comparisons.
Dataset Preparation
Files Used
The project uses .XPT files from NHANES datasets, including:

Demographics
Physical activity
Smoking and alcohol consumption
Depression screening
Data Cleaning
The raw NHANES datasets are combined and processed to create meaningful categories:

Age Group: Categorized into age ranges.
Depression: Binary indicator for depression based on responses.
Smoking Behavior: Classified into categories such as "Dual User" or "Non-Smoker."
Physical Activity: Levels categorized based on weekly moderate-intensity activity.
Income: Grouped into low, moderate, and high-income categories.
The processed dataset is saved as merged_NHANES_data.csv.

Analysis Workflow
Statistical Analysis:
Perform chi-square, Fisher's exact, and Z-tests.
Evaluate differences in depression prevalence across groups.
Regression Models:
Develop crude models for each predictor.
Test interaction effects.
Build a combined and simplified final model.
Visualization
Key plots include:

Distribution of smoking, physical activity, and income categories by age group and gender.
Bar plots for depression prevalence across categories.
Faceted plots for multi-dimensional comparisons.
Statistical Tests
Tests Conducted:
Chi-square Tests: Evaluate relationships between categorical variables.
Fisher’s Exact Test: Analyze small sample categorical relationships.
Z-tests for Proportions: Compare proportions across groups.
Modeling
Models Developed:
Crude Models: Assess the effect of single predictors.
Interaction Models: Explore interactions between predictors.
Simplified Models: Combine significant predictors into a streamlined model.
AIC and Variance Inflation Factor (VIF) calculations guide model selection.
Requirements
Libraries
The project uses the following R packages:

haven, readr: Data import.
dplyr, tidyr: Data wrangling.
ggplot2: Visualization.
car: Variance Inflation Factor (VIF) calculations.
survey, MASS, broom: Statistical modeling.
Install required libraries:

R
Copy code
install.packages(c("haven", "dplyr", "ggplot2", "car", "survey", "broom", "MASS", "pwr"))
Usage
Running the Code
Clone the repository:
bash
Copy code
git clone https://github.com/your-repo/NHANES-Depression-Analysis.git
Run the R scripts in sequence:
data_preparation.R: Prepares the dataset.
analysis.R: Performs statistical tests and regression modeling.
visualization.R: Creates visualizations.
Contributing
Contributions are welcome! Submit a pull request or create an issue for any enhancements or bug fixes.

License
This project is licensed under the MIT License. See the LICENSE file for details.

