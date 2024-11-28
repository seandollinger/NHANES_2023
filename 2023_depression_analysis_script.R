#Code to create the dataset  
# Load necessary libraries
library(haven)
library(dplyr)

# File paths for the xpt files
file_paths <- c("DEMO_L.XPT", "PAQ_L.XPT", "SMQRTU_L.XPT", "SMQ_L.XPT", "BMX_L.XPT", "ALQ_L.XPT", "DPQ_L.XPT")

# Read each xpt file into a list of data frames
dfs <- lapply(file_paths, read_xpt)

# Combine all data frames by row-binding them
merged_df <- bind_rows(dfs)

# Display the merged DataFrame
print(head(merged_df))  # Display the first few rows

# Optionally, save the merged DataFrame to a new file
write.csv(merged_df, "merged_NHANES_data.csv", row.names = FALSE)


#Code for ananlysis 


# Organized and Streamlined NHANES Depression Analysis with Z-Tests
install.packages("pwr")  # Install the pwr package
# Load necessary libraries
library(pwr)  
library(glmnet)
library(survey)
library(readr)
library(dplyr)
library(ggplot2)
library(car)
library(tidyr)
library(broom)
library(MASS)
 
# Load the dataset
file_path <- "Downloads/merged_disnewdata.csv"
if (file.exists(file_path)) {
  df <- read_csv(file_path)
} else {
  stop("Data file not found.")
}

# Data Cleaning and Preparation
df <- df %>%
  mutate(
    # Age and Gender
    AgeGroup = cut(RIDAGEYR, breaks = c(0, 20, 40, 60, 80, Inf), labels = c("0-20", "21-40", "41-60", "61-80", "81+"), right = FALSE),
    Gender = ifelse(RIAGENDR == 1, "Male", ifelse(RIAGENDR == 2, "Female", NA)),
    DepressedBinary = case_when(DPQ020 %in% 1:3 ~ 1, DPQ020 == 0 ~ 0, TRUE ~ NA_real_),
    SmokesNow = case_when(SMQ040 %in% 1:2 ~ 1, SMQ040 == 3 ~ 0, TRUE ~ NA_real_),
    E_CigUse = case_when(SMQ846 == 1 ~ 1, SMQ846 == 2 ~ 0, TRUE ~ NA_real_),
    SmokingCategory = case_when(
      SmokesNow == 1 & E_CigUse == 1 ~ "Dual User",
      SmokesNow == 1 & (is.na(E_CigUse) | E_CigUse == 0) ~ "Cigarette Only",
      E_CigUse == 1 & (is.na(SmokesNow) | SmokesNow == 0) ~ "E-Cigarette Only",
      TRUE ~ "Non-Smoker"
    ),
    SedentaryCategory = case_when(
      PAD680 >= 0 & PAD680 <= 300 ~ "Low Sedentary",
      PAD680 > 300 & PAD680 <= 600 ~ "Moderate Sedentary",
      PAD680 > 600 & PAD680 <= 900 ~ "High Sedentary",
      PAD680 > 900 ~ "Very High Sedentary",
      TRUE ~ NA_character_
    ),
    AlcoholFrequencyCategory = case_when(
      ALQ121 == 0 ~ "Non-Drinker",
      ALQ121 %in% 6:10 ~ "Moderate Drinker",
      ALQ121 %in% 1:5 ~ "Frequent Drinker",
      TRUE ~ NA_character_
    ),
    Income_Category = case_when(
      INDFMPIR < 2.0 ~ "Low Income",
      INDFMPIR >= 2.0 & INDFMPIR < 5.0 ~ "Moderate Income",
      INDFMPIR >= 5.0 ~ "High Income",
      is.na(INDFMPIR) ~ "Missing"
    )
  ) %>%
  mutate(
    PAD790U_clean = gsub("^b'|'$", "", as.character(PAD790U))
  ) %>%
  filter(!is.na(PAD790Q) & !is.na(PAD790U_clean) & !is.na(PAD800)) %>%
  mutate(
    WeeklyFrequency = case_when(
      PAD790U_clean == "D" ~ PAD790Q * 7,
      PAD790U_clean == "M" ~ PAD790Q / 4.33,
      PAD790U_clean == "Y" ~ PAD790Q / 52,
      PAD790U_clean == "W" ~ PAD790Q,
      TRUE ~ NA_real_
    ),
    TotalModerateLTPA = WeeklyFrequency * PAD800,
    PhysicalActivityCategory = case_when(
      is.na(TotalModerateLTPA) ~ "Missing",
      TotalModerateLTPA == 0 ~ "Inactive",
      TotalModerateLTPA > 0 & TotalModerateLTPA < 150 ~ "Insufficiently Active",
      TotalModerateLTPA >= 150 ~ "Sufficiently Active"
    )
  )

# Convert to factor
df <- df %>%
  mutate(
    PhysicalActivityCategory = factor(
      PhysicalActivityCategory,
      levels = c("Inactive", "Insufficiently Active", "Sufficiently Active", "Missing")
    )
  )

# Check Distribution of Categories
cat("\nDistribution of PhysicalActivityCategory:\n")
print(table(df_clean$PhysicalActivityCategory, useNA = "ifany"))

# Ensure the column exists in the dataset
if ("DepressedBinary" %in% colnames(df)) {
  # Count the occurrences of each value
  depressed_counts <- table(df$DepressedBinary, useNA = "ifany")
  
  # Print the counts
  print(depressed_counts)
  
  # Optionally, calculate proportions
  depressed_proportions <- prop.table(depressed_counts)
  print(depressed_proportions)
} else {
  cat("DepressedBinary column not found in the dataset.\n")
}



# Example parameters
sample_size <- 4444 # Total cases
num_predictors <- 7 # Number of predictors in the simplified model
alpha <- 0.05       # Significance level
f2 <- 0.15          # Medium effect size 

# Calculate power
power <- pwr.f2.test(u = num_predictors, v = sample_size - num_predictors - 1, f2 = f2, sig.level = alpha)
print(power)


# Function for z-test of proportions
z_test_proportions <- function(success1, success2, total1, total2) {
  p1 <- success1 / total1
  p2 <- success2 / total2
  p_combined <- (success1 + success2) / (total1 + total2)
  z_stat <- (p1 - p2) / sqrt(p_combined * (1 - p_combined) * (1 / total1 + 1 / total2))
  p_value <- 2 * pnorm(-abs(z_stat))
  return(p_value)
}
 
# Plots 

# Smoking Category by AgeGroup and Gender
# Smoking Category: Non-Smoker
non_smoker_counts <- df %>%
  filter(SmokingCategory == "Non-Smoker", !is.na(Gender), !is.na(AgeGroup)) %>%
  count(AgeGroup, Gender, name = "Count")

ggplot(non_smoker_counts, aes(x = AgeGroup, y = Count, fill = Gender)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(
    title = "Distribution of Non-Smokers by Age Group and Gender",
    x = "Age Group",
    y = "Count of Participants",
    fill = "Gender"
  ) +
  theme_minimal()

# Smoking Category: Cigarette Only
cigarette_only_counts <- df %>%
  filter(SmokingCategory == "Cigarette Only", !is.na(Gender), !is.na(AgeGroup)) %>%
  count(AgeGroup, Gender, name = "Count")

ggplot(cigarette_only_counts, aes(x = AgeGroup, y = Count, fill = Gender)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(
    title = "Distribution of Cigarette Only Smokers by Age Group and Gender",
    x = "Age Group",
    y = "Count of Participants",
    fill = "Gender"
  ) +
  theme_minimal()

# Smoking Category: E-Cigarette Only
ecig_only_counts <- df %>%
  filter(SmokingCategory == "E-Cigarette Only", !is.na(Gender), !is.na(AgeGroup)) %>%
  count(AgeGroup, Gender, name = "Count")

ggplot(ecig_only_counts, aes(x = AgeGroup, y = Count, fill = Gender)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(
    title = "Distribution of E-Cigarette Only Users by Age Group and Gender",
    x = "Age Group",
    y = "Count of Participants",
    fill = "Gender"
  ) +
  theme_minimal()

# Smoking Category: Dual User
dual_user_counts <- df %>%
  filter(SmokingCategory == "Dual User", !is.na(Gender), !is.na(AgeGroup)) %>%
  count(AgeGroup, Gender, name = "Count")

ggplot(dual_user_counts, aes(x = AgeGroup, y = Count, fill = Gender)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(
    title = "Distribution of Dual Users by Age Group and Gender",
    x = "Age Group",
    y = "Count of Participants",
    fill = "Gender"
  ) +
  theme_minimal()


# Physical Activity Category by AgeGroup and Gender
activity_age_gender_counts <- df %>%
  filter(!is.na(PhysicalActivityCategory), !is.na(Gender), !is.na(AgeGroup)) %>%
  count(PhysicalActivityCategory, AgeGroup, Gender, name = "Count") %>%
  group_by(PhysicalActivityCategory, AgeGroup) %>%
  mutate(Proportion = Count / sum(Count))

ggplot(activity_age_gender_counts, aes(x = AgeGroup, y = Count, fill = Gender)) +
  geom_bar(stat = "identity", position = "dodge") +
  facet_wrap(~PhysicalActivityCategory) +
  labs(
    title = "Distribution of Physical Activity Categories by Age Group and Gender",
    x = "Age Group",
    y = "Count of Participants",
    fill = "Gender"
  ) +
  theme_minimal()

# Sedentary Category by AgeGroup and Gender
sedentary_age_gender_counts <- df %>%
  filter(!is.na(SedentaryCategory), !is.na(Gender), !is.na(AgeGroup)) %>%
  count(SedentaryCategory, AgeGroup, Gender, name = "Count") %>%
  group_by(SedentaryCategory, AgeGroup) %>%
  mutate(Proportion = Count / sum(Count))

ggplot(sedentary_age_gender_counts, aes(x = AgeGroup, y = Count, fill = Gender)) +
  geom_bar(stat = "identity", position = "dodge") +
  facet_wrap(~SedentaryCategory) +
  labs(
    title = "Distribution of Sedentary Categories by Age Group and Gender",
    x = "Age Group",
    y = "Count of Participants",
    fill = "Gender"
  ) +
  theme_minimal()

# Alcohol Frequency Category by AgeGroup and Gender
alcohol_age_gender_counts <- df %>%
  filter(!is.na(AlcoholFrequencyCategory), !is.na(Gender), !is.na(AgeGroup)) %>%
  count(AlcoholFrequencyCategory, AgeGroup, Gender, name = "Count") %>%
  group_by(AlcoholFrequencyCategory, AgeGroup) %>%
  mutate(Proportion = Count / sum(Count))

ggplot(alcohol_age_gender_counts, aes(x = AgeGroup, y = Count, fill = Gender)) +
  geom_bar(stat = "identity", position = "dodge") +
  facet_wrap(~AlcoholFrequencyCategory) +
  labs(
    title = "Distribution of Alcohol Frequency Categories by Age Group and Gender",
    x = "Age Group",
    y = "Count of Participants",
    fill = "Gender"
  ) +
  theme_minimal()

# Income Category by AgeGroup and Gender
income_age_gender_counts <- df %>%
  filter(!is.na(Income_Category), !is.na(Gender), !is.na(AgeGroup)) %>%
  count(Income_Category, AgeGroup, Gender, name = "Count") %>%
  group_by(Income_Category, AgeGroup) %>%
  mutate(Proportion = Count / sum(Count))

ggplot(income_age_gender_counts, aes(x = AgeGroup, y = Count, fill = Gender)) +
  geom_bar(stat = "identity", position = "dodge") +
  facet_wrap(~Income_Category) +
  labs(
    title = "Distribution of Income Categories by Age Group and Gender",
    x = "Age Group",
    y = "Count of Participants",
    fill = "Gender"
  ) +
  theme_minimal()

# Chi-Square Tests
# 1. SmokingCategory and Gender
chisq_smoking_gender <- chisq.test(table(df$SmokingCategory, df$Gender))
print(chisq_smoking_gender)

# 2. AgeGroup and AlcoholFrequencyCategory
chisq_age_alcohol <- chisq.test(table(df$AgeGroup, df$AlcoholFrequencyCategory))
print(chisq_age_alcohol)

# 3. Income_Category and DepressedBinary
chisq_income_depressed <- chisq.test(table(df$Income_Category, df$DepressedBinary))
print(chisq_income_depressed)

# Fisher’s Exact Test
# 4. E-Cigarette Use and Gender
fisher_ecig_gender <- fisher.test(table(df$E_CigUse, df$Gender))
print(fisher_ecig_gender)

# Z-Tests for Proportions
# 5. Proportion of SmokesNow by Gender
prop_table_smokesnow <- table(df$Gender, df$SmokesNow)
z_test_smokesnow <- prop.test(
  x = c(prop_table_smokesnow["Male", 1], prop_table_smokesnow["Female", 1]),
  n = c(sum(prop_table_smokesnow["Male", ]), sum(prop_table_smokesnow["Female", ]))
)
print(z_test_smokesnow)

# 6. Proportion of DepressedBinary by Gender
prop_table_depressed <- table(df$Gender, df$DepressedBinary)
z_test_depressed <- prop.test(
  x = c(prop_table_depressed["Male", 1], prop_table_depressed["Female", 1]),
  n = c(sum(prop_table_depressed["Male", ]), sum(prop_table_depressed["Female", ]))
)
print(z_test_depressed) 

# Crude Models
crude_models <- list(
  Income = glm(DepressedBinary ~ Income_Category, data = df, family = binomial),
  Smoking = glm(DepressedBinary ~ SmokingCategory, data = df, family = binomial),
  PhysicalActivity = glm(DepressedBinary ~ PhysicalActivityCategory, data = df, family = binomial),
  Sedentary = glm(DepressedBinary ~ SedentaryCategory, data = df, family = binomial),
  Alcohol = glm(DepressedBinary ~ AlcoholFrequencyCategory, data = df, family = binomial)
)

# Interaction Models
interaction_models <- list(
  Smoking = glm(DepressedBinary ~ SmokingCategory * AgeGroup + Gender + Income_Category, data = df, family = binomial),
  PhysicalActivity = glm(DepressedBinary ~ PhysicalActivityCategory * AgeGroup + Gender + Income_Category, data = df, family = binomial),
  Sedentary = glm(DepressedBinary ~ SedentaryCategory * AgeGroup + Gender + Income_Category, data = df, family = binomial),
  Alcohol = glm(DepressedBinary ~ AlcoholFrequencyCategory * AgeGroup + Gender + Income_Category, data = df, family = binomial)
)

# Combined Model
combined_model <- glm(
  DepressedBinary ~ PhysicalActivityCategory + SmokingCategory + AlcoholFrequencyCategory + 
    AgeGroup + Gender + Income_Category, data = df, family = binomial
)

# Variance Inflation Factor (VIF) for Combined Model
vif_combined <- vif(combined_model)

# Model Summaries
model_summaries <- lapply(c(crude_models, interaction_models, list(Combined = combined_model)), summary)

# AIC Comparison
aic_values <- AIC(crude_models$Income, crude_models$Smoking, crude_models$PhysicalActivity,
                  crude_models$Sedentary, crude_models$Alcohol, combined_model)

# Print Results
cat("\nModel Summaries:\n")
print(model_summaries)

cat("\nVariance Inflation Factor (VIF):\n")
print(vif_combined)

cat("\nAIC Values for Model Comparison:\n")
print(aic_values) 


# More Interaction Models
interaction_models <- list(
  Smoking_Age = glm(
    DepressedBinary ~ SmokingCategory * AgeGroup + Gender + Income_Category,
    data = df,
    family = binomial
  ),
  PhysicalActivity_Age = glm(
    DepressedBinary ~ PhysicalActivityCategory * AgeGroup + Gender + Income_Category,
    data = df,
    family = binomial
  ),
  Sedentary_Age = glm(
    DepressedBinary ~ SedentaryCategory * AgeGroup + Gender + Income_Category,
    data = df,
    family = binomial
  ),
  Alcohol_Age = glm(
    DepressedBinary ~ AlcoholFrequencyCategory * AgeGroup + Gender + Income_Category,
    data = df,
    family = binomial
  ),
  Smoking_PhysicalActivity = glm(
    DepressedBinary ~ SmokingCategory * PhysicalActivityCategory + AgeGroup + Gender + Income_Category,
    data = df,
    family = binomial
  ),
  Smoking_Alcohol = glm(
    DepressedBinary ~ SmokingCategory * AlcoholFrequencyCategory + AgeGroup + Gender + Income_Category,
    data = df,
    family = binomial
  ),
  Age_Income = glm(
    DepressedBinary ~ AgeGroup * Income_Category + SmokingCategory + PhysicalActivityCategory + Gender,
    data = df,
    family = binomial
  ),
  PhysicalActivity_Sedentary = glm(
    DepressedBinary ~ PhysicalActivityCategory * SedentaryCategory + SmokingCategory + AlcoholFrequencyCategory +
      AgeGroup + Gender + Income_Category,
    data = df,
    family = binomial
  ),
  Gender_Behaviors = glm(
    DepressedBinary ~ Gender * (SmokingCategory + AlcoholFrequencyCategory + PhysicalActivityCategory) +
      AgeGroup + Income_Category,
    data = df,
    family = binomial
  ),
  Gender_Income = glm(
    DepressedBinary ~ Gender * Income_Category + SmokingCategory + PhysicalActivityCategory +
      SedentaryCategory + AlcoholFrequencyCategory + AgeGroup,
    data = df,
    family = binomial
  )
)

# Summarize Models
interaction_summaries <- lapply(interaction_models, summary)

# Print Summaries
cat("\nInteraction Model Summaries:\n")
lapply(names(interaction_summaries), function(name) {
  cat("\nModel: ", name, "\n")
  print(interaction_summaries[[name]])
})

# Compare AIC Values for All Interaction Models
aic_values <- sapply(interaction_models, AIC)
cat("\nAIC Values for Interaction Models:\n")
print(aic_values)

# Check Variance Inflation Factor (VIF) for All Interaction Models
vif_values <- lapply(interaction_models, function(model) {
  vif(model)
})

cat("\nVariance Inflation Factor (VIF) for Interaction Models:\n")
lapply(names(vif_values), function(name) {
  cat("\nModel: ", name, "\n")
  print(vif_values[[name]])
})

reduced_model <- glm(
  DepressedBinary ~ PhysicalActivityCategory + SedentaryCategory + 
    SmokingCategory + AgeGroup + Gender + Income_Category, 
  family = binomial, data = df
)
summary(reduced_model) 
cat("\nReduced Model VIF:\n")
print(vif(reduced_model))

full_model <- glm(
  DepressedBinary ~ (Gender + AgeGroup + Income_Category + SmokingCategory +
                       AlcoholFrequencyCategory + PhysicalActivityCategory +
                       SedentaryCategory)^2,
  family = binomial,
  data = df
)

summary(full_model) 
cat("\nFull Model VIF:\n")
print(vif(full_model))
 
# Chosen Final model 
 
simplified_model <- glm(
  DepressedBinary ~ Gender + AgeGroup + SmokingCategory + PhysicalActivityCategory + 
    SedentaryCategory + Income_Category + AlcoholFrequencyCategory,
  family = binomial,
  data = df
)
summary(simplified_model)
cat("\nSimplified Model VIF:\n")
print(vif(simplified_model))

# Other test models 
 
further_simplified_model <- glm(
  DepressedBinary ~ Gender + AgeGroup + SmokingCategory + SedentaryCategory + Income_Category,
  family = binomial,
  data = df
)
summary(further_simplified_model)
cat("\nFurther Simplified Model VIF:\n")
print(vif(further_simplified_model))
