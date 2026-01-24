############################################################ 
# Importing libraries and global options
############################################################
library(tidyverse)        # data wrangling + ggplot
library(mice)             # multiple imputation
library(arm)              # binnedplot
library(faraway)          # halfnorm
library(pROC)             # ROC, AUC
library(DescTools)        # Pseudo R2, etc.
library(GGally)           # ggpairs, ggcorr
library(ggpubr)           # ggarrange
library(caret)            # confusionMatrix
library(car)              # vif
library(ResourceSelection)# hoslem.test
library(rms)              # val.prob calibration curve
library(broom)            # tidy summaries, if needed

options(scipen = 999)     # nicer number printing
set.seed(123)             # reproducibility

############################################################
# Load datasets 
############################################################
icustays <- read.csv("../mimic_data/ICUSTAYS.csv")
pt_icu_outcome <- read.csv("../mimic_data/pt_icu_outcome.csv")
admissions <- read.csv("../mimic_data/admissions.csv")
patients <- read.csv("../mimic_data/patients.csv")
gcs_hourly <- read.csv("../mimic_data/gcs_hourly.csv")
vitals_hourly <- read.csv("../mimic_data/vitals_hourly.csv")
labs_hourly <- read.csv("../mimic_data/labs_hourly.csv")


# Merge and create a final Dataset 
final_dataset <- icustays %>%
  # Join ICU outcomes
  left_join(
    pt_icu_outcome %>%
      dplyr::select(icustay_id, age_years, los, ttd_days, hosp_deathtime),
    by = "icustay_id"
  ) %>%
  
  # Filter for adults (patients aged over 18 years old)
  filter(age_years >= 18) %>%
  
  # Join hospital admission info
  left_join(
    admissions %>%
      dplyr::select(subject_id, hadm_id, admission_type, admission_location,
                    discharge_location, hospital_expire_flag, diagnosis),
    by = c("subject_id", "hadm_id")
  ) %>%
  
  # Join patient demographics
  left_join(
    patients %>%
      dplyr::select(subject_id, gender, dob, dod, expire_flag),
    by = "subject_id"
  ) %>%
  
  # Apply filter for the first 24 hours 
  # First 24 hours for GCS
  left_join(
    gcs_hourly %>%
      filter(hr >= 0 & hr <= 24) %>%
      group_by(icustay_id) %>%
      summarise(
        mean_gcs     = mean(gcs, na.rm = TRUE),
        mean_gcs_verbal = mean(gcsverbal, na.rm = TRUE)
      ),
    by = "icustay_id"
  ) %>%
  
  
  # First 24 hours on Vitals 
  left_join(
    vitals_hourly %>%
      filter(hr >= 0 & hr <= 24) %>%
      filter(
        meanarterialpressure >= 50 & meanarterialpressure <= 200,
        temperature          >= 32 & temperature          <= 42,
        resprate             >= 5  & resprate             <= 40,
        spo2                 >= 50 & spo2                 <= 100
      ) %>%
      group_by(icustay_id) %>%
      summarise(
        mean_heartrate   = mean(heartrate, na.rm = TRUE),
        mean_map         = mean(meanarterialpressure, na.rm = TRUE),
        mean_resprate    = mean(resprate, na.rm = TRUE),
        mean_temperature = mean(temperature, na.rm = TRUE),
        mean_spo2        = mean(spo2, na.rm = TRUE)
      ),
    by = "icustay_id"
  ) %>%
  
  # First 24 hours on labs 
  left_join(
    labs_hourly %>%
      filter(hr >= 0 & hr <= 24) %>%
      filter(
        creatinine     >= 0.1  & creatinine     <= 10,
        platelets      >= 10   & platelets      <= 1000,
        hemoglobin     >= 5    & hemoglobin     <= 20,
        whitebloodcell >= 0.5  & whitebloodcell <= 50
      ) %>%
      group_by(icustay_id) %>%
      summarise(
        max_lactate      = suppressWarnings(max(lactate, na.rm = TRUE)),
        mean_creatinine  = mean(creatinine, na.rm = TRUE),
        mean_wbc         = mean(whitebloodcell, na.rm = TRUE),
        mean_hemoglobin  = mean(hemoglobin, na.rm = TRUE),
        mean_platelets   = mean(platelets, na.rm = TRUE)
      ),
    by = "icustay_id"
  ) %>%
  
  # Replace inf and -inf with Na, 
  mutate(across(where(is.numeric), ~ ifelse(is.infinite(.), NA, .))) %>%
  
  # Resolve the duplicated los variable 
  mutate(los = coalesce(los.x, los.y)) %>%
  dplyr::select(-los.x, -los.y) %>%
  
  # Filter los to greater than 1 
  filter(los >= 1) %>%
  
  
  dplyr::select(
    subject_id,
    icustay_id,
    first_careunit,
    age_years,
    admission_type,
    hospital_expire_flag,
    gender,
    mean_gcs,
    mean_map,
    mean_temperature,
    mean_resprate,
    mean_wbc,
    los
  )



# Display the dimensions of the final dataset 
print(dim(final_dataset))
# Display the structure of the final dataset 
str(final_dataset)

############################################
# Preprocessing
############################################
# Convert categorical variables to factors

final_dataset <- final_dataset %>%
  mutate(
    first_careunit = factor(
      first_careunit,
      levels = c("MICU", "SICU", "CCU", "CSRU", "TSICU", "NICU"),
      labels = c("Medical ICU", "Surgical ICU", "Coronary Care",
                 "Cardiac Surgery", "Trauma/Surgical", "Neuro ICU")
    ),
    hospital_expire_flag = factor(
      hospital_expire_flag,
      levels = c(0, 1),
      labels = c("Survived", "Died")
    ),
    gender = factor(gender),
    admission_type = factor(admission_type)
  )

# 
print(dim(final_dataset))
str(final_dataset)


# Check for missing varaibles
# identify the percentage of missing per variable
missing_pct <- colSums(is.na(final_dataset)) / nrow(final_dataset) * 100
print(round(missing_pct[missing_pct > 0], 2))

# Missingness pattern
missing_pattern <- mice::md.pattern(final_dataset, plot = FALSE)
print(missing_pattern)

# Multiple imputation using predictive mean matching
mice_imp <- mice(
  final_dataset,
  method    = "pmm",
  m         = 5,
  maxit     = 50,
  seed      = 500,
  printFlag = FALSE
)

# Convergence diagnostics
plot(mice_imp, main = "MICE Convergence Plot")

# Complete first imputed dataset
icu_data_completed <- complete(mice_imp, 1)

# Drop exact duplicate rows if any
icu_data_completed <- icu_data_completed[!duplicated(icu_data_completed), ]

# Check for duplicate ICU stays (partial duplicates)
dup_by_icustay <- icu_data_completed %>%
  group_by(icustay_id) %>%
  summarise(n = n()) %>%
  filter(n > 1)

cat("Number of ICU stays with duplicates:", nrow(dup_by_icustay), "\n")


cat("Number of rows after removing duplicates:", nrow(icu_data_completed), "\n")
cat("Any remaining NA", sum(is.na(icu_data_completed)), "\n")

# ----------------------------------------------------------
# EXPLORATORY DATA ANALYSIS (EDA)
# ----------------------------------------------------------

## Descriptive Tables

# Continuous vars summary
summary(icu_data_completed %>% dplyr::select(where(is.numeric)))

# Categorical tables distribution 
# Gender distribution
t1 <- table(icu_data_completed$gender, exclude = NULL)
print(t1)
print(round(prop.table(t1) * 100, 1))

# ICU type distribution
t2 <- table(icu_data_completed$first_careunit, exclude = NULL)
print(t2)
print(round(prop.table(t2) * 100, 1))

# Hospital outcome (primary outcome)
t3 <- table(icu_data_completed$hospital_expire_flag, exclude = NULL)
print(t3)
print(round(prop.table(t3) * 100, 1))

# Admission type distribution
t4 <- table(icu_data_completed$admission_type, exclude = NULL)
print(t4)
print(round(prop.table(t4) * 100, 1))

# Mortality by ICU type (bivariate relationship)
mortality_by_icu <- table(
  icu_data_completed$first_careunit,
  icu_data_completed$hospital_expire_flag
)
print(mortality_by_icu)
print(round(prop.table(mortality_by_icu, margin = 1) * 100, 1))


#############################################################
## Univariable Plots 
##############################################################

# Histograms for continous variables 
u1 <- ggplot(icu_data_completed, aes(x = age_years)) +
  geom_histogram(fill = "steelblue", color = "black", bins = 30) +
  labs(title = "Distribution of Age", x = "Age (years)", y = "Count") +
  theme_minimal()

u2 <- ggplot(icu_data_completed, aes(x = mean_gcs)) +
  geom_histogram(fill = "steelblue", color = "black", bins = 20) +
  labs(title = "Mean GCS (first 24h)", x = "Mean GCS", y = "Count") +
  theme_minimal()

u3 <- ggplot(icu_data_completed, aes(x = mean_map)) +
  geom_histogram(fill = "steelblue", color = "black", bins = 20) +
  labs(title = "Mean Arterial Pressure", x = "MAP (mmHg)", y = "Count") +
  theme_minimal()

u4 <- ggplot(icu_data_completed, aes(x = mean_temperature)) +
  geom_histogram(fill = "steelblue", color = "black", bins = 20) +
  labs(title = "Mean Temperature", x = "Temperature (°C)", y = "Count") +
  theme_minimal()

u5 <- ggplot(icu_data_completed, aes(x = mean_resprate)) +
  geom_histogram(fill = "steelblue", color = "black", bins = 20) +
  labs(title = "Mean Respiratory Rate", x = "Resp. Rate (breaths/min)", y = "Count") +
  theme_minimal()

u6 <- ggplot(icu_data_completed, aes(x = mean_wbc)) +
  geom_histogram(fill = "steelblue", color = "black", bins = 20) +
  labs(title = "Mean White Blood Cell Count", x = "WBC (×10³/μL)", y = "Count") +
  theme_minimal()

u7 <- ggplot(icu_data_completed, aes(x = los)) +
  geom_histogram(fill = "steelblue", color = "black", bins = 30) +
  labs(title = "ICU Length of Stay", x = "LOS (days)", y = "Count") +
  theme_minimal()

# Combine all continuous variable plots
ggarrange(u1, u2, u3, u4, u5, u6, u7,
          ncol = 3, nrow = 3, labels = "AUTO")



# Bar plots for categorical Variables

u8 <- ggplot(icu_data_completed, aes(x = gender, fill = gender)) +
  geom_bar() +
  labs(title = "Gender Distribution", x = "Gender", y = "Count") +
  theme_minimal() +
  theme(legend.position = "none")

u9 <- ggplot(icu_data_completed, aes(x = first_careunit, fill = first_careunit)) +
  geom_bar() +
  labs(title = "ICU Type Distribution", x = "ICU Type", y = "Count") +
  theme_minimal() +
  theme(legend.position = "none",
        axis.text.x = element_text(angle = 45, hjust = 1))

u10 <- ggplot(icu_data_completed, aes(x = admission_type, fill = admission_type)) +
  geom_bar() +
  labs(title = "Admission Type", x = "Admission Type", y = "Count") +
  theme_minimal() +
  theme(legend.position = "none",
        axis.text.x = element_text(angle = 45, hjust = 1))

u11 <- ggplot(icu_data_completed, aes(x = hospital_expire_flag, fill = hospital_expire_flag)) +
  geom_bar() +
  labs(title = "Hospital Outcome", x = "Outcome", y = "Count") +
  scale_fill_manual(values = c("Survived" = "lightgreen", "Died" = "coral")) +
  theme_minimal() +
  theme(legend.position = "none")

# Combine categorical plots
ggarrange(u8, u9, u10, u11,
          ncol = 2, nrow = 2, labels = "AUTO")


## Continuous Predictors vs Mortality Outcome
bplots <- list(
  ggplot(icu_data_completed, aes(hospital_expire_flag, age_years, fill = hospital_expire_flag)) +
    geom_boxplot() +
    labs(title = "Age by Outcome", x = "Outcome", y = "Age (years)") +
    theme_minimal(),
  
  ggplot(icu_data_completed, aes(hospital_expire_flag, mean_gcs, fill = hospital_expire_flag)) +
    geom_boxplot() +
    labs(title = "GCS by Outcome", x = "Outcome", y = "Mean GCS") +
    theme_minimal(),
  
  ggplot(icu_data_completed, aes(hospital_expire_flag, mean_map, fill = hospital_expire_flag)) +
    geom_boxplot() +
    labs(title = "MAP by Outcome", x = "Outcome", y = "MAP (mmHg)") +
    theme_minimal(),
  
  ggplot(icu_data_completed, aes(hospital_expire_flag, mean_temperature, fill = hospital_expire_flag)) +
    geom_boxplot() +
    labs(title = "Temperature by Outcome", x = "Outcome", y = "Temperature (°C)") +
    theme_minimal(),
  
  ggplot(icu_data_completed, aes(hospital_expire_flag, mean_resprate, fill = hospital_expire_flag)) +
    geom_boxplot() +
    labs(title = "Respiratory Rate by Outcome", x = "Outcome", y = "Resp. Rate") +
    theme_minimal(),
  
  ggplot(icu_data_completed, aes(hospital_expire_flag, mean_wbc, fill = hospital_expire_flag)) +
    geom_boxplot() +
    labs(title = "WBC by Outcome", x = "Outcome", y = "WBC (×10³/μL)") +
    theme_minimal(),
  
  ggplot(icu_data_completed, aes(hospital_expire_flag, los, fill = hospital_expire_flag)) +
    geom_boxplot() +
    labs(title = "LOS by Outcome", x = "Outcome", y = "LOS (days)") +
    theme_minimal()
)

ggarrange(plotlist = bplots, ncol = 3, nrow = 3, labels = "AUTO")



# Continuous Predictors by ICU Type
icuplots <- list(
  ggplot(icu_data_completed, aes(first_careunit, age_years, fill = first_careunit)) +
    geom_boxplot() +
    labs(title = "Age by ICU Type", x = "ICU Type", y = "Age (years)") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)),
  
  ggplot(icu_data_completed, aes(first_careunit, mean_gcs, fill = first_careunit)) +
    geom_boxplot() +
    labs(title = "GCS by ICU Type", x = "ICU Type", y = "Mean GCS") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)),
  
  ggplot(icu_data_completed, aes(first_careunit, mean_map, fill = first_careunit)) +
    geom_boxplot() +
    labs(title = "MAP by ICU Type", x = "ICU Type", y = "MAP (mmHg)") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)),
  
  ggplot(icu_data_completed, aes(first_careunit, los, fill = first_careunit)) +
    geom_boxplot() +
    labs(title = "LOS by ICU Type", x = "ICU Type", y = "LOS (days)") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
)

ggarrange(plotlist = icuplots, ncol = 2, nrow = 2, labels = "AUTO")

# Categorical Predictors vs Mortality
c1 <- ggplot(icu_data_completed, aes(x = gender, fill = hospital_expire_flag)) +
  geom_bar(position = "fill") +
  labs(title = "Mortality by Gender", x = "Gender", y = "Proportion") +
  scale_y_continuous(labels = scales::percent) +
  scale_fill_manual(values = c("Survived" = "lightgreen", "Died" = "coral"), name = "Outcome") +
  theme_minimal()

c2 <- ggplot(icu_data_completed, aes(x = first_careunit, fill = hospital_expire_flag)) +
  geom_bar(position = "fill") +
  labs(title = "Mortality by ICU Type", x = "ICU Type", y = "Proportion") +
  scale_y_continuous(labels = scales::percent) +
  scale_fill_manual(values = c("Survived" = "lightgreen", "Died" = "coral"), name = "Outcome") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

c3 <- ggplot(icu_data_completed, aes(x = admission_type, fill = hospital_expire_flag)) +
  geom_bar(position = "fill") +
  labs(title = "Mortality by Admission Type", x = "Admission Type", y = "Proportion") +
  scale_y_continuous(labels = scales::percent) +
  scale_fill_manual(values = c("Survived" = "lightgreen", "Died" = "coral"), name = "Outcome") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggarrange(c1, c2, c3, ncol = 2, nrow = 2, labels = "AUTO")


# Correlations and Pairwise Relationships
icu_data_completed %>%
  dplyr::select(age_years, mean_gcs, mean_map, mean_temperature,
                mean_resprate, mean_wbc, los) %>%
  ggcorr(label = TRUE, label_round = 2, label_size = 3,
         size = 3, hjust = 0.75, layout.exp = 1) +
  labs(title = "Correlation Matrix of Continuous Variables")

icu_data_completed %>%
  dplyr::select(age_years, mean_gcs, mean_map, mean_temperature,
                mean_resprate, mean_wbc, los, hospital_expire_flag) %>%
  ggpairs(aes(color = hospital_expire_flag, alpha = 0.5),
          title = "Pairwise Relationships by Hospital Outcome")


###############################################################
# Model building
###############################################################
icu_data_completed <- icu_data_completed %>%
  mutate(outcome_num = ifelse(hospital_expire_flag == "Died", 1, 0))

logit_null <- glm(hospital_expire_flag ~ 1,
                  data = icu_data_completed,
                  family = binomial(link = "logit"))

logit_first_careunit <- glm(hospital_expire_flag ~ first_careunit,
                            data = icu_data_completed,
                            family = binomial(link = "logit"))

anova_test1 <- anova(logit_null, logit_first_careunit, test = "Chisq")
print(anova_test1)

logit_demo <- glm(hospital_expire_flag ~ first_careunit + age_years + gender,
                  data = icu_data_completed,
                  family = binomial(link = "logit"))

anova_test2 <- anova(logit_first_careunit, logit_demo, test = "Chisq")
print(anova_test2)

logit_demo_labs <- glm(hospital_expire_flag ~ first_careunit + age_years + gender +
                         mean_gcs + mean_map + mean_resprate +
                         mean_temperature + mean_wbc,
                       data = icu_data_completed,
                       family = binomial(link = "logit"))

anova_test3 <- anova(logit_demo, logit_demo_labs, test = "Chisq")
print(anova_test3)

logit_full <- glm(hospital_expire_flag ~ first_careunit + age_years + gender +
                    mean_gcs + mean_map + mean_resprate +
                    mean_temperature + mean_wbc +
                    admission_type + los,
                  data = icu_data_completed,
                  family = binomial(link = "logit"))

anova_test4 <- anova(logit_demo_labs, logit_full, test = "Chisq")
print(anova_test4)


logit_full_no_los <- glm(hospital_expire_flag ~ first_careunit +
                           age_years + gender +
                           mean_map + mean_resprate +
                           mean_temperature + mean_wbc +
                           admission_type,
                         data = icu_data_completed,
                         family = binomial(link = "logit"))

models <- list(
  null              = logit_null,
  unit_only         = logit_first_careunit,
  demo              = logit_demo,
  demo_labs         = logit_demo_labs,
  full              = logit_full,
  full_no_loss  = logit_full_no_los
)

model_comparison <- data.frame(
  Model     = names(models),
  AIC       = sapply(models, AIC),
  BIC       = sapply(models, BIC),
  Deviance  = sapply(models, deviance)
)
print(model_comparison)


# ----------------------------------------------------------
# 8. Effect estimates (adjusted ORs)
# ----------------------------------------------------------

coefs <- coef(logit_full)
ci    <- confint(logit_full)

results_table <- data.frame(
  Variable   = names(coefs),
  OR         = exp(coefs),
  CI_Lower   = exp(ci[,1]),
  CI_Upper   = exp(ci[,2]),
  P_Value    = coef(summary(logit_full))[,4]
) %>%
  mutate(across(where(is.numeric), ~ round(., 3)))

print(results_table)

logit_no_icu <- glm(hospital_expire_flag ~ age_years + gender + mean_gcs +
                      mean_map + mean_resprate + mean_temperature +
                      mean_wbc + admission_type + los,
                    data = icu_data_completed,
                    family = binomial(link = "logit"))

anova_icu_effect <- anova(logit_no_icu, logit_full, test = "Chisq")
print(anova_icu_effect)

# Safety for prediction:
# make sure new_data uses same factor levels as training data
new_data <- with(icu_data_completed,
                 data.frame(
                   first_careunit   = levels(first_careunit),
                   age_years        = median(age_years, na.rm = TRUE),
                   gender           = names(sort(table(gender), decreasing = TRUE))[1],
                   mean_gcs         = median(mean_gcs, na.rm = TRUE),
                   mean_map         = median(mean_map, na.rm = TRUE),
                   mean_resprate    = median(mean_resprate, na.rm = TRUE),
                   mean_temperature = median(mean_temperature, na.rm = TRUE),
                   mean_wbc         = median(mean_wbc, na.rm = TRUE),
                   admission_type   = names(sort(table(admission_type), decreasing = TRUE))[1],
                   los              = median(los, na.rm = TRUE)
                 ))

# Reapply factor levels to match model
new_data$first_careunit <- factor(
  new_data$first_careunit,
  levels = levels(icu_data_completed$first_careunit)
)
new_data$gender <- factor(
  new_data$gender,
  levels = levels(icu_data_completed$gender)
)
new_data$admission_type <- factor(
  new_data$admission_type,
  levels = levels(icu_data_completed$admission_type)
)

new_data <- new_data %>%
  filter(first_careunit %in% levels(logit_full$model$first_careunit))

new_data$pred_prob <- predict(logit_full,
                              newdata = new_data,
                              type = "response")
print(new_data)


# ----------------------------------------------------------
# 9. Model diagnostics / influence / multicollinearity
# ----------------------------------------------------------

vif_values <- car::vif(logit_full)
print(round(vif_values, 2))

binnedplot(
  x    = predict(logit_full, type = "response"),
  y    = residuals(logit_full, type = "response"),
  main = "Binned Residual Plot",
  xlab = "Predicted Probability",
  ylab = "Average Residual"
)


halfnorm(
  hatvalues(logit_full),
  main = "Half-Normal Plot of Leverage"
)

cooks_d <- cooks.distance(logit_full)
plot(cooks_d,
     type = "h",
     main = "Cook's Distance",
     ylab = "Cook's Distance",
     xlab = "Observation Index")
abline(h = 4 / (nrow(icu_data_completed) - length(coef(logit_full))),
       lty = 2, col = "red")

influential_obs <- which(cooks_d > 4 / nrow(icu_data_completed))
cat("Number of influential observations:", length(influential_obs), "\n")

termplot(logit_full, partial.resid = TRUE, smooth = panel.smooth,
         main = "Term plots with partial residuals")

suspect_rows <- icu_data_completed[c(1151, 11948), ]
suspect_rows


# ----------------------------------------------------------
# 10. Model performance (discrimination & calibration)
# ----------------------------------------------------------

icu_data_completed <- icu_data_completed %>%
  mutate(
    pred_prob    = predict(logit_full, type = "response"),
    pred_outcome = factor(
      ifelse(pred_prob < 0.5, "Survived", "Died"),
      levels = c("Survived", "Died")
    )
  )


conf_matrix <- table(
  Observed  = icu_data_completed$hospital_expire_flag,
  Predicted = icu_data_completed$pred_outcome
)
cat("\n=== Confusion Matrix (0.5 threshold) ===\n")
print(conf_matrix)

tn <- conf_matrix[1,1]
fp <- conf_matrix[1,2]
fn <- conf_matrix[2,1]
tp <- conf_matrix[2,2]

accuracy    <- (tp + tn) / sum(conf_matrix)
sensitivity <- tp / (tp + fn)
specificity <- tn / (tn + fp)
precision   <- tp / (tp + fp)
f1_score    <- 2 * (precision * sensitivity) / (precision + sensitivity)

cat("\nClassification Metrics (threshold = 0.5):\n")
cat("Accuracy:", round(accuracy, 4), "\n")
cat("Sensitivity (Recall):", round(sensitivity, 4), "\n")
cat("Specificity:", round(specificity, 4), "\n")
cat("Precision (PPV):", round(precision, 4), "\n")
cat("F1 Score:", round(f1_score, 4), "\n")

brier_interaction <- mean(
  (icu_data_completed$pred_prob - icu_data_completed$outcome_num)^2
)
cat("Brier Score:", round(brier_interaction, 4), "\n")

roc_obj <- roc(
  icu_data_completed$hospital_expire_flag,
  icu_data_completed$pred_prob,
  levels = c("Survived", "Died"),
  direction = "<"
)

auc_value <- auc(roc_obj)
cat("AUC:", round(auc_value, 4), "\n")

plot(
  roc_obj,
  col = "darkred",
  lwd = 2,
  legacy.axes = TRUE,
  main = "ROC Curve: Logistic Regression Model",
  xlab = "1 - Specificity (False Positive Rate)",
  ylab = "Sensitivity (True Positive Rate)"
)
abline(a = 0, b = 1, lty = 2, col = "gray")
text(0.6, 0.2, paste("AUC =", round(auc_value, 3)), cex = 1.2)

cat("\n=== Pseudo R-squared Measures ===\n")
pseudo_r2 <- PseudoR2(logit_full, which = "all")
print(round(pseudo_r2, 4))

cat("\n=== Hosmer-Lemeshow Test ===\n")
icu_data_completed$decile <- cut(
  icu_data_completed$pred_prob,
  breaks = quantile(icu_data_completed$pred_prob,
                    probs = seq(0, 1, 0.1)),
  include.lowest = TRUE,
  labels = 1:10
)

hl_table <- icu_data_completed %>%
  group_by(decile) %>%
  summarise(
    observed_died = sum(hospital_expire_flag == "Died"),
    expected_died = sum(pred_prob),
    n             = n(),
    .groups = "drop"
  )
print(hl_table)

ResourceSelection::hoslem.test(
  x = icu_data_completed$outcome_num,
  y = icu_data_completed$pred_prob,
  g = 10
)


