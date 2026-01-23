# Predicting Prolonged ICU Stay (≥7 Days) – Health Data Analytics Project

## Overview
This project leverages  clinical data from 42,496 adult ICU patients  to predict prolonged ICU stays (≥7 days) using a combination of classical and modern machine learning techniques. The goal is to identify early predictors from the first 24 hours of patient data to support  resource allocation, patient management, and risk stratification  in critical care.

---

## Data Sources
The dataset includes:
-  Patient demographics  and hospital admissions  
-  ICU outcomes : length of stay, mortality  
-  Hourly physiological data : vitals, labs, Glasgow Coma Scale (GCS) scores  
-  Treatment records : mechanical ventilation, vasopressors, antibiotics  

 Cohort selection:   
- Adults ≥18 years  
- ICU stay ≥1 day  
- One ICU stay per hospital admission

---

## Data Preprocessing
Key steps:
-  Filtering & cleaning:  Removed physiologically implausible values and highly incomplete features  
-  Aggregation:  Summarized first 24-hour vitals, labs, and GCS into mean, min, and max  
-  Imputation:  Median imputation for missing values; missing neurological scores set as GCS=15  
-  Encoding:  One-hot encoding for categorical variables; identifiers and temporal variables removed  
-  Class balancing:  Only 16% of patients had prolonged ICU stay; SMOTE was applied to handle class imbalance

---

## Exploratory Data Analysis (EDA)
-  Continuous variables:  Higher heart and respiratory rates, lower MAP, worse renal function, and lower GCS in prolonged-stay patients  
-  Categorical variables:  Prolonged-stay patients more likely to be emergency admissions and had higher hospital mortality (5.5% vs 3.4%)  
-  Visualizations:  Histograms, boxplots, and correlation heatmaps highlighted feature distributions and low multicollinearity

---

## Modeling Approach
-  Data split:  70% train, 10% validation, 20% test (stratified)  
-  Algorithms tested:  Logistic Regression, Decision Tree, Random Forest, AdaBoost, XGBoost, KNN, MLP Neural Network  
-  Hyperparameter tuning:  GridSearchCV  
-  Evaluation metrics:  Accuracy, Precision, Recall, F1-score, ROC AUC (with focus on minority-class recall)

---

## Model Performance

| Model                | Accuracy | Recall (Prolonged Stay) | Precision | F1-Score | ROC AUC |
|----------------------|----------|-------------------------|-----------|-----------|---------|
| Logistic Regression  | 0.666    | 0.667                   | 0.276     | 0.391     | 0.734   |
| Decision Tree        | 0.730    | 0.398                   | 0.268     | 0.321     | 0.711   |
| Random Forest        | 0.761    | 0.374                   | 0.301     | 0.334     | 0.758   |
| AdaBoost             | 0.688    | 0.565                   | 0.272     | 0.367     | 0.739   |
| XGBoost              | 0.688    | 0.565                   | 0.272     | 0.367     | 0.680   |
| KNN                  | 0.645    | 0.562                   | 0.240     | 0.336     | 0.651   |
| MLP Neural Network   | 0.706    | 0.612                   | 0.297     | 0.400     | 0.727   |

 Insights: 
- MLP achieved highest recall for prolonged ICU stays (0.612)  
- Random Forest had highest overall accuracy (0.761) but poor minority-class sensitivity  
- Logistic Regression remains interpretable and allows clear communication of risk factors

---

## Interpretability & Clinical Implications
- Logistic Regression coefficients indicate key predictors: low GCS, high creatinine, unstable vitals  
- Transparent models support  trust and clinical adoption   
- Trade-off between complex models (higher recall) and interpretability must be considered for high-stakes ICU settings

---

## Conclusion
This project demonstrates that early ICU data can predict prolonged ICU stays offering actionable insights for clinicians and hospital administrators. While neural networks and ensemble methods capture nonlinear patterns, interpretable models like Logistic Regression balance performance and clinical transparency

---

