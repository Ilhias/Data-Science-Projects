# Evaluating Predictive Models for Breast Cancer Classification
---

## 1. Problem Context
Early and accurate classification of breast cancer tumours is critical for timely clinical decision-making. Predictive models in health systems must balance classification performance**, interpretability and risk management particularly where false negatives may delay diagnosis and treatment.

This project evaluates different modelling approaches for binary tumour classification, focusing on how model choice impacts decision-making in health contexts rather than simply maximising accuracy.

---

## 2. Data Source
- **Dataset:** Wisconsin Breast Cancer Dataset (scikit-learn)  
- **Observations:** 569  
- **Features:** 30 numeric predictors derived from cell nucleus characteristics  
- **Outcome:** Malignant vs benign tumours  



---

## 3. Methodology
To ensure robust and reproducible evaluation, the dataset was split using stratified sampling:

- **Training set:** 81%  
- **Validation set:** 9%  
- **Test set:** 10%  

**Feature scaling** was performed using `StandardScaler`, fitted exclusively on the training data to prevent data leakage.

### Models Evaluated
- **Artificial Neural Network (ANN)**  
  - Four hidden layers with SELU activation  
  - Hyperparameter tuning for learning rate using Keras Tuner  
- **Logistic Regression** (baseline comparator)  
  - Provides interpretable benchmark commonly used in clinical research  

### Evaluation Metrics
- Accuracy  
- Precision  
- Recall (sensitivity)  
- F1-score  
- Confusion matrices  

---

## 4. Key Results (Test Set)
| Metric | ANN |
|--------|-----|
| Accuracy | 94.7% |
| Recall (Malignant) | 90.5% |
| Precision (Malignant) | 95.0% |
| False Negative Rate | 9.5% |

> **Note:** Recall for malignant cases was prioritised due to the higher clinical cost of false negatives.

---

## 5. Interpretability & Trade-offs
While the ANN achieved high classification performance, it introduces challenges around transparency and explainability, which are critical in health and government contexts. Logistic Regression offers several advantages in these settings:

- Clear coefficient interpretation: Each feature’s impact on the outcome can be directly understood, supporting evidence-based decision-making.
- Easier clinical communication: Results can be explained to clinicians, policymakers, and non-technical stakeholders without requiring deep ML knowledge.
- Greater suitability for policy and governance settings: Transparent models align with compliance, auditability, and ethical standards required in public health.
- Improved trust and accountability: Stakeholders are more likely to adopt recommendations when they can see how predictions are generated.
- Simpler risk management: Easier to identify and mitigate potential errors or biases in the model, ensuring patient safety and ethical responsibility.

In health and government analytics, simpler models may be preferred when marginal performance gains from complex models do not justify the loss of interpretability, particularly when decisions have direct impacts on patient care, resource allocation, or policy planning.
