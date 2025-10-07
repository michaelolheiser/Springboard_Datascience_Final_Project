Project Capstone for Springboard's Introduction to Data Science certification course. 
This project was focused on reducing fraud at Moneygram by predicting high risk agent locations and analyzing the drivers. 
As a MoneyGram employee, I used internal SQL databases to gather a data set for this analysis in R Studio. 
Only aggregated statistics are presented. All identfiable and proprietary infromation was kept internal to MoneyGram.
The rest of this README is the text extracted from the Powerpoint presentation. 

# Predicting Social Engineering Fraud
**Michael Olheiser**

---

## Overview
Fraudsters call MoneyGram agents, pretending to be company representatives, and convince them to process a "test" transaction.  
Agents range from large retail stores (e.g., Wal-Mart) to small shops and markets.

### Why This Is a Problem
- **Loss to revenue**  
- **Damage to company reputation**  
- **Increased agent turnover**

### Key Questions
- Where are these agents located?  
- Why do agents fall victim to this fraud?  
- How can we stop it?

---

## What Is Social Engineering Fraud?
- Fraudsters exploit trust and authority to manipulate agents.  
- Common forms: impersonation of technical support or company staff.

---

## Solutions & Challenges
### Solutions
- **Alerting systems**  
- **Enhanced POS security**  
- **Transaction monitoring**  
- **Training\***  

\* Training is proactive — but how can we make it **cost effective**?

### Challenges
- Reactive approaches are **expensive** and **time-consuming**  
- Over **300,000 agents** worldwide  
- Limited resources make universal training infeasible  

---

## Fraud Training Strategies

| Approach | Pros | Cons |
|-----------|------|------|
| **Train All Agents** | Every agent trained; eliminates fraud | Highest cost; time-consuming; unsustainable |
| **Randomly Select Agents** | Random sampling; reduced training costs | Higher fraud costs; may miss high-risk agents |
| **Predictive Modeling** | Identifies agents with highest fraud risk; lower cost; scalable | Requires model development and validation |
| **Operations As-Is** | No initial development or training cost | Most vulnerable; highest fraud cost |

---

## Modeling Process Outline
**Goal:** Build a model that identifies which agents should receive additional fraud training.

### Data Sources
- **MoneyGram Databases:** agent location, software, credit, account blocks  
- **External Data:**  
  - FBI Crime Statistics  
  - US Census (population, education, poverty)

### Scope
- U.S.-based social engineering fraud  
- Data from **2014–2017**  
- Includes transaction data and agent characteristics  

### Hypothesis
> Fraudsters target agents based on population, crime, poverty rate, and education level in the surrounding area.

---

## Data Preparation

### Challenges
- Missing data (FBI, Census)
- Inconsistent formats and IDs
- No fraud data before 2014
- Joining across multiple data sources

### Solutions
- Approximated missing values using algorithmic interpolation  
- Inferred send status from credit and fraud fields  
- Standardized fields across sources  
- Focused on completeness over perfection  
- Grouped and joined data after preprocessing  

---

## Exploratory Data Analysis

### Findings
- Agents with fraud (red dots) cluster near other agents (black dots)  
- No clear geographic preference for fraud attacks  

### Statistical Significance
- Significance tested via **two-tailed T-Test (p < 0.05)**  

### Correlations
- High correlation among population metrics (zip, county, city)  
- Education levels correlated across degree types  
- Crime and poverty correlated  
- These correlations reduce predictive model effectiveness  

---

## Predictive Modeling

### Fields Used
- Poverty rate  
- Agent’s credit amount  
- Account blocks  
- Median age  

### Models Tested
- **Logistic Regression**  
- **Regression Tree**  
- **Random Forest**

### Key Predictors
- **Credit Amount:** Low credit agents may be less experienced.  
- **Account Blocks:** Technical issues make agents more vulnerable to impersonation.  
- **Median Age:** Older populations are more susceptible.  
- **Poverty Rate:** Higher turnover and lower training rates increase fraud risk.  

---

## Choosing the Right Model

### Model Trade-offs
- **Sensitive models:** Catch more fraud but increase training costs (false positives)  
- **Specific models:** Reduce training costs but miss more fraud cases  
- **Accuracy:** May be misleading due to low base fraud rate  

---

## Financial Impact of Models
**Scenario:**  
- 2,044 agents (testing data)  
- Fraud loss per case: **$3,000**  
- Training cost per agent: **$200**

### Case Comparisons
| Case | Description | Total Cost |
|-------|--------------|-------------|
| **1. Operations As-Is** | No new training; 111 fraud cases | $330,000 |
| **2. Train All Agents** | 100% prevention | $408,800 |
| **3. Train Random Agents** | Train half; 55 fraud cases missed | $369,400 |
| **4. Train Model-Selected Agents** | Based on TP, FP, FN from model | Varies by model |

---

## Discussion & Results

- **Random Forest 3** achieved the **lowest total cost**, preventing 77 of 111 fraud cases with minimal false positives.  
- **Logistic Regression 1** caught 102 of 111 fraud cases, but total cost was higher due to false positives.  
- **Model choice** depends on organizational priorities:
  - **Cost-conscious:** Random Forest  
  - **Fraud reduction focus:** Logistic Regression  

### Further Research
- Test different predictor sets per model type  
- Expand dataset beyond 2017  

---

## Next Steps
1. Determine approach: fraud cost vs training cost.  
2. Select appropriate model (sensitive vs. specific).  
3. Run model against new agents.  
4. Train agents flagged as high risk.  
5. Repeat with new data as operations evolve.  

---

## Summary
Predictive modeling can significantly reduce the cost of fraud prevention by targeting high-risk agents for training.  
Balancing sensitivity and specificity is key to optimizing fraud reduction while controlling operational costs.  

