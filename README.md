# Quantitative-Analysis-of-England-covid-mortality
Statistical analysis of socio-economic factors affecting COVID-19 mortality rates across English local authorities. Includes data processing, exploratory analysis, regression modeling, and an interactive R Shiny dashboard. Developed for MSc Data Science quantitative research project.

## Objectives

- **Analyze Regional Disparities**  
  Quantify how factors like education, health status, and mobility patterns correlate with mortality.

- **Develop Predictive Models**  
  Build and compare multiple regression models (35.8% variance explained) to identify significant predictors.

- **Create Policy Insights**  
  Highlight vulnerable populations through data-driven findings (e.g., low education → 47% higher mortality correlation).

---

## Key Features

### 1. Data Processing & Standardization
- Integrated ONS Census data (296 local authorities) with COVID-19 mortality records.
- Standardized variables (per 1,000 population) for cross-region comparison.
- Resolved data mismatches in `LA_Code` using SQLite.
---
### 2. Exploratory Data Analysis (EDA)
- Identified outliers in mortality rates via boxplots and Q-Q plots.
- Verified normality with **Shapiro-Wilk test** (p = 0.25).
- Automated EDA through an interactive **R Shiny dashboard**.
---
### 3.  Statistical Modeling
####  Correlation Analysis:
- **Low_Qualification** → **+0.47** mortality correlation  
- **Good_Health** → **-0.37** mortality correlation  
  *(via Pearson/Spearman correlation tests)*

####  Principal Component Analysis (PCA):
- Reduced 21 variables to 2 components
- Explained **92% of the variance**

####  Regression Models:
- **Model 1 (Best Fit)** – **35.8% adjusted R²**
  - **Low_Qualification** (p < 0.001)
  - **Adults** (p = 0.037)
  - **Mobility patterns**: Short/Long_Distance (p < 0.05)
---
### 4. Interactive Dashboard

#### Key Features:
- Dynamic visualizations: histograms, boxplots, QQ plots
- Real-time statistical outputs: correlation matrices, PCA, regression
- User-driven regression model customization
---

##  Summary
This project bridges statistical methods with real-world public health outcomes. By integrating official datasets, deploying interactive tools, and providing actionable insights, it serves as a data-driven foundation for understanding and mitigating pandemic impact across England.

---
